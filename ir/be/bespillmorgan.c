/*
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bespillmorgan.h"

#include "bechordal_t.h"
#include "bespill.h"
#include "belive_t.h"
#include "irgwalk.h"
#include "besched.h"
#include "beutil.h"
#include "irloop_t.h"
#include "irgraph_t.h"
#include "irprintf.h"
#include "obstack.h"

#include "bespillbelady.h"
#include "beverify.h"

#define DBG_LIVE		1
#define DBG_LOOPANA		2
#define DBG_PRESSURE	4
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct morgan_env {
	const be_chordal_env_t *cenv;
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	ir_graph *irg;
	struct obstack obst;
	/** maximum safe register pressure */
	int registers_available;

	spill_env_t *senv;

	set *loop_attr_set;
	set *block_attr_set;
} morgan_env_t;

typedef struct loop_edge {
	ir_node *block;
	int pos;
} loop_edge_t;

typedef struct loop_attr {
	const ir_loop *loop;
	set *out_edges;
	set *in_edges;
	/** The set of all values that are live in the loop but not used in the loop */
	bitset_t *livethrough_unused;
} loop_attr_t;

typedef struct block_attr {
	const ir_node *block;
	/** set of all values that are live in the block but not used in the block */
	bitset_t *livethrough_unused;
} block_attr_t;

//---------------------------------------------------------------------------

static int loop_edge_cmp(const void* p1, const void* p2, size_t s) {
	loop_edge_t *e1 = (loop_edge_t*) p1;
	loop_edge_t *e2 = (loop_edge_t*) p2;

	return e1->block != e2->block || e1->pos != e2->pos;
}

static int loop_attr_cmp(const void *e1, const void *e2, size_t s) {
	loop_attr_t *la1 = (loop_attr_t*) e1;
	loop_attr_t *la2 = (loop_attr_t*) e2;

	return la1->loop != la2->loop;
}

static int block_attr_cmp(const void *e1, const void *e2, size_t s) {
	block_attr_t *b1 = (block_attr_t*) e1;
	block_attr_t *b2 = (block_attr_t*) e2;

	return b1->block != b2->block;
}

static INLINE int loop_attr_hash(const loop_attr_t *a) {
	return HASH_PTR(a->loop);
}

static INLINE int block_attr_hash(const block_attr_t *b) {
	return HASH_PTR(b->block);
}

static INLINE int loop_edge_hash(const loop_edge_t *e) {
	return HASH_PTR(e->block) ^ (e->pos * 31);
}

static INLINE loop_attr_t *get_loop_attr(morgan_env_t *env, const ir_loop *loop) {
	loop_attr_t l_attr, *res;
	int hash;
	l_attr.loop = loop;

	hash = loop_attr_hash(&l_attr);
	res = set_find(env->loop_attr_set, &l_attr, sizeof(l_attr), hash);

	// create new loop_attr if none exists yet
	if (res == NULL) {
		l_attr.out_edges = new_set(loop_edge_cmp, 1);
		l_attr.in_edges = new_set(loop_edge_cmp, 1);
		l_attr.livethrough_unused = bitset_obstack_alloc(&env->obst, get_irg_last_idx(env->irg));
		res = set_insert(env->loop_attr_set, &l_attr, sizeof(l_attr), hash);
	}

	return res;
}

static INLINE block_attr_t *get_block_attr(morgan_env_t *env, const ir_node *block) {
	block_attr_t b_attr, *res;
	int hash;
	b_attr.block = block;

	hash = block_attr_hash(&b_attr);
	res = set_find(env->block_attr_set, &b_attr, sizeof(b_attr), hash);

	if(res == NULL) {
		b_attr.livethrough_unused = bitset_obstack_alloc(&env->obst, get_irg_last_idx(env->irg));
		res = set_insert(env->block_attr_set, &b_attr, sizeof(b_attr), hash);
	}

	return res;
}

//---------------------------------------------------------------------------

static INLINE int consider_for_spilling(const arch_env_t *env, const arch_register_class_t *cls, const ir_node *node) {
	if(!arch_irn_has_reg_class(env, node, -1, cls))
		return 0;

	return !(arch_irn_get_flags(env, node) & (arch_irn_flags_ignore | arch_irn_flags_dont_spill));
}

/**
 * Determine edges going out of a loop (= edges that go to a block that is not inside
 * the loop or one of its subloops)
 */
static INLINE void construct_loop_edges(ir_node* block, void* e) {
	morgan_env_t *env = (morgan_env_t*) e;
	int n_cfgpreds = get_Block_n_cfgpreds(block);
	int i;
	ir_loop* loop = get_irn_loop(block);
	loop_attr_t *loop_attr = get_loop_attr(env, loop);
	DBG((dbg, DBG_LOOPANA, "Loop for %+F: %d (depth %d)\n", block, loop->loop_nr, loop->depth));

	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node* cfgpred = get_Block_cfgpred(block, i);
		ir_node* cfgpred_block = get_nodes_block(cfgpred);
		ir_loop* cfgpred_loop = get_irn_loop(cfgpred_block);
		loop_attr_t *outedges = get_loop_attr(env, cfgpred_loop);

		if(cfgpred_loop == loop)
			continue;

		// is it an edge into the loop?
		if(get_loop_depth(loop) > get_loop_depth(cfgpred_loop)) {
			loop_edge_t edge;
			edge.block = block;
			edge.pos = i;
			DBG((dbg, DBG_LOOPANA, "Loop in edge from %+F (loop %d) to %+F (loop %d)\n", cfgpred_block, get_loop_loop_nr(cfgpred_loop), block, get_loop_loop_nr(loop)));
			set_insert(loop_attr->in_edges, &edge, sizeof(edge), loop_edge_hash(&edge));
		} else {
			ir_loop *p_loop = cfgpred_loop;
			while(get_loop_depth(p_loop) > get_loop_depth(loop)) {
				p_loop = get_loop_outer_loop(p_loop);
			}
			if(p_loop != loop) {
				loop_edge_t edge;
				edge.block = block;
				edge.pos = i;
				DBG((dbg, DBG_LOOPANA, "Loop in edge from %+F (loop %d) to %+F (loop %d)\n", cfgpred_block, get_loop_loop_nr(cfgpred_loop), block, get_loop_loop_nr(loop)));
				set_insert(loop_attr->in_edges, &edge, sizeof(edge), loop_edge_hash(&edge));
			}
		}

		// an edge out of the loop?
		if(get_loop_depth(cfgpred_loop) >= get_loop_depth(loop)) {
			loop_edge_t edge;
			edge.block = block;
			edge.pos = i;
			DBG((dbg, DBG_LOOPANA, "Loop out edge from %+F (loop %d) to %+F\n", cfgpred_block, cfgpred_loop->loop_nr, block));
			set_insert(outedges->out_edges, &edge, sizeof(edge), loop_edge_hash(&edge));
		} else {
			ir_loop *o_loop = loop;

			// we might jump in the middle of another inner loop which is not inside
			// our loop (happens for irreducible graphs). This would be a
			// real out edge then.
			while(get_loop_depth(o_loop) > get_loop_depth(cfgpred_loop)) {
				o_loop = get_loop_outer_loop(o_loop);
			}

			if(cfgpred_loop != o_loop) {
				loop_edge_t edge;
				edge.block = block;
				edge.pos = i;
				DBG((dbg, DBG_LOOPANA, "Loop out edge from %+F (loop %d) to %+F (into jump)\n", cfgpred_block, cfgpred_loop->loop_nr, block));
				set_insert(outedges->out_edges, &edge, sizeof(edge), loop_edge_hash(&edge));
			}
		}
	}
}

static void free_loop_edges(morgan_env_t *env) {
	loop_attr_t *l_attr;

	for(l_attr = set_first(env->loop_attr_set); l_attr != NULL; l_attr = set_next(env->loop_attr_set)) {
		del_set(l_attr->out_edges);
		del_set(l_attr->in_edges);
	}
}

/**
 * Debugging help, shows all nodes in a (node-)bitset
 */
static void show_nodebitset(ir_graph* irg, const bitset_t* bitset) {
	int i;

	bitset_foreach(bitset, i) {
		ir_node* node = get_idx_irn(irg, i);
		DBG((dbg, DBG_LIVE, "\t%+F\n", node));
	}
}

/**
 * Construct the livethrough unused set for a block
 */
static bitset_t *construct_block_livethrough_unused(morgan_env_t* env, const ir_node* block) {
	block_attr_t *block_attr = get_block_attr(env, block);
	ir_node *node;
	int i;

	DBG((dbg, DBG_LIVE, "Processing block %d\n", get_irn_node_nr(block)));
	// copy all live-outs into the livethrough_unused set
	be_lv_foreach(env->cenv->lv, block, be_lv_state_in | be_lv_state_out, i) {
		ir_node *irn = be_lv_get_irn(env->cenv->lv, block, i);
		int node_idx;

		/*
		if(!live_is_in(li) || !live_is_out(li))
			continue;
		*/
		if(!consider_for_spilling(env->arch, env->cls, irn))
			continue;

		node_idx = get_irn_idx(irn);
		bitset_set(block_attr->livethrough_unused, node_idx);
	}

	/*
	 * All values that are used within the block are not unused (and therefore not
	 * livethrough_unused)
	 */
	sched_foreach(block, node) {
		int i, arity;

		for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			int idx = get_irn_idx(get_irn_n(node, i));
			bitset_clear(block_attr->livethrough_unused, idx);
		}
	}

	show_nodebitset(env->irg, block_attr->livethrough_unused);
	return block_attr->livethrough_unused;
}

/**
 * Construct the livethrough unused set for a loop (and all its subloops+blocks)
 */
static bitset_t *construct_loop_livethrough_unused(morgan_env_t *env, const ir_loop *loop) {
	int i;
	loop_attr_t* loop_attr = get_loop_attr(env, loop);

	DBG((dbg, DBG_LIVE, "Processing Loop %d\n", loop->loop_nr));
	assert(get_loop_n_elements(loop) > 0);
	for(i = 0; i < get_loop_n_elements(loop); ++i) {
		loop_element elem = get_loop_element(loop, i);
		switch (*elem.kind) {
		case k_ir_node: {
			bitset_t *livethrough_block_unused;
			assert(is_Block(elem.node));
			livethrough_block_unused = construct_block_livethrough_unused(env, elem.node);
			if(i == 0) {
				bitset_copy(loop_attr->livethrough_unused, livethrough_block_unused);
			} else {
				bitset_and(loop_attr->livethrough_unused, livethrough_block_unused);
			}
			break;
		}
		case k_ir_loop: {
			bitset_t *livethrough_son_unused;

			livethrough_son_unused = construct_loop_livethrough_unused(env, elem.son);
			if(i == 0) {
				bitset_copy(loop_attr->livethrough_unused, livethrough_son_unused);
			} else {
				bitset_and(loop_attr->livethrough_unused, livethrough_son_unused);
			}
			break;
		}
	    default:
			assert(0);
			break;
		}
    }
	DBG((dbg, DBG_LIVE, "Done with loop %d\n", loop->loop_nr));

	// remove all unused livethroughs that are remembered for this loop from child loops and blocks
	for(i = 0; i < get_loop_n_elements(loop); ++i) {
		const loop_element elem = get_loop_element(loop, i);

		if(*elem.kind == k_ir_loop) {
			loop_attr_t *son_attr = get_loop_attr(env, elem.son);
			bitset_andnot(son_attr->livethrough_unused, loop_attr->livethrough_unused);

			DBG((dbg, DBG_LIVE, "Livethroughs for loop %d:\n", loop->loop_nr));
			show_nodebitset(env->irg, son_attr->livethrough_unused);
		} else if(*elem.kind == k_ir_node) {
			block_attr_t *block_attr = get_block_attr(env, elem.node);
			bitset_andnot(block_attr->livethrough_unused, loop_attr->livethrough_unused);

			DBG((dbg, DBG_LIVE, "Livethroughs for block %+F\n", elem.node));
			show_nodebitset(env->irg, block_attr->livethrough_unused);
		} else {
			assert(0);
		}
	}

	return loop_attr->livethrough_unused;
}

/*---------------------------------------------------------------------------*/

static int reduce_register_pressure_in_block(morgan_env_t *env, const ir_node* block, int loop_unused_spills_possible) {
	int pressure;
	ir_node *irn;
	int max_pressure = 0;
	int spills_needed;
	int loop_unused_spills_needed;
	block_attr_t *block_attr = get_block_attr(env, block);
	int block_unused_spills_possible = bitset_popcnt(block_attr->livethrough_unused);
	int unused_spills_possible = loop_unused_spills_possible + block_unused_spills_possible;
	pset *live_nodes = pset_new_ptr_default();

	be_liveness_end_of_block(env->cenv->lv, env->arch, env->cls, block, live_nodes);
	pressure = pset_count(live_nodes);

	DBG((dbg, DBG_LIVE, "Reduce pressure to %d In Block %+F:\n", env->registers_available, block));

	/**
	 * Walk over all irns in the schedule and check register pressure for each of them
	 */
	sched_foreach_reverse(block, irn) {
		// do we need more spills than possible with unused libethroughs?
		int spills_needed = pressure - env->registers_available - unused_spills_possible;
		if(spills_needed > 0) {
			DBG((dbg, DBG_PRESSURE, "\tWARNING %d more spills needed at %+F\n", spills_needed, irn));
			// TODO further spills needed
			//assert(0);
		}
		if(pressure > max_pressure) {
			max_pressure = pressure;
		}

		/* Register pressure is only important until we reach the first phi (the rest of the block
		 * will only be phis.)
		 */
		if(is_Phi(irn))
			break;

		// update pressure
		be_liveness_transfer(env->arch, env->cls, irn, live_nodes);
		pressure = pset_count(live_nodes);
	}

	DBG((dbg, DBG_PRESSURE, "\tMax Pressure in %+F: %d\n", block, max_pressure));

	/*
	 * Calculate number of spills from loop_unused_spills_possible that we want to use,
	 * and spill unused livethroughs from the block if we still don't have enough registers
	 */
	spills_needed = max_pressure - env->registers_available;
	if(spills_needed < 0) {
		loop_unused_spills_needed = 0;
	} else if(spills_needed > loop_unused_spills_possible) {
		int i, spills;
		int block_unused_spills_needed;

		loop_unused_spills_needed = loop_unused_spills_possible;
		block_unused_spills_needed = spills_needed - loop_unused_spills_possible;
		if(block_unused_spills_needed > block_unused_spills_possible) {
			block_unused_spills_needed = block_unused_spills_possible;
		}

		spills = 0;
		/*
		 * Spill/Reload unused livethroughs from the block
		 */
		bitset_foreach(block_attr->livethrough_unused, i) {
			ir_node *to_spill;
			const ir_edge_t *edge;

			if(spills >= block_unused_spills_needed)
				break;

			to_spill = get_idx_irn(env->irg, i);
			foreach_block_succ(block, edge) {
				DBG((dbg, DBG_PRESSURE, "Spilling node %+F around block %+F\n", to_spill, block));
				be_add_reload_on_edge(env->senv, to_spill, edge->src, edge->pos);
			}
			spills++;
		}
	} else {
		loop_unused_spills_needed = spills_needed;
	}

	del_pset(live_nodes);

	DBG((dbg, DBG_PRESSURE, "Unused spills for Block %+F needed: %d\n", block, loop_unused_spills_needed));
	return loop_unused_spills_needed;
}

/**
 * Reduce register pressure in a loop
 *
 * @param unused_spills_possible	Number of spills from livethrough_unused variables possible in outer loops
 * @return							Number of spills of livethrough_unused variables needed in outer loops
 */
static int reduce_register_pressure_in_loop(morgan_env_t *env, const ir_loop *loop, int outer_spills_possible) {
	int i;
	loop_attr_t* loop_attr = get_loop_attr(env, loop);
	int spills_needed = 0;
	int spills_possible = outer_spills_possible + bitset_popcnt(loop_attr->livethrough_unused);
	int outer_spills_needed;

	DBG((dbg, DBG_PRESSURE, "Reducing Pressure in loop %d\n", loop->loop_nr));
	for(i = 0; i < get_loop_n_elements(loop); ++i) {
		loop_element elem = get_loop_element(loop, i);
		switch (*elem.kind) {
		case k_ir_node: {
			int needed;
			assert(is_Block(elem.node));
			needed = reduce_register_pressure_in_block(env, elem.node, spills_possible);
			assert(needed <= spills_possible);
			if(needed > spills_needed)
				spills_needed = needed;
			break;
		}
		case k_ir_loop: {
			int needed = reduce_register_pressure_in_loop(env, elem.son, spills_possible);
			assert(needed <= spills_possible);
			if(needed > spills_needed)
				spills_needed = needed;
			break;
		}
	    default:
			assert(0);
			break;
		}
    }

	/* calculate number of spills needed in outer loop and spill
	 * unused livethrough nodes around this loop
	 */
	if(spills_needed > outer_spills_possible) {
		int spills_to_place;
		outer_spills_needed = outer_spills_possible;
		spills_needed -= outer_spills_possible;

		spills_to_place = spills_needed;

		bitset_foreach(loop_attr->livethrough_unused, i) {
			loop_edge_t *edge;
			ir_node *to_spill = get_idx_irn(env->irg, i);

			for(edge = set_first(loop_attr->out_edges); edge != NULL; edge = set_next(loop_attr->out_edges)) {
				DBG((dbg, DBG_PRESSURE, "Spilling node %+F around loop %d\n", to_spill, loop->loop_nr));
				be_add_reload_on_edge(env->senv, to_spill, edge->block, edge->pos);
			}

			spills_to_place--;
			if(spills_to_place <= 0) {
				break;
			}
		}
	} else {
		outer_spills_needed = spills_needed;
	}

	return outer_spills_needed;
}

void be_spill_morgan(const be_chordal_env_t *chordal_env) {
	morgan_env_t env;

	FIRM_DBG_REGISTER(dbg, "ir.be.spillmorgan");
	//firm_dbg_set_mask(dbg, DBG_LOOPANA | DBG_PRESSURE);

	env.cenv = chordal_env;
	env.arch = chordal_env->birg->main_env->arch_env;
	env.irg = chordal_env->irg;
	env.cls = chordal_env->cls;
	env.senv = be_new_spill_env(chordal_env);
	DEBUG_ONLY(be_set_spill_env_dbg_module(env.senv, dbg);)

	obstack_init(&env.obst);

	env.registers_available = arch_count_non_ignore_regs(env.arch, env.cls);

	env.loop_attr_set = new_set(loop_attr_cmp, 5);
	env.block_attr_set = new_set(block_attr_cmp, 20);

	/*-- Part1: Analysis --*/
	be_liveness_recompute(chordal_env->lv);

	/* construct control flow loop tree */
	construct_cf_backedges(chordal_env->irg);

	/* construct loop out edges and livethrough_unused sets for loops and blocks */
	irg_block_walk_graph(chordal_env->irg, NULL, construct_loop_edges, &env);
	construct_loop_livethrough_unused(&env, get_irg_loop(env.irg));

	/*-- Part2: Transformation --*/

	/* spill unused livethrough values around loops and blocks where
	 * the pressure is too high
	 */
	reduce_register_pressure_in_loop(&env, get_irg_loop(env.irg), 0);

	/* Place copies for spilled phis */
	be_place_copies(env.senv);
	/* Insert real spill/reload nodes and fix usages */
	be_insert_spills_reloads(env.senv);

	/* Verify the result */
	if (chordal_env->opts->vrfy_option == BE_CH_VRFY_WARN) {
		be_verify_schedule(env.irg);
	} else if (chordal_env->opts->vrfy_option == BE_CH_VRFY_ASSERT) {
		assert(be_verify_schedule(env.irg));
	}

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_SPILL)
		be_dump(env.irg, "-spillmorgan", dump_ir_block_graph_sched);

	/* cleanup */
	free_loop_edges(&env);
	del_set(env.loop_attr_set);
	del_set(env.block_attr_set);

	/* fix the remaining places with too high register pressure with beladies algorithm */

	/* we have to remove dead nodes from schedule to not confuse liveness calculation */
	be_remove_dead_nodes_from_schedule(env.irg);
	be_liveness_recompute(chordal_env->lv);

	be_spill_belady_spill_env(chordal_env, env.senv);

	be_delete_spill_env(env.senv);
	obstack_free(&env.obst, NULL);
}
