/*
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bespillmorgan.h"

#include "bechordal.h"
#include "bechordal_t.h"
#include "bespill.h"
#include "belive.h"
#include "belive_t.h"
#include "irgwalk.h"
#include "besched.h"
#include "beutil.h"
#include "irloop.h"
#include "irloop_t.h"
#include "irgraph.h"
#include "irgraph_t.h"
#include "irphase.h"
#include "irphase_t.h"
#include "irprintf.h"

#include "bespillbelady.h"
#include "beverify.h"

#define DBG_LIVE		1
#define DBG_PRESSURE	2
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct _morgan_env_t {
	const arch_env_t *arch;
	const arch_register_class_t *cls;
	ir_graph *irg;
	phase_t phase;
	// maximum safe register pressure
	int registers_available;

	spill_env_t *senv;

	set *loop_attr_set;
	set *block_attr_set;
} morgan_env_t;

typedef struct _loop_out_edge_t {
	ir_node *block;
	int pos;
} loop_out_edge_t;

typedef struct _loop_attr_t {
	ir_loop *loop;
	set *out_edges;
	/// The set of all values that live through the loop and are not used
	bitset_t *livethrough_unused;
} loop_attr_t;

typedef struct _block_attr_t {
	ir_node *block;
	bitset_t *livethrough_unused;
} block_attr_t;

//---------------------------------------------------------------------------

int loop_out_edge_cmp(const void* p1, const void* p2, size_t s) {
	loop_out_edge_t *e1 = (loop_out_edge_t*) p1;
	loop_out_edge_t *e2 = (loop_out_edge_t*) p2;

	return e1->block != e2->block || e1->pos != e2->pos;
}

int loop_attr_cmp(const void *e1, const void *e2, size_t s) {
	loop_attr_t *la1 = (loop_attr_t*) e1;
	loop_attr_t *la2 = (loop_attr_t*) e2;

	return la1->loop != la2->loop;
}

int block_attr_cmp(const void *e1, const void *e2, size_t s) {
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

static INLINE int loop_out_edge_hash(const loop_out_edge_t *e) {
	return HASH_PTR(e->block) ^ (e->pos * 31);
}

static INLINE loop_attr_t *get_loop_attr(morgan_env_t *env, ir_loop *loop) {
	loop_attr_t l_attr, *res;
	int hash;
	l_attr.loop = loop;

	hash = loop_attr_hash(&l_attr);
	res = set_find(env->loop_attr_set, &l_attr, sizeof(l_attr), hash);

	// create new loop_attr if none exists yet
	if (!res) {
		l_attr.out_edges = new_set(loop_out_edge_cmp, 1);
		l_attr.livethrough_unused = bitset_obstack_alloc(&env->phase.obst, get_irg_last_idx(env->irg));
		res = set_insert(env->loop_attr_set, &l_attr, sizeof(l_attr), hash);
	}

	return res;
}

static INLINE block_attr_t *get_block_attr(morgan_env_t *env, ir_node *block) {
	block_attr_t b_attr, *res;
	int hash;
	b_attr.block = block;

	hash = block_attr_hash(&b_attr);
	res = set_find(env->block_attr_set, &b_attr, sizeof(b_attr), hash);

	if(!res) {
		b_attr.livethrough_unused = bitset_obstack_alloc(&env->phase.obst, get_irg_last_idx(env->irg));
		res = set_insert(env->block_attr_set, &b_attr, sizeof(b_attr), hash);
	}

	return res;
}

//---------------------------------------------------------------------------

/**
 * Determine edges going out of a loop (= edges that go to a block that is not inside
 * the loop or one of its subloops)
 */
static INLINE void construct_loop_out_edges(ir_node* block, void* e) {
	morgan_env_t *env = (morgan_env_t*) e;
	int n_cfgpreds = get_Block_n_cfgpreds(block);
	int i;
	ir_loop* loop = get_irn_loop(block);

	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node* cfgpred = get_Block_cfgpred(block, i);
		ir_node* cfgpred_block = get_nodes_block(cfgpred);
		ir_loop* cfgpred_loop = get_irn_loop(cfgpred_block);
		loop_attr_t *outedges = get_loop_attr(env, cfgpred_loop);

		if(cfgpred_loop != loop && get_loop_depth(cfgpred_loop) >= get_loop_depth(loop)) {
			loop_out_edge_t edge;
			edge.block = block;
			edge.pos = i;
			set_insert(outedges->out_edges, &edge, sizeof(edge), loop_out_edge_hash(&edge));
		}
	}
}

static void free_loop_out_edges(morgan_env_t *env) {
	loop_attr_t *l_attr;

	for(l_attr = set_first(env->loop_attr_set); l_attr != NULL; l_attr = set_next(env->loop_attr_set)) {
		del_set(l_attr->out_edges);
	}
}

/**
 * Debugging help, shows all nodes in a (node-)bitset
 */
static void show_nodebitset(ir_graph* irg, bitset_t* bitset) {
	int i;

	bitset_foreach(bitset, i) {
		ir_node* node = get_idx_irn(irg, i);
		DBG((dbg, DBG_LIVE, "\t%+F\n", node));
	}
}

/**
 * Construct the livethrough unused set for a block
 */
static bitset_t *construct_block_livethrough_unused(morgan_env_t* env, ir_node* block) {
	block_attr_t *block_attr = get_block_attr(env, block);
	irn_live_t *li;
	ir_node *node;

	DBG((dbg, DBG_LIVE, "Processing block %d\n", get_irn_node_nr(block)));
	// copy all live-outs into the livethrough_unused set
	live_foreach(block, li) {
		int node_idx;

		if(!live_is_in(li) || !live_is_out(li))
			continue;
		if(!arch_irn_consider_in_reg_alloc(env->arch, env->cls, li->irn))
			continue;

		node_idx = get_irn_idx(li->irn);
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
static bitset_t *construct_loop_livethrough_unused(morgan_env_t *env, ir_loop *loop) {
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

//---------------------------------------------------------------------------

static int reduce_register_pressure_in_block(morgan_env_t *env, ir_node* block, int loop_unused_spills_possible) {
	int pressure;
	ir_node *irn;
	int max_pressure = 0;
	int spills_needed;
	int loop_unused_spills_needed;
	block_attr_t *block_attr = get_block_attr(env, block);
	int block_unused_spills_possible = bitset_popcnt(block_attr->livethrough_unused);
	int unused_spills_possible = loop_unused_spills_possible + block_unused_spills_possible;
	pset *live_nodes = pset_new_ptr_default();

	be_liveness_end_of_block(env->arch, env->cls, block, live_nodes);
	pressure = pset_count(live_nodes);

	DBG((dbg, DBG_LIVE, "Reduce pressure to %d In Block %+F:\n", env->registers_available, block));

	/**
	 * Walk over all irns in the schedule and check register pressure for each of them
	 */
	sched_foreach_reverse(block, irn) {
		// do we need more spills than possible with unused libethroughs?
		int spills_needed = pressure - unused_spills_possible - env->registers_available;
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
		{
			int pressure_old = pressure;
			be_liveness_transfer(env->arch, env->cls, irn, live_nodes);
			pressure = pset_count(live_nodes);
			DBG((dbg, DBG_PRESSURE, "\tPressure at %+F - before: %d after: %d\n", irn, pressure_old, pressure));
		}
	}

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
				if(is_Phi(to_spill)) {
					be_spill_phi(env->senv, to_spill);
				}
				be_add_reload_on_edge(env->senv, to_spill, edge->src, edge->pos);
			}
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
static int reduce_register_pressure_in_loop(morgan_env_t *env, ir_loop *loop, int outer_spills_possible) {
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

	// calculate number of spills needed in outer loop and spill
	// unused livethrough nodes around this loop
	if(spills_needed > outer_spills_possible) {
		outer_spills_needed = outer_spills_possible;
		spills_needed -= outer_spills_possible;

		bitset_foreach(loop_attr->livethrough_unused, i) {
			loop_out_edge_t *edge;
			ir_node *to_spill = get_idx_irn(env->irg, i);

			for(edge = set_first(loop_attr->out_edges); edge != NULL; edge = set_next(loop_attr->out_edges)) {
				if(is_Phi(to_spill)) {
					be_spill_phi(env->senv, to_spill);
				}

				be_add_reload_on_edge(env->senv, to_spill, edge->block, edge->pos);
			}
		}
	} else {
		outer_spills_needed = spills_needed;
	}

	return outer_spills_needed;
}

static void *init_phase_data(phase_t *phase, ir_node *irn, void *old) {
	return old;
}

void be_spill_morgan(const be_chordal_env_t *chordal_env) {
	morgan_env_t env;

	FIRM_DBG_REGISTER(dbg, "ir.be.spillmorgan");

	env.arch = chordal_env->birg->main_env->arch_env;
	env.irg = chordal_env->irg;
	env.cls = chordal_env->cls;
	env.senv = be_new_spill_env(chordal_env);
	DEBUG_ONLY(be_set_spill_env_dbg_module(env.senv, dbg);)

	phase_init(&env.phase, "spillmorgan", env.irg, PHASE_DEFAULT_GROWTH, init_phase_data);

	env.registers_available = arch_count_non_ignore_regs(env.arch, env.cls);

	env.loop_attr_set = new_set(loop_attr_cmp, 5);
	env.block_attr_set = new_set(block_attr_cmp, 20);

	/*-- Part1: Analysis --*/
	be_liveness(env.irg);

	// construct control flow loop tree
	construct_cf_backedges(chordal_env->irg);

	// construct loop out edges and livethrough_unused sets for loops and blocks
	irg_block_walk_graph(chordal_env->irg, construct_loop_out_edges, NULL, &env);
	construct_loop_livethrough_unused(&env, get_irg_loop(env.irg));

	/*-- Part2: Transformation --*/

	// reduce register pressure to number of available registers
	reduce_register_pressure_in_loop(&env, get_irg_loop(env.irg), 0);

	be_insert_spills_reloads(env.senv);
	if (chordal_env->opts->vrfy_option == BE_CH_VRFY_WARN) {
		be_verify_schedule(env.irg);
	} else if (chordal_env->opts->vrfy_option == BE_CH_VRFY_ASSERT) {
		assert(be_verify_schedule(env.irg));
	}

	// we have to remove dead nodes from schedule to not confuse liveness calculation
	be_remove_dead_nodes_from_schedule(env.irg);

	// cleanup
	be_dump(env.irg, "-spillmorgan", dump_ir_block_graph_sched);
	free_loop_out_edges(&env);
	del_set(env.loop_attr_set);
	del_set(env.block_attr_set);

	// fix the remaining places with too high register pressure with beladies algorithm
	be_liveness(env.irg);
	be_spill_belady_spill_env(chordal_env, env.senv);

	be_delete_spill_env(env.senv);
	phase_free(&env.phase);
}
