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
#include "beabi.h"
#include "irgwalk.h"
#include "besched.h"
#include "beutil.h"
#include "irloop_t.h"
#include "irgraph_t.h"
#include "irprintf.h"
#include "obstack.h"

#include "bespillbelady.h"
#include "beverify.h"
#include "benodesets.h"

#define DBG_LIVE		1
#define DBG_LOOPANA		2
#define DBG_PRESSURE	4
#define DBG_SPILLS      8
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
#ifdef DEBUG_libfirm
	return a->loop->loop_nr;
#else
	return HASH_PTR(a->loop);
#endif
}

static INLINE int block_attr_hash(const block_attr_t *b) {
	return nodeset_hash(b->block);
}

static INLINE int loop_edge_hash(const loop_edge_t *e) {
	return nodeset_hash(e->block) ^ (e->pos * 31);
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
		b_attr.livethrough_unused = NULL;
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
static INLINE void construct_loop_edges(ir_node* block, void* data) {
	morgan_env_t *env = data;
	int n_cfgpreds = get_Block_n_cfgpreds(block);
	int i;
	ir_loop* loop = get_irn_loop(block);
	DBG((dbg, DBG_LOOPANA, "Loop for %+F: %d (depth %d)\n", block, loop->loop_nr, loop->depth));

	for(i = 0; i < n_cfgpreds; ++i) {
		loop_edge_t edge;
		int hash;
		ir_node* cfgpred = get_Block_cfgpred(block, i);
		ir_node* cfgpred_block = get_nodes_block(cfgpred);
		ir_loop* cfgpred_loop = get_irn_loop(cfgpred_block);

		if(cfgpred_loop == loop)
			continue;

		assert(get_loop_depth(cfgpred_loop) != get_loop_depth(loop));

		edge.block = block;
		edge.pos = i;
		hash = loop_edge_hash(&edge);

		// edge out of a loop?
		if(get_loop_depth(cfgpred_loop) > get_loop_depth(loop)) {
			ir_loop *l;

			DBG((dbg, DBG_LOOPANA, "Loop out edge from %+F (loop %d) to %+F (loop %d)\n", block, get_loop_loop_nr(loop),
			     cfgpred_block, get_loop_loop_nr(cfgpred_loop)));

			/* this might be a jump out of multiple loops, so add this to all
		     * needed outedge sets */
			l = cfgpred_loop;
			do {
				loop_attr_t *l_attr = get_loop_attr(env, l);
				set_insert(l_attr->out_edges, &edge, sizeof(edge), hash);

				l = get_loop_outer_loop(l);
				assert(l != NULL);
			} while(l != loop);
		} else {
			ir_loop *l;

			// edge into a loop
			DBG((dbg, DBG_LOOPANA, "Loop in edge from %+F (loop %d) to %+F (loop %d)\n", block, get_loop_loop_nr(loop),
			     cfgpred_block, get_loop_loop_nr(cfgpred_loop)));

			l = loop;
			do {
				loop_attr_t *l_attr = get_loop_attr(env, l);
				set_insert(l_attr->in_edges, &edge, sizeof(edge), hash);

				l = get_loop_outer_loop(l);
			} while(l != cfgpred_loop);
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

#if 0
/**
 * Debugging help, shows all nodes in a (node-)bitset
 */
static void show_nodebitset(ir_graph* irg, const bitset_t* bitset) {
	int i;

	bitset_foreach(bitset, i) {
		ir_node* node = get_idx_irn(irg, i);
		ir_fprintf(stderr, " %+F", node);
	}
	fprintf(stderr, "\n");
}
#endif

static INLINE void init_livethrough_unuseds(block_attr_t *attr, morgan_env_t *env) {
	const ir_node *block;
	int i;

	if(attr->livethrough_unused != NULL)
		return;

	block = attr->block;

	attr->livethrough_unused = bitset_obstack_alloc(&env->obst, get_irg_last_idx(env->irg));

	// copy all live-outs into the livethrough_unused set
	be_lv_foreach(env->cenv->lv, block, be_lv_state_in | be_lv_state_out, i) {
		ir_node *irn = be_lv_get_irn(env->cenv->lv, block, i);
		int node_idx;

		if(!consider_for_spilling(env->arch, env->cls, irn))
			continue;

		node_idx = get_irn_idx(irn);
		bitset_set(attr->livethrough_unused, node_idx);
	}
}

/**
 * Construct the livethrough unused set for a block
 */
static void construct_block_livethrough_unused(ir_node *block, void *data) {
	morgan_env_t* env = data;
	block_attr_t *block_attr = get_block_attr(env, block);
	ir_node *node;
	int n_cfgpreds;
	block_attr_t **pred_attrs = NULL;
	int i;

	init_livethrough_unuseds(block_attr, env);

	DBG((dbg, DBG_LIVE, "Processing block %d\n", get_irn_node_nr(block)));

	n_cfgpreds = get_Block_n_cfgpreds(block);
	if(n_cfgpreds > 1) {
		pred_attrs = alloca(sizeof(pred_attrs[0]) * n_cfgpreds);
		for(i = 0; i < n_cfgpreds; ++i) {
			ir_node *pred_block = get_Block_cfgpred_block(block, i);
			pred_attrs[i] = get_block_attr(env, pred_block);
			init_livethrough_unuseds(pred_attrs[i], env);
		}
	}

	/*
	 * All values that are used within the block are not unused (and therefore not
	 * livethrough_unused)
	 */
	sched_foreach(block, node) {
		int i, arity;

		// phis are really uses in the pred block
		if(is_Phi(node)) {
			int j;
			for(j = 0; j < n_cfgpreds; ++j) {
				ir_node *used_value = get_Phi_pred(node, j);
				int idx = get_irn_idx(used_value);
				block_attr_t *pred_attr = pred_attrs[j];

				bitset_clear(pred_attr->livethrough_unused, idx);
			}
		} else {
			// mark all used values as used
			for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				int idx = get_irn_idx(get_irn_n(node, i));
				bitset_clear(block_attr->livethrough_unused, idx);
			}
		}
	}
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
			ir_node *block = elem.node;
			block_attr_t *block_attr = get_block_attr(env, block);
			bitset_t *livethrough_block_unused = block_attr->livethrough_unused;

			assert(is_Block(elem.node));
			assert(livethrough_block_unused != NULL);

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
		} else if(*elem.kind == k_ir_node) {
			block_attr_t *block_attr = get_block_attr(env, elem.node);
			bitset_andnot(block_attr->livethrough_unused, loop_attr->livethrough_unused);

			DBG((dbg, DBG_LIVE, "Livethroughs for block %+F\n", elem.node));
		} else {
			assert(0);
		}
	}

	return loop_attr->livethrough_unused;
}

/*---------------------------------------------------------------------------*/

static int reduce_register_pressure_in_block(morgan_env_t *env, const ir_node* block, int loop_unused_spills_possible) {
	ir_node *node;
	int max_pressure;
	int loop_unused_spills_needed;
	pset *live_nodes = pset_new_ptr_default();

	be_liveness_end_of_block(env->cenv->lv, env->arch, env->cls, block, live_nodes);
	max_pressure = pset_count(live_nodes);

	DBG((dbg, DBG_LIVE, "Reduce pressure to %d In Block %+F:\n", env->registers_available, block));

	/**
	 * Determine register pressure in block
	 */
	sched_foreach_reverse(block, node) {
		int pressure;

		if(is_Phi(node))
			break;

		be_liveness_transfer(env->arch, env->cls, node, live_nodes);
		pressure = pset_count(live_nodes);
		if(pressure > max_pressure)
			max_pressure = pressure;
	}
	del_pset(live_nodes);

	DBG((dbg, DBG_PRESSURE, "\tMax Pressure in %+F: %d\n", block, max_pressure));

	loop_unused_spills_needed = max_pressure - env->registers_available;

	if(loop_unused_spills_needed < 0) {
		loop_unused_spills_needed = 0;
	} else if(loop_unused_spills_needed > loop_unused_spills_possible) {
		loop_unused_spills_needed = loop_unused_spills_possible;
	}

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
			assert(needed >= 0);
			assert(needed <= spills_possible);
			if(needed > spills_needed)
				spills_needed = needed;
			break;
		}
		case k_ir_loop: {
			int needed = reduce_register_pressure_in_loop(env, elem.son, spills_possible);
			assert(needed >= 0);
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

		DBG((dbg, DBG_SPILLS, "%d values unused in loop %d, spilling %d\n",
	         spills_possible - outer_spills_possible, loop->loop_nr, spills_to_place));

		bitset_foreach(loop_attr->livethrough_unused, i) {
			loop_edge_t *edge;
			ir_node *to_spill = get_idx_irn(env->irg, i);

			DBG((dbg, DBG_SPILLS, "Spilling node %+F around loop %d\n", to_spill, loop->loop_nr));

			for(edge = set_first(loop_attr->out_edges); edge != NULL; edge = set_next(loop_attr->out_edges)) {
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

void be_spill_morgan(be_chordal_env_t *chordal_env) {
	morgan_env_t env;

	FIRM_DBG_REGISTER(dbg, "ir.be.spillmorgan");
	//firm_dbg_set_mask(dbg, DBG_SPILLS | DBG_LOOPANA);

	env.cenv = chordal_env;
	env.arch = chordal_env->birg->main_env->arch_env;
	env.irg = chordal_env->irg;
	env.cls = chordal_env->cls;
	env.senv = be_new_spill_env(chordal_env);
	DEBUG_ONLY(be_set_spill_env_dbg_module(env.senv, dbg);)

	obstack_init(&env.obst);

	env.registers_available = env.cls->n_regs - be_put_ignore_regs(chordal_env->birg, env.cls, NULL);

	env.loop_attr_set = new_set(loop_attr_cmp, 5);
	env.block_attr_set = new_set(block_attr_cmp, 20);

	/*-- Part1: Analysis --*/
	be_liveness_recompute(chordal_env->lv);

	/* construct control flow loop tree */
	construct_cf_backedges(chordal_env->irg);

	/* construct loop out edges and livethrough_unused sets for loops and blocks */
	irg_block_walk_graph(chordal_env->irg, construct_block_livethrough_unused, construct_loop_edges, &env);
	construct_loop_livethrough_unused(&env, get_irg_loop(env.irg));

	/*-- Part2: Transformation --*/

	/* spill unused livethrough values around loops and blocks where
	 * the pressure is too high
	 */
	reduce_register_pressure_in_loop(&env, get_irg_loop(env.irg), 0);

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
	be_spill_belady_spill_env(chordal_env, env.senv);

	be_delete_spill_env(env.senv);
	obstack_free(&env.obst, NULL);
}
