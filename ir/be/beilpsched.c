/**
 * Scheduling algorithms.
 * An ILP scheduler based on
 * "ILP-based Instruction Scheduling for IA-64"
 * by Daniel Kaestner and Sebastian Winkel
 *
 * @date   22.10.2005
 * @author Christian Wuerdig
 * @cvs-id $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode_t.h"
#include "irgwalk.h"
#include "irbitset.h"
#include "irphase_t.h"
#include "iredges.h"
#include "debug.h"
#include "pdeq.h"

#include "be.h"
#include "benode_t.h"

typedef struct _ilpsched_node_attr_t {
	unsigned asap;
	unsigned alap;
	unsigned visit_idx : 31;
	unsigned enqueued  : 1;
	bitset_t *transitive_block_nodes;
} ilpsched_node_attr_t;

typedef struct _ilpsched_block_attr_t {
	unsigned n_interesting_nodes;
	ir_node  *head_root_nodes;
} ilpsched_block_attr_t;

typedef union _ilpsched_attr_ {
	ilpsched_node_attr_t  node_attr;
	ilpsched_block_attr_t block_attr;
} ilpsched_attr_t;

typedef struct {
	ir_node         *irn;
	ilpsched_attr_t attr;
} be_ilpsched_irn_t;

typedef struct {
	phase_t  ph;
	ir_graph *irg;
	waitq    *alap_queue;
	DEBUG_ONLY(firm_dbg_module_t *dbg);
} be_ilpsched_env_t;

#define get_ilpsched_irn(ilpsched_env, irn) (phase_get_or_set_irn_data(&(ilpsched_env)->ph, (irn)))
#define is_ilpsched_block(node)             (is_Block((node)->irn))
#define get_ilpsched_block_attr(block)      (&(block)->attr.block_attr)
#define get_ilpsched_node_attr(node)        (&(node)->attr.node_attr)

#define foreach_linked_irns(head, iter) for ((iter) = (head); (iter); (iter) = get_irn_link((iter)))

#define consider_for_sched(irn) \
	(! (is_Block(irn) || is_Proj(irn) || is_Phi(irn) || be_is_Keep(irn) || is_NoMem(irn) || is_Jmp(irn)))

/**
 * In case there is no phase information for irn, initialize it.
 */
static void *init_ilpsched_irn(phase_t *ph, ir_node *irn, void *old) {
	be_ilpsched_irn_t *res = old ? old : phase_alloc(ph, sizeof(res[0]));

	if (res == old) {
		/* if node has phase data and it's not a block: clear the bitset */
		if (! is_Block(res->irn)) {
			ilpsched_node_attr_t *na = get_ilpsched_node_attr(res);
			bitset_clear_all(na->transitive_block_nodes);
			na->visit_idx = 0;
		}
		return old;
	}

	res->irn = irn;

	if (is_Block(irn)) {
		ilpsched_block_attr_t *ba = get_ilpsched_block_attr(res);

		ba->n_interesting_nodes = 0;
		ba->head_root_nodes     = NULL;
	}
	else {
		ilpsched_node_attr_t *na = get_ilpsched_node_attr(res);

		na->asap      = 0;
		na->alap      = 0;
		na->visit_idx = 0;
		na->enqueued  = 0;
		na->transitive_block_nodes = bitset_irg_obstack_alloc(phase_obst(ph), phase_get_irg(ph));
	}

	return res;
}

/**
 * Add all nodes having no user in current block to last_nodes list.
 */
static void collect_alap_root_nodes(ir_node *irn, void *walk_env) {
	ir_node               *block;
	const ir_edge_t       *edge;
	be_ilpsched_env_t     *env;
	be_ilpsched_irn_t     *block_node;
	ilpsched_block_attr_t *ba;
	int                   has_block_user = 0;

	if (! consider_for_sched(irn))
		return;

	block = get_nodes_block(irn);

	foreach_out_edge(irn, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if (is_Proj(user)) {
			const ir_edge_t *user_edge;

			if (get_irn_mode(user) == mode_X)
				continue;

			/* The ABI ensures, that there will be no ProjT nodes in the graph. */
			foreach_out_edge(user, user_edge) {
				ir_node *real_user = get_edge_src_irn(user_edge);

				if (! is_Phi(real_user) && get_nodes_block(real_user) == block) {
					has_block_user = 1;
					break;
				}
			}

			if (has_block_user)
				break;
		}
		else if (is_Block(user)) {
			continue;
		}
		else if (! is_Phi(user) && get_nodes_block(user) == block) {
			has_block_user = 1;
			break;
		}
	}

	env        = walk_env;
	block_node = get_ilpsched_irn(env, block);
	ba         = get_ilpsched_block_attr(block_node);

	ba->n_interesting_nodes++;

	/* current irn has no user inside this block */
	if (! has_block_user) {
		set_irn_link(irn, ba->head_root_nodes);
		ba->head_root_nodes = irn;
	}
}

/**
 * Calculate the ASAP scheduling step for current irn.
 */
static void calculate_irn_asap(ir_node *irn, void *walk_env) {
	be_ilpsched_irn_t *node;
	be_ilpsched_env_t *env = walk_env;
	int      i;
	unsigned idx;
	ir_node  *block;
	ilpsched_node_attr_t *na;

	/* These nodes are handled separate */
	if (! consider_for_sched(irn))
		return;

	DBG((env->dbg, LEVEL_1, "Calculating ASAP of node %+F\n", irn));

	node  = get_ilpsched_irn(env, irn);
	block = get_nodes_block(irn);
	idx   = get_irn_idx(irn);
	na    = get_ilpsched_node_attr(node);

	/* accumulate all transitive predecessors of current node */
	for (i = get_irn_ins_or_deps(irn) - 1; i >= 0; --i) {
		ir_node              *pred = skip_Proj(get_irn_in_or_dep(irn, i));
		be_ilpsched_irn_t    *pred_node;
		ilpsched_node_attr_t *pna;

		if (be_is_Keep(pred))
			pred = skip_Proj(get_irn_n(pred, 0));

		if (is_Phi(pred) || block != get_nodes_block(pred) || is_NoMem(pred))
			continue;

		pred_node = get_ilpsched_irn(env, pred);
		pna       = get_ilpsched_node_attr(pred_node);

		assert(pna->asap && "missing ASAP of predecessor");

		/*
			We have not already visited this predecessor
			-> accumulate it's predecessors
		*/
		if (pna->visit_idx != idx) {
			pna->visit_idx = idx;
			na->transitive_block_nodes = bitset_or(na->transitive_block_nodes, pna->transitive_block_nodes);
			DBG((env->dbg, LEVEL_1, "\taccumulating preds of %+F\n", pred));
		}
	}

	/* every node is it's own transitive predecessor in block */
	bitset_set(na->transitive_block_nodes, idx);

	/* asap = number of transitive predecessors in this block */
	na->asap = bitset_popcnt(na->transitive_block_nodes);

	DBG((env->dbg, LEVEL_1, "\tcalculated ASAP is %u\n", na->asap));
}

/**
 * Accumulate the successors of all nodes from irn on upwards.
 */
static void accumulate_succs(be_ilpsched_env_t *env, ir_node *irn) {
	unsigned             i, n;
	unsigned             idx    = get_irn_idx(irn);
	be_ilpsched_irn_t    *node  = get_ilpsched_irn(env, irn);
	ilpsched_node_attr_t *na    = get_ilpsched_node_attr(node);
	ir_node              *block = get_nodes_block(irn);
	waitq                *wq    = new_waitq();

	DBG((env->dbg, LEVEL_1, "\taccumulating succs of %+F\n", irn));

	/* enqueue node for final alap calculation */
	if (! na->enqueued) {
		be_ilpsched_irn_t     *block_node = get_ilpsched_irn(env, block);
		ilpsched_block_attr_t *ba         = get_ilpsched_block_attr(block_node);

		na->enqueued = 1;
		na->alap     = ba->n_interesting_nodes;
		waitq_put(env->alap_queue, node);
		DBG((env->dbg, LEVEL_2, "\t\tenqueueing %+F for final ALAP calculation\n", irn));
	}

	for (i = 0, n = get_irn_ins_or_deps(irn); i < n; ++i) {
		ir_node              *pred = skip_Proj(get_irn_in_or_dep(irn, i));
		be_ilpsched_irn_t    *pred_node;
		ilpsched_node_attr_t *pna;

		if (be_is_Keep(pred))
			pred = skip_Proj(get_irn_n(pred, 0));

		if (is_Phi(pred) || block != get_nodes_block(pred) || is_NoMem(pred))
			continue;

		pred_node = get_ilpsched_irn(env, pred);
		pna       = get_ilpsched_node_attr(pred_node);

		/* accumulate the successors */
		if (pna->visit_idx != idx) {
			pna->visit_idx = idx;
			pna->transitive_block_nodes = bitset_or(pna->transitive_block_nodes, na->transitive_block_nodes);

			/* set current node as successor */
			bitset_set(pna->transitive_block_nodes, idx);
			waitq_put(wq, pred);

			DBG((env->dbg, LEVEL_1, "\taccumulating succs of %+F to %+F\n", irn, pred));
		}
	}

	/* process all predecessors */
	while (! waitq_empty(wq)) {
		accumulate_succs(env, waitq_get(wq));
	}
}

/**
 * Calculate the ALAP scheduling step of all irns in current block.
 * Depends on ASAP beeing calculated.
 */
static void calculate_block_alap(ir_node *block, void *walk_env) {
	be_ilpsched_env_t     *env        = walk_env;
	be_ilpsched_irn_t     *block_node = get_ilpsched_irn(env, block);
	ilpsched_block_attr_t *ba         = get_ilpsched_block_attr(block_node);
	bitset_t              *alap_bs    = bitset_irg_alloca(env->irg);
	ir_node *root;

	assert(is_Block(block));

	DBG((env->dbg, LEVEL_1, "Calculating ALAP for nodes in %+F (%u nodes)\n", block, ba->n_interesting_nodes));

	phase_reinit_block_irn_data(&env->ph, block);

	/* calculate the alap of all nodes, starting at collected roots upwards */
	foreach_linked_irns(ba->head_root_nodes, root) {
		accumulate_succs(env, root);
	}

	/* all interesting nodes should have their successors accumulated now */
	while (! waitq_empty(env->alap_queue)) {
		be_ilpsched_irn_t    *node = waitq_get(env->alap_queue);
		ilpsched_node_attr_t *na   = get_ilpsched_node_attr(node);

		na->alap -= bitset_popcnt(na->transitive_block_nodes);
		DBG((env->dbg, LEVEL_1, "\tALAP of %+F is %u (%u succs)\n", node->irn, na->alap, bitset_popcnt(na->transitive_block_nodes)));
	}
}

void be_ilp_sched(const be_irg_t *birg) {
	be_ilpsched_env_t env;

	FIRM_DBG_REGISTER(env.dbg, "firm.be.sched.ilp");

	//firm_dbg_set_mask(env.dbg, 31);

	env.irg        = birg->irg;
	env.alap_queue = new_waitq();
	phase_init(&env.ph, "be ilp scheduling", env.irg, PHASE_DEFAULT_GROWTH, init_ilpsched_irn);

	irg_walk_graph(env.irg, collect_alap_root_nodes, calculate_irn_asap, &env);
	irg_block_walk_graph(env.irg, NULL, calculate_block_alap, &env);

	del_waitq(env.alap_queue);
	phase_free(&env.ph);
}
