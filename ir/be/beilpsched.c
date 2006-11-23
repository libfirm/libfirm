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

#ifdef WITH_ILP

#include <math.h>

#ifndef _WIN32
#include <strings.h>
#endif /* _WIN32 */

#include "irnode_t.h"
#include "irgwalk.h"
#include "irbitset.h"
#include "irphase_t.h"
#include "iredges.h"
#include "debug.h"
#include "pdeq.h"
#include "irtools.h"
#include "irdump.h"

#include <lpp/lpp.h>
#include <lpp/lpp_net.h>

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>
#endif /* WITH_LIBCORE */

#include "be.h"
#include "benode_t.h"
#include "besched_t.h"

typedef struct _ilpsched_options_t {
	unsigned time_limit;
	char     log_file[1024];
} ilpsched_options_t;

typedef struct _unit_type_info_t {
	int                            n_units;
	const be_execution_unit_type_t *tp;
} unit_type_info_t;

/* attributes for a node */
typedef struct _ilpsched_node_attr_t {
	unsigned asap;                     /**< The ASAP scheduling control step */
	unsigned alap;                     /**< The ALAP scheduling control step */
	unsigned sched_point;              /**< the step in which the node is finally scheduled */
	unsigned visit_idx;                /**< Index of the node having visited this node last */
	unsigned block_idx : 31;           /**< A unique per block index */
	unsigned enqueued  : 1;            /**< Node is already enqueued for ALAP calculation */
	bitset_t *transitive_block_nodes;  /**< Set of transitive block nodes (predecessors
											for ASAP, successors for ALAP */
	unsigned n_unit_types;             /**< number of allowed execution unit types */
	unit_type_info_t *type_info;       /**< list of allowed execution unit types */
	int      *ilp_vars;                /**< the binary ilp variables x_{nt}^k assigned to this node
											(== 1 iff: node n is executed at step t on execunit k
											So we have: |ASAP(n) ... ALAP(n)| * |execunits| variables
										*/
} ilpsched_node_attr_t;

/* attributes for a block */
typedef struct _ilpsched_block_attr_t {
	unsigned block_last_idx;        /**< The highest node index in block so far */
	unsigned n_interesting_nodes;   /**< The number of nodes interesting for scheduling */
	waitq    *root_nodes;           /**< A queue of nodes having no user in current block */
	ir_node  *head_ilp_nodes;       /**< A linked list of nodes which will contribute to ILP */
} ilpsched_block_attr_t;

typedef union _ilpsched_attr_ {
	ilpsched_node_attr_t  node_attr;
	ilpsched_block_attr_t block_attr;
} ilpsched_attr_t;

/* A irn for the phase and it's attributes (either node or block) */
typedef struct {
	ir_node         *irn;
	ilpsched_attr_t attr;
} be_ilpsched_irn_t;

/* The ILP scheduling environment */
typedef struct {
	phase_t             ph;            /**< The phase */
	ir_graph            *irg;          /**< The current irg */
	waitq               *alap_queue;   /**< An queue of nodes waiting for final ALAP calculation */
	const arch_env_t    *arch_env;
	const be_main_env_t *main_env;
	const be_machine_t  *cpu;          /**< the current abstract machine */
	ilpsched_options_t  *opts;         /**< the ilp options for current irg */
	DEBUG_ONLY(firm_dbg_module_t *dbg);
} be_ilpsched_env_t;

/* convenience macros to handle phase irn data */
#define get_ilpsched_irn(ilpsched_env, irn) (phase_get_or_set_irn_data(&(ilpsched_env)->ph, (irn)))
#define is_ilpsched_block(node)             (is_Block((node)->irn))
#define get_ilpsched_block_attr(block)      (&(block)->attr.block_attr)
#define get_ilpsched_node_attr(node)        (&(node)->attr.node_attr)

/* iterate over a list of ir_nodes linked by link field */
#define foreach_linked_irns(head, iter) for ((iter) = (head); (iter); (iter) = get_irn_link((iter)))

/* check if node is considered for ILP scheduling */
#define consider_for_sched(irn) \
	(! (is_Block(irn)   ||      \
		is_Proj(irn)    ||      \
		is_Phi(irn)     ||      \
		be_is_Keep(irn) ||      \
		is_NoMem(irn)   ||      \
		is_Jmp(irn)     ||      \
		is_End(irn)             \
		))

/* gives the valid scheduling time step interval for a node */
#define VALID_SCHED_INTERVAL(na) ((na)->alap - (na)->asap + 1)

/* gives the corresponding ILP variable for given node, unit and time step */
#define ILPVAR_IDX(na, unit, control_step) \
	((unit) * VALID_SCHED_INTERVAL((na)) + (control_step) - (na)->asap + 1)

/* check if a double value is within an epsilon environment of 0 */
#define LPP_VALUE_IS_0(dbl) (fabs((dbl)) <= 1e-10)

/* option variable */
static ilpsched_options_t ilp_opts = {
	120,   /* 120 sec per block time limit */
	""     /* no log file */
};

#ifdef WITH_LIBCORE
/* ILP options */
static const lc_opt_table_entry_t ilpsched_option_table[] = {
	LC_OPT_ENT_INT("time_limit", "ILP time limit per block", &ilp_opts.time_limit),
	LC_OPT_ENT_STR("lpp_log",    "LPP logfile (stderr and stdout are supported)", ilp_opts.log_file, sizeof(ilp_opts.log_file)),
	{ NULL }
};
#endif /* WITH_LIBCORE */

/**
 * Compare scheduling time steps of two be_ilpsched_irn's.
 */
static int cmp_ilpsched_irn(const void *a, const void *b) {
	be_ilpsched_irn_t    *n1   = *(be_ilpsched_irn_t **)a;
	be_ilpsched_irn_t    *n2   = *(be_ilpsched_irn_t **)b;
	ilpsched_node_attr_t *n1_a = get_ilpsched_node_attr(n1);
	ilpsched_node_attr_t *n2_a = get_ilpsched_node_attr(n2);

	return QSORT_CMP(n1_a->sched_point, n2_a->sched_point);
}

/**
 * In case there is no phase information for irn, initialize it.
 */
static void *init_ilpsched_irn(phase_t *ph, ir_node *irn, void *old) {
	be_ilpsched_irn_t *res = old ? old : phase_alloc(ph, sizeof(res[0]));

	if (res == old) {
		/* if we have already some data: check for reinitialization */

		if (! is_Block(irn)) {
			ilpsched_node_attr_t *na = get_ilpsched_node_attr(res);

			if (! na->transitive_block_nodes) {
				ir_node               *block      = get_nodes_block(irn);
				be_ilpsched_irn_t     *block_node = phase_get_or_set_irn_data(ph, block);
				ilpsched_block_attr_t *ba         = get_ilpsched_block_attr(block_node);

				/* we are called after the block indicees have been build: create bitset */
				na->transitive_block_nodes = bitset_obstack_alloc(phase_obst(ph), ba->block_last_idx);
			}
			else {
				/* we are called from reinit block data: clear the bitset */
				bitset_clear_all(na->transitive_block_nodes);
				na->visit_idx = 0;
			}
		}
		return old;
	}

	res->irn = irn;

	/* set ilpsched irn attributes (either block or irn) */
	if (is_Block(irn)) {
		ilpsched_block_attr_t *ba = get_ilpsched_block_attr(res);

		ba->n_interesting_nodes = 0;
		ba->block_last_idx      = 0;
		ba->root_nodes          = new_waitq();
		ba->head_ilp_nodes      = NULL;
	}
	else {
		ilpsched_node_attr_t *na = get_ilpsched_node_attr(res);
		memset(na, 0, sizeof(*na));
	}

	return res;
}

/**
 * Assign a per block unique number to each node.
 */
static void build_block_idx(ir_node *irn, void *walk_env) {
	be_ilpsched_env_t     *env = walk_env;
	be_ilpsched_irn_t     *node, *block_node;
	ilpsched_node_attr_t  *na;
	ilpsched_block_attr_t *ba;

	if (! consider_for_sched(irn))
		return;

	node       = get_ilpsched_irn(env, irn);
	na         = get_ilpsched_node_attr(node);
	block_node = get_ilpsched_irn(env, get_nodes_block(irn));
	ba         = get_ilpsched_block_attr(block_node);

	na->block_idx = ba->block_last_idx++;
}

/********************************************************
 *                              __        _
 *                             / /       | |
 *   __ _ ___  __ _ _ __      / /    __ _| | __ _ _ __
 *  / _` / __|/ _` | '_ \    / /    / _` | |/ _` | '_ \
 * | (_| \__ \ (_| | |_) |  / /    | (_| | | (_| | |_) |
 *  \__,_|___/\__,_| .__/  /_/      \__,_|_|\__,_| .__/
 *                 | |                           | |
 *                 |_|                           |_|
 ********************************************************/

/**
 * Add all nodes having no user in current block to last_nodes list.
 */
static void collect_alap_root_nodes(ir_node *irn, void *walk_env) {
	ir_node               *block;
	const ir_edge_t       *edge;
	be_ilpsched_irn_t     *block_node;
	ilpsched_block_attr_t *ba;
	be_ilpsched_env_t     *env           = walk_env;
	int                   has_block_user = 0;
	ir_edge_kind_t        ekind[2]       = { EDGE_KIND_NORMAL, EDGE_KIND_DEP };
	int                   i, j;

	if (! consider_for_sched(irn))
		return;

	block = get_nodes_block(irn);

	DBG((env->dbg, LEVEL_3, "%+F (%+F) is interesting, examining ... ", irn, block));

	for (i = 0; i < 2; ++i) {
		foreach_out_edge_kind(irn, edge, ekind[i]) {
			ir_node *user = get_edge_src_irn(edge);

			if (is_Proj(user)) {
				const ir_edge_t *user_edge;

				if (get_irn_mode(user) == mode_X)
					continue;

				/* The ABI ensures, that there will be no ProjT nodes in the graph. */
				for (j = 0; j < 2; ++j) {
					foreach_out_edge_kind(user, user_edge, ekind[j]) {
						ir_node *real_user = get_edge_src_irn(user_edge);

						if (! is_Phi(real_user) && ! be_is_Keep(real_user) && get_nodes_block(real_user) == block) {
							has_block_user = 1;
							break;
						}
					}
				}

				if (has_block_user)
					break;
			}
			else if (is_Block(user)) {
				continue;
			}
			else if (! is_Phi(user) && ! be_is_Keep(user) && get_nodes_block(user) == block) {
				has_block_user = 1;
				break;
			}
		}
	}

	block_node = get_ilpsched_irn(env, block);
	ba         = get_ilpsched_block_attr(block_node);

	ba->n_interesting_nodes++;

	/* current irn has no user inside this block, add to queue */
	if (! has_block_user) {
		DB((env->dbg, LEVEL_3, "root node\n"));
		waitq_put(ba->root_nodes, irn);
	}
	else {
		DB((env->dbg, LEVEL_3, "normal node\n"));
	}
}

/**
 * Calculate the ASAP scheduling step for current irn.
 */
static void calculate_irn_asap(ir_node *irn, void *walk_env) {
	be_ilpsched_irn_t *node;
	be_ilpsched_env_t *env = walk_env;
	int      i;
	ir_node  *block;
	ilpsched_node_attr_t *na;

	/* These nodes are handled separate */
	if (! consider_for_sched(irn))
		return;

	DBG((env->dbg, LEVEL_2, "Calculating ASAP of node %+F\n", irn));

	node  = get_ilpsched_irn(env, irn);
	block = get_nodes_block(irn);
	na    = get_ilpsched_node_attr(node);

	/* accumulate all transitive predecessors of current node */
	for (i = get_irn_ins_or_deps(irn) - 1; i >= 0; --i) {
		ir_node              *pred = skip_Proj(get_irn_in_or_dep(irn, i));
		be_ilpsched_irn_t    *pred_node;
		ilpsched_node_attr_t *pna;
		unsigned             idx;

		if (be_is_Keep(pred))
			pred = skip_Proj(get_irn_n(pred, 0));

		if (is_Phi(pred) || block != get_nodes_block(pred) || is_NoMem(pred))
			continue;

		pred_node = get_ilpsched_irn(env, pred);
		pna       = get_ilpsched_node_attr(pred_node);
		idx       = get_irn_idx(irn);

		assert(pna->asap && "missing ASAP of predecessor");

		/*
			We have not already visited this predecessor
			-> accumulate it's predecessors
		*/
		if (pna->visit_idx != idx) {
			pna->visit_idx = idx;
			na->transitive_block_nodes = bitset_or(na->transitive_block_nodes, pna->transitive_block_nodes);
			DBG((env->dbg, LEVEL_3, "\taccumulating preds of %+F\n", pred));
		}
	}

	/* every node is it's own transitive predecessor in block */
	bitset_set(na->transitive_block_nodes, na->block_idx);

	/* asap = number of transitive predecessors in this block */
	na->asap = bitset_popcnt(na->transitive_block_nodes);

	DBG((env->dbg, LEVEL_2, "\tcalculated ASAP is %u\n", na->asap));
}

/**
 * Accumulate the successors of all nodes from irn on upwards.
 */
static void accumulate_succs(be_ilpsched_env_t *env, ir_node *irn) {
	unsigned             i, n;
	be_ilpsched_irn_t    *node  = get_ilpsched_irn(env, irn);
	ilpsched_node_attr_t *na    = get_ilpsched_node_attr(node);
	ir_node              *block = get_nodes_block(irn);
	waitq                *wq    = new_waitq();

	DBG((env->dbg, LEVEL_3, "\taccumulating succs of %+F\n", irn));

	/* enqueue node for final alap calculation */
	if (! na->enqueued) {
		be_ilpsched_irn_t     *block_node = get_ilpsched_irn(env, block);
		ilpsched_block_attr_t *ba         = get_ilpsched_block_attr(block_node);

		na->enqueued = 1;
		na->alap     = ba->n_interesting_nodes;
		waitq_put(env->alap_queue, node);

		set_irn_link(irn, ba->head_ilp_nodes);
		ba->head_ilp_nodes = irn;
		DBG((env->dbg, LEVEL_5, "\t\tlinked %+F to ilp nodes of %+F, attr %p\n", irn, block, ba));
		DBG((env->dbg, LEVEL_4, "\t\tenqueueing %+F for final ALAP calculation\n", irn));
	}

	for (i = 0, n = get_irn_ins_or_deps(irn); i < n; ++i) {
		ir_node              *pred = skip_Proj(get_irn_in_or_dep(irn, i));
		unsigned             idx;
		be_ilpsched_irn_t    *pred_node;
		ilpsched_node_attr_t *pna;

		if (be_is_Keep(pred))
			pred = skip_Proj(get_irn_n(pred, 0));

		if (is_Phi(pred) || block != get_nodes_block(pred) || is_NoMem(pred))
			continue;

		pred_node = get_ilpsched_irn(env, pred);
		pna       = get_ilpsched_node_attr(pred_node);
		idx       = get_irn_idx(irn);

		/* accumulate the successors */
		if (pna->visit_idx != idx) {
			pna->visit_idx = idx;
			pna->transitive_block_nodes = bitset_or(pna->transitive_block_nodes, na->transitive_block_nodes);

			/* set current node as successor */
			bitset_set(pna->transitive_block_nodes, na->block_idx);
			waitq_put(wq, pred);

			DBG((env->dbg, LEVEL_3, "\taccumulating succs of %+F to %+F\n", irn, pred));
		}
	}

	/* process all predecessors */
	while (! waitq_empty(wq)) {
		accumulate_succs(env, waitq_get(wq));
	}

	del_waitq(wq);
}

/**
 * Calculate the ALAP scheduling step of all irns in current block.
 * Depends on ASAP beeing calculated.
 */
static void calculate_block_alap(ir_node *block, void *walk_env) {
	be_ilpsched_env_t     *env        = walk_env;
	be_ilpsched_irn_t     *block_node = get_ilpsched_irn(env, block);
	ilpsched_block_attr_t *ba         = get_ilpsched_block_attr(block_node);

	assert(is_Block(block));

	DBG((env->dbg, LEVEL_2, "Calculating ALAP for nodes in %+F (%u nodes)\n", block, ba->n_interesting_nodes));

	/* TODO: Might be faster to use out edges and call phase_reinit_single_irn_data */
	phase_reinit_block_irn_data(&env->ph, block);

	/* calculate the alap of all nodes, starting at collected roots upwards */
	while (! waitq_empty(ba->root_nodes)) {
		accumulate_succs(env, waitq_get(ba->root_nodes));
	}

	/* we don't need it anymore */
	del_waitq(ba->root_nodes);
	ba->root_nodes = NULL;

	/* all interesting nodes should have their successors accumulated now */
	while (! waitq_empty(env->alap_queue)) {
		be_ilpsched_irn_t    *node = waitq_get(env->alap_queue);
		ilpsched_node_attr_t *na   = get_ilpsched_node_attr(node);

		/* control flow ops must always be scheduled last */
		if (is_cfop(node->irn) && ! is_Start(node->irn) && get_irn_opcode(node->irn) != iro_End)
			na->asap = na->alap;
		else
			na->alap -= bitset_popcnt(na->transitive_block_nodes);
		DBG((env->dbg, LEVEL_2, "\tALAP of %+F is %u (%u succs)\n", node->irn, na->alap, bitset_popcnt(na->transitive_block_nodes)));
	}
}


/*************************
 *   _____ _      _____
 *  |_   _| |    |  __ \
 *    | | | |    | |__) |
 *    | | | |    |  ___/
 *   _| |_| |____| |
 *  |_____|______|_|
 *
 *************************/

/**
 * Check if node can be executed on given unit type.
 */
static INLINE int is_valid_unit_type_for_node(const be_execution_unit_type_t *tp, be_ilpsched_irn_t *node) {
	int                  i;
	ilpsched_node_attr_t *na = get_ilpsched_node_attr(node);

	for (i = na->n_unit_types - 1; i >= 0; --i) {
		if (na->type_info[i].tp == tp)
			return i;
	}

	return -1;
}

static INLINE void check_for_keeps(waitq *keeps, ir_node *block, ir_node *irn) {
	const ir_edge_t *edge;

	foreach_out_edge(irn, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if (be_is_Keep(user)) {
			assert(get_nodes_block(user) == block && "Keep must not be in different block.");
			waitq_put(keeps, user);
		}
	}
}

/**
 * Adds a node, it's Projs (in case of mode_T nodes) and
 * it's Keeps to schedule.
 */
static void add_to_sched(ir_node *block, ir_node *irn) {
	const ir_edge_t *edge;
	waitq           *keeps = new_waitq();

	if (get_irn_mode(irn) == mode_M)
		return;

	sched_add_before(block, irn);

	/* add Projs */
	if (get_irn_mode(irn) == mode_T) {
		foreach_out_edge(irn, edge) {
			ir_node *user = get_edge_src_irn(edge);

			assert(is_Proj(user) && "User of mode_T node must be a Proj");

			if (to_appear_in_schedule(user))
				sched_add_before(block, user);

			check_for_keeps(keeps, block, user);
		}
	}
	else {
		check_for_keeps(keeps, block, irn);
	}

	/* add Keeps */
	while (! waitq_empty(keeps)) {
		ir_node *keep = waitq_get(keeps);
		if (! sched_is_scheduled(keep))
			sched_add_before(block, keep);
	}

	del_waitq(keeps);
}

/**
 * Schedule all nodes in the given block, according to the ILP solution.
 */
static void apply_solution(be_ilpsched_env_t *env, lpp_t *lpp, ir_node *block) {
	be_ilpsched_irn_t     *block_node = get_ilpsched_irn(env, block);
	ilpsched_block_attr_t *ba         = get_ilpsched_block_attr(block_node);
	sched_info_t          *info       = get_irn_sched_info(block);
	be_ilpsched_irn_t     **sched_nodes;
	unsigned              i, l;
	ir_node               *cfop, *irn;
	const ir_edge_t       *edge;

	/* init block schedule list */
	INIT_LIST_HEAD(&info->list);
	info->scheduled = 1;

	/* collect nodes and their scheduling time step */
	sched_nodes = NEW_ARR_F(be_ilpsched_irn_t *, 0);
	if (ba->n_interesting_nodes == 0) {
		/* ignore */
	}
	else if (ba->n_interesting_nodes == 1) {
		be_ilpsched_irn_t *node = get_ilpsched_irn(env, ba->head_ilp_nodes);

		/* add the single node */
		ARR_APP1(be_ilpsched_irn_t *, sched_nodes, node);
	}
	else {
		foreach_linked_irns(ba->head_ilp_nodes, irn) {
			be_ilpsched_irn_t    *node;
			ilpsched_node_attr_t *na;
			int                  tp_idx, found;
			unsigned             cur_var, t;

			node    = get_ilpsched_irn(env, irn);
			na      = get_ilpsched_node_attr(node);
			cur_var = 0;
			found   = 0;

			for (tp_idx = na->n_unit_types - 1; ! found && tp_idx >= 0; --tp_idx) {
				for (t = na->asap - 1; ! found && t <= na->alap - 1; ++t) {
					double val = lpp_get_var_sol(lpp, na->ilp_vars[cur_var++]);
					if (! LPP_VALUE_IS_0(val)) {
						na->sched_point = t;
						ARR_APP1(be_ilpsched_irn_t *, sched_nodes, node);
						DBG((env->dbg, LEVEL_1, "Schedpoint of %+F is %u at unit type %s\n",
							irn, t, na->type_info[tp_idx].tp->name));
						found = 1;
					}
				}
			}
		}

		/* sort nodes ascending by scheduling time step */
		qsort(sched_nodes, ARR_LEN(sched_nodes), sizeof(sched_nodes[0]), cmp_ilpsched_irn);
	}

	/* make all Phis ready and remember the single cf op */
	cfop = NULL;
	foreach_out_edge(block, edge) {
		irn = get_edge_src_irn(edge);

		switch (get_irn_opcode(irn)) {
			case iro_Phi:
				add_to_sched(block, irn);
				break;
			case iro_Start:
			case iro_End:
			case iro_Proj:
			case iro_Bad:
				break;
			default:
				if (is_cfop(irn)) {
					assert(cfop == NULL && "Highlander!");
					cfop = irn;
				}
			break;
		}
	}

	/* add all nodes from list */
	for (i = 0, l = ARR_LEN(sched_nodes); i < l; ++i) {
		add_to_sched(block, sched_nodes[i]->irn);
	}

	/* schedule control flow node if not already done */
	if (cfop && ! sched_is_scheduled(cfop))
		add_to_sched(block, cfop);

	DEL_ARR_F(sched_nodes);
}

/**
 * Create the ilp (add variables, build constraints, solve, build schedule from solution).
 */
static void create_ilp(ir_node *block, void *walk_env) {
	be_ilpsched_env_t     *env           = walk_env;
	be_ilpsched_irn_t     *block_node    = get_ilpsched_irn(env, block);
	ilpsched_block_attr_t *ba            = get_ilpsched_block_attr(block_node);
	unsigned              num_block_var  = 0;
	unsigned              num_nodes      = 0;
	unsigned              n_instr_max    = env->cpu->bundle_size * env->cpu->bundels_per_cycle;
	bitset_t              *bs_block_irns = bitset_alloca(ba->block_last_idx);
	FILE                  *logfile       = NULL;
	lpp_t                 *lpp           = NULL;
	unsigned              num_cst_assign, num_cst_prec, num_cst_resrc, num_cst_bundle;
	unsigned              t;
	int                   glob_type_idx;
	ir_node               *irn;
	char                  buf[1024];
	struct obstack        var_obst;
	lc_timer_t            *t_var;
	lc_timer_t            *t_cst_assign;
	lc_timer_t            *t_cst_prec;
	lc_timer_t            *t_cst_rsrc;
	lc_timer_t            *t_cst_bundle;
	double                fact_var, fact_cst;

#ifdef WITH_LIBCORE
	t_var        = lc_timer_register("beilpsched_var",        "create ilp variables");
	t_cst_assign = lc_timer_register("beilpsched_cst_assign", "create assignment constraints");
	t_cst_prec   = lc_timer_register("beilpsched_cst_prec",   "create precedence constraints");
	t_cst_rsrc   = lc_timer_register("beilpsched_cst_rsrc",   "create resource constraints");
	t_cst_bundle = lc_timer_register("beilpsched_cst_bundle", "create bundle constraints");
	LC_STOP_AND_RESET_TIMER(t_var);
	LC_STOP_AND_RESET_TIMER(t_cst_assign);
	LC_STOP_AND_RESET_TIMER(t_cst_prec);
	LC_STOP_AND_RESET_TIMER(t_cst_rsrc);
	LC_STOP_AND_RESET_TIMER(t_cst_bundle);
#endif /* WITH_LIBCORE */

	DBG((env->dbg, 255, "\n\n\n=========================================\n"));
	DBG((env->dbg, 255, "  ILP Scheduling for %+F\n", block));
	DBG((env->dbg, 255, "=========================================\n\n"));

	DBG((env->dbg, LEVEL_1, "Creating ILP Variables for nodes in %+F (%u interesting nodes)\n",
		block, ba->n_interesting_nodes));

	if (ba->n_interesting_nodes > 1) {
		fact_var = ba->n_interesting_nodes < 25 ? 1.0 : 0.6;
		fact_cst = ba->n_interesting_nodes < 25 ? 0.8 : 0.6;

		lpp = new_lpp_userdef(
			"be ilp scheduling",
			lpp_minimize,
			(int)((double)(ba->n_interesting_nodes * ba->n_interesting_nodes) * fact_var) + 1,  /* num vars */
			(int)((double)(ba->n_interesting_nodes * ba->n_interesting_nodes) * fact_cst) + 20, /* num cst */
			1.2);                                                                               /* grow factor */
		obstack_init(&var_obst);

		lc_timer_push(t_var);
		foreach_linked_irns(ba->head_ilp_nodes, irn) {
			const be_execution_unit_t ***execunits = arch_isa_get_allowed_execution_units(env->arch_env->isa, irn);
			be_ilpsched_irn_t         *node;
			ilpsched_node_attr_t      *na;
			unsigned                  n_unit_types, tp_idx, unit_idx, cur_var, n_var, cur_unit;

			/* count number of available unit types for this node */
			for (n_unit_types = 0; execunits[n_unit_types]; ++n_unit_types)
				/* just count */ ;

			node = get_ilpsched_irn(env, irn);
			na   = get_ilpsched_node_attr(node);

			na->n_unit_types = n_unit_types;
			na->type_info    = NEW_ARR_D(unit_type_info_t, &var_obst, n_unit_types);

			/* fill the type info array */
			for (tp_idx = 0; tp_idx < n_unit_types; ++tp_idx) {
				for (unit_idx = 0; execunits[tp_idx][unit_idx]; ++unit_idx)
					/* just count units of this type */;

				na->type_info[tp_idx].tp      = execunits[tp_idx][0]->tp;
				na->type_info[tp_idx].n_units = unit_idx;
			}

			/* allocate space for ilp variables */
			na->ilp_vars = NEW_ARR_D(int, &var_obst, n_unit_types * VALID_SCHED_INTERVAL(na));
			memset(na->ilp_vars, -1, n_unit_types * VALID_SCHED_INTERVAL(na) * sizeof(na->ilp_vars[0]));

			DBG((env->dbg, LEVEL_3, "\thandling %+F (asap %u, alap %u, unit types %u):\n",
				irn, na->asap, na->alap, na->n_unit_types));

			cur_var = cur_unit = n_var = 0;
			/* create variables */
			for (tp_idx = 0; tp_idx < n_unit_types; ++tp_idx) {
				for (t = na->asap - 1; t <= na->alap - 1; ++t) {
					snprintf(buf, sizeof(buf), "n%u_%s_%u",
						get_irn_idx(irn), na->type_info[tp_idx].tp->name, t);
					na->ilp_vars[cur_var++] = lpp_add_var(lpp, buf, lpp_binary, (double)(t + 1));
					n_var++;
					num_block_var++;
					DBG((env->dbg, LEVEL_4, "\t\tcreated ILP variable %s\n", buf));
				}
			}

			DB((env->dbg, LEVEL_3, "%u variables created\n", n_var));
			num_nodes++;
		}
		lc_timer_pop();
		DBG((env->dbg, LEVEL_1, "... %u variables for %u nodes created (%g sec)\n",
			num_block_var, num_nodes, lc_timer_elapsed_usec(t_var) / 1000000.0));

		/*
			1st:
			- the assignment constraints:
				assure each node is executed once by exactly one (allowed) execution unit
			- the precedence constraints:
				assure that no data dependencies are violated
		*/
		num_cst_assign = num_cst_prec = num_cst_resrc = num_cst_bundle = 0;
		DBG((env->dbg, LEVEL_1, "Creating constraints for nodes in %+F:\n", block));
		foreach_linked_irns(ba->head_ilp_nodes, irn) {
			int                  cst, tp_idx, i;
			unsigned             cur_var;
			be_ilpsched_irn_t    *node;
			ilpsched_node_attr_t *na;

			/* the assignment constraint */
			lc_timer_push(t_cst_assign);
			snprintf(buf, sizeof(buf), "assignment_cst_n%u", get_irn_idx(irn));
			cst = lpp_add_cst_uniq(lpp, buf, lpp_equal, 1.0);
			DBG((env->dbg, LEVEL_2, "added constraint %s\n", buf));
			num_cst_assign++;

			node    = get_ilpsched_irn(env, irn);
			na      = get_ilpsched_node_attr(node);
			cur_var = 0;

			lpp_set_factor_fast_bulk(lpp, cst, na->ilp_vars, na->n_unit_types * VALID_SCHED_INTERVAL(na), 1.0);
			lc_timer_pop();

			/* the precedence constraints */
			lc_timer_push(t_cst_prec);
			bs_block_irns = bitset_clear_all(bs_block_irns);
			for (i = get_irn_ins_or_deps(irn) - 1; i >= 0; --i) {
				ir_node              *pred = skip_Proj(get_irn_in_or_dep(irn, i));
				unsigned             t_low, t_high;
				be_ilpsched_irn_t    *pred_node;
				ilpsched_node_attr_t *pna;

				if (be_is_Keep(pred))
					pred = skip_Proj(get_irn_n(pred, 0));

				if (is_Phi(pred) || block != get_nodes_block(pred) || is_NoMem(pred))
					continue;

				pred_node = get_ilpsched_irn(env, pred);
				pna       = get_ilpsched_node_attr(pred_node);

				assert(pna->asap > 0 && pna->alap >= pna->asap && "Invalid scheduling interval.");

				if (! bitset_is_set(bs_block_irns, pna->block_idx))
					bitset_set(bs_block_irns, pna->block_idx);
				else
					continue;

				/* irn = n, pred = m */
				t_low  = MAX(na->asap, pna->asap);
				t_high = MIN(na->alap, pna->alap);
				for (t = t_low - 1; t <= t_high - 1; ++t) {
					int tn, tm, cur_idx;
					int int_na       = (t >= na->asap - 1) ? t - na->asap + 2 : 0;
					int int_pna      = (t < pna->alap)     ? pna->alap - t    : 0;
					int *tmp_var_idx = NEW_ARR_F(int, int_na * na->n_unit_types + int_pna * pna->n_unit_types);

					snprintf(buf, sizeof(buf), "precedence_n%u_n%u_%u", get_irn_idx(pred), get_irn_idx(irn), t);
					cst = lpp_add_cst_uniq(lpp, buf, lpp_less, 1.0);
					DBG((env->dbg, LEVEL_2, "added constraint %s\n", buf));
					num_cst_prec++;

					cur_idx = 0;

					/* lpp_set_factor_fast_bulk needs variables sorted ascending by index */
					if (na->ilp_vars[0] < pna->ilp_vars[0]) {
						/* node variables have smaller index than pred variables */
						for (tp_idx = na->n_unit_types - 1; tp_idx >= 0; --tp_idx) {
							for (tn = na->asap - 1; tn <= t; ++tn) {
								unsigned idx = ILPVAR_IDX(na, tp_idx, tn);
								tmp_var_idx[cur_idx++] = na->ilp_vars[idx];
							}
						}

						for (tp_idx = pna->n_unit_types - 1; tp_idx >= 0; --tp_idx) {
							for (tm = t; tm < pna->alap; ++tm) {
								unsigned idx = ILPVAR_IDX(pna, tp_idx, tm);
								tmp_var_idx[cur_idx++] = pna->ilp_vars[idx];
							}
						}
					}
					else {
						/* pred variables have smaller index than node variables */
						for (tp_idx = pna->n_unit_types - 1; tp_idx >= 0; --tp_idx) {
							for (tm = t; tm < pna->alap; ++tm) {
								unsigned idx = ILPVAR_IDX(pna, tp_idx, tm);
								tmp_var_idx[cur_idx++] = pna->ilp_vars[idx];
							}
						}

						for (tp_idx = na->n_unit_types - 1; tp_idx >= 0; --tp_idx) {
							for (tn = na->asap - 1; tn <= t; ++tn) {
								unsigned idx = ILPVAR_IDX(na, tp_idx, tn);
								tmp_var_idx[cur_idx++] = na->ilp_vars[idx];
							}
						}
					}

					lpp_set_factor_fast_bulk(lpp, cst, tmp_var_idx, cur_idx, 1.0);

					DEL_ARR_F(tmp_var_idx);
				}
			}
			lc_timer_pop();
		}
		DBG((env->dbg, LEVEL_1, "\t%u assignement constraints (%g sec)\n",
			num_cst_assign, lc_timer_elapsed_usec(t_cst_assign) / 1000000.0));
		DBG((env->dbg, LEVEL_1, "\t%u precedence constraints (%g sec)\n",
			num_cst_prec, lc_timer_elapsed_usec(t_cst_prec) / 1000000.0));

		/*
			2nd: the resource constraints:
			assure that for each time step not more instructions are scheduled
			to the same unit types as units of this type are available
		*/
		lc_timer_push(t_cst_rsrc);
		for (glob_type_idx = env->cpu->n_unit_types - 1; glob_type_idx >= 0; --glob_type_idx) {
			unsigned t;
			be_execution_unit_type_t *cur_tp = &env->cpu->unit_types[glob_type_idx];

			for (t = 0; t < ba->n_interesting_nodes; ++t) {
				int cst;
				int *tmp_var_idx = NEW_ARR_F(int, ba->n_interesting_nodes);
				int cur_idx      = 0;

				snprintf(buf, sizeof(buf), "resource_cst_%s_%u", cur_tp->name, t);
				cst = lpp_add_cst_uniq(lpp, buf, lpp_less, (double)cur_tp->n_units);
				DBG((env->dbg, LEVEL_2, "added constraint %s\n", buf));
				num_cst_resrc++;

				foreach_linked_irns(ba->head_ilp_nodes, irn) {
					be_ilpsched_irn_t    *node = get_ilpsched_irn(env, irn);
					ilpsched_node_attr_t *na   = get_ilpsched_node_attr(node);
					int                  tp_idx;

					tp_idx = is_valid_unit_type_for_node(cur_tp, node);

					if (tp_idx >= 0 && t >= na->asap - 1 && t <= na->alap - 1) {
						int cur_var = ILPVAR_IDX(na, tp_idx, t);
						tmp_var_idx[cur_idx++] = na->ilp_vars[cur_var];
					}
				}

				if (cur_idx)
					lpp_set_factor_fast_bulk(lpp, cst, tmp_var_idx, cur_idx, 1.0);

				DEL_ARR_F(tmp_var_idx);
			}
		}
		lc_timer_pop();
		DBG((env->dbg, LEVEL_1, "\t%u resource constraints (%g sec)\n",
			num_cst_resrc, lc_timer_elapsed_usec(t_cst_rsrc) / 1000000.0));

		/*
			3rd: bundle constraints:
			assure, at most bundle_size * bundles_per_cycle instructions
			can be started at a certain point.
		*/
		lc_timer_push(t_cst_bundle);
		for (t = 0; t < ba->n_interesting_nodes; ++t) {
			int cst;
			int *tmp_var_idx = NEW_ARR_F(int, 0);

			snprintf(buf, sizeof(buf), "bundle_cst_%u", t);
			cst = lpp_add_cst_uniq(lpp, buf, lpp_less, (double)n_instr_max);
			DBG((env->dbg, LEVEL_2, "added constraint %s\n", buf));
			num_cst_bundle++;

			foreach_linked_irns(ba->head_ilp_nodes, irn) {
				be_ilpsched_irn_t    *node;
				ilpsched_node_attr_t *na;
				int                  tp_idx;

				node = get_ilpsched_irn(env, irn);
				na   = get_ilpsched_node_attr(node);

				if (t >= na->asap - 1 && t <= na->alap - 1) {
					for (tp_idx = na->n_unit_types - 1; tp_idx >= 0; --tp_idx) {
						int idx = ILPVAR_IDX(na, tp_idx, t);
						ARR_APP1(int, tmp_var_idx, na->ilp_vars[idx]);
					}
				}
			}

			if (ARR_LEN(tmp_var_idx) > 0)
				lpp_set_factor_fast_bulk(lpp, cst, tmp_var_idx, ARR_LEN(tmp_var_idx), 1.0);

			DEL_ARR_F(tmp_var_idx);
		}
		lc_timer_pop();
		DBG((env->dbg, LEVEL_1, "\t%u bundle constraints (%g sec)\n",
			num_cst_bundle, lc_timer_elapsed_usec(t_cst_bundle) / 1000000.0));

		DBG((env->dbg, LEVEL_1, "ILP to solve: %u variables, %u constraints\n", lpp->var_next, lpp->cst_next));

		/* debug stuff, dump lpp when debugging is on  */
		DEBUG_ONLY(
			if (firm_dbg_get_mask(env->dbg) > 0) {
				FILE *f;

				snprintf(buf, sizeof(buf), "lpp_block_%lu.txt", get_irn_node_nr(block));
				f = fopen(buf, "w");
				lpp_dump_plain(lpp, f);
				fclose(f);
				snprintf(buf, sizeof(buf), "lpp_block_%lu.mps", get_irn_node_nr(block));
				lpp_dump(lpp, "buf");
			}
		);

		/* set solve time limit */
		lpp_set_time_limit(lpp, env->opts->time_limit);

		/* set logfile if requested */
		if (strlen(env->opts->log_file) > 0) {
			if (strcasecmp(env->opts->log_file, "stdout") == 0)
				lpp_set_log(lpp, stdout);
			else if (strcasecmp(env->opts->log_file, "stderr") == 0)
				lpp_set_log(lpp, stderr);
			else {
				logfile = fopen(env->opts->log_file, "w");
				if (! logfile)
					fprintf(stderr, "Could not open logfile '%s'! Logging disabled.\n", env->opts->log_file);
				else
					lpp_set_log(lpp, logfile);
			}
		}

		/* solve the ILP */
		lpp_solve_net(lpp, env->main_env->options->ilp_server, env->main_env->options->ilp_solver);

		if (logfile)
			fclose(logfile);

		/* check for valid solution */
		if (! lpp_is_sol_valid(lpp)) {
			FILE *f;

			snprintf(buf, sizeof(buf), "lpp_block_%lu.assert.txt", get_irn_node_nr(block));
			f = fopen(buf, "w");
			lpp_dump_plain(lpp, f);
			fclose(f);
			snprintf(buf, sizeof(buf), "lpp_block_%lu.assert.mps", get_irn_node_nr(block));
			lpp_dump(lpp, buf);
			dump_ir_block_graph(env->irg, "-assert");

			assert(0 && "ILP solution is not feasible!");
		}

		DBG((env->dbg, LEVEL_1, "\nSolution:\n"));
		DBG((env->dbg, LEVEL_1, "\tsend time: %g sec\n", lpp->send_time / 1000000.0));
		DBG((env->dbg, LEVEL_1, "\treceive time: %g sec\n", lpp->recv_time / 1000000.0));
		DBG((env->dbg, LEVEL_1, "\titerations: %d\n", lpp->iterations));
		DBG((env->dbg, LEVEL_1, "\tsolution time: %g\n", lpp->sol_time));
		DBG((env->dbg, LEVEL_1, "\tobjective function: %g\n", LPP_VALUE_IS_0(lpp->objval) ? 0.0 : lpp->objval));
		DBG((env->dbg, LEVEL_1, "\tbest bound: %g\n", LPP_VALUE_IS_0(lpp->best_bound) ? 0.0 : lpp->best_bound));

		obstack_free(&var_obst, NULL);
	}

	/* apply solution */
	apply_solution(env, lpp, block);

	if (lpp)
		free_lpp(lpp);
}

/**
 * Perform ILP scheduling on the given irg.
 */
void be_ilp_sched(const be_irg_t *birg) {
	be_ilpsched_env_t env;
	const char        *name = "be ilp scheduling";

	FIRM_DBG_REGISTER(env.dbg, "firm.be.sched.ilp");

	//firm_dbg_set_mask(env.dbg, 31);

	env.irg        = birg->irg;
	env.main_env   = birg->main_env;
	env.alap_queue = new_waitq();
	env.arch_env   = birg->main_env->arch_env;
	env.cpu        = arch_isa_get_machine(birg->main_env->arch_env->isa);
	env.opts       = &ilp_opts;
	phase_init(&env.ph, name, env.irg, PHASE_DEFAULT_GROWTH, init_ilpsched_irn);

	irg_walk_in_or_dep_graph(env.irg, NULL, build_block_idx, &env);

	/*
		The block indicees are completely build after the walk,
		now we can allocate the bitsets (size depends on block indicees)
		for all nodes.
	*/
	phase_reinit_irn_data(&env.ph);

	/* Collect all root nodes (having no user in their block) and calculate ASAP. */
	irg_walk_in_or_dep_blkwise_graph(env.irg, collect_alap_root_nodes, calculate_irn_asap, &env);

	/* calculate ALAP and create variables */
	irg_block_walk_graph(env.irg, calculate_block_alap, create_ilp, &env);

	DEBUG_ONLY(
		if (firm_dbg_get_mask(env.dbg)) {
			phase_stat_t stat;
			phase_stat_t *stat_ptr = phase_stat(&env.ph, &stat);

			fprintf(stderr, "Phase used: %u bytes\n", stat_ptr->overall_bytes);
		}
	);

	/* free all allocated object */
	del_waitq(env.alap_queue);
	phase_free(&env.ph);
}

#ifdef WITH_LIBCORE
/**
 * Register ILP scheduler options.
 */
void ilpsched_register_options(lc_opt_entry_t *grp) {
	static int     run_once = 0;
	lc_opt_entry_t *sched_grp;

	if (! run_once) {
		run_once  = 1;
		sched_grp = lc_opt_get_grp(grp, "ilpsched");

		lc_opt_add_table(sched_grp, ilpsched_option_table);
	}
}
#endif /* WITH_LIBCORE */

#else /* WITH_ILP */

static int some_picky_compiler_do_not_allow_empty_files;

#endif /* WITH_ILP */
