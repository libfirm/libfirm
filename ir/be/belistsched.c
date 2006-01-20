/**
 * Scheduling algorithms.
 * Just a simple list scheduling algorithm is here.
 * @date 20.10.2004
 * @author Sebastian Hack
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>

#include "obst.h"
#include "list.h"
#include "iterator.h"

#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "irdump.h"
#include "irprintf_t.h"
#include "debug.h"

#include "besched_t.h"
#include "beutil.h"
#include "belive_t.h"
#include "belistsched.h"
#include "bearch.h"

#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN(x,y) ((x) < (y) ? (x) : (y))

/**
 * Scheduling environment for the whole graph.
 */
typedef struct _sched_env_t {
    const ir_graph *irg;                        /**< The graph to schedule. */
    const list_sched_selector_t *selector;		  /**< The node selector. */
    void *selector_env;                         /**< A pointer to give to the selector. */
} sched_env_t;

#if 0
/*
 * Ugly global variable for the compare function
 * since qsort(3) does not pass an extra pointer.
 */
static ir_node *curr_bl = NULL;

static int cmp_usage(const void *a, const void *b)
{
	struct trivial_sched_env *env;
	const ir_node *p = a;
	const ir_node *q = b;
	int res = 0;

	res = is_live_end(env->curr_bl, a) - is_live_end(env->curr_bl, b);

	/*
	 * One of them is live at the end of the block.
	 * Then, that one shall be scheduled at after the other
	 */
	if(res != 0)
		return res;


	return res;
}
#endif

static ir_node *trivial_select(void *block_env, pset *ready_set)
{
	ir_node *res;

	res = pset_first(ready_set);
	pset_break(ready_set);
	return res;
}

static int default_to_appear_in_schedule(void *env, const ir_node *irn)
{
	return to_appear_in_schedule(irn);
}

static const list_sched_selector_t trivial_selector_struct = {
	NULL,
	NULL,
	trivial_select,
	default_to_appear_in_schedule,
	NULL,
	NULL
};

const list_sched_selector_t *trivial_selector = &trivial_selector_struct;

typedef struct _usage_stats_t {
	ir_node *irn;
	struct _usage_stats_t *next;
	int max_hops;
	int uses_in_block;      /**< Number of uses inside the current block. */
	int already_consumed;   /**< Number of insns using this value already
							  scheduled. */
} usage_stats_t;

typedef struct {
	struct obstack obst;
	usage_stats_t *root;
	pset *already_scheduled;
	const list_sched_selector_t *vtab;
} reg_pressure_selector_env_t;

static INLINE usage_stats_t *get_or_set_usage_stats(reg_pressure_selector_env_t *env, ir_node *irn)
{
	usage_stats_t *us = get_irn_link(irn);

	if(!us) {
		us                   = obstack_alloc(&env->obst, sizeof(us[0]));
		us->irn              = irn;
		us->already_consumed = 0;
		us->max_hops         = INT_MAX;
		us->next             = env->root;
		env->root            = us;
		set_irn_link(irn, us);
	}

	return us;
}

static INLINE usage_stats_t *get_usage_stats(ir_node *irn)
{
	usage_stats_t *us = get_irn_link(irn);
	assert(us && "This node must have usage stats");
	return us;
}

static int max_hops_walker(ir_node *irn, ir_node *tgt, int depth, unsigned visited_nr)
{
	int i, n;
	int res = 0;

	if(irn != tgt) {
		res = INT_MAX;

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *operand = get_irn_n(irn, i);

			if(get_irn_visited(operand) < visited_nr) {
				int tmp;

				set_irn_visited(operand, visited_nr);
				tmp = max_hops_walker(operand, tgt, depth + 1, visited_nr);
				res = MAX(tmp, res);
			}
		}
	}

	return res;
}

static int compute_max_hops(reg_pressure_selector_env_t *env, ir_node *irn)
{
	ir_node *bl   = get_nodes_block(irn);
	ir_graph *irg = get_irn_irg(bl);
	int res       = INT_MAX;

	const ir_edge_t *edge;

	foreach_out_edge(irn, edge) {
		ir_node *user = get_edge_src_irn(edge);

		if(get_nodes_block(user) == bl && !pset_find_ptr(env->already_scheduled, user)) {
			unsigned visited_nr = get_irg_visited(irg) + 1;
			int max_hops;

			set_irg_visited(irg, visited_nr);
			max_hops = max_hops_walker(user, irn, 0, visited_nr);
			res = MAX(res, max_hops);
		}
	}

	return res;
}

static void *reg_pressure_graph_init(const list_sched_selector_t *vtab, const arch_isa_t *isa, ir_graph *irg)
{
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	return (void *) vtab;
}

static void *reg_pressure_block_init(void *graph_env, ir_node *bl)
{
	ir_node *irn;
	reg_pressure_selector_env_t *env  = xmalloc(sizeof(env[0]));

	obstack_init(&env->obst);
	env->already_scheduled = pset_new_ptr(32);
	env->root              = NULL;
	env->vtab              = graph_env;

	/*
	 * Collect usage statistics.
	 */
	sched_foreach(bl, irn) {
		if(env->vtab->to_appear_in_schedule(env, irn)) {
			int i, n;

			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);
				if(env->vtab->to_appear_in_schedule(env, irn)) {
					usage_stats_t *us = get_or_set_usage_stats(env, irn);
					if(is_live_end(bl, op))
						us->uses_in_block = 99999;
					else
						us->uses_in_block++;
				}
			}
		}
	}

	return env;
}

static void reg_pressure_block_free(void *block_env)
{
	reg_pressure_selector_env_t *env = block_env;
	usage_stats_t *us;

	for(us = env->root; us; us = us->next)
		set_irn_link(us->irn, NULL);

	obstack_free(&env->obst, NULL);
	del_pset(env->already_scheduled);
	free(env);
}

static INLINE int reg_pr_costs(reg_pressure_selector_env_t *env, ir_node *irn)
{
	int i, n;
	int sum = 0;

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if(env->vtab->to_appear_in_schedule(env, op))
			sum += compute_max_hops(env, op);
	}

	return sum;
}

ir_node *reg_pressure_select(void *block_env, pset *ready_set)
{
	reg_pressure_selector_env_t *env = block_env;
	ir_node *irn, *res     = NULL;
	int curr_cost          = INT_MAX;

	assert(pset_count(ready_set) > 0);

	for(irn = pset_first(ready_set); irn; irn = pset_next(ready_set)) {
		int costs = reg_pr_costs(env, irn);
		if(costs <= curr_cost) {
			res       = irn;
			curr_cost = costs;
		}
	}

	pset_insert_ptr(env->already_scheduled, res);
	return res;
}

static const list_sched_selector_t reg_pressure_selector_struct = {
	reg_pressure_graph_init,
	reg_pressure_block_init,
	reg_pressure_select,
	default_to_appear_in_schedule,
	reg_pressure_block_free,
	NULL
};

const list_sched_selector_t *reg_pressure_selector = &reg_pressure_selector_struct;

static void list_sched_block(ir_node *block, void *env_ptr);

void list_sched(const struct _arch_isa_t *isa, ir_graph *irg)
{
	sched_env_t env;
	const list_sched_selector_t *selector;

	memset(&env, 0, sizeof(env));
	selector = env.selector = isa->impl->get_list_sched_selector(isa);
	env.selector_env = selector->init_graph ? selector->init_graph(selector, isa, irg) : NULL;
	env.irg = irg;

	/* Assure, that the out edges are computed */
	edges_assure(irg);

	/* Schedule each single block. */
	irg_block_walk_graph(irg, list_sched_block, NULL, &env);

	if(selector->finish_graph)
		selector->finish_graph(env.selector_env);
}


/**
 * Environment for a block scheduler.
 */
typedef struct _block_sched_env_t {
	int curr_time;
	pset *ready_set;
	pset *already_scheduled;
	ir_node *block;
	firm_dbg_module_t *dbg;
	const list_sched_selector_t *selector;
	void *selector_block_env;
} block_sched_env_t;

/**
 * Try to put a node in the ready set.
 * @param env The block scheduler environment.
 * @param irn The node to make ready.
 * @return 1, if the node could be made ready, 0 else.
 */
static INLINE int make_ready(block_sched_env_t *env, ir_node *irn)
{
    int i, n;

    /* Blocks cannot be scheduled. */
    if(is_Block(irn))
        return 0;

    /*
     * Check, if the given ir node is in a different block as the
     * currently scheduled one. If that is so, don't make the node ready.
     */
    if(env->block != get_nodes_block(irn))
        return 0;

    for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
        ir_node *op = get_irn_n(irn, i);

        /* If the operand is local to the scheduled block and not yet
         * scheduled, this nodes cannot be made ready, so exit. */
        if(!pset_find_ptr(env->already_scheduled, op) && get_nodes_block(op) == env->block)
            return 0;
    }

    DBG((env->dbg, LEVEL_2, "\tmaking ready: %+F\n", irn));
    pset_insert_ptr(env->ready_set, irn);

    return 1;
}

/**
 * Check, if a node is ready in a block schedule.
 * @param env The block schedule environment.
 * @param irn The node to check for.
 * @return 1 if the node was ready, 0 if not.
 */
#define is_ready(env,irn) \
  (pset_find_ptr((env)->ready_set, irn) != NULL)

/**
 * Check, if a node has already been schedules.
 * @param env The block schedule environment.
 * @param irn The node to check for.
 * @return 1 if the node was already scheduled, 0 if not.
 */
#define is_scheduled(env,irn) \
  (pset_find_ptr((env)->already_scheduled, irn) != NULL)

/**
 * Try, to make all users of a node ready.
 * In fact, a usage node can only be made ready, if all its operands
 * have already been scheduled yet. This is checked my make_ready().
 * @param env The block schedule environment.
 * @param irn The node, which usages (successors) are to be made ready.
 */
static INLINE void make_users_ready(block_sched_env_t *env, ir_node *irn)
{
	const ir_edge_t *edge;

	foreach_out_edge(irn, edge) {
		ir_node *user = edge->src;
		if(!is_Phi(user))
			make_ready(env, user);
	}
}

/**
 * Compare to nodes using pointer equality.
 * @param p1 Node one.
 * @param p2 Node two.
 * @return 0 if they are identical.
 */
static int node_cmp_func(const void *p1, const void *p2)
{
    return p1 != p2;
}

/**
 * Append an instruction to a schedule.
 * @param env The block scheduleing environment.
 * @param irn The node to add to the schedule.
 * @return The given node.
 */
static ir_node *add_to_sched(block_sched_env_t *env, ir_node *irn)
{
    /* If the node consumes/produces data, it is appended to the schedule
     * list, otherwise, it is not put into the list */
    if(to_appear_in_schedule(irn)) {
        sched_info_t *info = get_irn_sched_info(irn);
        INIT_LIST_HEAD(&info->list);
        info->scheduled = 1;
        sched_add_before(env->block, irn);

        DBG((env->dbg, LEVEL_2, "\tadding %+F\n", irn));
    }

    /* Insert the node in the set of all already scheduled nodes. */
    pset_insert_ptr(env->already_scheduled, irn);

    /* Remove the node from the ready set */
    if(pset_find_ptr(env->ready_set, irn))
        pset_remove_ptr(env->ready_set, irn);

    return irn;
}


/**
 * Add the proj nodes of a tuple-mode irn to the schedule immediately
 * after the tuple-moded irn. By pinning the projs after the irn, no
 * other nodes can create a new lifetime between the tuple-moded irn and
 * one of its projs. This should render a realistic image of a
 * tuple-moded irn, which in fact models a node which defines multiple
 * values.
 *
 * @param irn The tuple-moded irn.
 * @param list The schedule list to append all the projs.
 * @param time The time step to which the irn and all its projs are
 * related to.
 * @param obst The obstack the scheduling data structures shall be
 * created upon.
 * @param ready_set The ready set of the list scheduler.
 * @param already_scheduled A set containing all nodes already
 * scheduled.
 */
static void add_tuple_projs(block_sched_env_t *env, ir_node *irn)
{
	const ir_edge_t *edge;

	assert(get_irn_mode(irn) == mode_T && "Mode of node must be tuple");

	foreach_out_edge(irn, edge) {
		ir_node *out = edge->src;

		assert(is_Proj(out) && "successor of a modeT node must be a proj");

		if(get_irn_mode(out) == mode_T)
			add_tuple_projs(env, out);
		else {
			add_to_sched(env, out);
			make_users_ready(env, out);
		}
	}
}

/**
 * Perform list scheduling on a block.
 *
 * Note, that the caller must compute a linked list of nodes in the block
 * using the link field before calling this function.
 *
 * Also the outs must have been computed.
 *
 * @param block The block node.
 * @param env Scheduling environment.
 */
static void list_sched_block(ir_node *block, void *env_ptr)
{
	sched_env_t *env = env_ptr;
	block_sched_env_t be;
	const list_sched_selector_t *selector = env->selector;
	const ir_edge_t *edge;
	ir_node *irn;
	ir_node *start_node = get_irg_start(get_irn_irg(block));
	ir_node *final_jmp  = NULL;
	int j, m;
	int phi_seen = 0;
	sched_info_t *info = get_irn_sched_info(block);

	/* Initialize the block's list head that will hold the schedule. */
	INIT_LIST_HEAD(&info->list);

	/* Initialize the block scheduling environment */
	be.dbg               = firm_dbg_register("firm.be.sched");
	be.block             = block;
	be.curr_time         = 0;
	be.ready_set         = new_pset(node_cmp_func, get_irn_n_edges(block));
	be.already_scheduled = new_pset(node_cmp_func, get_irn_n_edges(block));
	be.selector          = selector;

	firm_dbg_set_mask(be.dbg, 0);

	if(selector->init_block)
		be.selector_block_env = selector->init_block(env->selector_env, block);

	DBG((be.dbg, LEVEL_1, "scheduling %+F\n", block));

	/* Then one can add all nodes are ready to the set. */
	foreach_out_edge(block, edge) {
		ir_node *irn = get_edge_src_irn(edge);

		/* Skip the end node because of keepalive edges. */
		if(get_irn_opcode(irn) == iro_End)
			continue;

		/* Phi functions are scheduled immediately, since they only transfer
		 * data flow from the predecessors to this block. */
		if(is_Phi(irn)) {
			add_to_sched(&be, irn);
			make_users_ready(&be, irn);
			phi_seen = 1;
		}

		else if(irn == start_node) {
			add_to_sched(&be, irn);
			add_tuple_projs(&be, irn);
		}

		else if(get_irn_opcode(irn) == iro_Jmp) {
			final_jmp = irn;
		}

		/* Other nodes must have all operands in other blocks to be made
		 * ready */
		else {
			int ready = 1;

			/* Check, if the operands of a node are not local to this block */
			for(j = 0, m = get_irn_arity(irn); j < m; ++j) {
				ir_node *operand = get_irn_n(irn, j);

				if(get_nodes_block(operand) == block) {
					ready = 0;
					break;
				}
			}

			/* Make the node ready, if all operands live in a foreign block */
			if(ready) {
				DBG((be.dbg, LEVEL_2, "\timmediately ready: %+F\n", irn));
				make_ready(&be, irn);
			}
		}
	}

	/* Increase the time, if some phi functions have been scheduled */
	be.curr_time += phi_seen;

	while(pset_count(be.ready_set) > 0) {
		/* select a node to be scheduled and check if it was ready */
		irn = selector->select(be.selector_block_env, be.ready_set);

		DBG((be.dbg, LEVEL_3, "\tpicked node %+F\n", irn));

		/* Add the node to the schedule. */
		add_to_sched(&be, irn);

		if(get_irn_mode(irn) == mode_T)
			add_tuple_projs(&be, irn);
		else
			make_users_ready(&be, irn);

		/* Increase the time step. */
		be.curr_time += 1;

		/* remove the scheduled node from the ready list. */
		if(pset_find_ptr(be.ready_set, irn))
			pset_remove_ptr(be.ready_set, irn);
	}

	if(selector->finish_block)
		selector->finish_block(be.selector_block_env);

	if(final_jmp)
		add_to_sched(&be, final_jmp);

	del_pset(be.ready_set);
	del_pset(be.already_scheduled);
}
