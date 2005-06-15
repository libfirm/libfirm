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

#include "fourcc.h"
#include "obst.h"
#include "irouts.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "list.h"
#include "iterator.h"
#include "irdump.h"
#include "irprintf_t.h"
#include "debug.h"

#include "besched_t.h"
#include "beutil.h"
#include "belistsched.h"

/**
 * Scheduling environment for the whole graph.
 */
typedef struct _sched_env_t {
    const ir_graph *irg;                        /**< The graph to schedule. */
    const list_sched_selector_t *selector;		  /**< The node selector. */
    void *selector_env;                         /**< A pointer to give to the selector. */
} sched_env_t;

static ir_node *trivial_select(void *env, void *block_env, const struct list_head *sched_head,
		int curr_time, pset *ready_set)
{
    ir_node *res = pset_first(ready_set);
    pset_break(ready_set);
    return res;
}

static const list_sched_selector_t trivial_selector_struct = {
	NULL,
	NULL,
	trivial_select,
	NULL,
	NULL
};

const list_sched_selector_t *trivial_selector = &trivial_selector_struct;

static void list_sched_block(ir_node *block, void *env_ptr);

void list_sched(ir_graph *irg, const list_sched_selector_t *selector)
{
    sched_env_t env;

    memset(&env, 0, sizeof(env));
    env.selector = selector;
    env.selector_env = selector->init_graph ? selector->init_graph(irg) : NULL;
    env.irg = irg;

    /* Normalize proj nodes. */
    normalize_proj_nodes(irg);

    /* Compute the outs */
    if(get_irg_outs_state(irg) != outs_consistent)
        compute_outs(irg);

    /* Dump the graph. */
    //dump_ir_block_graph(irg, "-before-sched");

    /* Schedule each single block. */
    irg_block_walk_graph(irg, list_sched_block, NULL, &env);

		if(selector->finish_graph)
			selector->finish_graph(env.selector_env, irg);
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
    int i, n;

    for(i = 0, n = get_irn_n_outs(irn); i < n; ++i) {
        ir_node *user = get_irn_out(irn, i);
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
    int i, n;
    assert(get_irn_mode(irn) == mode_T && "Mode of node must be tuple");

    for(i = 0, n = get_irn_n_outs(irn); i < n; ++i) {
        ir_node *out = get_irn_out(irn, i);

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
 * @param env Schedulting environment.
 */
static void list_sched_block(ir_node *block, void *env_ptr)
{
	void *block_env = NULL;
	sched_env_t *env = env_ptr;
	block_sched_env_t be;
	const list_sched_selector_t *selector = env->selector;

	ir_node *irn;
	int i, n, j, m;
	int phi_seen = 0;
	sched_info_t *info = get_irn_sched_info(block);

	/* Initialize the block's list head that will hold the schedule. */
	INIT_LIST_HEAD(&info->list);

	/* Initialize the block scheduling environment */
	be.dbg = firm_dbg_register("firm.be.sched");
	be.block = block;
	be.curr_time = 0;
	be.ready_set = new_pset(node_cmp_func, get_irn_n_outs(block));
	be.already_scheduled = new_pset(node_cmp_func, get_irn_n_outs(block));

  firm_dbg_set_mask(be.dbg, 0);

	if(selector->init_block)
		block_env = selector->init_block(env->selector_env, block);

	DBG((be.dbg, LEVEL_1, "scheduling %+F\n", block));

	/* Then one can add all nodes are ready to the set. */
	for(i = 0, n = get_irn_n_outs(block); i < n; ++i) {
		ir_node *irn = get_irn_out(block, i);

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

		/* Other nodes must have all operands in other blocks to be made
		 * ready */
		else {
			bool ready = true;

			/* Check, if the operands of a node are not local to this block */
			for(j = 0, m = get_irn_arity(irn); j < m; ++j) {
				ir_node *operand = get_irn_n(irn, j);

				if(get_nodes_block(operand) == block) {
					ready = false;
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
		// DBG((be.dbg, LEVEL_2, "\tready set: %*n\n", pset_iterator, be.ready_set));

		/* select a node to be scheduled and check if it was ready */
		irn = selector->select(env->selector_env, block_env, &info->list, be.curr_time, be.ready_set);

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
		selector->finish_block(env->selector_env, block_env, block);

	del_pset(be.ready_set);
	del_pset(be.already_scheduled);
}
