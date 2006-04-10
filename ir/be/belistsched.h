/**
 * Primitive list scheduling.
 * @date 20.10.2004
 * @author Sebastian Hack
 */

#ifndef _FIRM_LIST_SCHED
#define _FIRM_LIST_SCHED

#include "firm_types.h"

#include "benodesets.h"
#include "bearch_t.h"

typedef struct _list_sched_selector_t list_sched_selector_t;

/**
 * A selector interface which is used by the list schedule framework.
 * You can implement your own list scheduler by implementing these
 * functions.
 */
struct _list_sched_selector_t {

	/**
	 * Called before a graph is being scheduled.
	 * @param arch_env The architecture environment.
	 * @param irg      The graph.
	 * @return         The environment pointer that is passed to all other functions in this struct.
	 */
	void *(*init_graph)(const list_sched_selector_t *vtab, const arch_env_t *arch_env, ir_graph *irg);

	/**
	 * Called before scheduling starts on a block.
	 * @param graph_env The environment.
	 * @param block The block which is to be scheduled.
	 * @return A per-block pointer that is additionally passed to select.
	 */
	void *(*init_block)(void *graph_env, ir_node *block);

	/**
	 * The selection function.
	 * It picks one node out of the ready list to be scheduled next.
	 * The function does not have to delete the node from the ready set.
	 *
	 * @return block_env Some private information as returned by init_block().
	 * @param sched_head The schedule so far.
	 * @param curr_time The current time step which the picked node
	 * will be assigned to.
	 * @param ready_list A set containing all ready nodes. Pick one of these
	 * nodes.
	 * @return The chosen node.
	 */
	ir_node *(*select)(void *block_env, nodeset *ready_set);

	/**
	 * This function decides, if a node should appear in a schedule.
	 * @param block_env The block environment.
	 * @param irn       The node.
	 * @return 1, if the node should be scheduled, 0 if not.
	 */
	int (*to_appear_in_schedule)(void *block_env, const ir_node *irn);

	/**
	 * Returns the execution time of node irn.
	 */
	unsigned (*exectime)(void *block_env, const ir_node *irn);

	/**
	 * Calculates the latency of executing cycle curr_cycle of node curr in cycle pred_cycle
	 * of node pred.
	 *
	 * @param block_env   The block environment.
	 * @param pred        The previous node.
	 * @param pred_cycle  The previous node execution cycle.
	 * @param curr        The current node.
	 * @param curr_cycle  The current node execution cycle.
	 */
	unsigned (*latency)(void *block_env, const ir_node *pred, int pred_cycle, const ir_node *curr, int curr_cycle);

	/**
	 * Called after a block has been scheduled.
	 * @param env The environment.
	 * @param block_env The per block environment as returned by init_block().
	 */
	void (*finish_block)(void *block_env);

	/**
	 * Called after a whole graph has been scheduled.
	 * @param env The environment.
	 */
	void (*finish_graph)(void *env);

};


/**
 * A trivial selector, that just selects the first ready node.
 */
extern const list_sched_selector_t *trivial_selector;

/**
 * A selector that tries to minimize the register pressure.
 * @note Not really operational yet.
 */
extern const list_sched_selector_t *reg_pressure_selector;

/**
 * List schedule a graph.
 * Each block in the graph gets a list head to its link field being the
 * head of the schedule. You can walk this list using the functions in
 * list.h.
 * @param arch_env The architecture environment.
 * @param irg      The graph to schedule.
 */
void list_sched(const arch_env_t *arch_env, ir_graph *irg);

#endif /* _FIRM_LIST_SCHED */
