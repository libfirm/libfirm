/**
 * Primitive list scheduling.
 * @date 20.10.2004
 * @author Sebastian Hack
 */

#ifndef _FIRM_LIST_SCHED
#define _FIRM_LIST_SCHED

#include "pset.h"
#include "pmap.h"
#include "list.h"

#include "besched_t.h"

/**
 * The selection function.
 * It picks one node out of the ready list to be scheduled next.
 * The function does not have to delete the node from the ready set.
 *
 * @param env Some private information as passed to list_schedule().
 * @param block The block which is currentliy scheduled.
 * @param curr_time The current time step which the picked node
 * will be assigned to.
 * @param already_scheduled A set containing all nodes already
 * scheduled.
 * @param ready_list A set containing all ready nodes. Pick one of these
 * nodes.
 * @return The chosen node.
 */
typedef ir_node *(list_sched_selector_t)(void *env, ir_node *block,
		int curr_time, pset *already_scheduled, pset *ready_list);

ir_node *trivial_selector(void *env, ir_node *block, int curr_time,
		pset *already_scheduled, pset *ready_list);

/**
 * List schedule a graph.
 * Each block in the graph gets a list head to its link field being the
 * head of the schedule. You can walk this list using the functions in
 * list.h.
 * @param irg The graph to schedule.
 * @param sched_obst An obstack to allocate the lists on.
 * @param map Maps each block to a list head giving the schedule.
 * @param select_func A selection function.
 * @param env Some private data to give to the select function.
 */
void list_sched(ir_graph *irg, list_sched_selector_t *select_func, void *env);



#endif
