
#ifndef _BESCHED_T_H
#define _BESCHED_T_H

#include "list.h"
#include "irnode_t.h"
#include "irgraph_t.h"

#include "besched.h"

extern size_t sched_irn_data_offset;

typedef struct _sched_info_t {
	struct list_head list;
	int time_step;
} sched_info_t;

#define _sched_entry(list_head) (list_entry(list_head, sched_info_t, list))

#define get_irn_sched_info(irn) get_irn_data(irn, sched_info_t, sched_irn_data_offset)
#define get_sched_info_irn(sched_info) get_irn_data_base(sched_info, sched_irn_data_offset)

/**
 * Init the scheduling stuff.
 * To be called from the central backend initialization routine.
 */
void be_sched_init(void);

/**
 * Get the time step of an irn in a schedule.
 * @param irn The node.
 * @return The time step in the schedule.
 */
static INLINE int __sched_get_time_step(const ir_node *irn)
{
	return get_irn_sched_info(irn)->time_step;
}

/**
 * Check, if an ir_node has a scheduling successor.
 * @param irn The ir node.
 * @return 1, if the node has a scheduling successor, 0 if not.
 */
static INLINE int __sched_has_succ(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	const sched_info_t *block_info = get_irn_sched_info(get_nodes_block(irn));
	return info->list.next != &block_info->list;
}

/**
 * Check, if an ir_node has a scheduling predecessor.
 * @param irn The ir node.
 * @return 1, if the node has a scheduling predecessor, 0 if not.
 */
static INLINE int __sched_has_prev(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	const sched_info_t *block_info = get_irn_sched_info(get_nodes_block(irn));
	return info->list.prev != &block_info->list;
}

/**
 * Get the scheduling successor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or NULL, if this node has no
 * successor.
 */
static INLINE const ir_node *__sched_succ(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	return __sched_has_succ(irn) ? get_sched_info_irn(_sched_entry(info->list.next)) : NULL;
}

/**
 * Get the scheduling predecessor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or NULL, if this node has no
 * predecessor.
 */
static INLINE const ir_node *__sched_prev(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	return __sched_has_prev(irn) ? get_sched_info_irn(_sched_entry(info->list.prev)) : NULL;
}

/**
 * Get the first node in a block schedule.
 * @param block The block of which to get the schedule.
 * @return The first node in the schedule or NULL if there is none.
 */
static INLINE const ir_node *__sched_first(const ir_node *block)
{
	const sched_info_t *info = get_irn_sched_info(block);
	assert(is_Block(block) && "Need a block here");
	return !list_empty(&info->list) ? get_sched_info_irn(_sched_entry(info->list.next)) : NULL;
}

/**
 * Get the last node in a schedule.
 * @param block The block to get the schedule for.
 * @return The last ir node in a schedule, or NULL if no schedule exists
 * or it is empty.
 */
static INLINE const ir_node *__sched_last(const ir_node *block)
{
	const sched_info_t *info = get_irn_sched_info(block);
	assert(is_Block(block) && "Need a block here");
	return !list_empty(&info->list) ? get_sched_info_irn(_sched_entry(info->list.prev)) : NULL;
}

/**
 * Add a node to a block schedule.
 * @param block The block to whose schedule the node shall be added to.
 * @param irn The node to add.
 * @return The given node.
 */
static INLINE const ir_node *__sched_add(ir_node *block, const ir_node *irn)
{
	assert(is_Block(block) && "Need a block here");
	list_add_tail(&get_irn_sched_info(irn)->list, &get_irn_sched_info(block)->list);
	return irn;
}

#define sched_get_time_step(irn)	  __sched_get_time_step(irn)
#define sched_has_succ(irn) 				__sched_has_succ(irn)
#define sched_has_prev(irn) 				__sched_has_prev(irn)
#define sched_succ(irn) 						__sched_succ(irn)
#define sched_prev(irn) 						__sched_prev(irn)
#define sched_first(irn) 						__sched_first(irn)
#define sched_last(irn) 						__sched_last(irn)
#define sched_add(block,irn) 				__sched_add(block,irn)


#endif
