/**
 * Internal headers for liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */

#ifndef _BELIVE_T_H
#define _BELIVE_T_H

#include "config.h"

#include "belive.h"
#include "pset.h"

typedef struct _block_live_info_t {
	pset *in; 				/**< The set of all values live in at that block. */
	pset *out;				/**< The set of all values live out. */
} block_live_info_t;

typedef struct _node_live_info_t {
	int is_phi_op; 		/**< Marks the node as a phi operand. */
} node_live_info_t;

typedef struct _live_info_t {
	union {
		block_live_info_t block;
		node_live_info_t node;
	} v;
} live_info_t;

extern size_t live_irn_data_offset;

#define get_irn_live_info(irn) get_irn_data(irn, live_info_t, live_irn_data_offset)
#define get_live_info_irn(inf) get_irn_data_base(inf, live_irn_data_offset)

#define get_block_live_info(irn) (&(get_irn_live_info(irn)->v.block))
#define get_node_live_info(irn) (&(get_irn_live_info(irn)->v.node))

static INLINE int __is_phi_operand(const ir_node *irn)
{
	assert(!is_Block(irn) && "No block node allowed here");
	return get_node_live_info(irn)->is_phi_op;
}

static INLINE int __is_live_in(const ir_node *block, const ir_node *irn)
{
	block_live_info_t *info = get_block_live_info(block);

	assert(is_Block(block) && "Need a block here");
	return pset_find_ptr(info->in, irn) != NULL;
}

static INLINE int __is_live_out(const ir_node *block, const ir_node *irn)
{
	block_live_info_t *info = get_block_live_info(block);

	assert(is_Block(block) && "Need a block here");
	return pset_find_ptr(info->out, irn) != NULL;
}

static INLINE pset *__get_live_in(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return get_block_live_info(block)->in;
}

static INLINE pset *__get_live_out(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return get_block_live_info(block)->out;
}

#define is_phi_operand(irn)			__is_phi_operand(irn)
#define is_live_in(bl,irn) 			__is_live_in(bl, irn)
#define is_live_out(bl,irn) 		__is_live_out(bl, irn)
#define get_live_in(bl)					__get_live_in(bl)
#define get_live_out(bl)				__get_live_out(bl)

/**
 * Initialize the liveness module.
 * To be called from be_init().
 */
void be_liveness_init(void);

#endif
