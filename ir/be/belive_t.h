/**
 * Internal headers for liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */

#ifndef _BELIVE_T_H
#define _BELIVE_T_H

#include "config.h"

#include "pset.h"

typedef struct _block_live_info_t {
	pset *in;
	pset *out;
} block_live_info_t;

typedef struct _node_live_info_t {
	unsigned last_use_in_block : 1;
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

#define get_block_live_info(irn) &(get_irn_live_info(irn)->v.block)

static INLINE int _is_live_in(const ir_node *block, const ir_node *irn)
{
	block_live_info_t *info = get_block_live_info(block);

	assert(is_Block(block) && "Need a block here");
	return pset_find_ptr(info->in, irn) != NULL;
}

static INLINE int _is_live_out(const ir_node *block, const ir_node *irn)
{
	block_live_info_t *info = get_block_live_info(block);

	assert(is_Block(block) && "Need a block here");
	return pset_find_ptr(info->out, irn) != NULL;
}

#define is_live_in(block,irn) _is_live_in(block, irn)
#define is_live_out(block,irn) _is_live_out(block, irn)

/**
 * Initialize the liveness module.
 * To be called from be_init().
 */
void be_liveness_init(void);

#endif
