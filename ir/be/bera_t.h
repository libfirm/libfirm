/**
 * Internal register allocation facility.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#ifndef _BERA_T_H
#define _BERA_T_H

#include "firm_config.h"
#include "bitset.h"
#include "list.h"

#include "bera.h"

#define DBG_BERA "firm.be.ra"

typedef struct _ra_node_info_t {
	int pressure;							/**< Register pressure at this node. */
	int color;								/**< The color assigned to this node. */
} ra_node_info_t;

typedef struct _ra_block_info_t {
	bitset_t *used_colors;					/**< A bitmask containing all colors used in the block. */
	struct list_head border_head;		/**< A list head to enqueue the borders. */
} ra_block_info_t;

/**
 * Register allocation data for a node.
 */
typedef struct _ra_info_t {
	union {
		ra_node_info_t node;
		ra_block_info_t block;
	} v;
} ra_info_t;

#define get_ra_irn_info(irn) get_irn_data(irn, ra_info_t, ra_irn_data_offset)
#define get_ra_info_irn(inf) get_irn_data_base(inf, ra_irn_data_offset)

#define get_ra_node_info(the_node) 		(&get_ra_irn_info(the_node)->v.node)
#define get_ra_block_info(the_block) 	(&get_ra_irn_info(the_block)->v.block)

extern size_t ra_irn_data_offset;

extern size_t ra_irg_data_offset;

#define get_irg_ra_link(irg) (*(get_irg_data(irg, void *, ra_irg_data_offset)))
#define set_irg_ra_link(irg,ptr) (*(get_irg_data(irg, void *, ra_irg_data_offset)) = ptr)

/**
 * Initialize the register allocation framework.
 */
void be_ra_init(void);

/**
 * The 'no color' color. The register allocator should use this value,
 * if a color cannot be assigned at some point.
 */
#define NO_COLOR (-1)

/**
 * Check, if a color is valid.
 * @param col The color.
 * @return 1, if the color is ok, 0 if the color is illegal.
 */
#define is_color(col) ((col) != NO_COLOR)

static INLINE int _get_irn_color(const ir_node *irn)
{
	assert(!is_Block(irn) && "No block allowed here");
	return get_ra_node_info(irn)->color;
}

static INLINE void _set_irn_color(const ir_node *irn, int color)
{
	assert(!is_Block(irn) && "No block allowed here");
	get_ra_node_info(irn)->color = color;
}

static INLINE int _is_allocatable_irn(const ir_node *irn)
{
	assert(!is_Block(irn) && "No block allowed here");
	return mode_is_datab(get_irn_mode(irn));
}

#define get_irn_color(irn)								_get_irn_color(irn)
#define set_irn_color(irn,col)						_set_irn_color(irn, col)
#define is_allocatable_irn(irn)						_is_allocatable_irn(irn)

/**
 * Check, if two phi operands interfere.
 * @param a A node which is operand to a phi function.
 * @param b Another node which is operand to a phi function.
 * @return 1, if @p a and @p b interfere, 0 if not.
 */
int phi_ops_interfere(const ir_node *a, const ir_node *b);

#endif /* BERA_T_H */
