/**
 * Internal register allocation facility.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#ifndef _BERA_H
#define _BERA_H

#include "firm_config.h"

/**
 * Register allocation data for a node.
 */
typedef struct _ra_info_t {
	int pressure;							/**< Register pressure at this node. */
	int color;								/**< The color assigned to this node. */
	int sim_live_phi_n;				/**< The number of simulatenously live phi operand.
															This is only used if the ir node occurs as an
															operand to a phi function. */
	int *sim_live_phi;				/**< The array of simultaneously live nodes. Same restrictions
															like @c sim_live_phi hold here. */
} ra_info_t;

#define get_irn_ra_info(irn) get_irn_data(irn, ra_info_t, ra_irn_data_offset)
#define get_ra_info_irn(inf) get_irn_data_base(inf, ra_irn_data_offset)

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

static INLINE int __get_irn_color(const ir_node *irn)
{
	return get_irn_ra_info(irn)->color;
}

static INLINE void __set_irn_color(const ir_node *irn, int color)
{
	get_irn_ra_info(irn)->color = color;
}

static INLINE int __is_allocatable_irn(const ir_node *irn)
{
	return mode_is_datab(get_irn_mode(irn));
}

#define get_irn_color(irn)								__get_irn_color(irn)
#define set_irn_color(irn,col)						__set_irn_color(irn, col)
#define is_allocatable_irn(irn)						__is_allocatable_irn(irn)

#endif
