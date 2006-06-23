/**
 * Register allocation functions.
 * @author Sebastian Hack
 * @date 13.1.2005
 */

#ifndef _BERA_H
#define _BERA_H

#include "firm_config.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_timing.h>
#endif

#include "firm_types.h"

#include "be.h"

typedef struct {
	lc_timer_t *t_prolog;
	lc_timer_t *t_epilog;
	lc_timer_t *t_live;
	lc_timer_t *t_spill;
	lc_timer_t *t_color;
	lc_timer_t *t_ifg;
	lc_timer_t *t_copymin;
	lc_timer_t *t_ssa;
} be_ra_timer_t;

typedef struct {
#ifdef WITH_LIBCORE
	void (*register_options)(lc_opt_entry_t *grp);
#endif
	be_ra_timer_t *(*allocate)(const be_irg_t *bi);
} be_ra_t;



/**
 * Check, if two values interfere.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if @p a and @p b interfere, 0 if not.
 */
int values_interfere(const ir_node *a, const ir_node *b);

/**
 * Check, if a value dominates the other one.
 * Note, that this function also considers the schedule and does thus
 * more than block_dominates().
 *
 * @param a The first.
 * @param b The second value.
 * @return 1 if a dominates b, 0 else.
 */
int value_dominates(const ir_node *a, const ir_node *b);

#endif /* _BERA_H */
