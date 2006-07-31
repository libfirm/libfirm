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
#include "belive.h"

typedef struct {
	lc_timer_t *t_prolog;      /**< timer for prolog */
	lc_timer_t *t_epilog;      /**< timer for epilog */
	lc_timer_t *t_live;        /**< timer for liveness calculation */
	lc_timer_t *t_spill;       /**< timer for spilling */
	lc_timer_t *t_color;       /**< timer for graph coloring */
	lc_timer_t *t_ifg;         /**< timer for building interference graph */
	lc_timer_t *t_copymin;     /**< timer for copy minimization */
	lc_timer_t *t_ssa;         /**< timer for ssa destruction */
	lc_timer_t *t_verify;      /**< timer for verification runs */
	lc_timer_t *t_other;       /**< timer for remaining stuff */
} be_ra_timer_t;

typedef struct {
#ifdef WITH_LIBCORE
	void (*register_options)(lc_opt_entry_t *grp);
#endif
	be_ra_timer_t *(*allocate)(const be_irg_t *bi);
} be_ra_t;



/**
 * Check, if two values interfere.
 * @param lv Liveness information.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if @p a and @p b interfere, 0 if not.
 */
int values_interfere(const be_lv_t *lv, const ir_node *a, const ir_node *b);

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
