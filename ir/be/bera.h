/**
 * Register allocation functions.
 * @author Sebastian Hack
 * @date 13.1.2005
 */

#ifndef _BERA_H
#define _BERA_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#endif

#include "irnode.h"
#include "irgraph.h"

#include "be.h"

typedef struct {
#ifdef WITH_LIBCORE
	void (*register_options)(lc_opt_entry_t *grp);
#endif
	void (*allocate)(const be_irg_t *bi);
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
