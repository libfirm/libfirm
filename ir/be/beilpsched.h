#ifndef _BEILPSCHED_H_
#define _BEILPSCHED_H_

#include "firm_config.h"

/**
 * Perform ILP scheduling on given birg.
 */
void be_ilp_sched(const be_irg_t *birg);

#ifdef WITH_LIBCORE

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

/**
 * Register ILP scheduler options.
 */
void ilpsched_register_options(lc_opt_entry_t *grp);

#endif /* WITH_LIBCORE */

#endif /* _BEILPSCHED_H_ */
