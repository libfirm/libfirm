/*
 * Block schedule calculator
 *
 * $Id$
 */
#ifndef _BEBLOCKSCHED_H
#define _BEBLOCKSCHED_H

#include "obst.h"
#include "execfreq.h"
#include "irnode.h"
#include "irgraph.h"

ir_node **be_create_block_schedule(ir_graph *irg, ir_exec_freq *execfreqs);

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
void be_block_schedule_register_options(lc_opt_entry_t *grp);
#endif

#endif
