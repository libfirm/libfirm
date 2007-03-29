/*
 * Block schedule calculator
 *
 * $Id$
 */
#ifndef _BEBLOCKSCHED_H
#define _BEBLOCKSCHED_H

#include "firm_config.h"

#include "obst.h"
#include "execfreq.h"
#include "irnode.h"
#include "irgraph.h"

ir_node **be_create_block_schedule(ir_graph *irg, ir_exec_freq *execfreqs);

#endif /* _BEBLOCKSCHED_H */
