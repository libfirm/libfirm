/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Common functions for creating listscheduling algorithms
 * @author      Sebastian Hack, Matthias Braun
 * @date        20.10.2004
 */
#ifndef FIRM_BE_BELISTSCHED_H
#define FIRM_BE_BELISTSCHED_H

#include "firm_types.h"
#include "irnodeset.h"

#include "be.h"
#include "be_types.h"

/**
 * Begin list scheduling for a graph.
 */
void be_list_sched_begin(ir_graph *irg);

/**
 * Free memory used by list scheduler.
 */
void be_list_sched_finish(void);

/**
 * Begin list scheduling for a block.
 * Returns the readyset used by the listscheduler.
 */
ir_nodeset_t *be_list_sched_begin_block(ir_node *block);

/**
 * Free memory used for scheduling the last block.
 */
void be_list_sched_end_block(void);

/**
 * Schedule @p node from the ready set.
 */
void be_list_sched_schedule(ir_node *node);

#endif
