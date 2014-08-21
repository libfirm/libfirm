/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend IRG modification routines.
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 *
 * This file contains the following IRG modifications for be routines:
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 */
#ifndef FIRM_BE_BEIRGMOD_H
#define FIRM_BE_BEIRGMOD_H

#include "be_types.h"
#include "firm_types.h"

/**
 * Insert a Perm which permutes all (non-ignore) live values of a given register class
 * before a certain instruction.
 * @param lv        Liveness Information.
 * @param irn       The node to insert the Perm before.
 * @return          The Perm or NULL if nothing was live before @p irn.
 */
ir_node *insert_Perm_before(ir_graph *irg, const arch_register_class_t *cls,
						   ir_node *irn);

/**
 * Removes dead nodes from schedule
 * @param irg  the graph
 */
void be_remove_dead_nodes_from_schedule(ir_graph *irg);

#endif
