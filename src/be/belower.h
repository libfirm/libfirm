/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Performs lowering of perm nodes. Inserts copies to assure
 *              register constraints.
 * @author      Christian Wuerdig
 * @date        14.12.2005
 */
#ifndef FIRM_BE_BELOWER_H
#define FIRM_BE_BELOWER_H

#include <stdbool.h>
#include "firm_types.h"
#include <stdbool.h>

/**
 * Walks over all blocks in an irg and performs lowering need to be
 * done after register allocation (e.g. perm lowering).
 *
 * @param irg         The graph
 * @param use_copies  Implement cycles using copies if a free reg is available
 */
void lower_nodes_after_ra(ir_graph *irg, bool use_copies);

#endif
