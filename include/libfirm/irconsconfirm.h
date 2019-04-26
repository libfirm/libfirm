/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Construction of Confirm nodes
 * @author   Michael Beck
 * @date     6.2005
 */
#ifndef FIRM_ANA_IRCONSCONFIRM_H
#define FIRM_ANA_IRCONSCONFIRM_H

#include "firm_types.h"

#include "begin.h"

/**
 * Inject Confirm nodes into a graph and perform Confirm-related
 * optimizations.
 *
 * @param irg  the graph
 *
 * Confirm nodes carry confirmation information, such as
 * a relation between a value a and another value (or a constant)
 * b.
 *
 * These allows to do some range dependent optimizations for Cmp,
 * Abs, Min, Max nodes as well as bounds checking deletion.
 *
 * The heap analysis might profit also. On the other side, Confirm
 * nodes disturb local optimizations, because patterns are destroyed.
 *
 * It is possible to avoid this by skipping Confirm nodes, but this
 * is not implemented and is not cheap. The same happens with Casts
 * nodes too. The current solution is to remove Confirms at a later
 * pass.
 */
FIRM_API void construct_confirms(ir_graph *irg);

/**
 * Inject Confirm nodes into a graph.
 *
 * @param irg  the graph
 *
 * Confirm nodes carry confirmation information, such as
 * a relation between a value a and another value (or a constant)
 * b.
 *
 * These allows to do some range dependent optimizations for Cmp,
 * Abs, Min, Max nodes as well as bounds checking deletion.
 *
 * The heap analysis might profit also. On the other side, Confirm
 * nodes disturb local optimizations, because patterns are destroyed.
 *
 * It is possible to avoid this by skipping Confirm nodes, but this
 * is not implemented and is not cheap. The same happens with Casts
 * nodes too. The current solution is to remove Confirms at a later
 * pass.
 */
FIRM_API void construct_confirms_only(ir_graph *irg);

/**
 * Remove all Confirm nodes from a graph.
 *
 * Note that local_optimize() can handle this if
 * the remove Confirm node setting is on (set_opt_remove_Confirm(1)).
 */
FIRM_API void remove_confirms(ir_graph *irg);

#include "end.h"

#endif
