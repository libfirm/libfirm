/*
 * Project:     libFIRM
 * File name:   ir/ana/irconsconfirm.h
 * Purpose:     Construction of Confirm nodes
 * Author:      Michael Beck
 * Modified by:
 * Created:     6.2005
 * CVS-ID:      $Id$
 * Copyright:   (C) 2002-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irconsconfirm.h
 *
 *  Construction of Confirm nodes
 *
 *  @author Michael Beck
 */
#ifndef _IRCONSCONFIRM_H_
#define _IRCONSCONFIRM_H_

#include "irgraph.h"

/*
 * Inject Confirm nodes into a graph.
 *
 * @param irg  the graph
 *
 * Confirm nodes carry confirmation information, such as
 * a relation between a value a and another value (or a constant)
 * b.
 *
 * These allows to do some range dependent optimizations for Cmp,
 * Abs, Min, Max nodes as well as bounds checking removement.
 *
 * The heap analysis might profit also. On the other side, Confirm
 * nodes disturb local optimizations, because patterns are destroyed.
 *
 * It is possible to avoid this by skipping Confirm nodes, but this
 * is not implemented and is not cheap. The same happens with Casts
 * nodes too. The current solution is to remove Confirms at a later
 * pass.
 */
void construct_confirms(ir_graph *irg);

/**
 * Remove all Confirm nodes from a graph.
 *
 * Note that local_optimize() can handle this if
 * the remove Confirm node setting is on (set_opt_remove_Confirm(1)).
 */
void remove_confirms(ir_graph *irg);

#endif /* _IRCONSCONFIRM_H_ */
