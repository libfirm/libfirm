/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Heuristic PBQP solver.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#ifndef KAPS_HEURISTICAL_H
#define KAPS_HEURISTICAL_H

#include "pbqp_t.h"

#include "plist.h"

void solve_pbqp_heuristical(pbqp *pbqp);
void solve_pbqp_heuristical_co(pbqp *pbqp, plist_t *rpeo);
void solve_pbqp_brute_force(pbqp *pbqp);

void apply_edge(pbqp *pbqp);

void apply_RI(pbqp *pbqp);
void apply_RII(pbqp *pbqp);
void apply_RN(pbqp *pbqp);
void apply_RN_co(pbqp *pbqp, plist_t *rpeo);

void back_propagate_RI(pbqp *pbqp, pbqp_node *node);
void back_propagate_RII(pbqp *pbqp, pbqp_node *node);

int node_is_reduced(pbqp_node *node);

#endif /* KAPS_HEURISTICAL_H */
