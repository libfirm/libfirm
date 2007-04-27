/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Copy node statistics.
 * @author      Daniel Grund
 * @date        11.04.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BECOPYSTAT_H
#define FIRM_BE_BECOPYSTAT_H

#include "firm_config.h"
#include "irgraph.h"
#include "bearch_t.h"
#include "bechordal_t.h"

void copystat_add_max_costs(int costs);
void copystat_add_inevit_costs(int costs);
void copystat_add_init_costs(int costs);
void copystat_add_heur_costs(int costs);
void copystat_add_opt_costs(int costs);
void copystat_add_heur_time(int time);
void copystat_dump(ir_graph *irg);
void copystat_dump_pretty(ir_graph *irg);

#ifdef WITH_ILP

void copystat_add_ilp_5_sec_costs(int costs);
void copystat_add_ilp_30_sec_costs(int costs);
void copystat_add_ilp_time(int time);
void copystat_add_ilp_vars(int vars);
void copystat_add_ilp_csts(int csts);
void copystat_add_ilp_iter(int iters);

#endif /* WITH_ILP */

/**
 * Compares different solutions of the same problem
 */
void co_compare_solvers(be_chordal_env_t *chordal_env);

#endif /* FIRM_BE_BECOPYSTAT_H */
