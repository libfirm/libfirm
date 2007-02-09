/**
 * Author:      Daniel Grund
 * Date:		11.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id$
 */
#ifndef _BECOPYSTAT_H
#define _BECOPYSTAT_H

#include "firm_config.h"
#include "irgraph.h"
#include "bearch.h"
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

#endif /* _BECOPYSTAT_H */
