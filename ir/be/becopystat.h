/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Copy node statistics.
 * @author      Daniel Grund
 * @date        11.04.2005
 */
#ifndef FIRM_BE_BECOPYSTAT_H
#define FIRM_BE_BECOPYSTAT_H

#include "firm_types.h"
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

void copystat_add_ilp_5_sec_costs(int costs);
void copystat_add_ilp_30_sec_costs(int costs);
void copystat_add_ilp_time(int time);
void copystat_add_ilp_vars(int vars);
void copystat_add_ilp_csts(int csts);
void copystat_add_ilp_iter(int iters);

#endif
