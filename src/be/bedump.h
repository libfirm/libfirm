/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Code for dumping backend data structures (i.e. interference graphs)
 * @author      Matthias Braun
 */
#ifndef FIRM_BE_BEDUMP_H
#define FIRM_BE_BEDUMP_H

#include <stdio.h>
#include <stdbool.h>

#include "bearch.h"

/**
 * Dump interference graph with affinity edges as calculated by a
 * copy-minimisation phase
 */
void be_dump_ifg_co(FILE *F, const copy_opt_t *co,
                    bool dump_costs, bool dump_colors);

/**
 * node_info hook that dumps liveness for blocks
 */
void be_dump_liveness_block(be_lv_t *lv, FILE *F, const ir_node *block);

void be_dump_reqs_and_registers(FILE *F, const ir_node *node);

static inline char const *be_dump_reg_name(arch_register_t const *const reg)
{
	return reg ? reg->name : "n/a";
}

static inline char const *be_dump_yesno(bool const b)
{
	return b ? "yes" : "no";
}

#endif
