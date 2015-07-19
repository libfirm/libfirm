/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Provides several statistic functions for the backend.
 * @author      Christian Wuerdig, Matthias Braun
 */
#ifndef FIRM_BE_BESTAT_H
#define FIRM_BE_BESTAT_H

#include "be_types.h"
#include "firm_types.h"

typedef enum be_stat_tag_t {
	BE_STAT_FIRST,
	BE_STAT_PHIS = BE_STAT_FIRST, /**< phi count (excluding mem-phis) */
	BE_STAT_MEM_PHIS,             /**< memory-phi count */
	BE_STAT_COPIES,               /**< copies */
	BE_STAT_PERMS,                /**< perms */
	BE_STAT_COUNT
} be_stat_tag_t;
ENUM_COUNTABLE(be_stat_tag_t)
typedef unsigned long be_node_stats_t[BE_STAT_COUNT];

/**
 * Collect statistics about node types
 */
void be_collect_node_stats(be_node_stats_t *stats, ir_graph *irg);

void be_subtract_node_stats(be_node_stats_t *stats, be_node_stats_t *sub);

void be_copy_node_stats(be_node_stats_t *dest, be_node_stats_t *src);

void be_emit_node_stats(be_node_stats_t *stats, const char *prefix);

/**
 * Collects statistics information about register pressure.
 * @param irg    The irg
 */
void be_do_stat_reg_pressure(ir_graph *irg, const arch_register_class_t *cls);

/**
 * Gives a cost estimate for the program (based on execution frequencies)
 * and backend op_estimated_cost
 */
double be_estimate_irg_costs(ir_graph *irg);

/**
 * return number of "instructions" (=nodes without some virtual nodes like Proj,
 * Start, End)
 */
unsigned long be_count_insns(ir_graph *irg);

/**
 * return number of basic blocks (without the end block)
 */
unsigned long be_count_blocks(ir_graph *irg);

/**
 * Count values
 */
void be_stat_values(ir_graph *irg);

#endif
