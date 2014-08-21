/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Statistics for Firm. Pattern history.
 * @author  Michael Beck
 */
#ifndef FIRM_STAT_PATTERN_H
#define FIRM_STAT_PATTERN_H

#include "firm_types.h"

/**
 * Calculates the pattern history.
 *
 * @param irg    The IR-graph
 */
void stat_calc_pattern_history(ir_graph *irg);

/**
 * Initializes the pattern history.
 *
 * @param enable  Enable flag.
 */
void stat_init_pattern_history(int enable);

/**
 * Finish the pattern history.
 */
void stat_finish_pattern_history(const char *fname);

#endif
