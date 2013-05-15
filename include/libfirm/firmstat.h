/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Statistics for Firm.
 * @author  Michael Beck
 */
#ifndef FIRM_STAT_FIRMSTAT_H
#define FIRM_STAT_FIRMSTAT_H

#include "firm_types.h"
#include "begin.h"

/**
 * Dump a snapshot of the statistic values.
 * Never called from libFirm should be called from user.
 *
 * @param fname  base name of the statistic output file
 * @param phase  a phase name. Prefix will be firmstat-\<phase\>-
 */
FIRM_API void stat_dump_snapshot(const char *fname, const char *phase);

/**
 * initialize the statistics module.
 * Should be called directly after ir_init
 */
FIRM_API void firm_init_stat(void);

/**
 * terminates the statistics module, frees all memory
 */
FIRM_API void stat_term(void);

/**
 * returns 1 if statistic module is active, 0 otherwise
 */
FIRM_API int stat_is_active(void);

#include "end.h"

#endif
