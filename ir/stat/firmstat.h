/*
 * Project:     libFIRM
 * File name:   ir/stat/firmstat.h
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRMSTAT_H_
#define _FIRMSTAT_H_

/**
 * @file firmstat.h
 */
#include "irop.h"
#include "irnode.h"
#include "irgraph.h"

/**
 * Statistic options, can be or'ed.
 */
enum firmstat_options_t {
  FIRMSTAT_ENABLED         = 0x00000001,    /**< enable statistics */
  FIRMSTAT_PATTERN_ENABLED = 0x00000002,    /**< enable pattern calculation */
  FIRMSTAT_COUNT_STRONG_OP = 0x00000004,    /**< if set, count Mul/Div/Mod/DivMod by constant */
  FIRMSTAT_COUNT_DAG       = 0x00000008,    /**< if set, count DAG statistics */
  FIRMSTAT_COUNT_DELETED   = 0x00000010,    /**< if set, count deleted graphs */
  FIRMSTAT_CSV_OUTPUT      = 0x10000000     /**< CSV output of some mini-statistic */
};

/**
 * Finish the statistics.
 * Never called from libFirm should be called from user.
 *
 * @param name   basename of the statistic output file
 */
void stat_finish(const char *name);

#ifdef FIRM_STATISTICS

/**
 * initialize the statistics module.
 *
 * @param enable_options  a bitmask containing the statistic options
 */
void init_stat(unsigned enable_options);

#endif /* FIRM_STATISTICS */

#endif /* _FIRMSTAT_H_ */
