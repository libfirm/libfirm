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
#include "irhooks.h"

/**
 * Statistic options, can be or'ed.
 */
enum firmstat_options_t {
  FIRMSTAT_ENABLED         = 0x00000001,    /**< enable statistics */
  FIRMSTAT_PATTERN_ENABLED = 0x00000002,    /**< enable pattern calculation */
  FIRMSTAT_COUNT_STRONG_OP = 0x00000004,    /**< if set, count Mul/Div/Mod/DivMod by constant */
  FIRMSTAT_COUNT_DAG       = 0x00000008,    /**< if set, count DAG statistics */
  FIRMSTAT_COUNT_DELETED   = 0x00000010,    /**< if set, count deleted graphs */
  FIRMSTAT_COUNT_SELS      = 0x00000020,    /**< if set, count Sel(Sel(..)) differently */
  FIRMSTAT_COUNT_CONSTS    = 0x00000040,    /**< if set, count Const statistics */
  FIRMSTAT_CSV_OUTPUT      = 0x10000000     /**< CSV output of some mini-statistic */
};

/**
 * Additional flags for statistics.
 */
enum firmstat_optimizations_t {
  FS_OPT_NEUTRAL_0  = HOOK_OPT_LAST,        /**< a op 0 = 0 op a = a */
  FS_OPT_NEUTRAL_1,                         /**< a op 1 = 1 op a = a */
  FS_OPT_ADD_A_A,                           /**< a + a = a * 2 */
  FS_OPT_ADD_A_MINUS_B,                     /**< a + -b = a - b */
  FS_OPT_ADD_SUB,                           /**< (a + x) - x = (a - x) + x */
  FS_OPT_SUB_0_A,                           /**< 0 - a = -a */
  FS_OPT_MUL_MINUS_1,                       /**< a * -1 = -a */
  FS_OPT_OR,                                /**< a | a = a | 0 = 0 | a = a */
  FS_OPT_AND,                               /**< a & 0b1...1 = 0b1...1 & a =  a & a = a */
  FS_OPT_EOR_A_A,                           /**< a ^ a = 0 */
  FS_OPT_EOR_TO_NOT_BOOL,                   /**< bool ^ 1 = !bool */
  FS_OPT_EOR_TO_NOT,                        /**< x ^ 0b1..1 = ~x */
  FS_OPT_NOT_CMP,                           /**< !(a cmp b) = a !cmp b */
  FS_OPT_OR_SHFT_TO_ROT,                    /**< (x << c) | (x >> (bits - c)) == Rot(x, c) */
  FS_OPT_REASSOC_SHIFT,                     /**< (x SHF c1) SHF c2 = x SHF (c1+c2) */
  FS_OPT_CONV,                              /**< a Conv could be removed */
  FS_OPT_CAST,                              /**< a Cast could be removed */
  FS_OPT_MIN_MAX_EQ,                        /**< Min(a,a) = Max(a,a) = a */
  FS_OPT_MUX_C,                             /**< Mux(C, f, t) = C ? t : f */
  FS_OPT_MUX_EQ,                            /**< Mux(v, x, x) = x */
  FS_OPT_MUX_TRANSFORM,                     /**< Mux(a, b, c) = b OR Mux(a,b, c) = c */
  FS_OPT_MUX_TO_MIN,                        /**< Mux(a < b, a, b) = Min(a,b) */
  FS_OPT_MUX_TO_MAX,                        /**< Mux(a > b, a, b) = Max(a,b) */
  FS_OPT_MUX_TO_ABS,                        /**< Mux(a > b, a, b) = Abs(a,b) */
  FS_OPT_MUX_TO_SHR,                        /**< Mux(a > b, a, b) = a >> b */
  FS_OPT_MAX
};

/**
 * Dump a snapshot of the statistic values.
 * Never called from libFirm should be called from user.
 *
 * @param name   base name of the statistic output file
 */
void stat_dump_snapshot(const char *name);

/**
 * initialize the statistics module.
 *
 * @param enable_options  a bitmask containing the statistic options
 */
void init_stat(unsigned enable_options);

/**
 * terminates the statistics module, frees all memory
 */
void stat_term(void);

#endif /* _FIRMSTAT_H_ */
