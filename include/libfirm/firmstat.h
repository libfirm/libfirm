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
 * @brief   Statistics for Firm.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_STAT_FIRMSTAT_H
#define FIRM_STAT_FIRMSTAT_H

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
	FIRMSTAT_COUNT_EXTBB     = 0x00000080,    /**< if set, count extended Basic Block statistics */
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
	FS_OPT_ADD_MUL_A_X_A,                     /**< a * x + a = a * (x + 1) */
	FS_OPT_SUB_0_A,                           /**< 0 - a = -a */
	FS_OPT_MINUS_SUB,                         /**< - (a - b) = b - a */
	FS_OPT_SUB_MUL_A_X_A,                     /**< a * x - a = a * (x - 1) */
	FS_OPT_SUB_SUB_X_Y_Z,                     /**< (x - y) - z = x - (y + z) */
	FS_OPT_MUL_MINUS_1,                       /**< a * -1 = -a */
	FS_OPT_OR,                                /**< a | a = a | 0 = 0 | a = a */
	FS_OPT_AND,                               /**< a & 0b1...1 = 0b1...1 & a =  a & a = a */
	FS_OPT_EOR_A_A,                           /**< a ^ a = 0 */
	FS_OPT_EOR_TO_NOT_BOOL,                   /**< bool ^ 1 = !bool */
	FS_OPT_EOR_TO_NOT,                        /**< x ^ 0b1..1 = ~x */
	FS_OPT_NOT_CMP,                           /**< !(a cmp b) = a !cmp b */
	FS_OPT_OR_SHFT_TO_ROT,                    /**< (x << c) | (x >> (bits - c)) == Rot(x, c) */
	FS_OPT_REASSOC_SHIFT,                     /**< (x SHF c1) SHF c2 = x SHF (c1+c2) */
	FS_OPT_SHIFT_AND,                         /**< (a SHF c) AND (b SHF c) = (a AND b) SHF c */
	FS_OPT_SHIFT_OR,                          /**< (a SHF c) OR (b SHF c) = (a OR b) SHF c */
	FS_OPT_SHIFT_EOR,                         /**< (a SHF c) XOR (b SHF c) = (a XOR b) SHF c */
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
	FS_OPT_IDEM_UNARY,                        /**< Idempotent unary operation */
	FS_OPT_MINUS_NOT,                         /**< -(~x) = x + 1 */
	FS_OPT_NOT_MINUS_1,                       /**< ~(x - 1) = -x */
	FS_OPT_NOT_PLUS_1,                        /**< ~x + 1 = -x */
	FS_OPT_FP_INV_MUL,                        /**< x / y = x * (1.0/y) */
	FS_OPT_CONST_PHI,                         /**< Constant evaluation on Phi */
	FS_BE_IA32_LEA,                           /**< Lea was created */
	FS_BE_IA32_LOAD_LEA,                      /**< Load merged with a Lea */
	FS_BE_IA32_STORE_LEA,                     /**< Store merged with a Lea */
	FS_BE_IA32_AM_S,                          /**< Source address mode node created */
	FS_BE_IA32_AM_D,                          /**< Destination address mode node created */
	FS_BE_IA32_CJMP,                          /**< CJmp created to save a cmp/test */
	FS_BE_IA32_2ADDRCPY,                      /**< Copy created due to 2-Addresscode constraints */
	FS_BE_IA32_SPILL2ST,                      /**< Created Store for a Spill */
	FS_BE_IA32_RELOAD2LD,                     /**< Created Load for a Reload */
	FS_BE_IA32_SUB2NEGADD,                    /**< Created Neg-Add for a Sub due to 2-Addresscode constraints */
	FS_BE_IA32_LEA2ADD,                       /**< Transformed Lea back into Add */
	FS_OPT_MAX
};

/**
 * Dump a snapshot of the statistic values.
 * Never called from libFirm should be called from user.
 *
 * @param name   base name of the statistic output file
 * @param phase  a phase name. Prefix will be firmstat-<phase>-
 */
void stat_dump_snapshot(const char *name, const char *phase);

/**
 * initialize the statistics module.
 *
 * @param enable_options  a bitmask containing the statistic options
 */
void firm_init_stat(unsigned enable_options);

/**
 * terminates the statistics module, frees all memory
 */
void stat_term(void);

/**
 * returns 1 if statistic module is active, 0 otherwise
 */
int stat_is_active(void);

#endif /* FIRM_STAT_FIRMSTAT_H */
