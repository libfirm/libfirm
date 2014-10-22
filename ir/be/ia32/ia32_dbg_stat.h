/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file contains macros to update ia32 firm statistics.
 * @author      Christian Wuerdig
 */
#ifndef FIRM_BE_IA32_IA32_DBG_STAT_H
#define FIRM_BE_IA32_IA32_DBG_STAT_H

#include "irhooks.h"
#include "dbginfo_t.h"
#include "firmstat.h"
#include "util.h"

/**
 * A Copy was created to fulfill two address code constraints
 *
 * @param cpy  the copy
 */
#define DBG_OPT_2ADDRCPY(cpy)                                    \
	do {                                                         \
		hook_merge_nodes(NULL, 0, &cpy, 1, (hook_opt_kind)FS_BE_IA32_2ADDRCPY); \
	} while(0)

/**
 * A Sub was transformed into Neg-Add due to 2 address code limitations
 *
 * @param sub   the old Sub
 * @param nadd  the new Add
 */
#define DBG_OPT_SUB2NEGADD(sub, nadd)                               \
	do {                                                            \
		hook_merge_nodes(&nadd, 1, &sub, 1, (hook_opt_kind)FS_BE_IA32_SUB2NEGADD); \
		__dbg_info_merge_pair(nadd, sub, dbg_backend);              \
	} while(0)

/**
 * A Lea was transformed back into an Add
 *
 * @param lea   the old Lea
 * @param nadd  the new Add
 */
#define DBG_OPT_LEA2ADD(lea, nadd)                               \
	do {                                                         \
		hook_merge_nodes(&nadd, 1, &lea, 1, (hook_opt_kind)FS_BE_IA32_LEA2ADD); \
		__dbg_info_merge_pair(nadd, lea, dbg_backend);           \
	} while(0)

#endif
