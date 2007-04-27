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
 * This file contains macros to update ia32 firm statistics
 * @author Christian Wuerdig
 * $Id$
 */
#ifndef _IA32_DBG_STAT_H_
#define _IA32_DBG_STAT_H_

#include "irhooks.h"
#include "dbginfo_t.h"
#include "firmstat.h"

#define SIZ(x)    sizeof(x)/sizeof((x)[0])

/**
 * Merge the debug info due to a LEA creation.
 *
 * @param oldn  the node
 * @param n     the new lea
 */
#define DBG_OPT_LEA1(oldn, n)                               \
	do {                                                    \
		hook_merge_nodes(&n, 1, &oldn, 1, FS_BE_IA32_LEA);  \
		__dbg_info_merge_pair(n, oldn, dbg_backend);        \
	} while(0)

/**
 * Merge the debug info due to a LEA creation.
 *
 * @param oldn  the node
 * @param n     the new lea
 */
#define DBG_OPT_LEA1(oldn, n)                               \
	do {                                                    \
		hook_merge_nodes(&n, 1, &oldn, 1, FS_BE_IA32_LEA);  \
		__dbg_info_merge_pair(n, oldn, dbg_backend);        \
	} while(0)

/**
 * Merge the debug info due to a LEA creation.
 *
 * @param oldn1  the old node
 * @param oldn2  an additional old node
 * @param n      the new lea
 */
#define DBG_OPT_LEA2(oldn1, oldn2, n)                              \
	do {                                                           \
		ir_node *ons[2];                                           \
		ons[0] = oldn1;                                            \
		ons[1] = oldn2;                                            \
		hook_merge_nodes(&n, 1, ons, SIZ(ons), FS_BE_IA32_LEA);    \
		__dbg_info_merge_sets(&n, 1, ons, SIZ(ons), dbg_backend);  \
	} while(0)

/**
 * Merge the debug info due to a LEA creation.
 *
 * @param oldn1  the old node
 * @param oldn2  an additional old node
 * @param oldn3  an additional old node
 * @param n      the new lea
 */
#define DBG_OPT_LEA3(oldn1, oldn2, oldn3, n)                       \
	do {                                                           \
		ir_node *ons[3];                                           \
		ons[0] = oldn1;                                            \
		ons[1] = oldn2;                                            \
		ons[2] = oldn3;                                            \
		hook_merge_nodes(&n, 1, ons, SIZ(ons), FS_BE_IA32_LEA);    \
		__dbg_info_merge_sets(&n, 1, ons, SIZ(ons), dbg_backend);  \
	} while(0)

/**
 * Merge the debug info due to a LEA creation.
 *
 * @param oldn1  the old node
 * @param oldn2  an additional old node
 * @param oldn3  an additional old node
 * @param oldn4  an additional old node
 * @param n      the new lea
 */
#define DBG_OPT_LEA4(oldn1, oldn2, oldn3, oldn4, n)                \
	do {                                                           \
		ir_node *ons[4];                                           \
		ons[0] = oldn1;                                            \
		ons[1] = oldn2;                                            \
		ons[2] = oldn3;                                            \
		ons[3] = oldn4;                                            \
		hook_merge_nodes(&n, 1, ons, SIZ(ons), FS_BE_IA32_LEA);    \
		__dbg_info_merge_sets(&n, 1, ons, SIZ(ons), dbg_backend);  \
	} while(0)

/**
 * Merge the debug info due to a Load with LEA creation.
 *
 * @param oldn  the lea
 * @param n     the new load
 */
#define DBG_OPT_LOAD_LEA(oldn, n)                                \
	do {                                                         \
		hook_merge_nodes(&n, 1, &oldn, 1, FS_BE_IA32_LOAD_LEA);  \
		__dbg_info_merge_pair(n, oldn, dbg_backend);             \
	} while(0)

/**
 * Merge the debug info due to a Store with LEA creation.
 *
 * @param oldn  the lea
 * @param n     the new store
 */
#define DBG_OPT_STORE_LEA(oldn, n)                                \
	do {                                                          \
		hook_merge_nodes(&n, 1, &oldn, 1, FS_BE_IA32_STORE_LEA);  \
		__dbg_info_merge_pair(n, oldn, dbg_backend);              \
	} while(0)

/**
 * Merge the debug info due to a source address mode creation.
 *
 * @param oldn  the old load
 * @param n     the new op
 */
#define DBG_OPT_AM_S(oldn, n)                                \
	do {                                                     \
		hook_merge_nodes(&n, 1, &oldn, 1, FS_BE_IA32_AM_S);  \
		__dbg_info_merge_pair(n, oldn, dbg_backend);         \
	} while(0)

/**
 * Merge the debug info due to a destination address mode creation.
 *
 * @param load   the old load
 * @param store  yhe old store
 * @param n      the new op
 */
#define DBG_OPT_AM_D(load, store, n)                               \
	do {                                                           \
		ir_node *ons[2];                                           \
		ons[0] = load;                                             \
		ons[1] = store;                                            \
		hook_merge_nodes(&n, 1, ons, SIZ(ons), FS_BE_IA32_AM_D);   \
		__dbg_info_merge_sets(&n, 1, ons, SIZ(ons), dbg_backend);  \
	} while(0)

/**
 * A CJmp was created to save a cmp
 *
 * @param oldn  the old node
 */
#define DBG_OPT_CJMP(oldn)                                    \
	do {                                                      \
		hook_merge_nodes(NULL, 0, &oldn, 1, FS_BE_IA32_CJMP); \
	} while(0)

/**
 * A Copy was created to fullfill two address code constraints
 *
 * @param cpy  the copy
 */
#define DBG_OPT_2ADDRCPY(cpy)                                    \
	do {                                                         \
		hook_merge_nodes(NULL, 0, &cpy, 1, FS_BE_IA32_2ADDRCPY); \
	} while(0)

/**
 * A Store was created for a Spill
 *
 * @param spill  the Spill
 * @param store  the Store
 */
#define DBG_OPT_SPILL2ST(spill, store)                               \
	do {                                                             \
		hook_merge_nodes(&store, 1, &spill, 1, FS_BE_IA32_SPILL2ST); \
		__dbg_info_merge_pair(store, spill, dbg_backend);            \
	} while(0)

/**
 * A Load was created for a Reload
 *
 * @param rload  the Reload
 * @param load   the Load
 */
#define DBG_OPT_RELOAD2LD(rload, load)                               \
	do {                                                             \
		hook_merge_nodes(&load, 1, &rload, 1, FS_BE_IA32_RELOAD2LD); \
		__dbg_info_merge_pair(load, rload, dbg_backend);             \
	} while(0)

/**
 * A Sub was transformed into Neg-Add due to 2 address code limitations
 *
 * @param sub   the old Sub
 * @param nadd  the new Add
 */
#define DBG_OPT_SUB2NEGADD(sub, nadd)                               \
	do {                                                            \
		hook_merge_nodes(&nadd, 1, &sub, 1, FS_BE_IA32_SUB2NEGADD); \
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
		hook_merge_nodes(&nadd, 1, &lea, 1, FS_BE_IA32_LEA2ADD); \
		__dbg_info_merge_pair(nadd, lea, dbg_backend);           \
	} while(0)

#endif /* _IA32_DBG_STAT_H_ */
