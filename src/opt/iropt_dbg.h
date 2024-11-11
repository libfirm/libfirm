/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Debug macros used in iropt.
 * @author  Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IROPT_DBG_H
#define FIRM_IR_IROPT_DBG_H

#include "dbginfo_t.h"
#include "irhooks.h"
#include "util.h"

/**
 * Merge the debug info due to dead block elimination.
 *
 * @param oldn  the block that it is eliminated
 * @param n     the new node for this block, may be equal to oldn
 */
#define DBG_OPT_DEAD_BLOCK(oldn, n)                           \
	do {                                                      \
	  __dbg_info_merge_pair(n, oldn, dbg_dead_code);          \
	} while(0)


/**
 * Merge the debug info due to a straightening optimization.
 * Block oldn is merged with n.
 *
 * @param oldn  the old block
 * @param n     the new block the merges with oldn
 */
#define DBG_OPT_STG(oldn, n)                                                 \
	do {                                                                     \
	  ir_node *ons[2];                                                       \
	  ons[0] = oldn;                                                         \
	  ons[1] = get_Block_cfgpred(oldn, 0);                                   \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_straightening); \
	} while(0)

/**
 * Merge the debug info due to an if simplification.
 *
 * @param oldn   the old Block
 * @param proj1  the first ProjX predecessor
 * @param proj2  the second ProjX predecessor
 * @param n      the new Block
 */
#define DBG_OPT_IFSIM1(oldn, proj1, proj2, n)                        \
	do {                                                             \
	  ir_node *ons[4];                                               \
	  ons[0] = oldn;                                                 \
	  ons[1] = proj1;                                                \
	  ons[2] = proj2;                                                \
	  ons[3] = get_Proj_pred(proj1);                                 \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_if_simplification); \
	} while(0)

/**
 * Merge the debug info due to an if simplification.
 * @param oldn   the old Cond
 * @param n      the new Jmp
 */
#define DBG_OPT_IFSIM2(oldn, n)                              \
	do {                                                     \
	  __dbg_info_merge_pair(n, oldn, dbg_if_simplification); \
	} while(0)

/**
 * Merge the debug info due to an algebraic_simplification.
 * A node could be evaluated into a Constant.
 *
 * @param oldn  the node
 * @param n     the new constant holding the value
 */
#define DBG_OPT_CSTEVAL(oldn, n)                                  \
	do {                                                          \
	  __dbg_info_merge_pair(n, oldn, dbg_const_eval);             \
	} while(0)

/**
 * Merge the debug info due to an algebraic_simplification.
 *
 * @param oldn  the old node
 * @param n     the new node replacing oldn
 */
#define DBG_OPT_ALGSIM0(oldn, n)                                    \
	do {                                                            \
	  __dbg_info_merge_pair(n, oldn, dbg_algebraic_simplification); \
	} while(0)

/**
 * Merge the debug info due to an algebraic_simplification.
 *
 * @param oldn  the old node
 * @param a     a predecessor of oldn
 * @param b     a predecessor of oldn
 * @param n     the new node replacing oldn
 */
#define DBG_OPT_ALGSIM1(oldn, a, b, n)                            \
	do {                                                          \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  ons[2] = b;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_algebraic_simplification); \
	} while(0)

/**
 * Merge the debug info due to an algebraic_simplification.
 *
 * @param oldn  the old node
 * @param pred  the predecessor of oldn
 * @param n     the new node replacing oldn
 */
#define DBG_OPT_ALGSIM2(oldn, pred, n)                            \
	do {                                                          \
	  ir_node *ons[3];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = pred;                                              \
	  ons[2] = n;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_algebraic_simplification); \
	} while(0)

/**
 * Merge the debug info due to an algebraic_simplification.
 */
#define DBG_OPT_ALGSIM3(oldn, a, n)                               \
	do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = oldn;                                              \
	  ons[1] = a;                                                 \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_algebraic_simplification); \
	} while(0)

/**
 * Merge the debug info due to a Phi optimization.
 * A Phi node was replaced by one of its input (the only meaningful)
 *
 * @param phi  the Phi node that will be replaced
 * @param n    in Phi Input that will replace Phi
 */
#define DBG_OPT_PHI(phi, n)                                      \
	do {                                                         \
	  __dbg_info_merge_sets(&n, 1, &phi, 1, dbg_opt_ssa);        \
	} while(0)


/**
 * Merge the debug info due to a Sync optimization.
 * A Sync node was replaced by one of its input (the only meaningful)
 *
 * @param sync  the Sync node that will be replaced
 * @param n     in Sync Input that will replace Sync
 */
#define DBG_OPT_SYNC(sync, n)                                     \
	do {                                                          \
	  __dbg_info_merge_sets(&n, 1, &sync, 1, dbg_opt_ssa);        \
	} while(0)


/**
 * Merge the debug info due to Write-after-Write optimization:
 * Store oldst will be removed, because Store st overwrites it.
 *
 * @param oldst  the old store that will be removed
 * @param st     the other store that overwrites oldst
 */
#define DBG_OPT_WAW(oldst, st)                                      \
	do {                                                            \
	  ir_node *ons[2];                                              \
	  ons[0] = oldst;                                               \
	  ons[1] = st;                                                  \
	  __dbg_info_merge_sets(&st, 1, ons, ARRAY_SIZE(ons), dbg_write_after_write); \
	} while(0)

/**
 * Merge the debug info due to Write-after-Read optimization:
 * A store will be removed because it rite a value just read back.
 *
 * @param store  the store that will be removed
 * @param load   the load that produces the value that store will write back
 */
#define DBG_OPT_WAR(store, load)                                      \
	do {                                                              \
	  ir_node *ons[2];                                                \
	  ons[0] = store;                                                 \
	  ons[1] = load;                                                  \
	  __dbg_info_merge_sets(&load, 1, ons, ARRAY_SIZE(ons), dbg_write_after_read); \
	} while(0)

/**
 * Merge the debug info due to Read-after-Write optimization:
 * A load will be replaced by a value that was just stored.
 *
 * @param load   the load that will be replaced
 * @param value  the value that will replace the load
 */
#define DBG_OPT_RAW(load, value)                                       \
	do {                                                               \
	  ir_node *ons[2];                                                 \
	  ons[0] = load;                                                   \
	  ons[1] = value;                                                  \
	  __dbg_info_merge_sets(&value, 1, ons, ARRAY_SIZE(ons), dbg_read_after_write); \
	} while(0)

/**
 * Merge the debug info due to Read-after-Read optimization:
 * Load oldld will be replace by a reference to Load ld.
 *
 * @param oldld  the old load that can be replaced
 * @param ld     the load that produces the same values
 */
#define DBG_OPT_RAR(oldld, ld)                                      \
	do {                                                            \
	  ir_node *ons[2];                                              \
	  ons[0] = oldld;                                               \
	  ons[1] = ld;                                                  \
	  __dbg_info_merge_sets(&ld, 1, ons, ARRAY_SIZE(ons), dbg_read_after_read); \
	} while(0)

/**
 * Merge the debug info due to Read-a-Const optimization:
 * Load ld will be replace by a Constant if the value that
 * will be loaded is known and immutable.
 *
 * @param ld  the load
 * @param c   the constant value that will replace the load's result
 */
#define DBG_OPT_RC(ld, c)                                         \
	do {                                                          \
	  ir_node *ons[2];                                            \
	  ons[0] = ld;                                                \
	  ons[1] = c;                                                 \
	  __dbg_info_merge_sets(&ld, 1, ons, ARRAY_SIZE(ons), dbg_read_a_const); \
	} while(0)

/**
 * Merge the debug info after a tuple optimization.
 * a Proj(Tuple) is replaced by the associated tuple value.
 *
 * @param proj   the Proj node
 * @param tuple  the Tuple node
 * @param n      the Proj(Tuple) value
 */
#define DBG_OPT_TUPLE(proj, tuple, n)                                      \
	do {                                                                   \
	  ir_node *ons[3];                                                     \
	  ons[0] = proj;                                                       \
	  ons[1] = tuple;                                                      \
	  ons[2] = n;                                                          \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_opt_auxnode); \
	} while(0)

/**
 * Merge the debug info after an Id optimization.
 * An Id node was replaced by its non-Id predecessor.
 *
 * @param id  the Id node
 * @param n   the predecessor
 */
#define DBG_OPT_ID(id, n)                                                  \
	do {                                                                   \
	  ir_node *ons[2];                                                     \
	  ons[0] = id;                                                         \
	  ons[1] = n;                                                          \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_opt_auxnode); \
	} while(0)

/**
 * Merge the debug info due to common-subexpression elimination.
 *
 * @param oldn  the old node
 * @param n     the node that replaces oldn
 */
#define DBG_OPT_CSE(oldn, n)                                           \
	do {                                                               \
	  ir_node *ons[2];                                                 \
	  ons[0] = oldn;                                                   \
	  ons[1] = n;                                                      \
	  __dbg_info_merge_sets(&n, 1, ons, ARRAY_SIZE(ons), dbg_opt_cse); \
	} while(0)

/**
 * A node was replaced by another node due to a Confirmation.
 *
 * @param oldn  the old node
 * @param n     the new node
 */
#define DBG_OPT_CONFIRM(oldn, n)                                  \
	do {                                                          \
	  __dbg_info_merge_pair(n, oldn, dbg_opt_confirm);            \
	} while(0)

/**
 * A node was replaced by a constant due to a Confimation.
 *
 * @param oldn  the old node
 * @param c     the new constant node
 */
#define DBG_OPT_CONFIRM_C(oldn, c)                                \
	do {                                                          \
	  __dbg_info_merge_pair(c, oldn, dbg_opt_confirm);            \
	} while(0)

/**
 * Merge the debug info due to a GVN-PRE result.
 *
 * @param oldn  the old node
 * @param n     the new node replacing oldn
 */
#define DBG_OPT_GVN_PRE(oldn, n)                              \
	do {                                                      \
	  __dbg_info_merge_pair(n, oldn, dbg_gvn_pre);            \
	} while(0)

/**
 * Merge the debug info due to a combo result.
 *
 * @param oldn  the old node
 * @param n     the new node replacing oldn
 */
#define DBG_OPT_COMBO(oldn, n)                                \
	do {                                                      \
	  __dbg_info_merge_pair(n, oldn, dbg_combo);              \
	} while(0)

/**
 * Merge the debug info due to a jump threading result.
 *
 * @param oldn  the old control flow node
 * @param n     the new control flow node replacing oldn
 */
#define DBG_OPT_JUMPTHREADING(oldn, n)                         \
	do {                                                   \
	  __dbg_info_merge_pair(n, oldn, dbg_jumpthreading);       \
	} while(0)

#endif
