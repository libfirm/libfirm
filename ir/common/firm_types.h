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
 * @brief      Definition of opaque firm types
 * @author     Michael Beck
 * @version    $Id$
 */
#ifndef FIRM_COMMON_FIRM_TYPES_H
#define FIRM_COMMON_FIRM_TYPES_H

#ifdef _MSC_VER
typedef          __int64 long64;
typedef unsigned __int64 ulong64;

#define LL_FMT	"i64"
#define ULL_FMT	"ui64"

#else
typedef          long long long64;
typedef unsigned long long ulong64;

#define LL_FMT	"ll"
#define ULL_FMT	"llu"

#endif /* _MSC_VER */

#ifndef _IDENT_TYPEDEF_
#define _IDENT_TYPEDEF_
typedef const struct _ident ident, *ir_ident_ptr;
#endif

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node, *ir_node_ptr;
#endif

#ifndef _IR_OP_TYPEDEF_
#define _IR_OP_TYPEDEF_
typedef struct ir_op ir_op, *ir_op_ptr;
#endif

#ifndef _IR_MODE_TYPEDEF_
#define _IR_MODE_TYPEDEF_
typedef struct ir_mode ir_mode, *ir_mode_ptr;
#endif

#ifndef _IR_EDGE_TYPEDEF_
#define _IR_EDGE_TYPEDEF_
typedef struct _ir_edge_t ir_edge_t, *ir_edge_ptr;
#endif

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
typedef struct tarval tarval, *ir_tarval_ptr;
#endif

#ifndef _IR_ENUM_CONST_TYPEDEF_
#define _IR_ENUM_CONST_TYPEDEF_
typedef struct ir_enum_const ir_enum_const, *ir_enum_const_ptr;
#endif

#ifndef _IR_TYPE_TYPEDEF_
#define _IR_TYPE_TYPEDEF_
typedef struct ir_type ir_type, *ir_type_ptr;
#endif

#ifndef _IR_GRAPH_TYPEDEF_
#define _IR_GRAPH_TYPEDEF_
typedef struct ir_graph ir_graph, *ir_graph_ptr;
#endif

#ifndef _IR_LOOP_TYPEDEF_
#define _IR_LOOP_TYPEDEF_
typedef struct ir_loop ir_loop, *ir_loop_ptr;
#endif

#ifndef _IR_REGION_TYPEDEF_
#define _IR_REGION_TYPEDEF_
typedef struct ir_region ir_region, *ir_region_ptr;
#endif

#ifndef _IR_REG_TREE_TYPEDEF_
#define _IR_REG_TREE_TYPEDEF_
typedef struct ir_reg_tree ir_reg_tree, *ir_reg_tree_ptr;
#endif

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
typedef struct ir_entity ir_entity, *ir_entity_ptr;
#endif

#ifndef _COMPOUND_GRAPH_PATH_TYPEDEF_
#define _COMPOUND_GRAPH_PATH_TYPEDEF_
typedef struct compound_graph_path compound_graph_path, *ir_compound_graph_path_ptr;
#endif

#ifndef _IR_PHASE_TYPEDEF_
#define _IR_PHASE_TYPEDEF_
typedef struct _ir_phase ir_phase, *ir_phase_ptr;
#endif

#ifndef _IR_EXTBB_TYPEDEF_
#define _IR_EXTBB_TYPEDEF_
typedef struct _ir_extblk ir_extblk, *ir_extblk_ptr;
#endif

#ifndef _IRG_WALK_FUNC_TYPEDEF_
#define _IRG_WALK_FUNC_TYPEDEF_
typedef void irg_walk_func(ir_node *, void *);
#endif

#ifndef _IRG_REG_WALK_FUNC_TYPEDEF_
#define _IRG_REG_WALK_FUNC_TYPEDEF_
typedef void irg_reg_walk_func(ir_region *, void *);
#endif

#ifndef _SEQNO_T_TYPEDEF_
#define _SEQNO_T_TYPEDEF_
typedef struct sn_entry *seqno_t;
#endif

#ifndef _EXECFREQ_TYPEDEF
#define _EXECFREQ_TYPEDEF
typedef struct ir_exec_freq ir_exec_freq, *ir_exec_freq_ptr;
#endif

#endif
