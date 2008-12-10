/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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

typedef unsigned long ir_visited_t;
typedef unsigned long ir_exc_region_t;
typedef unsigned long ir_label_t;

typedef struct dbg_info             dbg_info,            *dbg_info_ptr;
typedef const struct _ident         ident,               *ir_ident_ptr;
typedef struct ir_node              ir_node,             *ir_node_ptr;
typedef struct ir_op                ir_op,               *ir_op_ptr;
typedef struct ir_mode              ir_mode,             *ir_mode_ptr;
typedef struct _ir_edge_t           ir_edge_t,           *ir_edge_ptr;
typedef struct tarval               tarval,              *ir_tarval_ptr;
typedef struct ir_enum_const        ir_enum_const,       *ir_enum_const_ptr;
typedef struct ir_type              ir_type,             *ir_type_ptr;
typedef struct ir_graph             ir_graph,            *ir_graph_ptr;
typedef struct ir_loop              ir_loop,             *ir_loop_ptr;
typedef struct ir_region            ir_region,           *ir_region_ptr;
typedef struct ir_reg_tree          ir_reg_tree,         *ir_reg_tree_ptr;
typedef struct ir_entity            ir_entity,           *ir_entity_ptr;
typedef struct compound_graph_path  compound_graph_path, *ir_compound_graph_path_ptr;
typedef struct _ir_phase            ir_phase,            *ir_phase_ptr;
typedef struct _ir_extblk           ir_extblk,           *ir_extblk_ptr;
typedef struct ir_exec_freq         ir_exec_freq,        *ir_exec_freq_ptr;
typedef struct ir_cdep              ir_cdep,             *ir_cdep_ptr;
typedef struct sn_entry             *seqno_t;
typedef struct arch_irn_ops_t       arch_irn_ops_t;
typedef struct ident_if_t           ident_if_t;
typedef struct type_identify_if_t   type_identify_if_t;

typedef union  ir_initializer_t     ir_initializer_t,    *ir_initializer_ptr;

typedef void irg_walk_func(ir_node *, void *);
typedef void irg_reg_walk_func(ir_region *, void *);

/* settings */
typedef struct ir_settings_arch_dep_t ir_settings_arch_dep_t;
typedef struct ir_settings_if_conv_t  ir_settings_if_conv_t;

/* states */

/** The state of the interprocedural view.
 *
 * This value indicates the state of the interprocedural view.
 */
typedef enum {
	ip_view_no,       /**< The interprocedural view is not constructed.  There are no
	                       view specific nodes (EndReg, Filter, Break ...) in any graph.  */
	ip_view_valid,    /**< The interprocedural view is valid.  */
	ip_view_invalid   /**< The interprocedural view is invalid.  Specific nodes are
	                       all still in the representation, but the graph is no more complete. */
} ip_view_state;

/**
 * This function is called, whenever a local variable is used before definition
 *
 * @param irg       the IR graph on which this happens
 * @param mode      the mode of the local var
 * @param pos       position chosen be the frontend for this variable (n_loc)
 *
 * @return a firm node of mode @p mode that initializes the var at position pos
 *
 * @note
 *      Do not return NULL!
 *      If this function is not set, FIRM will create a const node with tarval BAD.
 *      Use set_irg_loc_description()/get_irg_loc_description() to assign additional
 *      informations to local variables.
 */
typedef ir_node *uninitialized_local_variable_func_t(ir_graph *irg, ir_mode *mode, int pos);

#endif
