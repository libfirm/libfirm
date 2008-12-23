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

/** op_pin_state_pinned states. */
typedef enum {
	op_pin_state_floats = 0,    /**< Nodes of this opcode can be placed in any basic block. */
	op_pin_state_pinned = 1,    /**< Nodes must remain in this basic block. */
	op_pin_state_exc_pinned,    /**< Node must be remain in this basic block if it can throw an
	                                 exception, else can float. Used internally. */
	op_pin_state_mem_pinned     /**< Node must be remain in this basic block if it can throw an
	                                 exception or uses memory, else can float. Used internally. */
} op_pin_state;

/**
 * Additional method type properties:
 * Tell about special properties of a method type. Some
 * of these may be discovered by analyses.
 */
typedef enum {
	mtp_no_property            = 0x00000000, /**< no additional properties, default */
	mtp_property_const         = 0x00000001, /**< This method did not access memory and calculates
	                                              its return values solely from its parameters.
	                                              GCC: __attribute__((const)). */
	mtp_property_pure          = 0x00000002, /**< This method did NOT write to memory and calculates
	                                              its return values solely from its parameters and
	                                              the memory they points to (or global vars).
	                                              GCC: __attribute__((pure)). */
	mtp_property_noreturn      = 0x00000004, /**< This method did not return due to an aborting system
	                                              call.
	                                              GCC: __attribute__((noreturn)). */
	mtp_property_nothrow       = 0x00000008, /**< This method cannot throw an exception.
	                                              GCC: __attribute__((nothrow)). */
	mtp_property_naked         = 0x00000010, /**< This method is naked.
	                                              GCC: __attribute__((naked)). */
	mtp_property_malloc        = 0x00000020, /**< This method returns newly allocate memory.
	                                              GCC: __attribute__((malloc)). */
	mtp_property_weak          = 0x00000040, /**< This method is weak. It is expected that
	                                              GCC: __attribute__((weak)). */
	mtp_property_returns_twice = 0x00000080, /**< This method can return more than one (typically setjmp).
                                                  GCC: __attribute__((returns_twice)). */
	mtp_property_intrinsic     = 0x00000100, /**< This method is intrinsic. It is expected that
	                                              a lowering phase will remove all calls to it. */
	mtp_property_runtime       = 0x00000200, /**< This method represents a runtime routine. */
	mtp_property_private       = 0x00000400, /**< All method invocations are known, the backend is free to
	                                              optimize the call in any possible way. */
	mtp_property_has_loop      = 0x00000800, /**< Set, if this method contains one possible endless loop. */
	mtp_property_inherited     = (1<<31)     /**< Internal. Used only in irg's, means property is
	                                              inherited from type. */
} mtp_additional_property;

/**  This enum names the three different kinds of symbolic Constants
     represented by SymConst.  The content of the attribute type_or_id
     depends on this tag.  Use the proper access routine after testing
     this flag. */
typedef enum {
	symconst_type_tag,    /**< The SymConst is a type tag for the given type.
	                           symconst_symbol is type *. */
	symconst_type_size,   /**< The SymConst is the size of the given type.
	                           symconst_symbol is type *. */
	symconst_type_align,  /**< The SymConst is the alignment of the given type.
	                           symconst_symbol is type *. */
	symconst_addr_name,   /**< The SymConst is a symbolic pointer to be filled in
	                           by the linker.  The pointer is represented by a string.
	                           symconst_symbol is ident *. */
	symconst_addr_ent,    /**< The SymConst is a symbolic pointer to be filled in
	                           by the linker.  The pointer is represented by an entity.
	                           symconst_symbol is entity *. */
	symconst_ofs_ent,     /**< The SymConst is the offset of its entity in the entities
	                           owner type. */
	symconst_enum_const,  /**< The SymConst is a enumeration constant of an
	                           enumeration type. */
	symconst_label        /**< The SymConst is a label address. */
} symconst_kind;

/** SymConst attribute.
 *
 *  This union contains the symbolic information represented by the node.
 */
typedef union symconst_symbol {
	ir_type       *type_p;    /**< The type of a SymConst. */
	ident         *ident_p;   /**< The ident of a SymConst. */
	ir_entity     *entity_p;  /**< The entity of a SymConst. */
	ir_enum_const *enum_p;    /**< The enumeration constant of a SymConst. */
	ir_label_t    label;      /**< The label of a SymConst. */
} symconst_symbol;

/**
 * Projection numbers for Cmp are defined several times.
 * The bit patterns are used for various tests, so don't change.
 * The "unordered" values are possible results of comparing
 * floating point numbers.
 * Note that the encoding is imported, so do NOT change the order.
 */
typedef enum {
	pn_Cmp_False = 0,                             /**< false */
	pn_Cmp_Eq    = 1,                             /**< equal */
	pn_Cmp_Lt    = 2,                             /**< less */
	pn_Cmp_Le    = pn_Cmp_Eq|pn_Cmp_Lt,           /**< less or equal */
	pn_Cmp_Gt    = 4,                             /**< greater */
	pn_Cmp_Ge    = pn_Cmp_Eq|pn_Cmp_Gt,           /**< greater or equal */
	pn_Cmp_Lg    = pn_Cmp_Lt|pn_Cmp_Gt,           /**< less or greater */
	pn_Cmp_Leg   = pn_Cmp_Lt|pn_Cmp_Eq|pn_Cmp_Gt, /**< less, equal or greater = ordered */
	pn_Cmp_Uo    = 8,                             /**< unordered */
	pn_Cmp_Ue    = pn_Cmp_Uo|pn_Cmp_Eq,           /**< unordered or equal */
	pn_Cmp_Ul    = pn_Cmp_Uo|pn_Cmp_Lt,           /**< unordered or less */
	pn_Cmp_Ule   = pn_Cmp_Uo|pn_Cmp_Eq|pn_Cmp_Lt, /**< unordered, less or equal */
	pn_Cmp_Ug    = pn_Cmp_Uo|pn_Cmp_Gt,           /**< unordered or greater */
	pn_Cmp_Uge   = pn_Cmp_Uo|pn_Cmp_Eq|pn_Cmp_Gt, /**< unordered, greater or equal */
	pn_Cmp_Ne    = pn_Cmp_Uo|pn_Cmp_Lt|pn_Cmp_Gt, /**< unordered, less or greater = not equal */
	pn_Cmp_True  = 15                             /**< true */
	/* not_mask = Leg*/   /* bits to flip to negate comparison * @@ hack for JNI interface */
} pn_Cmp;   /* Projection numbers for Cmp */

/** The allocation place. */
typedef enum {
	stack_alloc,          /**< Alloc allocates the object on the stack. */
	heap_alloc            /**< Alloc allocates the object on the heap. */
} ir_where_alloc;

/** A input/output constraint attribute. */
typedef struct {
	unsigned       pos;           /**< The inputs/output position for this constraint. */
	ident          *constraint;   /**< The constraint for this input/output. */
	ir_mode        *mode;         /**< The mode of the constraint. */
} ir_asm_constraint;

/** Supported libFirm builtins. */
/** Supported libFirm builtins. */
typedef enum {
	ir_bk_return_address,         /**< GCC __builtin_return_address() */
	ir_bk_frame_addess,           /**< GCC __builtin_frame_address() */
	ir_bk_prefetch,               /**< GCC __builtin_prefetch() */
	ir_bk_ffs,                    /**< GCC __builtin_ffs(): find first (least) significant 1 bit */
	ir_bk_clz,                    /**< GCC __builtin_clz(): count leading zero */
	ir_bk_ctz,                    /**< GCC __builtin_ctz(): count trailing zero */
	ir_bk_popcount,               /**< GCC __builtin_popcount(): population count */
	ir_bk_parity,                 /**< GCC __builtin_parity(): parity */
} ir_builtin_kind;

#endif
