/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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

#include "begin.h"

typedef unsigned long ir_visited_t;
typedef unsigned long ir_exc_region_t;
typedef unsigned long ir_label_t;

typedef struct dbg_info             dbg_info,            *dbg_info_ptr;
typedef struct type_dbg_info        type_dbg_info,       *type_dbg_info_ptr;
typedef struct ident                ident,               *ir_ident_ptr;
typedef struct ir_node              ir_node,             *ir_node_ptr;
typedef struct ir_op                ir_op,               *ir_op_ptr;
typedef struct ir_mode              ir_mode,             *ir_mode_ptr;
typedef struct ir_edge_t            ir_edge_t,           *ir_edge_ptr;
typedef struct ir_heights_t         ir_heights_t;
typedef struct ir_tarval            ir_tarval,           *ir_tarval_ptr;
typedef struct ir_enum_const        ir_enum_const,       *ir_enum_const_ptr;
typedef struct ir_type              ir_type,             *ir_type_ptr;
typedef struct ir_graph             ir_graph,            *ir_graph_ptr;
typedef struct ir_prog              ir_prog,             *ir_prog_ptr;
typedef struct ir_loop              ir_loop,             *ir_loop_ptr;
typedef struct ir_region            ir_region,           *ir_region_ptr;
typedef struct ir_reg_tree          ir_reg_tree,         *ir_reg_tree_ptr;
typedef struct ir_entity            ir_entity,           *ir_entity_ptr;
typedef struct ir_extblk            ir_extblk,           *ir_extblk_ptr;
typedef struct ir_exec_freq         ir_exec_freq,        *ir_exec_freq_ptr;
typedef struct ir_cdep              ir_cdep,             *ir_cdep_ptr;
typedef struct sn_entry             *seqno_t;
typedef struct arch_irn_ops_t       arch_irn_ops_t;
typedef struct type_identify_if_t   type_identify_if_t;
typedef struct ir_graph_pass_t      ir_graph_pass_t;
typedef struct ir_prog_pass_t       ir_prog_pass_t;

typedef struct ir_graph_pass_manager_t      ir_graph_pass_manager_t;
typedef struct ir_prog_pass_manager_t       ir_prog_pass_manager_t;

typedef union  ir_initializer_t     ir_initializer_t,    *ir_initializer_ptr;

typedef void irg_walk_func(ir_node *, void *);
typedef void irg_reg_walk_func(ir_region *, void *);

/* settings */
typedef struct ir_settings_arch_dep_t ir_settings_arch_dep_t;

/* Needed for MSVC to suppress warnings because it doest NOT handle const right. */
typedef const ir_node *ir_node_cnst_ptr;

/* states */

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

#ifdef __cplusplus
# define ENUM_BITSET(type) \
	extern "C++" { \
		static inline type operator ~  (type  a)         { return     (type)~(int)a;           } \
		static inline type operator &  (type  a, type b) { return     (type)((int)a & (int)b); } \
		static inline type operator &= (type& a, type b) { return a = (type)((int)a & (int)b); } \
		static inline type operator ^  (type  a, type b) { return     (type)((int)a ^ (int)b); } \
		static inline type operator ^= (type& a, type b) { return a = (type)((int)a ^ (int)b); } \
		static inline type operator |  (type  a, type b) { return     (type)((int)a | (int)b); } \
		static inline type operator |= (type& a, type b) { return a = (type)((int)a | (int)b); } \
	}
#else
# define ENUM_BITSET(type)
#endif

#ifdef __cplusplus
# define ENUM_COUNTABLE(type) \
	extern "C++" { \
		static inline type operator ++(type& a) { return a = (type)((int)a + 1); } \
		static inline type operator --(type& a) { return a = (type)((int)a - 1); } \
	}
#else
# define ENUM_COUNTABLE(type)
#endif

/**
 * Relations for comparing numbers
 */
typedef enum ir_relation {
	ir_relation_false              = 0,       /**< always false */
	ir_relation_equal              = 1u << 0, /**< equal */
	ir_relation_less               = 1u << 1, /**< less */
	ir_relation_greater            = 1u << 2, /**< greater */
	ir_relation_unordered          = 1u << 3, /**< unordered */
	ir_relation_less_equal         = ir_relation_equal|ir_relation_less,    /**< less or equal */
	ir_relation_greater_equal      = ir_relation_equal|ir_relation_greater, /**< greater or equal */
	ir_relation_less_greater       = ir_relation_less|ir_relation_greater,  /** less or greater ('not equal' for integer numbers) */
	ir_relation_less_equal_greater = ir_relation_equal|ir_relation_less|ir_relation_greater, /**< less equal or greater ('not unordered') */
	ir_relation_unordered_equal    = ir_relation_unordered|ir_relation_equal, /**< unordered or equal */
	ir_relation_unordered_less     = ir_relation_unordered|ir_relation_less,  /**< unorderedor less */
	ir_relation_unordered_less_equal = ir_relation_unordered|ir_relation_less|ir_relation_equal, /**< unordered, less or equal */
	ir_relation_unordered_greater    = ir_relation_unordered|ir_relation_greater, /**< unordered or greater */
	ir_relation_unordered_greater_equal = ir_relation_unordered|ir_relation_greater|ir_relation_equal, /**< unordered, greater or equal */
	ir_relation_unordered_less_greater  = ir_relation_unordered|ir_relation_less|ir_relation_greater, /**< unordered, less or greater ('not equal' for floatingpoint numbers) */
	ir_relation_true                    = ir_relation_equal|ir_relation_less|ir_relation_greater|ir_relation_unordered, /**< always true */
} ir_relation;
ENUM_BITSET(ir_relation)

/**
 * constrained flags for memory operations.
 */
typedef enum ir_cons_flags {
	cons_none             = 0,        /**< No constrains. */
	cons_volatile         = 1U << 0,  /**< Memory operation is volatile. */
	cons_unaligned        = 1U << 1,  /**< Memory operation is unaligned. */
	cons_floats           = 1U << 2,  /**< Memory operation can float. */
	cons_throws_exception = 1U << 3,  /**< fragile op throws exception (and
	                                       produces X_regular and X_except
	                                       values) */
} ir_cons_flags;
ENUM_BITSET(ir_cons_flags)

/** op_pin_state_pinned states. */
typedef enum op_pin_state {
	op_pin_state_floats = 0,    /**< Nodes of this opcode can be placed in any basic block. */
	op_pin_state_pinned = 1,    /**< Nodes must remain in this basic block. */
	op_pin_state_exc_pinned,    /**< Node must be remain in this basic block if it can throw an
	                                 exception, else can float. Used internally. */
	op_pin_state_mem_pinned     /**< Node must be remain in this basic block if it can throw an
	                                 exception or uses memory, else can float. Used internally. */
} op_pin_state;

/**
 * A type to express conditional jump predictions.
 */
typedef enum cond_jmp_predicate {
	COND_JMP_PRED_NONE,        /**< No jump prediction. Default. */
	COND_JMP_PRED_TRUE,        /**< The True case is predicted. */
	COND_JMP_PRED_FALSE        /**< The False case is predicted. */
} cond_jmp_predicate;

/**
 * Additional method type properties:
 * Tell about special properties of a method type. Some
 * of these may be discovered by analyses.
 */
typedef enum mtp_additional_properties {
	mtp_no_property            = 0x00000000, /**< no additional properties, default */
	mtp_property_const         = 0x00000001, /**< This method did not access memory and calculates
	                                              its return values solely from its parameters.
	                                              The only observable effect of a const function must be its
	                                              return value. So they must not exhibit infinite loops or wait
	                                              for user input. The return value must not depend on any
	                                              global variables/state.
	                                              GCC: __attribute__((const)). */
	mtp_property_pure          = 0x00000002, /**< This method did NOT write to memory and calculates
	                                              its return values solely from its parameters and
	                                              the memory they points to (or global vars).
	                                              The only observable effect of a const function must be its
	                                              return value. So they must not exhibit infinite loops or wait
	                                              for user input.
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
	mtp_property_returns_twice = 0x00000040, /**< This method can return more than one (typically setjmp).
                                                  GCC: __attribute__((returns_twice)). */
	mtp_property_intrinsic     = 0x00000080, /**< This method is intrinsic. It is expected that
	                                              a lowering phase will remove all calls to it. */
	mtp_property_runtime       = 0x00000100, /**< This method represents a runtime routine. */
	mtp_property_private       = 0x00000200, /**< All method invocations are known, the backend is free to
	                                              optimize the call in any possible way. */
	mtp_property_has_loop      = 0x00000400, /**< Set, if this method contains one possible endless loop. */
	mtp_property_inherited     = (1<<31)     /**< Internal. Used only in irg's, means property is
	                                              inherited from type. */
} mtp_additional_properties;
ENUM_BITSET(mtp_additional_properties)

/**  This enum names the different kinds of symbolic Constants represented by
 * SymConst.  The content of the attribute symconst_symbol depends on this tag.
 * Use the proper access routine after testing this flag. */
typedef enum symconst_kind {
	symconst_type_tag,    /**< The SymConst is a type tag for the given type.
	                           symconst_symbol is type *. */
	symconst_type_size,   /**< The SymConst is the size of the given type.
	                           symconst_symbol is type *. */
	symconst_type_align,  /**< The SymConst is the alignment of the given type.
	                           symconst_symbol is type *. */
	symconst_addr_ent,    /**< The SymConst is a symbolic pointer to be filled in
	                           by the linker.  The pointer is represented by an entity.
	                           symconst_symbol is entity *. */
	symconst_ofs_ent,     /**< The SymConst is the offset of its entity in the entities
	                           owner type. */
	symconst_enum_const   /**< The SymConst is a enumeration constant of an
	                           enumeration type. */
} symconst_kind;

/** SymConst attribute.
 *
 *  This union contains the symbolic information represented by the node.
 */
typedef union symconst_symbol {
	ir_type       *type_p;    /**< The type of a SymConst. */
	ir_entity     *entity_p;  /**< The entity of a SymConst. */
	ir_enum_const *enum_p;    /**< The enumeration constant of a SymConst. */
} symconst_symbol;

/** The allocation place. */
typedef enum ir_where_alloc {
	stack_alloc,          /**< Alloc allocates the object on the stack. */
	heap_alloc            /**< Alloc allocates the object on the heap. */
} ir_where_alloc;

/** A input/output constraint attribute. */
typedef struct ir_asm_constraint {
	unsigned       pos;           /**< The inputs/output position for this constraint. */
	ident          *constraint;   /**< The constraint for this input/output. */
	ir_mode        *mode;         /**< The mode of the constraint. */
} ir_asm_constraint;

/** Supported libFirm builtins. */
typedef enum ir_builtin_kind {
	ir_bk_trap,                   /**< GCC __builtin_trap(): insert trap */
	ir_bk_debugbreak,             /**< MS __debugbreak(): insert debug break */
	ir_bk_return_address,         /**< GCC __builtin_return_address() */
	ir_bk_frame_address,          /**< GCC __builtin_frame_address() */
	ir_bk_prefetch,               /**< GCC __builtin_prefetch() */
	ir_bk_ffs,                    /**< GCC __builtin_ffs(): find first (least) significant 1 bit */
	ir_bk_clz,                    /**< GCC __builtin_clz(): count leading zero */
	ir_bk_ctz,                    /**< GCC __builtin_ctz(): count trailing zero */
	ir_bk_popcount,               /**< GCC __builtin_popcount(): population count */
	ir_bk_parity,                 /**< GCC __builtin_parity(): parity */
	ir_bk_bswap,                  /**< byte swap */
	ir_bk_inport,                 /**< in port */
	ir_bk_outport,                /**< out port */
	ir_bk_inner_trampoline,       /**< address of a trampoline for GCC inner functions */
} ir_builtin_kind;

/**
 * Possible return values of value_classify().
 */
typedef enum ir_value_classify_sign {
	value_classified_unknown  = 0,   /**< could not classify */
	value_classified_positive = 1,   /**< value is positive, i.e. >= 0 */
	value_classified_negative = -1   /**< value is negative, i.e. <= 0 if
	                                      no signed zero exists or < 0 else */
} ir_value_classify_sign;

/**
 * This enumeration flags the volatility of entities and Loads/Stores.
 */
typedef enum {
	volatility_non_volatile,    /**< The entity is not volatile. Default. */
	volatility_is_volatile      /**< The entity is volatile. */
} ir_volatility;

/**
 * This enumeration flags the align of Loads/Stores.
 */
typedef enum {
	align_is_aligned = 0, /**< The entity is aligned. Default */
	align_non_aligned,    /**< The entity is not aligned. */
} ir_align;

#include "end.h"

#endif
