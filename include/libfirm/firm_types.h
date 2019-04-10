/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief      Definition of opaque firm types
 * @author     Michael Beck
 */
#ifndef FIRM_COMMON_FIRM_TYPES_H
#define FIRM_COMMON_FIRM_TYPES_H

#include "begin.h"

/**
 * @page visited_counters Visited Counters
 * A visited counter is an alternative to a visited flag for elements of a
 * graph data structure.
 * A visited counter is an integer number added to the elements of a graph.
 * There is also a global reference number for the whole data structure. It is
 * now possible to mark nodes by setting their visited counter to the global
 * reference counter. Testing is done by comparing with the global reference
 * counter.
 * The advantage to simple boolean flag variables is that you can clear all
 * element marks by increasing the global reference counter and don't need to
 * visit the whole structure.
 * This makes it more efficient when you only visit/mark a small amount of
 * nodes in the graph.
 */

/** Type for visited counters
 * @see visited_counters */
typedef unsigned long ir_visited_t;
/** A label in the code (usually attached to a @ref Block) */
typedef unsigned long ir_label_t;

/** @ingroup dbg_info
 * Source Reference */
typedef struct dbg_info             dbg_info;
/** @ingroup dbg_info
 * Source Type Reference */
typedef struct type_dbg_info        type_dbg_info;
/** @ingroup ir_ident
 * Identifier
 *
 * @note This is currently defined as a normal C string, but you should not
 *  rely on that and always use get_id_str() before using it as a C string.
 */
typedef const char                  ident;
/** @ingroup ir_node
 * Procedure Graph Node */
typedef struct ir_node              ir_node;
/** @ingroup ir_op
 * Node Opcode */
typedef struct ir_op                ir_op;
/** @ingroup ir_mode
 * SSA Value mode */
typedef struct ir_mode              ir_mode;
/** @ingroup iredges
 * Dynamic Reverse Edge */
typedef struct ir_edge_t            ir_edge_t;
/** @ingroup ir_heights
 * Computed graph Heights */
typedef struct ir_heights_t         ir_heights_t;
/** @ingroup ir_tarval
 * Target Machine Value */
typedef struct ir_tarval            ir_tarval;
/** @ingroup ir_type
 * Type */
typedef struct ir_type              ir_type;
/** @ingroup ir_graph
 * Procedure Graph */
typedef struct ir_graph             ir_graph;
/** @ingroup ir_prog
 * Program */
typedef struct ir_prog              ir_prog;
/** @ingroup ir_loop
 * Loop */
typedef struct ir_loop              ir_loop;
/** @ingroup ir_entity
 * Entity */
typedef struct ir_entity            ir_entity;
/** @ingroup ir_cdep
 * Control Dependence Analysis Results */
typedef struct ir_cdep              ir_cdep;
/** @ingroup ir_initializer
 * Initializer (for entities) */
typedef union  ir_initializer_t     ir_initializer_t;
/** @ingroup machine_triple
 * Machine triple */
typedef struct ir_machine_triple_t  ir_machine_triple_t;

/**
 * @ingroup irgwalk
 * type for graph-walk callbacks */
typedef void irg_walk_func(ir_node *, void *);

/**
 * @ingroup Switch
 * A switch table mapping integer numbers to proj-numbers of a Switch-node.
 * Entries map a continuous range of integer numbers to a proj-number.
 * There must never be two different entries matching the same integer number.
 */
typedef struct ir_switch_table  ir_switch_table;

/**
 * @ingroup ir_cons
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
 *      If this function is not set, FIRM will create an Unknown node.
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
/** Marks an enum type as bitset enum. That is the enumeration values will
 * probably be combined to form a (bit)set of flags.
 * When compiling for C++ this macro will define the ~, &, &=, ^, ^=, | and |=
 * operators for the enum values. */
# define ENUM_BITSET(type)
#endif

#ifdef __cplusplus
# define ENUM_COUNTABLE(type) \
	extern "C++" { \
		static inline type operator ++(type& a) { return a = (type)((int)a + 1); } \
		static inline type operator --(type& a) { return a = (type)((int)a - 1); } \
	}
#else
/** Marks an enum type as countable enum. The enumeration values will be a
 * linear sequence of numbers which can be iterated through by incrementing
 * by 1.
 * When compiling for C++ this macro will define the ++ and -- operators. */
# define ENUM_COUNTABLE(type)
#endif

/**
 * @ingroup ir_node
 * Relations for comparing numbers
 */
typedef enum ir_relation {
	ir_relation_false                   = 0,       /**< always false */
	ir_relation_equal                   = 1u << 0, /**< equal */
	ir_relation_less                    = 1u << 1, /**< less */
	ir_relation_greater                 = 1u << 2, /**< greater */
	ir_relation_unordered               = 1u << 3, /**< unordered */
	ir_relation_less_equal              = ir_relation_equal|ir_relation_less,    /**< less or equal */
	ir_relation_greater_equal           = ir_relation_equal|ir_relation_greater, /**< greater or equal */
	ir_relation_less_greater            = ir_relation_less|ir_relation_greater,  /**< less or greater ('not equal' for integer numbers) */
	ir_relation_less_equal_greater      = ir_relation_equal|ir_relation_less|ir_relation_greater, /**< less equal or greater ('not unordered') */
	ir_relation_unordered_equal         = ir_relation_unordered|ir_relation_equal, /**< unordered or equal */
	ir_relation_unordered_less          = ir_relation_unordered|ir_relation_less,  /**< unordered or less */
	ir_relation_unordered_less_equal    = ir_relation_unordered|ir_relation_less|ir_relation_equal, /**< unordered, less or equal */
	ir_relation_unordered_greater       = ir_relation_unordered|ir_relation_greater, /**< unordered or greater */
	ir_relation_unordered_greater_equal = ir_relation_unordered|ir_relation_greater|ir_relation_equal, /**< unordered, greater or equal */
	ir_relation_unordered_less_greater  = ir_relation_unordered|ir_relation_less|ir_relation_greater, /**< unordered, less or greater ('not equal' for floatingpoint numbers) */
	ir_relation_true                    = ir_relation_equal|ir_relation_less|ir_relation_greater|ir_relation_unordered, /**< always true */
} ir_relation;
ENUM_BITSET(ir_relation)

/**
 * @ingroup ir_node
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

/**
 * @ingroup ir_node
 * pinned states.
 */
typedef enum op_pin_state {
	op_pin_state_floats = 0,    /**< Nodes of this opcode can be placed in any basic block. */
	op_pin_state_pinned = 1,    /**< Nodes must remain in this basic block. */
	op_pin_state_exc_pinned,    /**< Node must remain in this basic block if it
	                                 can throw an exception, else can float. */
} op_pin_state;

/**
 * @ingroup Cond
 * A type to express conditional jump predictions.
 */
typedef enum cond_jmp_predicate {
	COND_JMP_PRED_NONE,        /**< No jump prediction. Default. */
	COND_JMP_PRED_TRUE,        /**< The True case is predicted. */
	COND_JMP_PRED_FALSE        /**< The False case is predicted. */
} cond_jmp_predicate;

/**
 * @ingroup method_type
 * Additional method type properties:
 * Tell about special properties of a method type. Some
 * of these may be discovered by analyses.
 */
typedef enum mtp_additional_properties {
	/** No additional properties */
	mtp_no_property                 = 0,
	/** This method does not change any memory known to the rest of the
	 * program. */
	mtp_property_no_write           = 1u << 0,
	/** The behaviour of the method does not depend on any global/external
	 * state. This mostly means that no waiting/reading of user input
	 * is performed, no global variables read, or pointers to memory visible
	 * outside of the function dereferenced. The result of the function
	 * solely depends on its arguments. */
	mtp_property_pure               = 1u << 1,
	/** This method never returns. The method may for example abort or exit the
	 * program or contain an infinite loop).
	 * GCC: __attribute__((noreturn)). */
	mtp_property_noreturn           = 1u << 2,
	/** The function is guaranteed not to end in an endless and to not abort
	 * the program. */
	mtp_property_terminates         = 1u << 3,
	/** This method cannot throw an exception. GCC: __attribute__((nothrow)). */
	mtp_property_nothrow            = 1u << 4,
	/** This method is naked. GCC: __attribute__((naked)). */
	mtp_property_naked              = 1u << 5,
	/** This method returns newly allocate memory.
	 * GCC: __attribute__((malloc)). */
	mtp_property_malloc             = 1u << 6,
	/** This method can return more than once (typically setjmp).
	 * GCC: __attribute__((returns_twice)). */
	mtp_property_returns_twice      = 1u << 7,
	/** All method invocations are known and inside the current compilation
	 * unit, the backend can freely choose the calling convention. */
	mtp_property_private            = 1u << 8,
	/** Try to always inline this function, even if it seems nonprofitable */
	mtp_property_always_inline      = 1u << 9,
	/** The function should not be inlined */
	mtp_property_noinline           = 1u << 10,
	/** The programmer recommends to inline the function */
	mtp_property_inline_recommended = 1u << 11,
	/** Marker used by opt_funccall (really a hack)... */
	mtp_temporary                   = 1u << 12,
	/** marker used for oo analyses needing info whether method is constructor or not */
	mtp_property_is_constructor     = 1u << 13,
} mtp_additional_properties;
ENUM_BITSET(mtp_additional_properties)

/** A input/output constraint attribute.
 * @ingroup ASM
 */
typedef struct ir_asm_constraint {
	int      in_pos;     /**< The input position for this constraint. */
	int      out_pos;    /**< The output position for this constraint. */
	ident   *constraint; /**< The constraint for this input/output. */
	ir_mode *mode;       /**< The mode of the constraint. */
} ir_asm_constraint;

/** Supported libFirm builtins.
 * @ingroup Builtin
 */
typedef enum ir_builtin_kind {
	ir_bk_trap,                 /**< GCC __builtin_trap(): insert trap */
	ir_bk_debugbreak,           /**< MS __debugbreak(): insert debug break */
	ir_bk_return_address,       /**< GCC __builtin_return_address() */
	ir_bk_frame_address,        /**< GCC __builtin_frame_address() */
	ir_bk_prefetch,             /**< GCC __builtin_prefetch() */
	ir_bk_ffs,                  /**< GCC __builtin_ffs(): find first (least)
	                                 significant 1 bit */
	ir_bk_clz,                  /**< GCC __builtin_clz(): count leading zero */
	ir_bk_ctz,                  /**< GCC __builtin_ctz(): count trailing zero */
	ir_bk_popcount,             /**< GCC __builtin_popcount(): population
	                                 count */
	ir_bk_parity,               /**< GCC __builtin_parity(): parity */
	ir_bk_bswap,                /**< byte swap */
	ir_bk_inport,               /**< in port */
	ir_bk_outport,              /**< out port */
	ir_bk_saturating_increment, /**< saturating increment */
	ir_bk_compare_swap,         /**< compare exchange (aka. compare and swap) */
	ir_bk_may_alias,            /**< replaced by 0 if args cannot alias,
	                                 1 otherwise */
	ir_bk_va_start,             /**< va_start from <stdarg.h> */
	ir_bk_va_arg,               /**< va_arg from <stdarg.h> */
	ir_bk_last = ir_bk_va_arg,
} ir_builtin_kind;

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

/**
 * Specifies what happens when a float value is converted to an integer and
 * overflow happens.
 */
typedef enum float_int_conversion_overflow_style_t {
	ir_overflow_indefinite,  /**< the integer indefinite value (=INT_MIN) is
	                              returned. (e.g. x86 does this) */
	ir_overflow_min_max,     /**< INT_MIN/INT_MAX is returned depending on the
	                              sign of the floatingpoint number. (e.g. sparc
	                              does this). */
} float_int_conversion_overflow_style_t;

typedef struct hook_entry hook_entry_t;

#include "end.h"

#endif
