/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Type definitions for ia32 node attributes.
 * @author      Christian Wuerdig
 */
#ifndef FIRM_BE_IA32_IA32_NODES_ATTR_H
#define FIRM_BE_IA32_IA32_NODES_ATTR_H

#include "firm_types.h"
#include "bearch.h"
#include "irnode_t.h"
#include "x86_cc.h"

typedef enum {
	ia32_Normal,
	ia32_AddrModeD,
	ia32_AddrModeS
} ia32_op_type_t;

typedef enum {
	ia32_am_none   = 0,
	ia32_am_unary  = 1,
	ia32_am_binary = 2
} ia32_am_type_t;

typedef enum {
	match_commutative       = 1 << 0, /**< inputs are commutative */
	match_am_and_immediates = 1 << 1, /**< node supports AM and immediate at
	                                       the same time */
	match_am                = 1 << 2, /**< node supports (32bit) source AM */
	match_8bit_am           = 1 << 3, /**< node supports 8bit source AM */
	match_16bit_am          = 1 << 4, /**< node supports 16bit source AM */
	match_immediate         = 1 << 5, /**< node supports immediates */
	/** for 8/16 bit modes, mode_neutral operations can be emulated by their
	 * 32bit equivalents, they just don't care about the upper bits (they can be
	 * arbitrary before the insn and are unknown after the instruction). */
	match_mode_neutral      = 1 << 6,
	/** for 8/16 bit modes, sign_ext operations can be emulated by their
	 * 32bit equivalents, however the upper bits must be sign extended. */
	match_sign_ext          = 1 << 7,
	/** for 8/16 bit modes, zero_ext operations can be emulated by their
	 * 32bit equivalents, however the upper bits must be zero extended. */
	match_zero_ext          = 1 << 8,
	/** for 8/16 bit modes, upconv operations can be emulated by their
	 * 32bit equivalents, however the upper bits have to sign/zero extended
	 * based on the operations mode. */
	match_upconv            = 1 << 9,
	match_try_am            = 1 << 10, /**< only try to produce AM node, don't
	                                       do anything if AM isn't possible */
	match_two_users         = 1 << 11,/**< the instruction uses a load two times ... */
} match_flags_t;
ENUM_BITSET(match_flags_t)

typedef struct ia32_op_attr_t ia32_op_attr_t;
struct ia32_op_attr_t {
	//match_flags_t  flags;
	unsigned       latency;
};

#ifndef NDEBUG
typedef enum {
	IA32_ATTR_INVALID                = 0,
	IA32_ATTR_ia32_attr_t            = 1 << 0,
	IA32_ATTR_ia32_x87_attr_t        = 1 << 1,
	IA32_ATTR_ia32_asm_attr_t        = 1 << 2,
	IA32_ATTR_ia32_immediate_attr_t  = 1 << 3,
	IA32_ATTR_ia32_condcode_attr_t   = 1 << 4,
	IA32_ATTR_ia32_copyb_attr_t      = 1 << 5,
	IA32_ATTR_ia32_call_attr_t       = 1 << 6,
	IA32_ATTR_ia32_climbframe_attr_t = 1 << 7,
	IA32_ATTR_ia32_switch_attr_t     = 1 << 8,
	IA32_ATTR_ia32_return_attr_t     = 1 << 9,
} ia32_attr_type_t;
#endif

typedef enum ia32_frame_use_t {
	IA32_FRAME_USE_NONE,
	IA32_FRAME_USE_32BIT,
	IA32_FRAME_USE_64BIT,
	IA32_FRAME_USE_AUTO,
} ia32_frame_use_t;

/**
 * The generic ia32 attributes. Every node has them.
 */
typedef struct ia32_attr_t ia32_attr_t;
struct ia32_attr_t {
	except_attr  exc;               /**< the exception attribute. MUST be the first one. */

	ENUMBF(ia32_op_type_t) tp:2;          /**< Indicator of used address mode. */
	unsigned am_arity:2;            /**< Indicates the address mode type supported by this node. */
	unsigned am_scale:2;            /**< The address mode scale for index register. */

	unsigned am_sc_no_pic_adjust:1; /**< AM entity can be relative to EIP */
	unsigned am_tls_segment:1;      /**< addresses are relative to TLS */
	ENUMBF(ia32_frame_use_t) frame_use:2; /**< Whether an entity on the frame is used and its size. */
	unsigned has_except_label:1;        /**< Set if this node needs a label because of possible exception. */

	unsigned is_commutative:1;      /**< Indicates whether op is commutative or not. */

	unsigned ins_permuted:1;        /**< inputs of node have been permuted (for commutative nodes) */
	unsigned is_reload:1;           /**< node performs a reload */
	unsigned is_spill:1;
	unsigned is_remat:1;

	int32_t    am_offs;       /**< offsets for AddrMode */
	ir_entity *am_ent;        /**< entity for AddrMode */

	ir_mode   *ls_mode;       /**< Load/Store mode: This is the mode of the
	                               value that is manipulated by this node. */

	ir_entity *frame_ent; /**< the frame entity attached to this node */

	ir_label_t        exc_label;       /**< the exception label iff this instruction can throw an exception */

#ifndef NDEBUG
	const char       *orig_node;      /**< holds the name of the original ir node */
	unsigned          attr_type;      /**< bitfield indicating the attribute type */
#endif
};

/**
 * The attributes for a Call node.
 */
typedef struct ia32_call_attr_t ia32_call_attr_t;
struct ia32_call_attr_t {
	ia32_attr_t  attr;    /**< generic attribute */
	unsigned     pop;     /**< number of bytes that get popped by the callee */
	ir_type     *call_tp; /**< The call type, copied from the original Call node. */
};

/**
 * The attributes for nodes with condition code.
 */
typedef struct ia32_condcode_attr_t ia32_condcode_attr_t;
struct ia32_condcode_attr_t {
	ia32_attr_t          attr;           /**< generic attribute */
	x86_condition_code_t condition_code; /**< condition code*/
};

/**
 * The attributes for Switches
 */
typedef struct ia32_switch_attr_t ia32_switch_attr_t;
struct ia32_switch_attr_t {
	ia32_attr_t            attr;        /**< generic attribute */
	const ir_switch_table *table;
	ir_entity             *jump_table;
};

/**
 * The attributes for CopyB code.
 */
typedef struct ia32_copyb_attr_t ia32_copyb_attr_t;
struct ia32_copyb_attr_t {
	ia32_attr_t  attr;      /**< generic attribute */
	unsigned     size;      /**< size of copied block */
};

/**
 * The attributes for immediates.
 */
typedef struct ia32_immediate_attr_t ia32_immediate_attr_t;
struct ia32_immediate_attr_t {
	ia32_attr_t  attr;              /**< generic attribute */
	ir_entity   *entity;            /**< An entity if any. */
	int32_t      offset;            /**< An offset if any. */
	unsigned     no_pic_adjust : 1; /**< constant can be relative to EIP */
};

/**
 * The attributes for x87 nodes.
 */
typedef struct ia32_x87_attr_t ia32_x87_attr_t;
struct ia32_x87_attr_t {
	ia32_attr_t            attr;       /**< the generic attribute */
	arch_register_t const *reg;        /**< The explicit register operand. */
	bool                   res_in_reg; /**< True if the result is in the explicit register operand, %st0 otherwise. */
	bool                   pop;        /**< Emit a pop suffix. */
};

typedef struct ia32_asm_reg_t ia32_asm_reg_t;
struct ia32_asm_reg_t {
	unsigned                   use_input  : 1; /* use input or output pos */
	unsigned                   valid      : 1;
	unsigned                   memory     : 1;
	unsigned                   dummy_fill : 13;
	unsigned                   inout_pos  : 16; /* in/out pos where the
	                                               register is assigned */
	ir_mode                   *mode;
};

/**
 * The attributes for ASM nodes.
 */
typedef struct ia32_asm_attr_t ia32_asm_attr_t;
struct ia32_asm_attr_t {
	ia32_attr_t           attr;         /**< the generic attribute */
	ident                *asm_text;
	const ia32_asm_reg_t *register_map;
};

/**
 * The attributes for the ClimbFrame node.
 */
typedef struct ia32_climbframe_attr_t ia32_climbframe_attr_t;
struct ia32_climbframe_attr_t {
	ia32_attr_t attr;      /**< generic attribute */
	unsigned    count;     /**< number of frames to climb up */
};

typedef struct ia32_return_attr_t ia32_return_attr_t;
struct ia32_return_attr_t {
	ia32_attr_t attr;
	uint16_t    pop;
	bool        emit_pop;
};

/* the following union is necessary to indicate to the compiler that we might want to cast
 * the structs (we use them to simulate OO-inheritance) */
union allow_casts_attr_t_ {
	ia32_attr_t            attr;
	ia32_call_attr_t       call_attr;
	ia32_condcode_attr_t   cc_attr;
	ia32_copyb_attr_t      cpy_attr;
	ia32_x87_attr_t        x87_attr;
	ia32_asm_attr_t        asm_attr;
	ia32_immediate_attr_t  immediate_attr;
	ia32_climbframe_attr_t climbframe_attr;
	ia32_switch_attr_t     switch_attr;
	ia32_return_attr_t     return_attr;
};

#ifndef NDEBUG
#define CAST_IA32_ATTR(type,ptr)        (assert( ((const ia32_attr_t*)(ptr))->attr_type & IA32_ATTR_ ## type ), (type*) (ptr))
#define CONST_CAST_IA32_ATTR(type,ptr)  (assert( ((const ia32_attr_t*)(ptr))->attr_type & IA32_ATTR_ ## type ), (const type*) (ptr))
#else
#define CAST_IA32_ATTR(type,ptr)        ((type*) (ptr))
#define CONST_CAST_IA32_ATTR(type,ptr)  ((const type*) (ptr))
#endif

#endif
