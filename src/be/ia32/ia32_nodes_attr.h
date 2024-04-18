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

#include "benode.h"
#include "firm_types.h"
#include "irnode_t.h"
#include "x86_asm.h"
#include "x86_node.h"
#include "x86_x87.h"
#include "x86_address_mode.h"

typedef enum  ia32_op_type_t {
	ia32_Normal,
	ia32_AddrModeD,
	ia32_AddrModeS
} ia32_op_type_t;

typedef enum ia32_am_type_t {
	ia32_am_none,
	ia32_am_unary,
	ia32_am_binary
} ia32_am_type_t;

typedef enum match_flags_t {
	match_none              = 0,
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
	match_try_am            = 1 << 9, /**< only try to produce AM node, don't
	                                       do anything if AM isn't possible */
	match_two_users         = 1 << 10,/**< the instruction uses a load two times ... */
} match_flags_t;
ENUM_BITSET(match_flags_t)

typedef struct ia32_op_attr_t ia32_op_attr_t;
struct ia32_op_attr_t {
	unsigned latency;
};

#ifndef NDEBUG
typedef enum ia32_attr_type_t {
	IA32_ATTR_INVALID               = 0,
	IA32_ATTR_ia32_attr_t           = 1 << 0,
	IA32_ATTR_ia32_x87_attr_t       = 1 << 1,
	IA32_ATTR_ia32_immediate_attr_t = 1 << 2,
	IA32_ATTR_ia32_condcode_attr_t  = 1 << 3,
	IA32_ATTR_ia32_copyb_attr_t     = 1 << 4,
	IA32_ATTR_ia32_call_attr_t      = 1 << 5,
	IA32_ATTR_ia32_switch_attr_t    = 1 << 6,
	IA32_ATTR_ia32_return_attr_t    = 1 << 7,
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

	ENUMBF(x86_insn_size_t) size:3;
	ENUMBF(ia32_op_type_t) tp:2;          /**< Indicator of used address mode. */
	unsigned am_arity:2;            /**< Indicates the address mode type supported by this node. */

	ENUMBF(ia32_frame_use_t) frame_use:2; /**< Whether an entity on the frame is used and its size. */
	unsigned has_except_label:1;        /**< Set if this node needs a label because of possible exception. */

	unsigned is_commutative:1;      /**< Indicates whether op is commutative or not. */

	unsigned ins_permuted:1;        /**< inputs of node have been permuted (for commutative nodes) */
	unsigned is_reload:1;           /**< node performs a reload */
	unsigned is_spill:1;
	unsigned is_remat:1;
	unsigned sign_extend:1;
	unsigned use_8bit_high:1;

	ir_label_t        exc_label;       /**< the exception label iff this instruction can throw an exception */

	x86_addr_t        addr; /**< address mode specification */

#ifndef NDEBUG
	ir_entity const  *old_frame_ent;  /**< frame entity referenced */
	unsigned          attr_type;      /**< bitfield indicating the attribute type */
#endif
};

/**
 * The attributes for a Call node.
 */
typedef struct ia32_call_attr_t ia32_call_attr_t;
struct ia32_call_attr_t {
	ia32_attr_t attr;    /**< generic attribute */
	uint8_t     pop;     /**< number of bytes that get popped by the callee */
	uint8_t     n_reg_results;
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
	ia32_attr_t      attr;  /**< generic attribute */
	be_switch_attr_t swtch;
};

/**
 * The attributes for CopyB code.
 */
typedef struct ia32_copyb_attr_t ia32_copyb_attr_t;
struct ia32_copyb_attr_t {
	ia32_attr_t attr;      /**< generic attribute */
	unsigned    size;      /**< size of copied block */
};

/**
 * The attributes for immediates.
 */
typedef struct ia32_immediate_attr_t ia32_immediate_attr_t;
struct ia32_immediate_attr_t {
	ia32_attr_t attr;              /**< generic attribute */
	x86_imm32_t imm;
};

/**
 * The attributes for x87 nodes.
 */
typedef struct ia32_x87_attr_t ia32_x87_attr_t;
struct ia32_x87_attr_t {
	ia32_attr_t attr;       /**< the generic attribute */
	x87_attr_t  x87;
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
	ia32_attr_t           attr;
	ia32_call_attr_t      call_attr;
	ia32_condcode_attr_t  cc_attr;
	ia32_copyb_attr_t     cpy_attr;
	ia32_x87_attr_t       x87_attr;
	ia32_immediate_attr_t immediate_attr;
	ia32_switch_attr_t    switch_attr;
	ia32_return_attr_t    return_attr;
};

#ifndef NDEBUG
#define CAST_IA32_ATTR(type,ptr)        (assert( ((const ia32_attr_t*)(ptr))->attr_type & IA32_ATTR_ ## type ), (type*) (ptr))
#define CONST_CAST_IA32_ATTR(type,ptr)  (assert( ((const ia32_attr_t*)(ptr))->attr_type & IA32_ATTR_ ## type ), (const type*) (ptr))
#else
#define CAST_IA32_ATTR(type,ptr)        ((type*) (ptr))
#define CONST_CAST_IA32_ATTR(type,ptr)  ((const type*) (ptr))
#endif

#endif
