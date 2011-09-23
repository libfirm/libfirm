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
 * @brief       Type definitions for ia32 node attributes.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_NODES_ATTR_H
#define FIRM_BE_IA32_IA32_NODES_ATTR_H

#include "firm_types.h"
#include "bearch.h"
#include "bemachine.h"
#include "irnode_t.h"

/** ia32 condition codes (the numbers correspond to the real encoding order) */
typedef enum ia32_condition_code_t {
	ia32_cc_negated       = 0x01, /**< negates condition */

	ia32_cc_overflow      = 0x00,                                /**< OF=1 */
	ia32_cc_below         = 0x02,                                /**< CF=1 */
	ia32_cc_equal         = 0x04,                                /**< ZF=1 */
	ia32_cc_below_equal   = 0x06,                                /**< ZF=1 or CF=1 */
	ia32_cc_sign          = 0x08,                                /**< SF=1 */
	ia32_cc_parity        = 0x0A,                                /**< PF=1 */
	ia32_cc_less          = 0x0C,                                /**< SF!=OF */
	ia32_cc_less_equal    = 0x0E,                                /**< ZF=1 or SF!=OF */
	ia32_cc_not_overflow  = ia32_cc_negated|ia32_cc_overflow,    /**< OF=0 */
	ia32_cc_above_equal   = ia32_cc_negated|ia32_cc_below,       /**< CF=0 */
	ia32_cc_not_equal     = ia32_cc_negated|ia32_cc_equal,       /**< ZF=0 */
	ia32_cc_above         = ia32_cc_negated|ia32_cc_below_equal, /**< ZF=0 and CF=0 */
	ia32_cc_not_sign      = ia32_cc_negated|ia32_cc_sign,        /**< SF=0 */
	ia32_cc_not_parity    = ia32_cc_negated|ia32_cc_parity,      /**< PF=0 */
	ia32_cc_greater_equal = ia32_cc_negated|ia32_cc_less,        /**< SF=OF */
	ia32_cc_greater       = ia32_cc_negated|ia32_cc_less_equal,  /**< ZF=0 and SF=OF */

	/* the following codes are (unfortunately) NOT real hardware codes but
	 * simplify our backend as you need these combinations for some
	 * floatingpoint compares (the emitter will split them into multiple
	 * instructions) */
	ia32_cc_float_parity_cases = 0x20,
	/* we need even more cases as inversing the cc is different for float
	 * comparisons (though for the following we need no special
	 * parity+x combinations) */
	ia32_cc_additional_float_cases = 0x10,

	/* make sure that the lower 4 bit correspond to the real encoding
	 * (of the comparison not involving the parity special) */
	ia32_cc_float_equal        = 0x34,                                /**< PF=0 and ZF=1 */
	ia32_cc_float_below        = 0x32,                                /**< PF=0 and CF=1 */
	ia32_cc_float_below_equal  = 0x36,                                /**< PF=0 and (ZF=1 or CF=1) */
	ia32_cc_float_not_equal    = ia32_cc_negated|ia32_cc_float_equal, /**< PF=1 or ZF=0 */
	ia32_cc_float_unordered_above_equal
		= ia32_cc_negated|ia32_cc_float_below,                        /**< PF=1 or CF=0 */
	ia32_cc_float_unordered_above
		= ia32_cc_negated|ia32_cc_float_below_equal,                  /**< PF=1 or (ZF=0 and CF=0) */

	ia32_cc_float_unordered_below_equal = 0x16,                       /**< ZF=1 or CF=1 */
	ia32_cc_float_unordered_below       = 0x12,                       /**< CF=1 */
	ia32_cc_float_above        =
		ia32_cc_negated|ia32_cc_float_unordered_below_equal,          /**< ZF=0 and CF=0 */
	ia32_cc_float_above_equal
		= ia32_cc_negated|ia32_cc_float_unordered_below,              /**< CF=0 */
} ia32_condition_code_t;
ENUM_BITSET(ia32_condition_code_t)

static inline ia32_condition_code_t ia32_negate_condition_code(
		ia32_condition_code_t code)
{
	return code ^ ia32_cc_negated;
}

static inline ia32_condition_code_t ia32_invert_condition_code(
		ia32_condition_code_t code)
{
	/* doesn't appear to have any systematic, so use a table */
	switch (code) {
	case ia32_cc_below:              return ia32_cc_above;
	case ia32_cc_below_equal:        return ia32_cc_above_equal;
	case ia32_cc_above:              return ia32_cc_below;
	case ia32_cc_above_equal:        return ia32_cc_below_equal;
	case ia32_cc_less:               return ia32_cc_greater;
	case ia32_cc_less_equal:         return ia32_cc_greater_equal;
	case ia32_cc_greater:            return ia32_cc_less;
	case ia32_cc_greater_equal:      return ia32_cc_less_equal;
	case ia32_cc_float_below:        return ia32_cc_float_above;
	case ia32_cc_float_below_equal:  return ia32_cc_float_above_equal;
	case ia32_cc_float_above:        return ia32_cc_float_below;
	case ia32_cc_float_above_equal:  return ia32_cc_float_below_equal;
	case ia32_cc_float_unordered_below:       return ia32_cc_float_unordered_above;
	case ia32_cc_float_unordered_below_equal: return ia32_cc_float_unordered_above_equal;
	case ia32_cc_float_unordered_above:       return ia32_cc_float_unordered_below;
	case ia32_cc_float_unordered_above_equal: return ia32_cc_float_unordered_below_equal;
	default:                         return code;
	}
}

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
	match_mode_neutral      = 1 << 6, /**< 16 and 8 bit modes can be emulated
	                                       by 32 bit operations */
	match_try_am            = 1 << 7, /**< only try to produce AM node, don't
	                                       do anything if AM isn't possible */
	match_two_users         = 1 << 8, /**< the instruction uses a load two times ... */
	match_upconv_32         = 1 << 9  /**< 8/16 bit insn are processed by doing
	                                       an upconv to 32bit */
} match_flags_t;
ENUM_BITSET(match_flags_t)

typedef struct ia32_op_attr_t ia32_op_attr_t;
struct ia32_op_attr_t {
	match_flags_t  flags;
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
} ia32_attr_type_t;
#endif

/**
 * The generic ia32 attributes. Every node has them.
 */
typedef struct ia32_attr_t ia32_attr_t;
struct ia32_attr_t {
	except_attr  exc;               /**< the exception attribute. MUST be the first one. */
	struct ia32_attr_data_bitfield {
		unsigned tp:3;                  /**< ia32 node type. */
		unsigned am_arity:2;            /**< Indicates the address mode type supported by this node. */
		unsigned am_scale:2;            /**< The address mode scale for index register. */
		unsigned am_sc_sign:1;          /**< The sign bit of the address mode symconst. */

		unsigned am_sc_no_pic_adjust : 1;/**< AM symconst can be relative to EIP */
		unsigned am_tls_segment:1;       /**< addresses are relative to TLS */
		unsigned use_frame:1;           /**< Indicates whether the operation uses the frame pointer or not. */
		unsigned has_except_label:1;        /**< Set if this node needs a label because of possible exception. */

		unsigned is_commutative:1;      /**< Indicates whether op is commutative or not. */

		unsigned need_stackent:1;       /**< Set to 1 if node need space on stack. */
		unsigned need_64bit_stackent:1; /**< needs a 64bit stack entity (see double->unsigned int conv) */
		unsigned need_32bit_stackent:1; /**< needs a 32bit stack entity */
		unsigned ins_permuted : 1;      /**< inputs of node have been permuted
		                                     (for commutative nodes) */
		unsigned is_reload : 1;         /**< node performs a reload */
		unsigned is_spill : 1;
		unsigned is_remat : 1;
	} data;

	int        am_offs;       /**< offsets for AddrMode */
	ir_entity *am_sc;         /**< SymConst for AddrMode */

	ir_mode   *ls_mode;       /**< Load/Store mode: This is the mode of the
	                               value that is manipulated by this node. */

	ir_entity *frame_ent; /**< the frame entity attached to this node */

	const be_execution_unit_t ***exec_units; /**< list of units this operation can be executed on */

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
	ia32_attr_t           attr;           /**< generic attribute */
	ia32_condition_code_t condition_code; /**< condition code*/
};

/**
 * The attributes for Switches
 */
typedef struct ia32_switch_attr_t ia32_switch_attr_t;
struct ia32_switch_attr_t {
	ia32_attr_t  attr;        /**< generic attribute */
	long         default_pn;
	ir_entity   *jump_table;
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
	ir_entity   *symconst;          /**< An entity if any. */
	long         offset;            /**< An offset if any. */
	unsigned     sc_sign : 1;       /**< The sign bit of the symconst. */
	unsigned     no_pic_adjust : 1; /**< constant can be relative to EIP */
};

/**
 * The attributes for x87 nodes.
 */
typedef struct ia32_x87_attr_t ia32_x87_attr_t;
struct ia32_x87_attr_t {
	ia32_attr_t            attr;      /**< the generic attribute */
	const arch_register_t *x87[3];    /**< register slots for x87 register */
};

typedef struct ia32_asm_reg_t ia32_asm_reg_t;
struct ia32_asm_reg_t {
	unsigned                   use_input  : 1; /* use input or output pos */
	unsigned                   valid      : 1;
	unsigned                   memory     : 1;
	unsigned                   dummy_fill : 13;
	unsigned                   inout_pos  : 16; /* in/out pos where the
	                                               register is assigned */
	const ir_mode             *mode;
};

/**
 * The attributes for ASM nodes.
 */
typedef struct ia32_asm_attr_t ia32_asm_attr_t;
struct ia32_asm_attr_t {
	ia32_x87_attr_t       x87_attr;
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
};

#ifndef NDEBUG
#define CAST_IA32_ATTR(type,ptr)        (assert( ((const ia32_attr_t*)(ptr))->attr_type & IA32_ATTR_ ## type ), (type*) (ptr))
#define CONST_CAST_IA32_ATTR(type,ptr)  (assert( ((const ia32_attr_t*)(ptr))->attr_type & IA32_ATTR_ ## type ), (const type*) (ptr))
#else
#define CAST_IA32_ATTR(type,ptr)        ((type*) (ptr))
#define CONST_CAST_IA32_ATTR(type,ptr)  ((const type*) (ptr))
#endif

#endif
