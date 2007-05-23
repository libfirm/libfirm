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
 * @brief       Type definitions for ia32 node attributes.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_NODES_ATTR_H
#define FIRM_BE_IA32_IA32_NODES_ATTR_H

#include "firm_config.h"

#include "firm_types.h"
#include "../bearch_t.h"
#include "../bemachine.h"

typedef enum { flavour_Div = 1, flavour_Mod, flavour_DivMod } ia32_op_flavour_t;
typedef enum { pn_EAX, pn_EDX } pn_ia32_Register;

typedef enum {
	ia32_Normal,
	ia32_AddrModeD,
	ia32_AddrModeS
} ia32_op_type_t;

typedef enum {
	ia32_ImmNone     = 0,
	ia32_ImmConst    = 1,
	ia32_ImmSymConst = 2
} ia32_immop_type_t;

typedef	enum {
	ia32_am_None   = 0,   /**<< no addrmode support */
	ia32_am_Dest   = 1,   /**<< addrmode for destination only */
	ia32_am_Source = 2,   /**<< addrmode for source only */
	ia32_am_Full   = 3    /**<< full addmode support */
} ia32_am_type_t;

/**
 * Different Address Mode properties:
 * O - Offset is set
 * B - Base is set
 * I - Index is set
 * S - Scale is set
 */
enum {
	ia32_O = (1 << 0),  /**< O - Offset is set */
	ia32_B = (1 << 1),  /**< B - Base is set */
	ia32_I = (1 << 2),  /**< I - Index is set */
	ia32_S = (1 << 3)   /**< S - Scale is set */
};

/** Possible Address mode types */
typedef enum {
	ia32_am_N    = 0,
	ia32_am_O    = ia32_O,
	ia32_am_B    = ia32_B,
	ia32_am_I    = ia32_I,
	ia32_am_IS   = ia32_I | ia32_S,
	ia32_am_BI   = ia32_B | ia32_I,
	ia32_am_OB   = ia32_O | ia32_B,
	ia32_am_OIS  = ia32_O | ia32_I | ia32_S,
	ia32_am_OBIS = ia32_O | ia32_B | ia32_I | ia32_S
} ia32_am_flavour_t;

enum {
	ia32_pn_Cmp_Unsigned = 0x100 /**< set this flag in a pnc to indicate an unsigned compare operation */
};

typedef struct ia32_attr_t ia32_attr_t;
struct ia32_attr_t {
	struct {
		unsigned tp:3;              /**< ia32 node type */
		unsigned imm_tp:2;          /**< ia32 immop type */
		unsigned am_support:2;      /**< indicates addrmode type supported by this node */
		unsigned am_flavour:4;      /**< the concrete addrmode characteristics */
		unsigned am_scale:2;        /**< addrmode scale for index register */

		unsigned offs_sign:1;       /**< sign bit of the first offset */
		unsigned am_sc_sign:1;      /**< sign bit of the address mode symconst */

		unsigned use_frame:1;       /**< indicates whether the operation uses the frame pointer or not */

		ia32_op_flavour_t op_flav:2;/**< flavour of an op (flavour_Div/Mod/DivMod) */

		unsigned flags:4;           /**< indicating if spillable, rematerializeable, stack modifying and/or ignore */

		unsigned is_commutative:1;  /**< indicates whether op is commutative or not */

		unsigned emit_cl:1;         /**< indicates whether we must emit cl instead of ecx (needed for shifts) */

		unsigned got_lea:1;         /**< indicates whether or not this node already consumed a LEA */

		unsigned need_stackent:1;   /**< set to 1 if node need space on stack */

		unsigned n_res:6;           /**< number of results produced by this node */
	} data;

	int       *out_flags;     /**< flags for each produced value */

	int        am_offs;       /**< offsets for AddrMode */
	ir_entity *am_sc;         /**< SymConst for AddrMode */

	union {
		tarval    *tv;        /**< tarval for immediate operations */
		ir_entity *sc;        /**< the symconst ident */
	} cnst_val;

	ir_mode *ls_mode;     /**< Load/Store mode: This is the mode of the value
	                           that is manipulated by this node. */

	ir_entity *frame_ent; /**< the frame entity attached to this node */

	long pn_code;       /**< projnum "types" (e.g. indicate compare operators and argument numbers for switches) */

	unsigned latency;   /**< the latency of the instruction in clock cycles */

#ifndef NDEBUG
	const char *orig_node;      /**< holds the name of the original ir node for debugging purposes */
#endif /* NDEBUG */

	const be_execution_unit_t ***exec_units; /**< list of units this operation can be executed on */

	const arch_register_req_t **in_req;  /**< register requirements for arguments */
	const arch_register_req_t **out_req; /**< register requirements for results */

	const arch_register_t *x87[3];       /**< register slots for x87 register */

	/* must be last, dynamic */
	const arch_register_t *slots[1];     /**< register slots for assigned registers */
};

#endif /* FIRM_BE_IA32_IA32_NODES_ATTR_H */
