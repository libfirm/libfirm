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
 * @brief       Backend ABI implementation.
 * @author      Sebastian Hack
 */
#ifndef FIRM_BE_BEABI_H
#define FIRM_BE_BEABI_H

#include "firm_types.h"

#include "pset.h"
#include "pmap.h"
#include "bitset.h"

#include "be.h"
#include "beirg.h"
#include "bearch.h"
#include "beabi.h"
#include "beabihelper.h"

struct be_abi_call_flags_t {
	bool try_omit_fp   : 1; /**< Try to omit the frame pointer. */
	bool call_has_imm  : 1; /**< A call can take the callee's address as an
	                             immediate. */
};

struct be_abi_callbacks_t {
	/**
	 * Get the between type for that call.
	 * @param self The callback object.
	 * @return The between type of for that call.
	 */
	ir_type *(*get_between_type)(ir_graph *irg);
};

/**
 * Set the flags for a call.
 * @param call          The call.
 * @param flags         Some flags to be set.
 * @param cb            The call callbacks for that call.
 * @note                The ABI phase might change the flags due to analysis.
 */
void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags, const be_abi_callbacks_t *cb);

/**
 * Sets the number of bytes the stackframe is shrinked by the callee on return
 */
void be_abi_call_set_pop(be_abi_call_t *call, int pop);

/**
 * Set register class for call address.
 * @param call      The call.
 * @param cls       The register class for call address.
 */
void be_abi_call_set_call_address_reg_class(be_abi_call_t *call, const arch_register_class_t *cls);

/**
 * The ABI can change when we call a function vs. when we have
 * been called.
 */
typedef enum {
	ABI_CONTEXT_CALLEE = 1 << 0,
	ABI_CONTEXT_CALLER = 1 << 1,
	ABI_CONTEXT_BOTH   = ABI_CONTEXT_CALLEE | ABI_CONTEXT_CALLER
} be_abi_context_t;

/**
 * Record the that ABI transmits call argument pos on the stack. Modifies the abi object.
 *
 * @param call          the abi call object
 * @param pos           the parameter position
 * @param load_mode     load the parameter with this mode (if the parameter mode is different from this mode a Conv is inserted)
 * @param alignment     stack alignment for the parameter on the current architecture
 * @param space_before  size of allocated additional space before the parameter
 * @param space_after   size of allocated additional space after the parameter
 */
void be_abi_call_param_stack(be_abi_call_t *call, int pos, ir_mode *load_mode,
                             unsigned alignment, unsigned space_before,
                             unsigned space_after, be_abi_context_t context);

/**
 * Record the that ABI transmits call argument pos in the given register.
 *
 * @param call          the abi call object
 * @param pos           the parameter position
 * @param reg           the register used
 */
void be_abi_call_param_reg(be_abi_call_t *call, int pos,
                           const arch_register_t *reg,
                           be_abi_context_t context);

/**
 * Record the that ABI transmits return value pos in the given register.
 *
 * @param call          the abi call object
 * @param pos           the return value position
 * @param reg           the register used
 */
void be_abi_call_res_reg(be_abi_call_t *call, int pos,
                         const arch_register_t *reg,
                         be_abi_context_t context);

/**
 * Get the flags of a ABI call object.
 * Note that the flags must not be the same as set by be_abi_call_set_flags(). Analysis may have
 * altered several flags, so getting them from the call object is always a good idea.
 * @param call The call object.
 * @return The flags.
 */
be_abi_call_flags_t be_abi_call_get_flags(const be_abi_call_t *call);

/**
 * Get the method type of an ABI call object.
 * @param call The call object.
 * @return The method type for that call object.
 */
ir_type *be_abi_call_get_method_type(const be_abi_call_t *call);

void be_abi_introduce(ir_graph *irg);

#endif
