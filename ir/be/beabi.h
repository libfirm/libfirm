
/**
 * Backend ABI implementation.
 */

#ifndef _BEABI_H
#define _BEABI_H

#include "pset.h"
#include "firm_types.h"

#include "be.h"
#include "bearch.h"
#include "beabi_t.h"

struct _be_abi_call_flags_bits_t {
	unsigned left_to_right         : 1;  /**< Arguments are from left to right. */
	unsigned store_args_sequential : 1;  /**< Use sequential stores for arguments. */
	unsigned try_omit_fp           : 1;  /**< Try to omit the frame pointer. */
	unsigned fp_free               : 1;  /**< The function can use any register as frame pointer. */
	unsigned call_has_imm          : 1;  /**< A call can take the callee's address as an immediate. */
	unsigned irg_is_leaf           : 1;  /**< 1
	, if the IRG is a leaf function. */
};

union _be_abi_call_flags_t {
	be_abi_call_flags_bits_t bits;
	unsigned val;
};

struct _be_abi_callbacks_t {
	/**
	 * Initialize the callback object.
	 * @param call The call object.
	 * @param isa  The current ISA.
	 * @param irg  The graph with the method.
	 * @return     Some pointer. This pointer is passed to all other callback functions as self object.
	 */
	void *(*init)(const be_abi_call_t *call, const arch_isa_t *isa, ir_graph *irg);

	/**
	 * Destroy the callback object.
	 * @param self The callback object.
	 */
	void (*done)(void *self);

	/**
	 * Get the between type for that call.
	 * @param self The callback object.
	 * @return The between type of for that call.
	 */
	ir_type *(*get_between_type)(void *self);

	/**
	 * Put all registers which are saved by the prologue/epilogue in a set.
	 * @param self The callback object.
	 * @param regs A set.
	 */
	void (*regs_saved_by_me)(void *self, pset *regs);

	/**
	 * Generate the prologue.
	 * @param self    The callback object.
	 * @param reg_map A mapping mapping all callee_save/ignore/parameter registers to their defining nodes.
	 * @return        The register which shall be used as a stack frame base.
	 *
     * All nodes which define registers in @p reg_map must keep @p reg_map current.
	 */
	const arch_register_t *(*prologue)(void *self, pmap *reg_map);

	/**
	 * Generate the epilogue.
	 * @param self    The callback object.
	 * @param mem     Memory one can attach to.
	 * @param reg_map A mapping mapping all callee_save/ignore/return registers to their defining nodes.
	 *
     * All nodes which define registers in @p reg_map must keep @p reg_map current.
	 * Also, the @p mem variable must be updated, if memory producing nodes are inserted.
	 */
	void (*epilogue)(void *self, ir_node *bl, ir_node **mem, pmap *reg_map);
};

/**
 * Set the flags for a call.
 * @param call  The call.
 * @param flags Some flags to be set.
 * @param cb    The call callbacks for that call.
 * @note        The ABI phase might change the flags due to analysis.
 */
void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags, const be_abi_callbacks_t *cb);


void be_abi_call_param_stack(be_abi_call_t *call, int pos);
void be_abi_call_param_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);
void be_abi_call_res_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);

/**
 * Get the flags of a ABI call object.
 * Note that the flags must not be the same as set by be_abi_call_set_flags(). Alayses may have
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

be_abi_irg_t *be_abi_introduce(be_irg_t *bi);
void be_abi_fix_stack_bias(be_abi_irg_t *env);
void be_abi_fix_stack_nodes(be_abi_irg_t *env);
void be_abi_free(be_abi_irg_t *abi);

ir_node *be_abi_get_callee_save_irn(be_abi_irg_t *abi, const arch_register_t *reg);

#endif
