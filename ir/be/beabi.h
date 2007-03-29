
/**
 * Backend ABI implementation.
 */

#ifndef _BEABI_H
#define _BEABI_H

#include "firm_types.h"

#include "pset.h"
#include "pmap.h"
#include "bitset.h"

#include "be.h"
#include "bearch.h"
#include "beabi_t.h"

struct _be_abi_call_flags_bits_t {
	unsigned left_to_right          : 1;  /**< Arguments are from left to right. */
	unsigned store_args_sequential  : 1;  /**< Use sequential stores for arguments. */
	unsigned try_omit_fp            : 1;  /**< Try to omit the frame pointer. */
	unsigned fp_free                : 1;  /**< The function can use any register as frame pointer. */
	unsigned call_has_imm           : 1;  /**< A call can take the callee's address as an immediate. */
	unsigned irg_is_leaf            : 1;  /**< 1, if the IRG is a leaf function. */
	unsigned frame_is_setup_on_call : 1;  /**< Set to one, if there is already enough room on the stack for call args. */
};

union _be_abi_call_flags_t {
	be_abi_call_flags_bits_t bits;
	unsigned val;
};

struct _be_abi_callbacks_t {
	/**
	 * Initialize the callback object.
	 * @param call The call object.
	 * @param aenv The architecture environment.
	 * @param irg  The graph with the method.
	 * @return     Some pointer. This pointer is passed to all other callback functions as self object.
	 */
	void *(*init)(const be_abi_call_t *call, const arch_env_t *aenv, ir_graph *irg);

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
	 * @param mem     A pointer to the mem node. Update this if you define new memory.
	 * @param reg_map A map mapping all callee_save/ignore/parameter registers to their defining nodes.
	 * @return        The register which shall be used as a stack frame base.
	 *
	 * All nodes which define registers in @p reg_map must keep @p reg_map current.
	 */
	const arch_register_t *(*prologue)(void *self, ir_node **mem, pmap *reg_map);

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
 * @param call          The call.
 * @param flags	        Some flags to be set.
 * @param cb            The call callbacks for that call.
 * @note                The ABI phase might change the flags due to analysis.
 */
void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags, const be_abi_callbacks_t *cb);

/**
 * Set register class for call address.
 * @param call      The call.
 * @param cls       The register class for call address.
 */
void be_abi_call_set_call_address_reg_class(be_abi_call_t *call, const arch_register_class_t *cls);

void be_abi_call_param_stack(be_abi_call_t *call, int pos, unsigned alignment, unsigned space_before, unsigned space_after);
void be_abi_call_param_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);
void be_abi_call_res_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);

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

be_abi_irg_t *be_abi_introduce(be_irg_t *bi);
void be_abi_fix_stack_bias(be_abi_irg_t *env);
void be_abi_free(be_abi_irg_t *abi);

/**
 * Rewire all stack modifying nodes and their users to assure SSA property.
 * @param env   The abi
 */
void be_abi_fix_stack_nodes(be_abi_irg_t *env);

/**
 * Put the registers which are forbidden specifically for this IRG in a bitset.
 */
void be_abi_put_ignore_regs(be_abi_irg_t *abi, const arch_register_class_t *cls, bitset_t *bs);

ir_node *be_abi_get_callee_save_irn(be_abi_irg_t *abi, const arch_register_t *reg);
ir_node *be_abi_get_ignore_irn(be_abi_irg_t *abi, const arch_register_t *reg);
ir_node *be_abi_get_start_barrier(be_abi_irg_t *abi);

#define be_abi_reg_map_get(map, reg)	   pmap_get((map), (void *) (reg))
#define be_abi_reg_map_set(map, reg, irn)  pmap_insert((map), (void *) (reg), (irn))

/** The number of parts of the stack layout. */
#define N_FRAME_TYPES 3

/**
 * This type describes the stack layout.
 * The stack is divided into 3 parts:
 * - arg_type:     A struct type describing the stack arguments and it's order.
 * - between_type: A struct type describing the stack layout between arguments
 *                 and frame type. In architectures that put the return address
 *                 automatically on the stack, the return address is put here.
 * - frame_type:   A class type describing the frame layout
 */
struct _be_stack_layout_t {
	ir_type *arg_type;                 /**< A type describing the stack argument layout. */
	ir_type *between_type;             /**< A type describing the "between" layout. */
	ir_type *frame_type;               /**< The frame type. */

	ir_type *order[N_FRAME_TYPES];     /**< arg, between and frame types ordered. */

	int initial_offset;
	int stack_dir;                     /**< -1 for decreasing, 1 for increasing. */
	ir_entity **param_map;             /**< An array mapping type parameters to arg_type entries */
};

/**
 * Returns the stack layout from a abi environment.
 */
const be_stack_layout_t *be_abi_get_stack_layout(const be_abi_irg_t *abi);

/**
 * Returns non-zero if the ABI has omitted the frame pointer in
 * the current graph.
 */
int be_abi_omit_fp(const be_abi_irg_t *abi);

#endif /* _BEABI_H */
