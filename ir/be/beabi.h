
/**
 * Backend ABI implementation.
 */

#ifndef _BEABI_H
#define _BEABI_H

#include "firm_types.h"

#include "be.h"
#include "bearch.h"
#include "beabi_t.h"

typedef struct {
	unsigned left_to_right         :1;  /**< Arguments are from left to right. */
	unsigned store_args_sequential :1;  /**< Use sequential stores for arguments. */
	unsigned try_omit_fp           :1;  /**< Try to omit the frame pointer. */
	unsigned fp_free               :1;  /**< The function can use any register as frame pointer. */
	unsigned call_has_imm          :1;  /**< A call can take the callee's address as an immediate. */
} be_abi_call_flags_bits_t;

typedef union {
	be_abi_call_flags_bits_t bits;
	unsigned val;
} be_abi_call_flags_t;

void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags, ir_type *add_frame);
void be_abi_call_param_stack(be_abi_call_t *call, int pos);
void be_abi_call_param_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);
void be_abi_call_res_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);

be_abi_irg_t *be_abi_introduce(be_irg_t *bi);
void be_abi_fix_stack_bias(be_abi_irg_t *env);
void be_abi_fix_stack_nodes(be_abi_irg_t *env);
void be_abi_free(be_abi_irg_t *abi);

ir_node *be_abi_get_callee_save_irn(be_abi_irg_t *abi, const arch_register_t *reg);

#endif
