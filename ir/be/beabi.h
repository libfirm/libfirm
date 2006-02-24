
/**
 * Backend ABI implementation.
 */

#ifndef _BEABI_H
#define _BEABI_H

#include "be.h"
#include "bearch.h"
#include "beabi_t.h"

typedef enum {
	BE_ABI_NONE = 0,
	BE_ABI_LEFT_TO_RIGHT = 1,
	BE_ABI_USE_PUSH = 2
} be_abi_call_flags_t;

void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags);
void be_abi_call_param_stack(be_abi_call_t *call, int pos);
void be_abi_call_param_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);
void be_abi_call_res_reg(be_abi_call_t *call, int pos, const arch_register_t *reg);

be_abi_irg_t *be_abi_introduce(be_irg_t *bi);
void be_abi_fix_stack(be_abi_irg_t *abi);
void be_abi_free(be_abi_irg_t *abi);

#endif
