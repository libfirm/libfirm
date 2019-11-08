/**
 * @file
 * @brief       Implements function parameter lowering for the RISC-V ILP32 ABI
 * @author      Johannes Bucher
 */
#ifndef LIBFIRM_RISCV_ABI_H
#define LIBFIRM_RISCV_ABI_H

#include "firm_types.h"
#include "lower_calls.h"

aggregate_spec_t riscv_lower_parameter(void *env, ir_type const *type);

aggregate_spec_t riscv_lower_result(void *env, ir_type const *type);

#endif //LIBFIRM_RISCV_ABI_H
