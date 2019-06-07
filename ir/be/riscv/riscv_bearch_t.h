/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#ifndef FIRM_BE_RISCV_RISCV_BEARCH_T_H
#define FIRM_BE_RISCV_RISCV_BEARCH_T_H

#define RISCV_MACHINE_SIZE 32
#define RISCV_PO2_STACK_ALIGNMENT 4
#define RISCV_REGISTER_SIZE 4

#include <stdbool.h>
#include <stdint.h>

#include "beirg.h"
#include "firm_types.h"

typedef struct riscv_irg_data_t {
	bool     omit_fp;        /**< No frame pointer is used. */
} riscv_irg_data_t;

static inline riscv_irg_data_t *riscv_get_irg_data(const ir_graph *irg)
{
	return (riscv_irg_data_t*)be_birg_from_irg(irg)->isa_link;
}

static inline bool is_uimm5(long const val)
{
	return 0 <= val && val < 32;
}

static inline bool is_simm12(long const val)
{
	return -2048 <= (int32_t)val && (int32_t)val < 2048;
}

typedef struct riscv_hi_lo_imm {
	int32_t hi;
	int32_t lo;
} riscv_hi_lo_imm;

riscv_hi_lo_imm calc_hi_lo(int32_t val);

void riscv_finish_graph(ir_graph *irg);

#endif
