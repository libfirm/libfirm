/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for SPARC backend -- private header
 * @author  Hannes Rapp, Matthias Braun
 */
#ifndef FIRM_BE_SPARC_SPARC_BEARCH_T_H
#define FIRM_BE_SPARC_SPARC_BEARCH_T_H

#include <stdbool.h>
#include <stdint.h>

#include "beirg.h"
#include "firm_types.h"
#include "pmap.h"

typedef struct calling_convention_t calling_convention_t;

typedef struct sparc_irg_data_t {
	bool omit_fp;
} sparc_irg_data_t;

typedef struct sparc_codegen_config_t {
	bool     use_fpu;
	bool     use_cas;
	unsigned cas_asi : 8;
} sparc_codegen_config_t;
extern sparc_codegen_config_t sparc_cg_config;

extern pmap *sparc_constants;

extern ir_mode *sparc_mode_Q;

/**
 * SPARC ABI requires some space which is always available at the top of
 * the stack. It contains:
 * 16*4 bytes space for spilling the register window
 * 1*4 byte   holding a pointer to space for aggregate returns (the space is
 *            always reserved, regardless whether we have an aggregate return
 *            or not)
 * 6*4 bytes  Space for spilling parameters 0-5. For the cases when someone
 *            takes the address of a parameter. I guess this is also there so
 *            the implementation of va_args gets easier -> We can simply store
 *            param 0-5 in this spaces and then handle va_next by simply
 *            incrementing the stack pointer
 */
#define SPARC_IMMEDIATE_MIN           -4096
#define SPARC_IMMEDIATE_MAX            4095
#define SPARC_MIN_STACKSIZE              92
#define SPARC_AGGREGATE_RETURN_OFFSET    64
#define SPARC_PARAMS_SPILL_OFFSET        68
#define SPARC_SAVE_AREA_SIZE             64
#define SPARC_N_PARAM_REGS                6
#define SPARC_PO2_STACK_ALIGNMENT         3
#define SPARC_STACK_ALIGNMENT             (1<<SPARC_PO2_STACK_ALIGNMENT)
#define SPARC_REGISTER_SIZE               4

static inline bool sparc_is_value_imm_encodeable(int32_t value)
{
	return SPARC_IMMEDIATE_MIN <= value && value <= SPARC_IMMEDIATE_MAX;
}

static inline sparc_irg_data_t *sparc_get_irg_data(ir_graph const *const irg)
{
	return (sparc_irg_data_t*)be_birg_from_irg(irg)->isa_link;
}

void sparc_finish_graph(ir_graph *irg);

void sparc_lower_64bit(void);

calling_convention_t *sparc_prepare_calling_convention(ir_graph *irg);

void sparc_fix_stack_bias(ir_graph *irg);

ir_entity *sparc_get_frame_entity(const ir_node *node);

#endif
