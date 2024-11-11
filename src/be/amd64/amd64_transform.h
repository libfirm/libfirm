/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declaration for the transform function (code selection)
 */
#ifndef FIRM_BE_AMD64_AMD64_TRANSFORM_H
#define FIRM_BE_AMD64_AMD64_TRANSFORM_H

#include "firm_types.h"
#include "x86_address_mode.h"
#include "x86_asm.h"

extern const x86_asm_constraint_list_t amd64_asm_constraints;

extern arch_register_req_t const         amd64_requirement_gp_same_0;
extern arch_register_req_t const        *amd64_xmm_reqs[];
extern arch_register_req_t const **const gp_am_reqs[];
extern arch_register_req_t const        *reg_reqs[];
extern arch_register_req_t const        *rsp_reg_mem_reqs[];
extern arch_register_req_t const        *xmm_reg_mem_reqs[];
extern arch_register_req_t const        *amd64_reg_reg_reqs[];
extern arch_register_req_t const        *amd64_xmm_xmm_reqs[];


void amd64_init_transform(void);

ir_node *amd64_new_spill(ir_node *value, ir_node *after);

ir_node *amd64_new_reload(ir_node *value, ir_node *spill, ir_node *before);

void amd64_transform_graph(ir_graph *irg);

ir_node *amd64_new_IncSP(ir_node *block, ir_node *old_sp, int offset,
                         bool no_align);

/**
 * Creates an entity for a constant floating point value.
 */
ir_entity *create_float_const_entity(ir_tarval *const tv);

void init_lconst_addr(x86_addr_t *addr, ir_entity *entity);

/** Creates a tarval with the given mode and only
  * the most-significant (first) bit set.
  *
  * @param mode The mode of the resulting tarval, which lso decides
  * between 32 and 64 bit long tarval.
  */
ir_tarval *create_sign_tv(ir_mode *mode);

#endif
