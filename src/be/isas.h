/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#ifndef FIRM_BE_ISAS_H
#define FIRM_BE_ISAS_H

#include "firm_types.h"
#include "bearch.h"

void be_init_arch_TEMPLATE(void);
extern arch_isa_if_t const TEMPLATE_isa_if;

void be_init_arch_amd64(void);
extern arch_isa_if_t const amd64_isa_if;

void be_init_arch_arm(void);
extern arch_isa_if_t const arm_isa_if;

void be_init_arch_ia32(void);
extern arch_isa_if_t const ia32_isa_if;

void be_init_arch_mips(void);
extern arch_isa_if_t const mips_isa_if;

void be_init_arch_riscv32(void);
extern arch_isa_if_t const riscv32_isa_if;

void be_init_arch_sparc(void);
extern arch_isa_if_t const sparc_isa_if;

#endif
