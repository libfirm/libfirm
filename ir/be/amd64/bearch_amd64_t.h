/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for TEMPALTE backend -- private header
 */
#ifndef FIRM_BE_amd64_BEARCH_amd64_T_H
#define FIRM_BE_amd64_BEARCH_amd64_T_H

#include "debug.h"
#include "amd64_nodes_attr.h"
#include "be.h"
#include "beemitter.h"
#include "set.h"

typedef struct amd64_isa_t            amd64_isa_t;

struct amd64_isa_t {
	arch_env_t  base;      /**< must be derived from arch_isa */
};

ir_node *amd64_new_NoReg_gp(ir_graph *irg);

#endif
