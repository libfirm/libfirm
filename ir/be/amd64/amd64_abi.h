/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements function parameter lowering for the System V AMD64 ABI
 * @author      Andreas Fried
 */
#ifndef FIRM_BE_AMD64_AMD64_ABI_H
#define FIRM_BE_AMD64_AMD64_ABI_H

#include "firm_types.h"
#include "lower_calls.h"

typedef struct amd64_abi_state {
	unsigned integer_params;
	unsigned sse_params;
} amd64_abi_state;

aggregate_spec_t amd64_lower_parameter(void *env, ir_type const *type);

aggregate_spec_t amd64_lower_result(void *env, ir_type const *type);

void amd64_reset_abi_state(void *param_env, void *result_env);

#endif
