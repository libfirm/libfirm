/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

#ifndef FIRM_BE_AMD64_ARCHITECTURE_H
#define FIRM_BE_AMD64_ARCHITECTURE_H

#include <stdbool.h>

#include "firm_types.h"
#include "irarch.h"

typedef struct {
	/** gcc compatibility */
	bool use_red_zone:1;
	/** use FMA3 instructions */
	bool use_scalar_fma3:1;
} amd64_code_gen_config_t;

extern amd64_code_gen_config_t amd64_cg_config;

/** Initialize the amd64 architecture module. */
void amd64_init_architecture(void);

/** Setup the amd64_cg_config structure by inspecting current user settings. */
void amd64_setup_cg_config(void);
#endif
