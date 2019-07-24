/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#ifndef FIRM_BE_VHDL_VHDL_BEARCH_T_H
#define FIRM_BE_VHDL_VHDL_BEARCH_T_H

#include <stdio.h>
#include "vhdl_modes.h"

void vhdl_init(void);
void vhdl_finish(void);

void vhdl_generate_code(const char *cup_name);

#endif
