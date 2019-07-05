/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#ifndef FIRM_BE_VHDL_VHDL_BEARCH_T_H
#define FIRM_BE_VHDL_VHDL_BEARCH_T_H

#include <stdio.h>

#define VHDL_MACHINE_SIZE 32

void vhdl_generate_code(FILE *output, const char *cup_name);

#endif
