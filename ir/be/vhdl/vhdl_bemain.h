/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

/**
 * @file
 * @brief   main backend functions for VHDL backend
 * @author  Johannes Bucher, Daniel Biester
 */

#ifndef LIBFIRM_VHDL_BEMAIN_H
#define LIBFIRM_VHDL_BEMAIN_H

#include "irgraph.h"
#include "stdio.h"

void vhdl_be_begin(FILE *, const char *);
void vhdl_be_step_last(ir_graph *);
void vhdl_be_finish(void);

#endif //LIBFIRM_VHDL_BEMAIN_H
