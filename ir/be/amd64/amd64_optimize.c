/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Peephole optimizations.
 */
#include "amd64_optimize.h"

#include "bepeephole.h"

void amd64_peephole_optimization(ir_graph *const irg)
{
	ir_clear_opcodes_generic_func();
	be_peephole_opt(irg);
}
