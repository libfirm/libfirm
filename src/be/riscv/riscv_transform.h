/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#ifndef FIRM_BE_RISCV_TRANSFORM_H
#define FIRM_BE_RISCV_TRANSFORM_H

#include "firm_types.h"

ir_node *get_Start_zero(ir_graph *irg);

void riscv_transform_graph(ir_graph *irg);

#endif
