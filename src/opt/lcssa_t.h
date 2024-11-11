/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Karlsruhe Institute of Technology
 */

/**
 * @file
 * @brief   loop-closed SSA transformation
 * @author  Elias Aebi
 */
#ifndef FIRM_IR_LCSSA_T_H
#define FIRM_IR_LCSSA_T_H

#include "firm.h"

void assure_lcssa(ir_graph *const irg);
void assure_loop_lcssa(ir_graph *const irg, ir_loop *const loop);

#endif
