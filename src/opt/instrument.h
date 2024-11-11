/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Instrumentation of graphs.
 */
#ifndef FIRM_IR_INSTRUMENT_H
#define FIRM_IR_INSTRUMENT_H

#include "firm_types.h"

/**
 * Adds a Call at the beginning of the given irg.
 *
 * @param irg  the graph to instrument
 * @param ent  the entity to call
 */
void instrument_initcall(ir_graph *irg, ir_entity *ent);

#endif
