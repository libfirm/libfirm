/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Removal of unreachable methods.
 * @author  Hubert Schmid
 * @date    09.06.2002
 * @brief
 *  Removal of unreachable methods. The set of unreachable methods is computed
 *  by the callgraph.
 */
#ifndef FIRM_IR_ICGOPT_H
#define FIRM_IR_ICGOPT_H

#include <stddef.h>

#include "firm_types.h"
#include "begin.h"

/**
 * Removes all methods which are not reachable from "keep_arr".
 *
 * Frees all interprocedural loop information.
 */
FIRM_API void gc_irgs(size_t n_keep, ir_entity *keep_arr[]);

/**
 * Creates an ir_prog pass for gc_irgs().
 *
 * @param name     the name of this pass or NULL
 * @return  the newly created ir_graph pass
 */
FIRM_API ir_prog_pass_t *gc_irgs_pass(const char *name);

#include "end.h"

#endif
