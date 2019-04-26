/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     read/write analyze of graph argument, which have mode reference
 * @author    Beyhan Veliev
 */
#ifndef FIRM_ANA_ANALYZE_IRG_ARGS_H
#define FIRM_ANA_ANALYZE_IRG_ARGS_H

#include <stddef.h>
#include "firm_types.h"
#include "typerep.h"

#include "begin.h"

/**
 * Returns for a method with pointer parameter
 * if they will be read or written.
 *
 * @param ent  The entity that represent this method.
 * @param pos  The position of method's parameter for that
 *             we need information.

 * If the pos'th parameter is NOT of a pointer type, ptr_access_none
 * is returned;
 */
FIRM_API ptr_access_kind get_method_param_access(ir_entity *ent, size_t pos);

/**
 * Analyze how pointer arguments of a given
 * ir graph are accessed.
 *
 * @param irg   The ir graph to analyze.
 */
FIRM_API void analyze_irg_args(ir_graph *irg);

/**
 * Returns for a method the 'weight' that every parameter
 * has on optimization possibility. Higher values allows
 * higher optimization with procedure cloning.
 *
 * The values are calculated on demand only.
 */
FIRM_API unsigned get_method_param_weight(ir_entity *ent, size_t pos);

/**
 * Analyze the parameters of a given ir graph.
 *
 * @param irg The ir graph to analyze.
 */
FIRM_API void analyze_irg_args_weight(ir_graph *irg);

#include "end.h"

#endif
