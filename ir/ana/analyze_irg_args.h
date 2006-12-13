/*
 * Project:     libFIRM
 * File name:   ir/ana/analyze_irg_args.h
 * Purpose:     rea/write analyze of graph argument, which have mode reference.
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _ANALYZE_IRG_ARGS_H_
#define _ANALYZE_IRG_ARGS_H_

#include "irgraph.h"
#include "entity.h"

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
ptr_access_kind get_method_param_access(ir_entity *ent, int pos);

/**
 * Analyze how pointer arguments of a given
 * ir graph are accessed.
 *
 * @param irg   The ir graph to analyze.
 */
void analyze_irg_args(ir_graph *irg);

/**
 * Returns for a method the 'weight' that every parameter
 * has on optimization possibility. Higher values allows
 * higher optimization with procedure cloning.
 *
 * The values are calculation on demand only.
 */
float get_method_param_weight(ir_entity *ent, int pos);

/**
 * Analyze the parameters of a given ir graph.
 *
 * @param irg The ir graph to analyze.
 */
void analyze_irg_args_weight(ir_graph *irg);

#endif /*_ANALYZE_IRG_ARGS_H_ */
