/*
 * Project:     libFIRM
 * File name:   ir/opt/ifconv.h
 * Purpose:     If conversion.
 * Author:      Sebastian Hack.
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_IF_CONV_H
#define _FIRM_IF_CONV_H

/**
 * @file ifconv.h
 *
 * If conversion.
 * @author Sebastian Hack
 * @date 10.2.2005
 */
#include "irnode.h"

/**
 * This function is called to evaluate, if a mux can build
 * of the current architecture.
 * If it returns non-zero, a mux is created, else the code
 * is not modified.
 */
typedef int (*arch_allow_mux_func)(ir_node *sel, ir_node *false_res, ir_node *true_res);

/**
 * The parameters structure.
 */
typedef struct _opt_if_conv_info_t {
  int                 max_depth;    /**< The maximum depth up to which expressions
                                         are examined when it has to be decided if they
                                         can be placed into another block. */
  arch_allow_mux_func allow_mux;    /**< Evaluator function, if not set all possible Mux
                                         nodes will be created. */
} opt_if_conv_info_t;

/**
 * Perform If conversion on a graph.
 * @param irg The graph.
 * @param params The parameters for the if conversion.
 */
void opt_if_conv(ir_graph *irg, const opt_if_conv_info_t *params);

#endif
