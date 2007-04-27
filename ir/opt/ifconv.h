/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/*
 * Project:     libFIRM
 * File name:   ir/opt/ifconv.h
 * Purpose:     If conversion.
 * Author:      Sebastian Hack.
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
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
 * @param sel        A selector of a Cond.
 * @param phi_list   List of Phi nodes about to be converted (linked via link field)
 * @param i          First data predecessor involved in if conversion
 * @param j          Second data predecessor involved in if conversion
 */
typedef int (*arch_allow_ifconv_func)(ir_node *sel, ir_node* phi_list, int i, int j);

/**
 * The parameters structure.
 */
typedef struct _opt_if_conv_info_t {
  int                 max_depth;    /**< The maximum depth up to which expressions
                                         are examined when it has to be decided if they
                                         can be placed into another block. */
  arch_allow_ifconv_func allow_ifconv; /**< Evaluator function, if not set all possible Psi
                                         nodes will be created. */
} opt_if_conv_info_t;

/**
 * Perform If conversion on a graph.
 *
 * @param irg The graph.
 * @param params The parameters for the if conversion.
 *
 * Cannot handle blocks with Bad control predecessors, so call it after control
 * flow optimization.
 */
void opt_if_conv(ir_graph *irg, const opt_if_conv_info_t *params);

#endif /* _FIRM_IF_CONV_H */
