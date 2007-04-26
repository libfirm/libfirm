/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

/**
 * @file
 * @brief     read/write analyze of graph argument, which have mode reference
 * @author    Beyhan Veliev
 * @version   $Id$
 */
#ifndef FIRM_ANA_ANALYZE_IRG_ARGS_H
#define FIRM_ANA_ANALYZE_IRG_ARGS_H

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

#endif
