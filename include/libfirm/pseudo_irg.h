/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief    interface to pseudo irgs
 * @author   Goetz Lindenmaier, Boris Boesler
 * @date     Oktober 2004
 * @version  $Id$
 */
#ifndef FIRM_IR_PSEUDO_IRG_H
#define FIRM_IR_PSEUDO_IRG_H

#include "firm_types.h"

#include "begin.h"

/** Create a new ir graph to build a pseudo representation of a procedure.
 *
 *  The pseudo representation can only be used for analyses.  It may not be
 *  optimized.  Pseudo graphs are kept in a separate graph list in irprog.
 */
FIRM_API ir_graph *new_pseudo_ir_graph(ir_entity *ent, int n_loc);

/** Returns non-zero ir ir_graph is pseudo graph.
 *  Is irg a pseudo graph for analysis? */
FIRM_API int is_pseudo_ir_graph(ir_graph *irg);

/** Returns the number of pseudo graphs in the program. */
FIRM_API int get_irp_n_pseudo_irgs(void);

/** Add a graph as pseudo irg */
FIRM_API void add_irp_pseudo_irg(ir_graph *irg);

/** Returns the pos'th  pseudo graph in the program. */
FIRM_API ir_graph *get_irp_pseudo_irg(int pos);

/** If set, get_irp_n_irgs() and get_irp_irg() behave as if all pseudo
    graphs are in the irg list. If not set, get_entity_irg() returns
    NULL if the entity refers to a pseudo irg. */
FIRM_API void set_visit_pseudo_irgs(int x);
FIRM_API int  get_visit_pseudo_irgs(void);

#include "end.h"

#endif
