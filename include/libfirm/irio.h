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
 * @brief   Import/export textual representation of firm.
 * @author  Moritz Kroll
 * @version $Id$
 */
#ifndef FIRM_IR_IRIO_H
#define FIRM_IR_IRIO_H

#include <stdio.h>

#include "firm_types.h"

/**
 * Exports the given ir graph to the given file in a textual form.
 *
 * @param irg       the ir graph
 * @param filename  the name of the resulting file
 *
 * Exports the type graph used by the given graph and the graph itself.
 */
void ir_export_irg(ir_graph *irg, const char *filename);

/**
 * Imports the data stored in the given file.
 *
 * @param filename  the name of the file
 *
 * Imports any type graphs and ir graphs contained in the file.
 */
void ir_import(const char *filename);

#endif
