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
 *
 * Note: The file format is not considered stable yet. So expect
 * incompatibilities between file formats of different libfirm versions.
 */
#ifndef FIRM_IR_IRIO_H
#define FIRM_IR_IRIO_H

#include <stdio.h>

#include "firm_types.h"
#include "begin.h"

/**
 * Exports the whole irp to the given file in a textual form.
 *
 * @param filename  the name of the resulting file
 *
 * Exports all types, all ir graphs, and the constant graph.
 */
FIRM_API void ir_export(const char *filename);

/**
 * same as ir_export but writes to a FILE*
 */
FIRM_API void ir_export_file(FILE *output, const char *outputname);

/**
 * Write the given ir graph to a stream in a textual format
 *
 * @param irg         the ir graph
 * @param output      output stream the irg is written to
 * @param outputname  a name for the output stream (used for error messages)
 *
 * Exports the type graph used by the given graph and the graph itself.
 */
FIRM_API void ir_export_irg(ir_graph *irg, FILE *output,
                            const char *outputname);

/**
 * Imports the data stored in the given file.
 *
 * @param filename  the name of the file
 *
 * Imports any type graphs and ir graphs contained in the file.
 */
FIRM_API void ir_import(const char *filename);

/**
 * same as ir_import but imports from a FILE*
 */
FIRM_API void ir_import_file(FILE *input, const char *inputname);

#include "end.h"

#endif
