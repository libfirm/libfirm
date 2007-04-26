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

/**
 * @file
 * @brief    Compute statistics about firm library.
 * @author   Goetz Lindenmaier
 * @version  $Id$
 * @summary
 *  This file defines a set ouf routines to output statistics
 *  about the firm library.  These statistics include
 *   - number of datastructures allocated, as entities, types, nodes...
 *   - memory consumption of data structures
 *   - effectiveness of optimizations
 *   - ... more to come.
 *
 *  This file is thought for compiler optimization, not to run it in a
 *  production compiler. I.e., the routines may be inefficient.
 */
#ifndef FIRM_COMMON_STATISTICS_H
#define FIRM_COMMON_STATISTICS_H

/* Statistics about allocated datastructures: counts. */
/** verbosity:
 *  0: information about the whole program
 *  1: information per type/procedure
 */

void print_all_counts(int verbosity);

/** Prints number of irgraphs, number of nodes in them and
 *  totals in intRAprocedural view. */
void print_graph_counts(int verbosity);

/** Prints number of types, number of entities and totals.
 *  Does not consider frame types or types to representent call by
 *  value arguments/results. */
void print_type_counts(int verbosity);

/** Prints number of tarvals.
 *   */
void print_tarval_counts(int verbosity);

/** Prints number of idents.
 *   */
void print_ident_counts(int verbosity);


#endif
