/*
 * Project:     libFIRM
 * File name:   ir/common/statistics.h
 * Purpose:     Compute statistics about firm library.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 *  @file statistics.h
 *
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

# ifndef _STATISTICS_H_
# define _STATISTICS_H_

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


# endif /* _STATISTICS_H_ */
