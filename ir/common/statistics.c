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
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "statistics.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irprog.h"
#include "irgwalk.h"

/***********************************************************************/
/* Statistics about allocated datastructures: counts.                  */

static void count_nodes(ir_node *n, void *env) {
	int * counter_ptr = (int *)env;
	(void) n;
	(*counter_ptr)++;
}


/** Prints number of irgraphs, number of nodes in them and
 *  totals. */
void print_graph_counts(int verbosity) {
  int i, counter, total = 0;
  ir_graph *old = current_ir_graph;

#ifdef INTERPROCEDURAL_VIEW
  int view = get_interprocedural_view();
  set_interprocedural_view(0);
#endif

  for (i = 0; i < get_irp_n_irgs(); i++) {
    counter = 0;
    irg_walk_graph(get_irp_irg(i), count_nodes, NULL, &counter);
    if (verbosity == 1)
      printf(" +%4d nodes in graph %s.\n", counter, get_entity_name(get_irg_entity(get_irp_irg(i))));
    total += counter;
  }
  printf(" +++ There are %d graphs with total %d nodes.\n", get_irp_n_irgs(), total);

  current_ir_graph = old;
#ifdef INTERPROCEDURAL_VIEW
  set_interprocedural_view(view);
#endif
}

/** Prints number of types, number of entities and totals.
 *   */
void print_type_counts(int verbosity) {
  int i, counter, total = 0;
  for (i = 0; i < get_irp_n_types(); i++) {
    ir_type *tp = get_irp_type(i);
    counter = -1;
    if (is_Class_type(tp)) counter = get_class_n_members(tp);
    if (is_Struct_type(tp)) counter = get_struct_n_members(tp);
    if (is_Union_type(tp)) counter = get_union_n_members(tp);
    if (counter > -1) {
      if (verbosity == 1)
    printf(" +%3d entities in %s type %s.\n", counter, get_type_tpop_name(tp), get_type_name(tp));
      total += counter;
    }
  }
  printf(" +++ There are %d types with total %d entities.\n", get_irp_n_types(), total);
  printf(" +++ Global type has %d entities\n",
     get_class_n_members(get_glob_type()));

}

/** Prints number of tarvals.
 *   */
void print_tarval_counts(int verbosity) {
  (void) verbosity;
  printf("tarval count not implemented.\n\n");
}

/** Prints number of idents.
 *   */
void print_ident_counts(int verbosity) {
  (void) verbosity;
  printf("ident count not implemented.\n\n");
}


void print_all_counts(int verbosity) {
  printf(" +++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  print_graph_counts(verbosity);
  printf(" +++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  print_type_counts(verbosity);
  printf(" +++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  /*
  print_tarval_counts(verbosity);
  printf(" +++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  print_ident_counts(verbosity);
  printf(" +++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  */
}
