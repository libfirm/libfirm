/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/external/pseudo_irg.c
 * Purpose:     implementation
 * Author:      G"otz Lindenmaier
 * Modified by: Boris Boesler
 * Created:     xx.10.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


#include "pseudo_irg.h"

#include "irgraph_t.h"
#include "irprog_t.h"
#include "array.h"


/* Returns the number of pseudo graphs in the program. */
int get_irp_n_pseudo_irgs(void) {
  assert (irp && irp->pseudo_graphs);
  return ARR_LEN(irp->pseudo_graphs);
}

/* Returns the pos'th  pseudo graph in the program. */
ir_graph *get_irp_pseudo_irg(int pos) {
  assert(0 <= pos && pos <= get_irp_n_pseudo_irgs());
  return irp->pseudo_graphs[pos];
}

void add_irp_pseudo_irg(ir_graph *irg) {
  assert (irp && irp->pseudo_graphs);
  ARR_APP1(ir_graph *, irp->pseudo_graphs, irg);
}


/* Create a new ir graph to build a pseudo representation of a procedure.
 *
 *  The pseudo representation can only be used for analyses.  It may not be
 *  optimized.  Pseudo graphs are kept in a separate graph list in irprog.
 */
ir_graph *
new_pseudo_ir_graph(entity *ent, int n_loc) {
  ir_graph *res = new_r_ir_graph (ent, n_loc);
  add_irp_pseudo_irg(res);          /* remember this graph global. */
  return res;
}

/* Returns true ir ir_graph is pseudo graph. */
int is_pseudo_ir_graph(ir_graph *irg)
{
  int i, n_pseudo_irgs;

  assert(irg && "nothing here");
  assert(is_ir_graph(irg) && "no ir_graph given");

  n_pseudo_irgs = get_irp_n_pseudo_irgs();
  for (i = 0; i < n_pseudo_irgs; ++i) {
    if (irg == get_irp_pseudo_irg(i)) return true;
  }
  return false;
}

static int visit_pseudo_irgs = 0;

void set_visit_pseudo_irgs(int x) {
  visit_pseudo_irgs = x;
}

int get_visit_pseudo_irgs(void) {
  return visit_pseudo_irgs;
}
