/**
 *
 * @file loop_unrolling.c
 *
 * Project:     libFIRM
 * File name:   ir/opt/loop_unrolling.c
 * Purpose:     Make loop unrolling.
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     16.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# include "loop_unrolling.h"

# include "irgwalk.h"
# include "irnode_t.h"
# include "irloop_t.h"

static void do_loop_unroll(ir_node *n, void *env){
}







/* Performs loop unrolling for the passed graph. */
void optimize_loop_unrolling(ir_graph *irg) {

  if (!get_optimize() || !get_opt_loop_unrolling()) return;

  /* -- Search expressions that can be optimized -- */
  irg_walk_graph(irg, NULL, do_loop_unroll, NULL);

}
