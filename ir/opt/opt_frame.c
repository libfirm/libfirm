/*
 * Project:     libFIRM
 * File name:   ir/opt/opt_frame.c
 * Purpose:     optimize the frame type
 * Author:      Michael Beck
 * Created:     15.03.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file opt_frame.c
 *
 * Optimize the frame type by removing unused type members.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irgraph_t.h"
#include "type_t.h"
#include "irouts.h"
#include "iredges.h"
#include "opt_frame.h"

/*
 * Optimize the frame type of an irg by removing
 * never touched entities.
 */
void opt_frame_irg(ir_graph *irg) {
  ir_type   *frame_tp = get_irg_frame_type(irg);
  ir_entity *ent, *list;
  ir_node   *frame, *sel;
  int       i, n = get_class_n_members(frame_tp);

  assert(get_irg_phase_state(irg) == phase_high && "Graph must be in high phase");
  if (n <= 0)
    return;

  /* clear all entity links */
  for (i = n - 1; i >= 0; --i) {
    ent = get_class_member(frame_tp, i);
    set_entity_link(ent, NULL);
  }

  /* look for uses */
  frame = get_irg_frame(irg);

  if (edges_activated(irg)) { /* use inplace edges */
    const ir_edge_t *edge;

    /* mark all used entities */
    foreach_out_edge(frame, edge) {
      sel = get_edge_src_irn(edge);
      ent = get_Sel_entity(sel);
      set_entity_link(ent, ent);
    }
  }
  else {
    /* use traditionally out edges */
    if (get_irg_outs_state(irg) != outs_consistent)
      compute_irg_outs(irg);

    /* mark all used entities */
    for (i = get_irn_n_outs(frame) - 1; i >= 0; --i) {
      sel = get_irn_out(frame, i);
      ent = get_Sel_entity(sel);
      set_entity_link(ent, ent);
    }
  }

  /* link unused ones */
  list = NULL;
  for (i = n - 1; i >= 0; --i) {
    ent = get_class_member(frame_tp, i);
    if (get_entity_link(ent) == NULL) {
      set_entity_link(ent, list);
      list = ent;
    }
  }

  if (list) {
    /* delete list members */
    for (ent = list; ent; ent = list) {
      list = get_entity_link(ent);
      remove_class_member(frame_tp, ent);
    }
    /* we changed the frame type, it's layout should be redefined */
    set_type_state(frame_tp, layout_undefined);
  }
}
