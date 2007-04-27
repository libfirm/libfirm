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

/*
 * Project:     libFIRM
 * File name:   ir/ir/ircgopt.c
 * Purpose:     Removal of unreachable methods.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 */

/**
 * Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 * der nicht erreichbaren Methoden wird aus der Abschätzung der
 * Aufrufrelation bestimmt.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "ircgopt.h"

#include "array.h"
#include "irprog.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irflag_t.h"
#include "ircons.h"
#include "typewalk.h"
#include "irtools.h"

/**
 * Walker: adds Call operations to a head's link list.
 */
static void collect_call(ir_node * node, void *env) {
  ir_node *head = env;

  if (get_irn_op(node) == op_Call) {
    set_irn_link(node, get_irn_link(head));
    set_irn_link(head, node);
  }
}

static void make_entity_to_description(type_or_ent *tore, void *env) {
  if (get_kind(tore) == k_entity) {
    ir_entity *ent = (ir_entity *)tore;

    if ((is_Method_type(get_entity_type(ent)))                        &&
        (get_entity_peculiarity(ent) != peculiarity_description)      &&
        (get_entity_visibility(ent)  != visibility_external_allocated)   ) {
      ir_entity *impl = get_SymConst_entity(get_atomic_ent_value(ent));
      if (get_entity_link(impl) != env) {
        set_entity_peculiarity(ent, peculiarity_description);
      }
    }
  }
}

/* garbage collect methods: mark and remove */
void gc_irgs(int n_keep, ir_entity ** keep_arr) {
  void * MARK = &MARK; /* @@@ gefaehrlich!!! Aber wir markieren hoechstens zu viele ... */
  int i;

  if (!get_opt_dead_method_elimination()) return;

  if (n_keep >= get_irp_n_irgs()) {
    /* Shortcut. Obviously we have to keep all methods. */
    return;
  }

  /* Mark entities that are alive.  */
  if (n_keep > 0) {
    ir_entity ** marked = NEW_ARR_F(ir_entity *, n_keep);
    for (i = 0; i < n_keep; ++i) {
      marked[i] = keep_arr[i];
      set_entity_link(marked[i], MARK);
      if (get_opt_dead_method_elimination_verbose() && get_firm_verbosity() > 2) {
        printf("dead method elimination: method %s kept alive.\n", get_entity_ld_name(marked[i]));
      }
    }

    for (i = 0; i < ARR_LEN(marked); ++i) {
      /* check for extern methods, these don't have an IRG */
      if (get_entity_visibility(marked[i]) != visibility_external_allocated) {
        ir_graph * irg = get_entity_irg(marked[i]);
        ir_node * node = get_irg_end(irg);

        /* collect calls */
        irg_walk_graph(irg, firm_clear_link, collect_call, node);

        /* iterate calls */
        for (node = get_irn_link(node); node; node = get_irn_link(node)) {
          int i;
          assert(get_irn_op(node) == op_Call);

          for (i = get_Call_n_callees(node) - 1; i >= 0; --i) {
            ir_entity * ent = get_Call_callee(node, i);

            if (get_entity_irg(ent) && get_entity_link(ent) != MARK) {
              set_entity_link(ent, MARK);
              ARR_APP1(ir_entity *, marked, ent);
              if (get_opt_dead_method_elimination_verbose() && get_firm_verbosity() > 2) {
                printf("dead method elimination: method %s can be called from Call %ld: kept alive.\n",
	               get_entity_ld_name(ent), get_irn_node_nr(node));
              }
            }
          }
        }
      }
    }
    DEL_ARR_F(marked);
  }

  /* clean */
  type_walk(make_entity_to_description, NULL, MARK);
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    ir_entity * ent = get_irg_entity(irg);
    /* Removing any graph invalidates all interprocedural loop trees. */
    if (get_irg_loopinfo_state(irg) == loopinfo_ip_consistent ||
        get_irg_loopinfo_state(irg) == loopinfo_ip_inconsistent) {
      free_loop_information(irg);
    }
    if ((get_entity_visibility(ent) == visibility_local) && (get_entity_link(ent) != MARK)) {
      remove_irp_irg(irg);
      set_entity_peculiarity(ent, peculiarity_description);
      if (get_opt_dead_method_elimination_verbose() && get_firm_verbosity() > 1) {
        printf("dead method elimination: freeing method %s\n", get_entity_ld_name(ent));
      }
    }
    set_entity_link(ent, NULL);
  }
}
