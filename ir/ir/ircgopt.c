/*
 * Project:     libFIRM
 * File name:   ir/ir/ircgopt.c
 * Purpose:     Removal of unreachable methods.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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

static void clear_link(ir_node * node, void * env) {
  set_irn_link(node, NULL);
}


/* Call-Operationen an die "link"-Liste von "call_head" anhängen. */
static void collect_call(ir_node * node, ir_node * head) {
  if (get_irn_op(node) == op_Call) {
    set_irn_link(node, get_irn_link(head));
    set_irn_link(head, node);
  }
}

static void make_entity_to_description(type_or_ent *tore, void *env) {
  if (get_kind(tore) == k_entity) {
    entity *ent = (entity *)tore;

    if ((is_Method_type(get_entity_type(ent)))                        &&
	      (get_entity_peculiarity(ent) != peculiarity_description)      &&
	      (get_entity_visibility(ent)  != visibility_external_allocated)   ) {
      entity *impl = get_SymConst_entity(get_atomic_ent_value(ent));
      if (get_entity_link(impl) != env) {
	      set_entity_peculiarity(ent, peculiarity_description);
	      //set_atomic_ent_value(ent, new_r_Const(get_const_code_irg(), get_irg_start_block(get_const_code_irg()),
	      //				      mode_P, get_tarval_null(mode_P)));
      }
    }
  }
}

/* garbage collect methods: mark and remove */
void gc_irgs(int n_keep, entity ** keep_arr) {
  void * MARK = &MARK; /* @@@ gefaehrlich!!! Aber wir markieren hoechstens zu viele ... */
  int i;

  if (!get_opt_dead_method_elimination()) return;

  /* Mark entities that are alive.  */
  if (n_keep > 0) {
    entity ** marked = NEW_ARR_F(entity *, n_keep);
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
        irg_walk_graph(irg, clear_link, (irg_walk_func *) collect_call, node);
        /* iterate calls */
        for (node = get_irn_link(node); node; node = get_irn_link(node)) {
          int i;
          assert(get_irn_op(node) == op_Call);
          for (i = get_Call_n_callees(node) - 1; i >= 0; --i) {
            entity * ent = get_Call_callee(node, i);
            if (get_entity_irg(ent) && get_entity_link(ent) != MARK) {
              set_entity_link(ent, MARK);
              ARR_APP1(entity *, marked, ent);
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
    entity * ent = get_irg_entity(irg);
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
