/*
 * Project:     libFIRM
 * File name:   ir/ana/cgana.c
 * Purpose:     Intraprozedural analyses to estimate the call graph.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file cgana.c
 * Intraprozedurale Analyse zur Abschätzung der Aufrufrelation. Es
 * wird eine Menge von freien Methoden und anschließend die an den
 * Call-Operationen aufrufbaren Methoden bestimmt.
 *
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include "cgana.h"
#include "rta.h"

#include "irnode_t.h"
#include "irmode_t.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irgmod.h"

#include "irflag_t.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"

#include "eset.h"
#include "pmap.h"
#include "array.h"

#include "irdump.h"

#include "firmstat.h"

/* Eindeutige Adresse zur Markierung von besuchten Knoten und zur
 * Darstellung der unbekannten Methode. */
static void * MARK = &MARK;



/* --- sel methods ---------------------------------------------------------- */


static eset * entities = NULL;


/** Bestimmt die eindeutige Methode, die die Methode für den
 *  übergebenen (dynamischen) Typ überschreibt. */
static entity * get_implementation(type * class, entity * method) {
  int i;
  if (get_entity_peculiarity(method) != peculiarity_description &&
      get_entity_owner(method) == class) {
    return method;
  }
  for (i = get_entity_n_overwrittenby(method) - 1; i >= 0; --i) {
    entity * e = get_entity_overwrittenby(method, i);
    if (get_entity_peculiarity(e) != peculiarity_description && get_entity_owner(e) == class) {
      return e;
    }
  }
  for (i = get_class_n_supertypes(class) - 1; i >= 0; --i) {
    entity * e = get_implementation(get_class_supertype(class, i), method);
    if (e) {
      return e;
    }
  }
  assert(0 && "implementation not found");
  return NULL;
}

/** Returns the entity that contains the implementation of the inherited
    entity if available, else returns the entity passed. */
static entity *get_inherited_methods_implementation(entity *inh_meth) {
  entity *impl_meth = NULL;
  ir_node *addr = get_atomic_ent_value(inh_meth);

  assert(addr && "constant entity without value");

  if ((get_irn_op(addr) == op_SymConst) &&
      (get_SymConst_kind(addr) == symconst_addr_ent)) {
    impl_meth = get_SymConst_entity(addr);
  } else {
    assert(0 && "Complex constant values not supported -- address of method should be straight constant!");
  }

  if (impl_meth && (get_entity_peculiarity(impl_meth) != peculiarity_existent)) {
    /*
      printf("this_meth: "); DDMEO(inh_meth);
      printf("impl meth: "); DDMEO(impl_meth);
      assert(!impl_meth || get_entity_peculiarity(impl_meth) == peculiarity_existent);
    */
    impl_meth = NULL;
  }
  return impl_meth? impl_meth : inh_meth;
}


/** Collect the entity representing the implementation of this
 *  entity (not the same if inherited) and all entities for overwriting
 *  implementations in "set".
 *  If the implementation of the method is not included in the
 *  compilation unit "open" is set to true.
 *  A recursive descend in the overwritten relation.
 *  Cycle-free, therefore must terminate.
 *
 * @param method
 * @param set      A set of entities.
 * @param size     Number of entities in set.
 * @param open
 */
static void collect_impls(entity *method, eset *set, int *size, bool *open) {
  int i;

  if (get_entity_peculiarity(method) == peculiarity_existent) {
    if ((get_entity_visibility(method) == visibility_external_allocated)
	&& (NULL == get_entity_irg(method))) {
      *open = true;
    } else {
      assert(get_entity_irg(method) != NULL);
      if (!eset_contains(set, method)) {
        eset_insert(set, method);
        ++(*size);
      }
    }
  }

  if (get_entity_peculiarity(method) == peculiarity_inherited) {
    entity *impl_ent = get_inherited_methods_implementation(method);
    assert(impl_ent && "no implementation for inherited entity");
    if (get_entity_visibility(impl_ent) == visibility_external_allocated) {
      assert(get_entity_irg(impl_ent) == NULL);
      *open = true;
    } else {
      assert(get_entity_irg(impl_ent) != NULL);
      if (!eset_contains(set, impl_ent)) {
        eset_insert(set, impl_ent);
        ++(*size);
      }
    }
  }
  /*- recursive descent -*/
  for (i = get_entity_n_overwrittenby(method) - 1; i >= 0; --i)
    collect_impls(get_entity_overwrittenby(method, i), set, size, open);
}


/** Alle Methoden bestimmen, die die übergebene Methode überschreiben
 *  (und implementieren). In der zurückgegebenen Reihung kommt jede
 *  Methode nur einmal vor. Der Wert 'NULL' steht für unbekannte
 *  (externe) Methoden. Die zurückgegebene Reihung muß vom Aufrufer
 *  wieder freigegeben werden (siehe "DEL_ARR_F"). Gibt es überhaupt
 *  keine Methoden, die "method" überschreiben, so gibt die Methode
 *  "NULL" zurück.
 *
 *  @param method
 */
static entity ** get_impl_methods(entity * method) {
  eset * set = eset_create();
  int size = 0;
  entity ** arr;
  bool open = false;

  /* Collect all method entities that can be called here */
  collect_impls(method, set, &size, &open);

  /* Vorgaenger einfuegen. */
  if (size == 0 && !open) {
    /* keine implementierte überschriebene Methode */
    arr = NULL;
  } else if (open) {
    entity * ent;
    arr = NEW_ARR_F(entity *, size + 1);
    arr[0] = NULL;  /* Represents open method */
    for (ent = eset_first(set); size > 0; ent = eset_next(set), --size)
      arr[size] = ent;
  } else {
    entity * ent;
    arr = NEW_ARR_F(entity *, size);
    for (size -= 1, ent = eset_first(set); size >= 0; ent = eset_next(set), --size)
      arr[size] = ent;
  }
  eset_destroy(set);
  return arr;
}

/** Analyse address computations.
 *
 *  - If the node is a SymConst(name) replace it by SymConst(ent) if possible.
 *  - If the node is a Sel:
 *    * If the pointer to the Sel comes directly from an Alloc node
 *      replace the Sel by a SymConst(ent).
 *
 *
 *  @param node The node to analyze
 *  @param ldname_map A map that mapps names of entities to the entities.
 */
static void sel_methods_walker(ir_node * node, pmap * ldname_map) {

  /* replace SymConst(name)-operations by SymConst(ent) */
  if (get_irn_op(node) == op_SymConst) {
    if (get_SymConst_kind(node) == symconst_addr_name) {
      pmap_entry * entry = pmap_find(ldname_map, (void *) get_SymConst_name(node));
      if (entry != NULL) { /* Method is declared in the compiled code */
	entity * ent = entry->value;
	if (get_opt_normalize() && (get_entity_visibility(ent) != visibility_external_allocated)) { /* Meth. is defined */
	  set_irg_current_block(current_ir_graph, get_nodes_block(node));
	  ir_node *new_node = copy_const_value(get_atomic_ent_value(ent));

	  DBG_OPT_CSTEVAL(node, new_node);

	  assert(get_entity_irg(ent));
	  DDMN(new_node);
	  exchange(node, new_node);
	}
      }
    }
  }

  else if (get_irn_op(node) == op_Sel &&
	   is_method_type(get_entity_type(get_Sel_entity(node)))) {
    entity * ent = get_Sel_entity(node);

    /* Sel from Alloc: replace by constant */
    if (get_opt_optimize() && get_opt_dyn_meth_dispatch() &&
        (get_irn_op(skip_Proj(get_Sel_ptr(node))) == op_Alloc)) {
      ir_node *new_node;
      entity *called_ent;
      /* We know which method will be called, no dispatch necessary. */
      called_ent = resolve_ent_polymorphy(get_Alloc_type(skip_Proj(get_Sel_ptr(node))), ent);
      set_irg_current_block(current_ir_graph, get_nodes_block(node));
      /* called_ent may not be description: has no Address/Const to Call! */
      assert(get_entity_peculiarity(called_ent) != peculiarity_description);
      new_node = copy_const_value(get_atomic_ent_value(called_ent));

      DBG_OPT_POLY_ALLOC(node, new_node);
      exchange(node, new_node);
    }

    else {
      assert(get_entity_peculiarity(ent) != peculiarity_inherited);
      if (!eset_contains(entities, ent)) {
        /* Entity noch nicht behandelt. Alle (intern oder extern)
         * implementierten Methoden suchen, die diese Entity
         * überschreiben. Die Menge an entity.link speichern. */
        set_entity_link(ent, get_impl_methods(ent));
        eset_insert(entities, ent);
      }
      if (get_entity_link(ent) == NULL) {
        /* Die Sel-Operation kann nie einen Zeiger auf eine aufrufbare
         * Methode zurückgeben. Damit ist sie insbesondere nicht
         * ausführbar und nicht erreichbar. */
        /* Gib eine Warnung aus wenn die Entitaet eine Beschreibung ist
           fuer die es keine Implementierung gibt. */
        if (get_entity_peculiarity(ent) == peculiarity_description) {
	  /* This is possible:  We call a method in a dead part of the program. */
        } else {
	  DDMN(node);
	  assert(0);  /* Why should this happen ??? */
          //exchange(node, new_Bad());
        }
      } else {
        entity ** arr = get_entity_link(ent);

#if 0
        int i;
        printf("\nCall site "); DDMN(node);
        printf("  in "); DDME(get_irg_entity(current_ir_graph));
        printf("  can call:\n");
        for (i = 0; i < ARR_LEN(arr); i++) {
          printf("   - "); DDME(arr[i]);
          printf("     with owner "); DDMT(get_entity_owner(arr[i]));
        }
        printf("\n");
#endif

        if (get_opt_optimize() && get_opt_dyn_meth_dispatch() &&
            (ARR_LEN(arr) == 1 && arr[0] != NULL)) {
          ir_node *new_node;
          /* Die Sel-Operation kann immer nur _einen_ Wert auf eine
           * interne Methode zurückgeben. Wir können daher die
           * Sel-Operation durch eine Const- bzw. SymConst-Operation
           * ersetzen. */
          set_irg_current_block(current_ir_graph, get_nodes_block(node));
          assert(get_entity_peculiarity(get_SymConst_entity(get_atomic_ent_value(arr[0]))) ==
	         peculiarity_existent);
          new_node = copy_const_value(get_atomic_ent_value(arr[0]));
          DBG_OPT_POLY(node, new_node);
          exchange (node, new_node);
        }
      }
    }
  }
}


/** Datenstruktur initialisieren. Zusätzlich werden alle
 *  SymConst(name)-Operationen, die auf interne Methoden verweisen, durch
 *  SymConst(entity)-Operationen ersetzt. */
static void sel_methods_init(void) {
  int i;
  pmap * ldname_map = pmap_create();   /* Map entitiy names to entities: to replace SymConst(name) by SymConst(ent). */

  assert(entities == NULL);
  entities = eset_create();
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    entity * ent = get_irg_entity(get_irp_irg(i));
    /* Nur extern sichtbare Methode können überhaupt mit SymConst
     * aufgerufen werden. */
    if (get_entity_visibility(ent) != visibility_local) {
      pmap_insert(ldname_map, (void *) get_entity_ld_ident(ent), ent);
    }
  }
  all_irg_walk((irg_walk_func *) sel_methods_walker, NULL, ldname_map);
  pmap_destroy(ldname_map);
}

/*****************************************************************************/

/* Datenstruktur freigeben. */
static void sel_methods_dispose(void) {
  entity * ent;
  assert(entities);
  for (ent = eset_first(entities); ent; ent = eset_next(entities)) {
    entity ** arr = get_entity_link(ent);
    if (arr) {
      DEL_ARR_F(arr);
    }
    set_entity_link(ent, NULL);
  }
  eset_destroy(entities);
  entities = NULL;
}


/* Gibt die Menge aller Methoden zurück, die an diesem Sel-Knoten
 * zurückgegeben werden können. Die Liste enthält keine doppelten
 * Einträge. */
static entity ** get_Sel_arr(ir_node * sel) {
  static entity ** NULL_ARRAY = NULL;
  entity * ent;
  entity ** arr;
  assert(sel && get_irn_op(sel) == op_Sel);
  ent = get_Sel_entity(sel);
  assert(is_method_type(get_entity_type(ent))); /* what else? */
  arr = get_entity_link(ent);
  if (arr) {
    return arr;
  } else {
    /* "NULL" zeigt an, dass keine Implementierung existiert. Dies
     * kann für polymorphe (abstrakte) Methoden passieren. */
    if (!NULL_ARRAY) {
      NULL_ARRAY = NEW_ARR_F(entity *, 0);
    }
    return NULL_ARRAY;
  }
}


static int get_Sel_n_methods(ir_node * sel) {
  return ARR_LEN(get_Sel_arr(sel));
}


static entity * get_Sel_method(ir_node * sel, int pos) {
  entity ** arr = get_Sel_arr(sel);
  assert(pos >= 0 && pos < ARR_LEN(arr));
  return arr[pos];
}



/* --- callee analysis ------------------------------------------------------ */


static void callee_ana_node(ir_node * node, eset * methods);


static void callee_ana_proj(ir_node * node, long n, eset * methods) {
  assert(get_irn_mode(node) == mode_T);
  if (get_irn_link(node) == MARK) {
    /* already visited */
    return;
  }
  set_irn_link(node, MARK);

  switch (get_irn_opcode(node)) {
  case iro_Proj: {
    /* proj_proj: in einem "sinnvollen" Graphen kommt jetzt ein
     * op_Tuple oder ein Knoten, der eine "freie Methode"
     * zurückgibt. */
    ir_node * pred = get_Proj_pred(node);
    if (get_irn_link(pred) != MARK) {
      if (get_irn_op(pred) == op_Tuple) {
    callee_ana_proj(get_Tuple_pred(pred, get_Proj_proj(node)), n, methods);
      } else {
    eset_insert(methods, MARK); /* free method -> unknown */
      }
    }
    break;
  }

  case iro_Tuple:
    callee_ana_node(get_Tuple_pred(node, n), methods);
    break;

  case iro_Id:
    callee_ana_proj(get_Id_pred(node), n, methods);
    break;

  default:
    eset_insert(methods, MARK); /* free method -> unknown */
    break;
  }

  set_irn_link(node, NULL);
}


static void callee_ana_node(ir_node * node, eset * methods) {
  int i;

  assert(mode_is_reference(get_irn_mode(node)) || is_Bad(node));
  /* rekursion verhindern */
  if (get_irn_link(node) == MARK) {
    /* already visited */
    return;
  }
  set_irn_link(node, MARK);

  switch (get_irn_opcode(node)) {
  case iro_SymConst:
    if (get_SymConst_kind(node) == symconst_addr_ent) {
      entity * ent = get_SymConst_entity(node);
      assert(ent && is_method_type(get_entity_type(ent)));
      if (get_entity_visibility(ent) != visibility_external_allocated) {
	assert(get_entity_irg(ent));
	eset_insert(methods, ent);
      } else {
	eset_insert(methods, MARK); /* free method -> unknown */
      }
    } else {
      assert(get_SymConst_kind(node) == symconst_addr_name);
      /* externe Methode (wegen fix_symconst!) */
      eset_insert(methods, MARK); /* free method -> unknown */
    }
    break;
  case iro_Sel:
    /* polymorphe Methode */
    for (i = get_Sel_n_methods(node) - 1; i >= 0; --i) {
      entity * ent = get_Sel_method(node, i);
      if (ent) {
        eset_insert(methods, ent);
      } else {
        eset_insert(methods, MARK);
      }
    }
    break;

  case iro_Bad:
    /* nothing */
    break;

  case iro_Phi: /* Vereinigung */
    for (i = get_Phi_n_preds(node) - 1; i >= 0; --i) {
      callee_ana_node(get_Phi_pred(node, i), methods);
    }
    break;

  case iro_Id:
    callee_ana_node(get_Id_pred(node), methods);
    break;

  case iro_Proj:
    callee_ana_proj(get_Proj_pred(node), get_Proj_proj(node), methods);
    break;

  case iro_Add:
  case iro_Sub:
  case iro_Conv:
    /* extern */
    eset_insert(methods, MARK); /* free method -> unknown */
    break;

  default:
    assert(0 && "invalid opcode or opcode not implemented");
    break;
  }

  set_irn_link(node, NULL);
}


static void callee_walker(ir_node * call, void * env) {
  if (get_irn_op(call) == op_Call) {
    eset * methods = eset_create();
    entity * ent;
    entity ** arr = NEW_ARR_F(entity *, 0);
    callee_ana_node(skip_Id(get_Call_ptr(call)), methods);
    if (eset_contains(methods, MARK)) { /* unknown method */
      ARR_APP1(entity *, arr, unknown_entity);
    }
    for (ent = eset_first(methods); ent; ent = eset_next(methods)) {
      if (ent != MARK) {
	ARR_APP1(entity *, arr, ent);
      }
    }
#if 0  /* This generates Bad nodes when we don't want it.
	  Call it with a check for valid cgana information in local_optimize. */
    if (ARR_LEN(arr) == 0 && get_opt_optimize() && get_opt_dyn_meth_dispatch()) {
      /* Kann vorkommen, wenn der Vorgänger beispielsweise eine
       * Sel-Operation war, die keine Methoden zurückgeben
       * konnte. Wir ersetzen die Call-Operation ebenfalls durch
       * eine Bad-Operation. Die Verlinkung muss wiederhergestellt
       * werden! */
      ir_node *mem = get_Call_mem(call);
      turn_into_tuple (call, 5 /* pn_Call_max */);
      set_Tuple_pred(call, pn_Call_M_regular       , mem);
      set_Tuple_pred(call, pn_Call_T_result        , new_Bad());
      set_Tuple_pred(call, pn_Call_P_value_res_base, new_Bad());
      set_Tuple_pred(call, pn_Call_X_except        , new_Bad());  /* new_Jmp() ?? new_Raise() ?? */
      set_Tuple_pred(call, pn_Call_M_except        , new_Bad());

    } else
#endif
    {
      /* remove, what we repaired. */
      int i;
      for (i = 0; i < ARR_LEN(arr); ++i) {
	assert(arr[i]);
	//if (arr[i] == unknown_entity) arr[i] = NULL;
      }

      set_Call_callee_arr(call, ARR_LEN(arr), arr);
    }
    DEL_ARR_F(arr);
    eset_destroy(methods);
  }
}


static void remove_Tuples(ir_node * proj, void * env) {
  ir_node *new;
  if (get_irn_opcode(proj) != iro_Proj) return;

  new = skip_Tuple(proj);
  if (new != proj) exchange(proj, new);
}


/* Bestimmt für jede Call-Operation die Menge der aufrufbaren Methode
 * und speichert das Ergebnis in der Call-Operation. (siehe
 * "set_Call_callee"). "sel_methods" wird für den Aufbau benötigt und
 * muss bereits aufgebaut sein. */
static void callee_ana(void) {
  int i;
  /* Alle Graphen analysieren. */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg_walk_graph(get_irp_irg(i), callee_walker, remove_Tuples, NULL);
    set_irg_callee_info_state(get_irp_irg(i), irg_callee_info_consistent);
  }
  set_irp_callee_info_state(irg_callee_info_consistent);
}



/* --- free method analysis ------------------------------------------------- */


static void free_mark(ir_node * node, eset * set);

static void free_mark_proj(ir_node * node, long n, eset * set) {
  assert(get_irn_mode(node) == mode_T);
  if (get_irn_link(node) == MARK) {
    /* already visited */
    return;
  }
  set_irn_link(node, MARK);
  switch (get_irn_opcode(node)) {
  case iro_Proj: {
    /* proj_proj: in einem "sinnvollen" Graphen kommt jetzt ein
     * op_Tuple oder ein Knoten, der in "free_ana_walker" behandelt
     * wird. */
    ir_node * pred = get_Proj_pred(node);
    if (get_irn_link(pred) != MARK && get_irn_op(pred) == op_Tuple) {
      free_mark_proj(get_Tuple_pred(pred, get_Proj_proj(node)), n, set);
    } else {
      /* nothing: da in "free_ana_walker" behandelt. */
    }
    break;
  }

  case iro_Tuple:
    free_mark(get_Tuple_pred(node, n), set);
    break;

  case iro_Id:
    free_mark_proj(get_Id_pred(node), n, set);
    break;

  case iro_Start:
  case iro_Alloc:
  case iro_Load:
    /* nothing: Die Operationen werden in "free_ana_walker" selbst
     * behandelt. */
    break;

  default:
    assert(0 && "unexpected opcode or opcode not implemented");
    break;
  }
  set_irn_link(node, NULL);
}


static void free_mark(ir_node * node, eset * set) {
  int i;
//  assert(mode_is_reference(get_irn_mode(node)));
  if (get_irn_link(node) == MARK) {
    return; /* already visited */
  }
  set_irn_link(node, MARK);
  switch (get_irn_opcode(node)) {
  case iro_Sel: {
    entity * ent = get_Sel_entity(node);
    if (is_method_type(get_entity_type(ent))) {
      for (i = get_Sel_n_methods(node) - 1; i >= 0; --i) {
    eset_insert(set, get_Sel_method(node, i));
      }
    }
    break;
  }
  case iro_SymConst:
    if (get_SymConst_kind(node) == symconst_addr_ent) {
      entity * ent = get_SymConst_entity(node);
      if (is_method_type(get_entity_type(ent))) {
        eset_insert(set, ent);
      }
    } else {
      assert(get_SymConst_kind(node) == symconst_addr_name);
      /* nothing: SymConst points to extern method */
    }
    break;

  case iro_Phi:
    for (i = get_Phi_n_preds(node) - 1; i >= 0; --i) {
      free_mark(get_Phi_pred(node, i), set);
    }
    break;
  case iro_Id:
    free_mark(get_Id_pred(node), set);
    break;
  case iro_Proj:
    free_mark_proj(get_Proj_pred(node), get_Proj_proj(node), set);
    break;
  default:
    /* nothing: Wird unten behandelt! */
    break;
  }
  set_irn_link(node, NULL);
}


static void free_ana_walker(ir_node * node, eset * set) {
  int i;
  if (get_irn_link(node) == MARK) {
    /* bereits in einem Zyklus besucht. */
    return;
  }
  switch (get_irn_opcode(node)) {
  /* special nodes */
  case iro_Sel:
  case iro_SymConst:
  case iro_Const:
  case iro_Phi:
  case iro_Id:
  case iro_Proj:
  case iro_Tuple:
    /* nothing */
    break;
  /* Sonderbehandlung, da der Funktionszeigereingang natürlich kein
   * Verräter ist. */
  case iro_Call:
    set_irn_link(node, MARK);
    for (i = get_Call_arity(node) - 1; i >= 0; --i) {
      ir_node * pred = get_Call_param(node, i);
      if (mode_is_reference(get_irn_mode(pred))) {
        free_mark(pred, set);
      }
    }
    break;
  /* other nodes: Alle anderen Knoten nehmen wir als Verräter an, bis
   * jemand das Gegenteil implementiert. */
  default:
    set_irn_link(node, MARK);
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node * pred = get_irn_n(node, i);
      if (mode_is_reference(get_irn_mode(pred))) {
        free_mark(pred, set);
      }
    }
    break;
  }
  set_irn_link(node, NULL);
}


/* Die Datenstrukturen für sel-Methoden (sel_methods) muß vor dem
 * Aufruf von "get_free_methods" aufgebaut sein. Die (internen)
 * SymConst-Operationen müssen in passende Const-Operationen
 * umgewandelt worden sein, d.h. SymConst-Operationen verweisen immer
 * auf eine echt externe Methode.  */
static entity ** get_free_methods(void)
{
  eset * set = eset_create();
  int i;
  entity ** arr = NEW_ARR_F(entity *, 0);
  entity * ent;

  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    entity * ent = get_irg_entity(irg);
    /* insert "external visible" methods. */
    if (get_entity_visibility(ent) != visibility_local) {
      eset_insert(set, ent);
    }
    /* Finde alle Methoden die in dieser Methode extern sichtbar werden,
       z.B. da die Adresse einer Methode abgespeichert wird. */
    irg_walk_graph(irg, NULL, (irg_walk_func *) free_ana_walker, set);
  }

  /* insert sticky methods, too */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    entity * ent = get_irg_entity(irg);
    /* insert "external visible" methods. */
    if (get_entity_stickyness (ent) == stickyness_sticky) {
      eset_insert(set, ent);
    }
  }

  /* Hauptprogramm ist auch dann frei, wenn es nicht "external
   * visible" ist. */
  if (get_irp_main_irg()) {
    eset_insert(set, get_irg_entity(get_irp_main_irg()));
  }
  /* Wandle Menge in Feld um.  Effizienter. */
  for (ent = eset_first(set); ent; ent = eset_next(set)) {
    ARR_APP1(entity *, arr, ent);
  }
  eset_destroy(set);

  return arr;
}

void cgana(int *length, entity ***free_methods) {
  entity ** free_meths, **p;

  sel_methods_init();
  free_meths = get_free_methods();
  callee_ana();
  sel_methods_dispose();

  /* Convert the flexible array to an array that can be handled
     by standard C. */
  p = (entity **)malloc(sizeof(*p) * ARR_LEN(free_meths));
  memcpy(p, free_meths, ARR_LEN(free_meths) * sizeof(*p));

  *length       = ARR_LEN(free_meths);
  *free_methods = p;

  DEL_ARR_F(free_meths);
}



static void destruct_walker(ir_node * node, void * env) {
  if (get_irn_op(node) == op_Call) {
    remove_Call_callee_arr(node);
  }
}

void free_callee_info(ir_graph *irg) {
  irg_walk_graph(irg, destruct_walker, NULL, NULL);
  set_irg_callee_info_state(irg, irg_callee_info_none);
}


/* Optimize the address expressions passed to call nodes.
 *
 * This optimization performs the following transformations for
 * all ir graphs:
 * - All SymConst operations that refer to intern methods are replaced
 *   by Const operations refering to the corresponding entity.
 * - Sel nodes, that select entities that are not overwritten are
 *   replaced by Const nodes refering to the selected entity.
 * - Sel nodes, for witch no method exists at all are replaced by Bad
 *   nodes.
 * - Sel nodes with a pointer input that is an Alloc node are replaced
 *   by Const nodes refering to the entity that implements the method in
 *   the type given by the Alloc node.
 */
void opt_call_addrs(void) {
  sel_methods_init();
  sel_methods_dispose();
#if 0
  int i;
  pmap * ldname_map = pmap_create();
  assert(entities == NULL);
  entities = eset_create();
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    entity * ent = get_irg_entity(get_irp_irg(i));
    /* Nur extern sichtbare Methoden können überhaupt mit SymConst
     * aufgerufen werden. */
    if (get_entity_visibility(ent) != local) {
      pmap_insert(ldname_map, (void *) get_entity_ld_ident(ent), ent);
    }
  }
  all_irg_walk((irg_walk_func *) sel_methods_walker, NULL, ldname_map);
  pmap_destroy(ldname_map);
#endif
}
