/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Intraprozedurale Analyse zur Abschätzung der Aufrulrelation. Es
 * wird eine Menge von freien Methoden und anschließend die an den
 * Call-Operationen aufrufbaren Methoden bestimmt.
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */

#include "stdlib.h"
#include "cgana.h"


#include "eset.h"
#include "pmap.h"
#include "array.h"
#include "irprog.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irgmod.h"
#include "xprintf.h"
#include "irnode.h"

#include "dbginfo_t.h"

/* Eindeutige Adresse zur Markierung von besuchten Knoten und zur
 * Darstellung der unbekannten Methode. */
static void * MARK = &MARK;



/* --- sel methods ---------------------------------------------------------- */


static eset * entities = NULL;


/* Bestimmt die eindeutige Methode, die die Methode für den
 * übergebenene (dynamischen) Typ überschreibt. */
static entity * get_implementation(type * class, entity * method) {
  int i;
  if (get_entity_peculiarity(method) != description &&
      get_entity_owner(method) == class) {
    return method;
  }
  for (i = get_entity_n_overwrittenby(method) - 1; i >= 0; --i) {
    entity * e = get_entity_overwrittenby(method, i);
    if (get_entity_peculiarity(e) != description && get_entity_owner(e) == class) {
      return e;
    }
  }
  for (i = get_class_n_supertypes(class) - 1; i >= 0; --i) {
    entity * e = get_implementation(get_class_supertype(class, i), method);
    if (e) {
      return e;
    }
  }
  assert(0 && "implemenation not found");
}

/* Returns the entity that contains the implementation of the inherited
   entity if available, else returns the entity passed. */
entity *get_inherited_methods_implementation(entity *inh_meth) {
  entity *impl_meth = NULL;
  ir_node *addr = get_atomic_ent_value(inh_meth);
  assert(addr && "constant entity without value");

  if (get_irn_op(addr) == op_Const) {
    impl_meth = get_tv_entity(get_Const_tarval(addr));
  }

  assert(!impl_meth || get_entity_peculiarity(impl_meth) == existent);
  return impl_meth? impl_meth : inh_meth;
}


/* Collect the entity representing the implementation of this
   entity (not the same if inherited) and all entities for overwriting
   implementations in "set".
   If the implementation of the method is not included in the
   compilation unit "open" is set to true.
   A recursive descend in the overwritten relation.
   Cycle-free, therefore must terminate. */
void collect_impls(entity *method, eset *set, int *size, bool *open) {
  int i;
  if (get_entity_peculiarity(method) == existent) {
    if (get_entity_visibility(method) == external_allocated) {
      assert(get_entity_irg(method) == NULL);
      *open = true;
    } else {
      assert(get_entity_irg(method) != NULL);
      if (!eset_contains(set, method)) {
	eset_insert(set, method);
	++(*size);
      }
    }
  }
  if (get_entity_peculiarity(method) == inherited) {
    entity *impl_ent = get_inherited_methods_implementation(method);
    assert(impl_ent && "no implementation for inherited entity");
    if (get_entity_visibility(impl_ent) == external_allocated) {
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
  /** recursive descend **/
  for (i = get_entity_n_overwrittenby(method) - 1; i >= 0; --i)
    collect_impls(get_entity_overwrittenby(method, i), set, size, open);
}


/* Alle Methoden bestimmen, die die übergebene Methode überschreiben
 * (und implementieren). In der zurückgegebenen Reihung kommt jede
 * Methode nur einmal vor. Der Wert 'NULL' steht für unbekannte
 * (externe) Methoden. Die zurückgegebene Reihung muß vom Aufrufer
 * wieder freigegeben werden (siehe "DEL_ARR_F"). Gibt es überhaupt
 * keine Methoden, die die "method" überschreiben, so gibt die Methode
 * "NULL" zurück. */
static entity ** get_impl_methods(entity * method) {
  eset * set = eset_create();
  int size = 0;
  entity ** arr;
  bool open = false;

  /** Collect all method entities that can be called here **/
  collect_impls(method, set, &size, &open);

/**
     Vorgaenger einfuegen. **/
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


/* debug makros used in sel_methods_walker */
#define SIZ(x)    sizeof(x)/sizeof((x)[0])

#define DBG_OPT_NORMALIZE                                                      \
	  __dbg_info_merge_pair(new_node, node, dbg_const_eval)
#define DBG_OPT_POLY_ALLOC                                                     \
  do {                                                                         \
	ir_node *ons[2];                                                       \
	ons[0] = node;                                                         \
	ons[1] = skip_Proj(get_Sel_ptr(node));                                 \
	__dbg_info_merge_sets(&new_node, 1, ons, SIZ(ons), dbg_rem_poly_call); \
     } while(0)
#define DBG_OPT_POLY                                                           \
	  __dbg_info_merge_pair(new_node, node, dbg_rem_poly_call)


static void sel_methods_walker(ir_node * node, pmap * ldname_map) {
  if (get_irn_op(node) == op_SymConst) {
    /* Wenn möglich SymConst-Operation durch Const-Operation
     * ersetzen. */
    if (get_SymConst_kind(node) == linkage_ptr_info) {
      pmap_entry * entry = pmap_find(ldname_map, (void *) get_SymConst_ptrinfo(node));
      if (entry != NULL) { /* Method is declared in the compiled code */
	entity * ent = entry->value;
	if (get_opt_normalize() && (get_entity_visibility(ent) != external_allocated)) { /* Meth. is defined */
	  ir_node *new_node;
	  assert(get_entity_irg(ent));
	  set_irg_current_block(current_ir_graph, get_nodes_Block(node));
	  new_node = new_d_Const(get_irn_dbg_info(node),
				 mode_P, tarval_P_from_entity(ent));       DBG_OPT_NORMALIZE;
	  exchange(node, new_node);
	}
      }
    }
  } else if (get_irn_op(node) == op_Sel &&
	     is_method_type(get_entity_type(get_Sel_entity(node)))) {
    entity * ent = get_Sel_entity(node);
    if (get_optimize() && get_opt_dyn_meth_dispatch() &&
	(get_irn_op(skip_Proj(get_Sel_ptr(node))) == op_Alloc)) {
      ir_node *new_node;
      /* We know which method will be called, no dispatch necessary. */
      assert(get_entity_peculiarity(ent) != description);
      set_irg_current_block(current_ir_graph, get_nodes_Block(node));
      /* @@@ Is this correct?? Alloc could reference a subtype of the owner
	 of Sel that overwrites the method referenced in Sel. */
      new_node = copy_const_value(get_atomic_ent_value(ent));              DBG_OPT_POLY_ALLOC;
      exchange (node, new_node);
    } else {
      assert(get_entity_peculiarity(ent) != inherited);
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
	if (get_entity_peculiarity(ent) == description) {
	  /* @@@ GL Methode um Fehler anzuzeigen aufrufen! */
	  xprintf("WARNING: Calling method description %I in method %I which has "
		  "no implementation!\n", get_entity_ident(ent),
		  get_entity_ident(get_irg_ent(current_ir_graph)));
	} else {
	  exchange(node, new_Bad());
	}
      } else {
	entity ** arr = get_entity_link(ent);

#if 0
	int i;
	printf("\nCall site "); DDMN(node);
	printf("  in "); DDME(get_irg_ent(current_ir_graph));
	printf("  can call:\n");
	for (i = 0; i < ARR_LEN(arr); i++) {
	  printf("   - "); DDME(arr[i]);
	  printf("     with owner "); DDMT(get_entity_owner(arr[i]));
	}
	printf("\n");
#endif

	if (get_optimize() && get_opt_dyn_meth_dispatch() &&
	    (ARR_LEN(arr) == 1 && arr[0] != NULL)) {
	  ir_node *new_node;
	  /* Die Sel-Operation kann immer nur einen Wert auf eine
	   * interne Methode zurückgeben. Wir können daher die
	   * Sel-Operation durch eine Const- bzw. SymConst-Operation
	   * ersetzen. */
	  set_irg_current_block(current_ir_graph, get_nodes_Block(node));
	  new_node = copy_const_value(get_atomic_ent_value(arr[0]));	     DBG_OPT_POLY;
	  exchange (node, new_node);
	}
      }
    }
  }
}


/* Datenstruktur initialisieren. Zusätzlich werden alle
 * SymConst-Operationen, die auf interne Methoden verweisen, durch
 * Const-Operationen ersetzt. */
static void sel_methods_init(void) {
  int i;
  pmap * ldname_map = pmap_create();
  assert(entities == NULL);
  entities = eset_create();
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    entity * ent = get_irg_ent(get_irp_irg(i));
    /* Nur extern sichtbare Methode können überhaupt mit SymConst
     * aufgerufen werden. */
    if (get_entity_visibility(ent) != local) {
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

  assert((get_irn_mode(node) == mode_P) || is_Bad(node));
  /* rekursion verhindern */
  if (get_irn_link(node) == MARK) {
    /* already visited */
    return;
  }
  set_irn_link(node, MARK);

  switch (get_irn_opcode(node)) {
  case iro_SymConst:
    /* externe Methode (wegen fix_symconst!) */
    eset_insert(methods, MARK); /* free method -> unknown */
    break;

  case iro_Const: {
    /* interne Methode */
    entity * ent = get_Const_tarval(node)->u.P.ent;
    assert(ent && is_method_type(get_entity_type(ent)));
    if (get_entity_visibility(ent) != external_allocated) {
      assert(get_entity_irg(ent));
      eset_insert(methods, ent);
    } else {
      eset_insert(methods, MARK); /* free method -> unknown */
    }
    break;
  }

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
    callee_ana_node(skip_nop(get_Call_ptr(call)), methods);
    if (eset_contains(methods, MARK)) { /* unknown method */
      ARR_APP1(entity *, arr, NULL);
    }
    for (ent = eset_first(methods); ent; ent = eset_next(methods)) {
      if (ent != MARK) {
	ARR_APP1(entity *, arr, ent);
      }
    }
    if (ARR_LEN(arr) == 0) {
      /* Kann vorkommen, wenn der Vorgänger beispielsweise eine
       * Sel-Operation war, die keine Methoden zurückgeben
       * konnte. Wir ersetzen die Call-Operation ebenfalls durch
       * eine Bad-Operation. Die Verlinkung muss wiederhergestellt
       * werden! */
      exchange(call, new_Bad());
    } else {
      set_Call_callee_arr(call, ARR_LEN(arr), arr);
    }
    DEL_ARR_F(arr);
    eset_destroy(methods);
  }
}


/* Bestimmt für jede Call-Operation die Menge der aufrufbaren Methode
 * und speichert das Ergebnis in der Call-Operation. (siehe
 * "set_Call_callee"). "sel_methods" wird für den Aufbau benötigt und
 * muss bereits aufgebaut sein. */
static void callee_ana(void) {
  int i;
  /* Alle Graphen analysieren. */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    irg_walk_graph(get_irp_irg(i), callee_walker, NULL, NULL);
  }
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
  assert(get_irn_mode(node) == mode_P);
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
    /* nothing: SymConst points to extern method */
    break;
  case iro_Const: {
    tarval * val = get_Const_tarval(node);
    entity * ent = val->u.P.ent;
    if (ent != NULL && is_method_type(get_entity_type(ent))) {
      eset_insert(set, ent);
    }
    break;
  }
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
      if (get_irn_mode(pred) == mode_P) {
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
      if (get_irn_mode(pred) == mode_P) {
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
static entity ** get_free_methods(void) {
  eset * set = eset_create();
  int i;
  entity ** arr = NEW_ARR_F(entity *, 0);
  entity * ent;
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    entity * ent = get_irg_ent(irg);
    /* insert "external visible" methods. */
    if (get_entity_visibility(ent) != local) {
      eset_insert(set, ent);
    }
    irg_walk_graph(irg, NULL, (irg_walk_func *) free_ana_walker, set);
  }
  /* Hauptprogramm ist auch frei, auch wenn es nicht "external
   * visible" ist. */
  if(get_irp_main_irg()) {
    eset_insert(set, get_irg_ent(get_irp_main_irg()));
  }
  for (ent = eset_first(set); ent; ent = eset_next(set)) {
    ARR_APP1(entity *, arr, ent);
  }
  eset_destroy(set);

  return arr;
}

void cgana(int *length, entity ***free_methods) {
  entity ** free_meths;
  int i;

  sel_methods_init();
  free_meths = get_free_methods();
  callee_ana();
  sel_methods_dispose();

  /* Convert the flexible array to an array that can be handled
     by standard C. */
  *length = ARR_LEN(free_meths);
  *free_methods = (entity **)malloc(sizeof(entity *) * (*length));
  for (i = 0; i < (*length); i++) (*free_methods)[i] = free_meths[i];
  DEL_ARR_F(free_meths);
}

/* Optimize the address expressions passed to call nodes.
 * Alle SymConst-Operationen, die auf interne Methoden verweisen,
 * werden durch Const-Operationen ersetzt.
 * Sel Knoten deren entitaeten nicht ueberschrieben werden, werden
 * durch Const ersetzt.
 * Sel Knoten, fuer die keine Implementierung existiert, werden
 * durch Bad ersetzt. */
void opt_call_addrs(void) {
  sel_methods_init();
  sel_methods_dispose();
#if 0
  int i;
  pmap * ldname_map = pmap_create();
  assert(entities == NULL);
  entities = eset_create();
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    entity * ent = get_irg_ent(get_irp_irg(i));
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
