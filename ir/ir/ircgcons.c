/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Auf- und Abbau der interprozeduralen Darstellung (Explizite
 * interprozedurale Abhängigkeiten).
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#include <string.h>
#include "ircgcons.h"

#include "array.h"
#include "irprog.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irflag.h"
#include "misc.h"


/* Datenstruktur für jede Methode */
typedef struct {
  int count;
  bool open;			  /* offene Methode (mit unbekanntem Aufrufer) */
  ir_node * reg, * mem, ** res;   /* EndReg, Mem und Rückgabewerte */
  ir_node * except, * except_mem; /* EndExcept und Mem für Ausnahmeabbruch */
} irg_data_t;


static irg_data_t * irg_data_create(void) {
  irg_data_t * data = xmalloc(sizeof(irg_data_t));
  memset(data, 0, sizeof(irg_data_t)); /* init */
  return data;
}


/* Die Anzahl der Aufrufer jeder Methode zählen (irg_data_t->count), und die
 * offenen Methoden (mit unbekannten Vorgänger) markieren. */
static void caller_init(int arr_length, entity ** free_methods) {
  int i, j;
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    set_entity_link(get_irg_ent(get_irp_irg(i)), irg_data_create());
  }
  for (i = arr_length - 1; i >= 0; --i) {
    irg_data_t * data = get_entity_link(free_methods[i]);
    data->open = true;
  }
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    ir_node * call;
    /* Die Call-Knoten sind (mit den Proj-Knoten) am End-Knoten verlinkt! */
    for (call = get_irn_link(get_irg_end(irg)); call; call = get_irn_link(call)) {
      if (get_irn_op(call) != op_Call) continue;
      for (j = get_Call_n_callees(call) - 1; j >= 0; --j) {
	entity * ent = get_Call_callee(call, j);
	if (ent) {
	  irg_data_t * data = get_entity_link(ent);
	  assert(get_entity_irg(ent) && data);
	  ++data->count;
	}
      }
    }
  }
}


static void clear_link(ir_node * node, void * env) {
  set_irn_link(node, NULL);
}


static inline ir_node * tail(ir_node * node) {
  ir_node * link;
  for (; (link = get_irn_link(node)); node = link) ;
  return node;
}


/* Call-Operationen an die "link"-Liste von "call_tail" anhängen (und
 * "call_tail" aktualisieren), Proj-Operationen in die Liste ihrer Definition
 * (auch bei Proj->Call Operationen) und Phi-Operationen in die Liste ihres
 * Grundblocks einfügen. */
static void collect_phicallproj_walker(ir_node * node, ir_node ** call_tail) {
  if (get_irn_op(node) == op_Call) {
    /* Die Liste von Call an call_tail anhängen. */
    ir_node * link;
    assert(get_irn_link(*call_tail) == NULL);
    set_irn_link(*call_tail, node);
    /* call_tail aktualisieren: */
    for (link = get_irn_link(*call_tail); link; *call_tail = link, link = get_irn_link(link)) ;
  } else if (get_irn_op(node) == op_Proj) {
    ir_node * head = skip_Proj(get_Proj_pred(node));
    set_irn_link(node, get_irn_link(head));
    set_irn_link(head, node);
    /* call_tail gegebenenfalls aktualisieren: */
    if (head == *call_tail) {
      *call_tail = node;
    }
  } else if (get_irn_op(node) == op_Phi) {
    ir_node * block = get_nodes_Block(node);
    set_irn_link(node, get_irn_link(block));
    set_irn_link(block, node);
  }
}


static void link(ir_node * head, ir_node * node) {
  if (node) {
    set_irn_link(node, get_irn_link(head));
    set_irn_link(head, node);
  }
}


/* Die Call-Operationen aller Graphen an den End-Operationen
 * verlinken, die Proj-Operationen an ihren Definitionen und die
 * Phi-Operationen an ihren Grundblöcken. Die Liste der Calls sieht
 * dann so aus: End -> Call -> Proj -> ... -> Proj -> Call -> Proj ->
 * ... -> Proj -> NULL. */
static void collect_phicallproj(void) {
  int i;
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    ir_node * start = get_irg_start(irg);
    ir_node * end = get_irg_end(irg);
    assert(irg && start);
    /* Die speziellen Parameter der Start-Operation extra verlinken,
     * auch wenn sie nicht im intraprozeduralen Graphen erreichbar
     * sind. */
    link(start, get_irg_frame(irg));
    link(start, get_irg_globals(irg));
    /* walk */
    irg_walk_graph(irg, clear_link, (irg_walk_func) collect_phicallproj_walker, &end);
  }
}


/* Proj-Operation durch Filter-Operation im aktuellen Block ersetzen. */
static ir_node * exchange_proj(ir_node * proj) {
  ir_node * filter;
  assert(get_irn_op(proj) == op_Proj);
  filter = new_Filter(get_Proj_pred(proj), get_irn_mode(proj), get_Proj_proj(proj));
  assert(get_Proj_proj(proj) == get_Filter_proj(filter)); /* XXX:SID */
  /* Die Proj- (Id-) Operation sollte im gleichen Grundblock stehen, wie die
   * Filter-Operation. */
  set_nodes_Block(proj, get_nodes_Block(filter));
  exchange(proj, filter);
  return filter;
}


/* Echt neue Block-Operation erzeugen. CSE abschalten! */
static ir_node * create_Block(int n, ir_node ** in) {
  /* Turn off optimizations so that blocks are not merged again. */
  int rem_opt = get_optimize();
  ir_node * block;
  set_optimize(0);
  block = new_Block(n, in);
  set_optimize(rem_opt);
  return block;
}


static void prepare_irg_end(ir_graph * irg, irg_data_t * data);
static void prepare_irg_end_except(ir_graph * irg, irg_data_t * data);


/* IRG vorbereiten. Proj-Operationen der Start-Operation in Filter-Operationen
 * umwandeln. Die künstlichen Steuerzusammenflüsse EndReg und EndExcept
 * einfügen. An der Start-Operation hängt nach dem Aufruf eine Liste der
 * entsprechenden Filter-Knoten. */
static void prepare_irg(ir_graph * irg, irg_data_t * data) {
  ir_node * start_block = get_irg_start_block(irg);
  ir_node * link, * proj;
  int n_callers = data->count + (data->open ? 1 : 0);
  ir_node ** in = NEW_ARR_F(ir_node *, n_callers);

  current_ir_graph = irg;
  set_irg_current_block(irg, start_block);

  /* Grundblock interprozedural machen. */
  /* "in" ist nicht initialisiert. Das passiert erst in "construct_start". */
  set_Block_cg_cfgpred_arr(start_block, n_callers, in);
  /* Proj-Operationen durch Filter-Operationen ersetzen und (sonst) in
   * den Start-Block verschieben. */
  for (proj = get_irn_link(get_irg_start(irg)); proj; proj = get_irn_link(proj)) {
    if (get_Proj_pred(proj) != get_irg_start(irg)
	|| (get_Proj_proj(proj) != pns_initial_exec && get_Proj_proj(proj) != pns_args)) {
      ir_node * filter = exchange_proj(proj);
      set_Filter_cg_pred_arr(filter, n_callers, in);
    } else {
      set_nodes_Block(proj, start_block);
    }
  }

  DEL_ARR_F(in);

  /* Liste der Filter-Operationen herstellen. Dabei muss man beachten,
   * dass oben für "verschiedene" Proj-Operationen wegen CSE nur eine
   * Filter-Operation erzeugt worden sein kann. */
  for (link = get_irg_start(irg), proj = get_irn_link(link); proj; proj = get_irn_link(proj)) {
    if (get_irn_op(proj) == op_Id) { /* replaced with filter */
      ir_node * filter = get_Id_pred(proj);
      assert(get_irn_op(filter) == op_Filter);
      if (filter != link && get_irn_link(filter) == NULL) {
	set_irn_link(link, filter);
	link = filter;
      }
    }
  }
  /* Globle Einträge für ersetzte Operationen korrigieren. */
  set_irg_frame(irg, skip_nop(get_irg_frame(irg)));
  set_irg_globals(irg, skip_nop(get_irg_globals(irg)));

  /* Unbekannten Aufrufer sofort eintragen. */
  if (data->open) {
    set_Block_cg_cfgpred(start_block, 0, new_Unknown());
    for (proj = get_irn_link(get_irg_start(irg)); proj; proj = get_irn_link(proj)) {
      if (get_irn_op(proj) == op_Filter) {
	set_Filter_cg_pred(proj, 0, new_Unknown());
      }
    }
    data->count = 1;
  } else {
    data->count = 0;
  }

  prepare_irg_end(irg, data);
  prepare_irg_end_except(irg, data);
}


/* Künstlicher Steuerzusammenfluss EndReg einfügen. */
static void prepare_irg_end(ir_graph * irg, irg_data_t * data) {
  ir_node * end_block = get_irg_end_block(irg);
  ir_node * end  = get_irg_end(irg);
  ir_node ** ret_arr = NULL;
  int i, j;
  int n_ret = 0;
  ir_node ** cfgpred_arr = get_Block_cfgpred_arr(end_block);
  for (i = get_Block_n_cfgpreds(end_block) - 1; i >= 0; --i) {
    if (get_irn_op(cfgpred_arr[i]) == op_Return) {
      if (ret_arr) {
	ARR_APP1(ir_node *, ret_arr, cfgpred_arr[i]);
      } else {
	ret_arr = NEW_ARR_F(ir_node *, 1);
	ret_arr[0] = cfgpred_arr[i];
      }
      ++n_ret;
    }
  }
  if (n_ret > 0) {
    int n_res = get_method_n_res(get_entity_type(get_irg_ent(irg)));
    ir_node ** in = NEW_ARR_F(ir_node *, n_ret);
    /* block */
    for (i = n_ret - 1; i >= 0; --i) {
      set_irg_current_block(irg, get_nodes_Block(ret_arr[i]));
      in[i] = new_Jmp();
    }
    create_Block(n_ret, in);
    /* end */
    data->reg = new_EndReg();
    /* mem */
    for (i = n_ret - 1; i >= 0; --i) {
      in[i] = get_Return_mem(ret_arr[i]);
    }
    data->mem = new_Phi(n_ret, in, mode_M);
    /* This Phi is a merge, therefor needs not be kept alive.
       It might be optimized away, though.  */
    if (get_End_keepalive(end, get_End_n_keepalives(end)-1 ) == data->mem)
      set_End_keepalive(end, get_End_n_keepalives(end)-1, new_Bad());
    /* res */
    data->res = NEW_ARR_F(ir_node *, n_res);
    for (j = n_res - 1; j >= 0; --j) {
      ir_mode *mode = NULL;
      /* In[0] could be a Bad node with wrong mode. */
      for (i = n_ret - 1; i >= 0; --i) {
	in[i] = get_Return_res(ret_arr[i], j);
	if (!mode && get_irn_mode(in[i]) != mode_T)
	  mode = get_irn_mode(in[i]);
      }
      if (mode)
	data->res[j] = new_Phi(n_ret, in, mode);
      else  /* All preds are Bad */
	data->res[j] = new_Bad();
    }
    DEL_ARR_F(in);
  }
  if (ret_arr) DEL_ARR_F(ret_arr);
}


/* Künstlicher Steuerzusammenfluss EndExcept einfügen. */
static void prepare_irg_end_except(ir_graph * irg, irg_data_t * data) {
  ir_node * end_block = get_irg_end_block(irg);
  ir_node * end = get_irg_end(irg);
  ir_node ** except_arr = NULL;
  int i;
  int n_except = 0;
  ir_node ** cfgpred_arr = get_Block_cfgpred_arr(end_block);
  for (i = get_Block_n_cfgpreds(end_block) - 1; i >= 0; --i) {
    if (get_irn_op(cfgpred_arr[i]) != op_Return) {
      if (except_arr) {
	ARR_APP1(ir_node *, except_arr, cfgpred_arr[i]);
      } else {
	except_arr = NEW_ARR_F(ir_node *, 1);
	except_arr[0] = cfgpred_arr[i];
      }
      ++n_except;
    }
  }
  if (n_except > 0) {
    ir_node ** in = NEW_ARR_F(ir_node *, n_except);
    /* block */
    create_Block(n_except, except_arr);
    /* end_except */
    data->except = new_EndExcept();
    /* mem */
    for (i = n_except - 1; i >= 0; --i) {
      ir_node * node = skip_Proj(except_arr[i]);
      if (get_irn_op(node) == op_Call) {
	in[i] = new_r_Proj(irg, get_nodes_Block(node), node, mode_M, 3);
      } else if (get_irn_op(node) == op_Raise) {
	in[i] = new_r_Proj(irg, get_nodes_Block(node), node, mode_M, 1);
      } else {
	assert(is_fragile_op(node));
	/* We rely that all cfops have the memory output at the same position. */
	in[i] = new_r_Proj(irg, get_nodes_Block(node), node, mode_M, 0);
      }
    }
    data->except_mem = new_Phi(n_except, in, mode_M);
    /* This Phi is a merge, therefor needs not be kept alive.
       It might be optimized away, though.  */
    if (get_End_keepalive(end, get_End_n_keepalives(end)-1 )
	== data->except_mem)
      set_End_keepalive(end, get_End_n_keepalives(end)-1, new_Bad());
    DEL_ARR_F(in);
  }
  if (except_arr) DEL_ARR_F(except_arr);
}


/* Zwischengespeicherte Daten wieder freigeben. */
static void cleanup_irg(ir_graph * irg) {
  entity * ent = get_irg_ent(irg);
  irg_data_t * data = get_entity_link(ent);
  assert(data);
  if (data->res) DEL_ARR_F(data->res);
  set_entity_link(ent, NULL);
  free(data);
}


/* Alle Phi-Operationen aus "from_block" nach "to_block"
 * verschieben. Die Phi-Operationen müssen am zugehörigen Grundblock
 * verlinkt sein. Danach sind sie am neuen Grundblock verlinkt. */
static void move_phis(ir_node * from_block, ir_node * to_block) {
  ir_node * phi;
  for (phi = get_irn_link(from_block); phi != NULL; phi = get_irn_link(phi)) {
    set_nodes_Block(phi, to_block);
  }
  assert(get_irn_link(to_block) == NULL);
  set_irn_link(to_block, get_irn_link(from_block));
  set_irn_link(from_block, NULL);
}


/* Rekursiv die Operation "node" und alle ihre Vorgänger aus dem Block
 * "from_block" nach "to_block" verschieben. */
static void move_nodes(ir_node * from_block, ir_node * to_block, ir_node * node) {
  int i;
  for (i = get_irn_arity(node) - 1; i >= 0; --i) {
    ir_node * pred = get_irn_n(node, i);
    if (get_nodes_Block(pred) == from_block) {
      move_nodes(from_block, to_block, pred);
    }
  }
  set_nodes_Block(node, to_block);
}


/* Abhängigkeiten vom Start-Block und den Filter-Operationen im
 * Start-Block auf den Aufrufer hinzufügen. */
static void construct_start(entity * caller, entity * callee,
			    ir_node * call, ir_node * exec) {
  irg_data_t * data = get_entity_link(callee);
  ir_graph * irg = get_entity_irg(callee);
  ir_node * start = get_irg_start(irg), * filter;

  assert(irg);
  assert(get_entity_peculiarity(callee) == existent); /* Else data is not initalized. */
  assert((0 <= data->count) &&
	 (data->count < get_Block_cg_n_cfgpreds(get_nodes_Block(start))));
  set_Block_cg_cfgpred(get_nodes_Block(start), data->count, exec);
  for (filter = get_irn_link(start); filter; filter = get_irn_link(filter)) {
    if (get_irn_op(filter) != op_Filter) continue;
    if (get_Proj_pred(filter) == start) {
      switch ((int) get_Proj_proj(filter)) {
      case pns_global_store:
	set_Filter_cg_pred(filter, data->count, get_Call_mem(call));
	break;
      case pns_frame_base:
	/* "frame_base" wird nur durch Unknown dargestellt. Man kann ihn aber
	 * auch explizit darstellen, wenn sich daraus Vorteile für die
	 * Datenflussanalyse ergeben. */
	set_Filter_cg_pred(filter, data->count, new_Unknown());
	break;
      case pns_globals:
	/* "globals" wird nur durch Unknown dargestellt. Man kann ihn aber auch
	 * explizit darstellen, wenn sich daraus Vorteile für die
	 * Datenflussanalyse ergeben. */
	set_Filter_cg_pred(filter, data->count, new_Unknown());
	break;
      default:
	/* not reached */
	assert(0 && "not reached");
	break;
      }
    } else {
      set_Filter_cg_pred(filter, data->count, get_Call_param(call, get_Proj_proj(filter)));
    }
  }
  ++data->count;
}


/* Abhängigkeiten für den Speicherzustand über alle aufgerufenen
 * Methoden bestimmen. */
static void fill_mem(int length, irg_data_t * data[], ir_node * in[]) {
  int i;
  for (i = 0; i < length; ++i) {
    if (data[i]) { /* explicit */
      if (data[i]->reg) {
	in[i] = data[i]->mem;
      } else {
	in[i] = new_Bad();
      }
    } else { /* unknown */
      in[i] = new_Unknown();
    }
  }
}


/* Abhängigkeiten für den Ausnahme-Speicherzustand über alle
 * aufgerufenen Methoden bestimmen. */
static void fill_except_mem(int length, irg_data_t * data[], ir_node * in[]) {
  int i;
  for (i = 0; i < length; ++i) {
    if (data[i]) { /* explicit */
      if (data[i]->except) {
	in[i] = data[i]->except_mem;
      } else {
	in[i] = new_Bad();
      }
    } else { /* unknown */
      in[i] = new_Unknown();
    }
  }
}


/* Abhängigkeiten für ein Ergebnis über alle aufgerufenen Methoden
 * bestimmen. */
static void fill_result(int pos, int length, irg_data_t * data[], ir_node * in[]) {
  int i;
  for (i = 0; i < length; ++i) {
    if (data[i]) { /* explicit */
      if (data[i]->reg) {
	in[i] = data[i]->res[pos];
      } else {
	in[i] = new_Bad();
      }
    } else { /* unknown */
      in[i] = new_Unknown();
    }
  }
}


/* Proj auf Except-X einer Call-Operation (aus der Link-Liste) bestimmen. */
static ir_node * get_except(ir_node * call) {
  /* Mit CSE könnte man das effizienter machen! Die Methode wird aber für jede
   * Aufrufstelle nur ein einziges Mal aufgerufen. */
  ir_node * proj;
  for (proj = get_irn_link(call); proj && get_irn_op(proj) == op_Proj; proj = get_irn_link(proj)) {
    if (get_Proj_proj(proj) == 1 && get_irn_op(get_Proj_pred(proj)) == op_Call) {
      return proj;
    }
  }
  return NULL;
}


/* Grundblock der Call-Operation aufteilen. CallBegin- und Filter-Operationen
 * einfügen. Die Steuer- und Datenflussabhängigkeiten von den aufgerufenen
 * Methoden auf die CallBegin-Operation, und von der Aufrufstelle auf die
 * aufgerufenen Methoden eintragen. */
static void construct_call(ir_node * call) {
  int n_callees = get_Call_n_callees(call);
  ir_node * post_block = get_nodes_Block(call); /* block nach dem Aufruf */
  ir_node * pre_block = create_Block(get_Block_n_cfgpreds(post_block),
				     get_Block_cfgpred_arr(post_block)); /* block vor dem Aufruf (mit CallBegin) */
  ir_node * except_block = NULL, * proj;
  ir_node * jmp = new_Break(); /* Sprung für intraprozedurale Darstellung (in
				* pre_block) */
  ir_node * call_begin = new_CallBegin(call); /* (in pre_block) */
  ir_node ** in = NEW_ARR_F(ir_node *, n_callees);
  entity * caller = get_irg_ent(current_ir_graph); /* entity des aktuellen ir_graph */
  entity ** callees = NEW_ARR_F(entity *, n_callees); /* aufgerufene Methoden: entity */
  ir_graph ** irgs = NEW_ARR_F(ir_graph *, n_callees); /* aufgerufene Methoden: ir_graph */
  irg_data_t ** data = NEW_ARR_F(irg_data_t *, n_callees); /* aufgerufene Methoden: irg_data_t */
  int i;

  /* post_block kann bereits interprozedurale Steuerflussvorgänger
   * besitzen. Diese müssen dann auch noch für den pre_block gesetzt werden. */
  if (get_Block_cg_cfgpred_arr(post_block)) {
    set_Block_cg_cfgpred_arr(pre_block, get_Block_cg_n_cfgpreds(post_block),
			     get_Block_cg_cfgpred_arr(post_block));
    remove_Block_cg_cfgpred_arr(post_block);
  }

  /* Operationen verschieben */
  move_phis(post_block, pre_block);
  move_nodes(post_block, pre_block, call);
  /* @@@ GL Wer setzt die Laenge des PostBlock cgfpred array auf 1? */
  set_irn_in(post_block, 1, &jmp);

  /* Wiederverwendete Daten initialisieren. */
  for (i = 0; i < n_callees; ++i) {
    callees[i] = get_Call_callee(call, i);
    irgs[i] = callees[i] ? get_entity_irg(callees[i]) : NULL;
    data[i] = get_entity_link(callees[i]);
  }

  /* Die interprozeduralen Steuerflussvorgänger des post_block
   * bestimmen. */
  for (i = 0; i < n_callees; ++i) {
    if (data[i]) { /* explicit */
      if (data[i]->reg) {
	in[i] = new_r_Proj(irgs[i], get_nodes_Block(data[i]->reg),
			   data[i]->reg, mode_X, data[i]->count);
      } else {
	in[i] = new_Bad();
      }
    } else { /* unknown */
      in[i] = new_Unknown();
    }
  }
  set_Block_cg_cfgpred_arr(post_block, n_callees, in);

  /* Die interprozeduralen Steuerflussvorgänger des except_block
   * bestimmen. */
  if ((proj = get_except(call)) != NULL) {
    except_block = create_Block(1, &proj);
    set_nodes_Block(proj, except_block);
    exchange(proj, new_Break());
    set_irg_current_block(current_ir_graph, pre_block);
    set_irn_n(except_block, 0, new_Proj(call, mode_X, 1));
    set_irg_current_block(current_ir_graph, post_block);
    for (i = 0; i < n_callees; ++i) {
      entity * callee = get_Call_callee(call, i);
      if (data[i]) { /* explicit */
	if (data[i]->except) {
	  in[i] = new_r_Proj(get_entity_irg(callee), get_nodes_Block(data[i]->except),
			     data[i]->except, mode_X, data[i]->count);
	} else {
	  in[i] = new_Bad();
	}
      } else { /* unknown */
	in[i] = new_Unknown();
      }
    }
    set_Block_cg_cfgpred_arr(except_block, n_callees, in);
  }

  /* Diesen Vorgänger in den Start-Blöcken der aufgerufenen Methoden
   * eintragen. */
  set_irg_current_block(current_ir_graph, pre_block);
  for (i = 0; i < n_callees; ++i) {
    construct_start(caller, callees[i], call, new_Proj(call_begin, mode_X, i));
  }

  /* Proj-Operationen in Filter-Operationen umwandeln und
   * interprozedurale Vorgänger einfügen. */
  set_irg_current_block(current_ir_graph, post_block);
  for (proj = get_irn_link(call); proj && get_irn_op(proj) == op_Proj; proj = get_irn_link(proj)) {
    if (get_Proj_pred(proj) == call) {
      if (get_Proj_proj(proj) == 0) { /* memory */
	/* memory */
	ir_node * filter = exchange_proj(proj);
	/* filter in die Liste der Phis aufnehmen */
	if (get_irn_link(filter) == NULL) { /* note CSE */
	  set_irn_link(filter, get_irn_link(post_block));
	  set_irn_link(post_block, filter);
	}
	fill_mem(n_callees, data, in);
	set_Filter_cg_pred_arr(filter, n_callees, in);
      } else if (get_Proj_proj(proj) == 1) { /* except */
	/* nothing: siehe oben */
      } else if (get_Proj_proj(proj) == 2) { /* results */
	set_nodes_Block(proj, pre_block);
      } else if (get_Proj_proj(proj) == 3) { /* except_mem */
	/* except_mem */
	ir_node * filter;
	assert(except_block);
	set_irg_current_block(current_ir_graph, except_block);
	filter = exchange_proj(proj);
	/* filter in die Liste der Phis aufnehmen */
	if (get_irn_link(filter) == NULL) { /* note CSE */
	  set_irn_link(filter, get_irn_link(except_block));
	  set_irn_link(except_block, filter);
	}
	set_irg_current_block(current_ir_graph, post_block);
	fill_except_mem(n_callees, data, in);
	set_Filter_cg_pred_arr(filter, n_callees, in);
      } else {
	assert(0 && "not reached");
      }
    } else {
      /* result */
      ir_node * filter = exchange_proj(proj);
      /* filter in die Liste der Phis aufnehmen */
      if (get_irn_link(filter) == NULL) { /* note CSE */
	set_irn_link(filter, get_irn_link(post_block));
	set_irn_link(post_block, filter);
      }
      fill_result(get_Proj_proj(filter), n_callees, data, in);
      set_Filter_cg_pred_arr(filter, n_callees, in);
    }
  }
  DEL_ARR_F(in);
  DEL_ARR_F(callees);
  DEL_ARR_F(irgs);
  DEL_ARR_F(data);
}


void cg_construct(int arr_len, entity ** free_methods_arr) {
  int i;

  collect_phicallproj();

  /* count callers */
  caller_init(arr_len, free_methods_arr);

  /* prepare irgs */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    entity * ent = get_irg_ent(irg);
    irg_data_t * data = get_entity_link(ent);
    if (data->count) {
      prepare_irg(irg, data);
    } else if (data->open) {
      /* Die Methode wird nur von "der" unbekannten Aufrufstelle
       * aufgerufen. Darstellung wird für diese Methode nicht
       * geändert. */
    } else {
      /* Methode kann nicht aufgerufen werden. Die Darstellung wird
       * für diese Methode nicht geändert. Das kann nicht vorkommen,
       * wenn zuvor "gc_irgs()" aufgerufen wurde. */
    }
  }

  /* construct calls */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_node * node;
    current_ir_graph = get_irp_irg(i);
    for (node = get_irn_link(get_irg_end(current_ir_graph)); node; node = get_irn_link(node)) {
      if (get_irn_op(node) == op_Call) {
	int n_callees = get_Call_n_callees(node);
	if (n_callees > 1 || (n_callees == 1 && get_Call_callee(node, 0) != NULL)) {
	  construct_call(node);
	}
      }
    }
  }

  /* cleanup irgs: Abschlussarbeiten: Die Vorgänger der Methoden noch
   * explizit setzen und die zwischengespeicherten Daten wieder
   * freigeben. */
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    cleanup_irg(get_irp_irg(i));
  }
}



static void destruct_walker(ir_node * node, void * env) {
  if (get_irn_op(node) == op_Block) {
    remove_Block_cg_cfgpred_arr(node);
  } else if (get_irn_op(node) == op_Filter) {
    set_irg_current_block(current_ir_graph, get_nodes_Block(node));
    exchange(node, new_Proj(get_Filter_pred(node), get_irn_mode(node), get_Filter_proj(node)));
  } else if (get_irn_op(node) == op_Break) {
    set_irg_current_block(current_ir_graph, get_nodes_Block(node));
    exchange(node, new_Jmp());
  } else if (get_irn_op(node) == op_Call) {
    remove_Call_callee_arr(node);
  }
}


void cg_destruct(void) {
  int i;
  for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
    ir_graph * irg = get_irp_irg(i);
    irg_walk_graph(irg, destruct_walker, clear_link, NULL);
    set_irg_frame(irg, skip_nop(get_irg_frame(irg)));
    set_irg_globals(irg, skip_nop(get_irg_globals(irg)));
  }
}
