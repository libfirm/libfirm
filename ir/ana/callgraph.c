/*
 * Project:     libFIRM
 * File name:   ir/ana/callgraph.c
 * Purpose:     Representation and computation of the callgraph.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     21.7.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <stdlib.h>

#include "callgraph.h"

#include "irloop_t.h"
#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"

#include "cgana.h"

#include "array.h"
#include "pmap.h"

#include "irgwalk.h"

/* For callees, we want to remember the Call nodes, too. */
struct ana_entry {
  ir_graph *irg;
  ir_node  *call_list;
  int       max_depth;
};

typedef struct ana_entry ana_entry;

static int master_cg_visited = 0;
static INLINE int  cg_irg_visited     (ir_graph *n);
static INLINE void mark_cg_irg_visited(ir_graph *n);
static INLINE void set_cg_irg_visited (ir_graph *n, int i);

irp_callgraph_state get_irp_callgraph_state(void) {
  return irp->callgraph_state;
}
void                set_irp_callgraph_state(irp_callgraph_state s) {
  irp->callgraph_state = s;
}

/* The functions that call irg. */
int       get_irg_n_callers(ir_graph *irg) {
  if (irg->callers) return ARR_LEN(irg->callers);
  return -1;
}
ir_graph *get_irg_caller(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callers(irg));
  if (irg->callers) return irg->callers[pos];
  return NULL;
}
int       is_irg_caller_backedge(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callers(irg));
  return  irg->caller_isbe[pos];
}

static void       set_irg_caller_backedge(ir_graph *irg, ir_graph *caller) {
  int i, n_callers = get_irg_n_callers(irg);
  for (i = 0; i < n_callers; ++i) {
    if (get_irg_caller(irg, i) == caller) {
      irg->caller_isbe[i] = 1;
      break;
    }
  }
}

int       has_irg_caller_backedge(ir_graph *irg) {
  int i, n_callers = get_irg_n_callers(irg);
  for (i = 0; i < n_callers; ++i)
    if (is_irg_caller_backedge(irg, i)) return 1;
  return 0;
}

int get_irg_caller_loop_depth(ir_graph *irg, int pos) {
  ir_graph *caller = get_irg_caller(irg, pos);

  /* search the other relation for the corresponding edge. */
  int pos_callee = -1;
  int i, n_callees = get_irg_n_callees(caller);
  for (i = 0; i < n_callees; ++i) {
    if (get_irg_callee(caller, i) == irg) {
      pos_callee = i;
      break;
    }
  }

  assert(pos_callee >= 0);

  return get_irg_callee_loop_depth(caller, pos_callee);
}


/* The functions called by irg. */
int       get_irg_n_callees(ir_graph *irg) {
  if (irg->callees) return ARR_LEN(irg->callees);
  return -1;
}
ir_graph *get_irg_callee(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  if (irg->callees) return ((ana_entry *)irg->callees[pos])->irg;
  return NULL;
}
int       is_irg_callee_backedge(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  return irg->callee_isbe[pos];
}
int       has_irg_callee_backedge(ir_graph *irg) {
  int i, n_callees = get_irg_n_callees(irg);
  for (i = 0; i < n_callees; ++i)
    if (is_irg_callee_backedge(irg, i)) return 1;
  return 0;
}
void       set_irg_callee_backedge(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  irg->callee_isbe[pos] = 1;
}

int get_irg_callee_loop_depth(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  if (irg->callees) return ((ana_entry *)irg->callees[pos])->max_depth;
  return -1;
}

/* --------------------- Compute the callgraph ------------------------ */

/* Hash an address */
#define HASH_ADDRESS(adr)       (((unsigned)(adr)) >> 3)

/**
 * Walker called by compute_callgraph()
 */
static void ana_Call(ir_node *n, void *env) {
  int i, n_callees;
  ir_graph *irg;

  if (get_irn_op(n) != op_Call) return;

  irg = get_irn_irg(n);
  n_callees = get_Call_n_callees(n);
  for (i = 0; i < n_callees; ++i) {
    entity *callee_e = get_Call_callee(n, i);
    ir_graph *callee = get_entity_irg(callee_e);
    if (callee) {  /* For unknown caller */
      pset_insert((pset *)callee->callers, irg, (unsigned)irg >> 3);
      ana_entry buf = { callee, NULL, 0};
      ana_entry *found = pset_find((pset *)irg->callees, &buf, HASH_ADDRESS(callee));
      if (found) {  /* add Call node to list, compute new nesting. */
      } else { /* New node, add Call node and init nesting. */
	found = (ana_entry *) obstack_alloc (irg->obst, sizeof (ana_entry));
	found->irg = callee;
	found->call_list = NULL;
	found->max_depth = 0;
	pset_insert((pset *)irg->callees, found, HASH_ADDRESS(callee));
      }
      int depth = get_loop_depth(get_irn_loop(get_nodes_block(n)));
      found->max_depth = (depth > found->max_depth) ? depth : found->max_depth;
    }
  }
}

/** compare two ir graphs in a ana_entry */
static int ana_entry_cmp(const void *elt, const void *key) {
  const ana_entry *e1 = elt;
  const ana_entry *e2 = key;
  return e1->irg != e2->irg;
}

/** compare two ir graphs */
static int graph_cmp(const void *elt, const void *key) {
  return elt != key;
}


/* Construct and destruct the callgraph. */
void compute_callgraph(void) {
  int i, n_irgs = get_irp_n_irgs();

  assert(! get_interprocedural_view());  /* Else walking will not reach the Call nodes. */

  /* initialize */
  free_callgraph();
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
    assert(get_irg_callee_info_state(irg) == irg_callee_info_consistent);
    irg->callees = (ir_graph **)new_pset(ana_entry_cmp, 8);
    irg->callers = (ir_graph **)new_pset(graph_cmp, 8);
    construct_cf_backedges(irg);
  }

  /* Compute the call graph */
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
    construct_cf_backedges(irg);
    irg_walk_graph(irg, ana_Call, NULL, NULL);
  }

  /* Change the sets to arrays. */
  for (i = 0; i < n_irgs; ++i) {
    int j, count;
    ir_graph *c, *irg = get_irp_irg(i);

    pset *callee_set = (pset *)irg->callees;
    count = pset_count(callee_set);
    irg->callees = NEW_ARR_F(ir_graph *, count);
    irg->callee_isbe = calloc(count, sizeof(int));
    c = pset_first(callee_set);
    for (j = 0; j < count; ++j) {
      irg->callees[j] = c;
      c = pset_next(callee_set);
    }
    del_pset(callee_set);
    assert(c == NULL);

    pset *caller_set = (pset *)irg->callers;
    count = pset_count(caller_set);
    irg->callers = NEW_ARR_F(ir_graph *, count);
    irg->caller_isbe =  calloc(count, sizeof(int));
    c = pset_first(caller_set);
    for (j = 0; j < count; ++j) {
      irg->callers[j] = c;
      c = pset_next(caller_set);
    }
    del_pset(caller_set);
    assert(c == NULL);
  }
  set_irp_callgraph_state(irp_callgraph_consistent);
}

void free_callgraph(void) {
  int i, n_irgs = get_irp_n_irgs();
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
    if (irg->callees) DEL_ARR_F(irg->callees);
    if (irg->callers) DEL_ARR_F(irg->callers);
    if (irg->callee_isbe) free(irg->callee_isbe);
    if (irg->caller_isbe) free(irg->caller_isbe);
    irg->callees = NULL;
    irg->callers = NULL;
    irg->callee_isbe = NULL;
    irg->caller_isbe = NULL;
  }
  set_irp_callgraph_state(irp_callgraph_none);
}

/* ----------------------------------------------------------------------------------- */
/* A walker for the callgraph                                                          */
/* ----------------------------------------------------------------------------------- */


static void do_walk(ir_graph *irg, callgraph_walk_func *pre, callgraph_walk_func *post, void *env) {
  int i, n_callees;

  if (cg_irg_visited(irg)) return;
  mark_cg_irg_visited(irg);

  pre(irg, env);

  n_callees = get_irg_n_callees(irg);
  for (i = 0; i < n_callees; i++) {
    ir_graph *m = get_irg_callee(irg, i);
    do_walk(m, pre, post, env);
  }

  post(irg, env);
}

void callgraph_walk(callgraph_walk_func *pre, callgraph_walk_func *post, void *env) {
  int i, n_irgs = get_irp_n_irgs();
  master_cg_visited++;

  do_walk(get_irp_main_irg(), pre, post, env);
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if (!cg_irg_visited(irg) && get_irg_n_callers(irg) == 0)
      do_walk(irg, pre, post, env);
  }
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if (!cg_irg_visited(irg))
      do_walk(irg, pre, post, env);
  }
}

/* ----------------------------------------------------------------------------------- */
/* loop construction algorithm                                                         */
/* ----------------------------------------------------------------------------------- */

static ir_graph *outermost_ir_graph;      /* The outermost graph the scc is computed
                      for */
static ir_loop *current_loop;      /* Current cfloop construction is working
                      on. */
static int loop_node_cnt = 0;      /* Counts the number of allocated cfloop nodes.
                      Each cfloop node gets a unique number.
                      What for? ev. remove. @@@ */
static int current_dfn = 1;        /* Counter to generate depth first numbering
                      of visited nodes.  */


/**********************************************************************/
/* Node attributes                                                   **/
/**********************************************************************/

typedef struct scc_info {
  bool in_stack;         /* Marks whether node is on the stack. */
  int dfn;               /* Depth first search number. */
  int uplink;            /* dfn number of ancestor. */
  int visited;
} scc_info;

static INLINE scc_info* new_scc_info(void) {
  scc_info *info = obstack_alloc (outermost_ir_graph->obst, sizeof (scc_info));
  memset (info, 0, sizeof (scc_info));
  return info;
}

static INLINE int
cg_irg_visited(ir_graph *n) {
  return (((scc_info *)n->link)->visited >= master_cg_visited);
}

static INLINE void
mark_cg_irg_visited(ir_graph *n) {
  ((scc_info *)n->link)->visited = master_cg_visited;
}

static INLINE void
set_cg_irg_visited(ir_graph *n, int i) {
  ((scc_info *)n->link)->visited = i;
}

static INLINE int
get_cg_irg_visited(ir_graph *n) {
  return ((scc_info *)n->link)->visited;
}

static INLINE void
mark_irg_in_stack (ir_graph *n) {
  assert(get_irg_link(n));
  ((scc_info *)n->link)->in_stack = true;
}

static INLINE void
mark_irg_not_in_stack (ir_graph *n) {
  assert(get_irg_link(n));
  ((scc_info *)n->link)->in_stack = false;
}

static INLINE bool
irg_is_in_stack (ir_graph *n) {
  assert(get_irg_link(n));
  return ((scc_info *)n->link)->in_stack;
}

static INLINE void
set_irg_uplink (ir_graph *n, int uplink) {
  assert(get_irg_link(n));
  ((scc_info *)n->link)->uplink = uplink;
}

static INLINE int
get_irg_uplink (ir_graph *n) {
  assert(get_irg_link(n));
  return ((scc_info *)n->link)->uplink;
}

static INLINE void
set_irg_dfn (ir_graph *n, int dfn) {
  assert(get_irg_link(n));
  ((scc_info *)n->link)->dfn = dfn;
}

static INLINE int
get_irg_dfn (ir_graph *n) {
  assert(get_irg_link(n));
  return ((scc_info *)n->link)->dfn;
}

/**********************************************************************/
/* A stack.                                                          **/
/**********************************************************************/

static ir_graph **stack = NULL;
static int tos = 0;                /* top of stack */

static INLINE void init_stack(void) {
  if (stack) {
    ARR_RESIZE (ir_graph *, stack, 1000);
  } else {
    stack = NEW_ARR_F (ir_graph *, 1000);
  }
  tos = 0;
}


static INLINE void
push (ir_graph *n)
{
  if (tos == ARR_LEN (stack)) {
    int nlen = ARR_LEN (stack) * 2;
    ARR_RESIZE (ir_node *, stack, nlen);
  }
  stack [tos++] = n;
  mark_irg_in_stack(n);
}

static INLINE ir_graph *
pop (void)
{
  ir_graph *n = stack[--tos];
  mark_irg_not_in_stack(n);
  return n;
}

/* The nodes up to n belong to the current loop.
   Removes them from the stack and adds them to the current loop. */
static INLINE void
pop_scc_to_loop (ir_graph *n)
{
  ir_graph *m;

  do {
    m = pop();
    loop_node_cnt++;
    set_irg_dfn(m, loop_node_cnt);
    add_loop_node(current_loop, (ir_node *)m);
    m->l = current_loop;
    //m->callgraph_loop_depth = current_loop->depth;
  } while(m != n);
}

/* GL ??? my last son is my grandson???  Removes cfloops with no
   ir_nodes in them.  Such loops have only another loop as son. (Why
   can't they have two loops as sons? Does it never get that far? ) */
static void close_loop (ir_loop *l)
{
  int last = get_loop_n_elements(l) - 1;
  loop_element lelement = get_loop_element(l, last);
  ir_loop *last_son = lelement.son;

  if (get_kind(last_son) == k_ir_loop &&
      get_loop_n_elements(last_son) == 1) {
    ir_loop *gson;

    lelement = get_loop_element(last_son, 0);
    gson = lelement.son;
    if(get_kind(gson) == k_ir_loop) {
      loop_element new_last_son;

      gson -> outer_loop = l;
      new_last_son.son = gson;
      l -> children[last] = new_last_son;
    }
  }

  current_loop = l;
}

/* Removes and unmarks all nodes up to n from the stack.
   The nodes must be visited once more to assign them to a scc. */
static INLINE void
pop_scc_unmark_visit (ir_graph *n)
{
  ir_graph *m = NULL;

  while (m != n) {
    m = pop();
    set_cg_irg_visited(m, 0);
  }
}

/**********************************************************************/
/* The loop datastructure.                                           **/
/**********************************************************************/

/* Allocates a new loop as son of current_loop.  Sets current_loop
   to the new loop and returns the father. */
static ir_loop *new_loop (void) {
  ir_loop *father, *son;

  father = current_loop;

  son = (ir_loop *) obstack_alloc (outermost_ir_graph->obst, sizeof (ir_loop));
  memset (son, 0, sizeof (ir_loop));
  son->kind = k_ir_loop;
  son->children = NEW_ARR_F (loop_element, 0);
  son->n_nodes = 0;
  son->n_sons = 0;
  if (father) {
    son->outer_loop = father;
    add_loop_son(father, son);
    son->depth = father->depth+1;
  } else {  /* The root loop */
    son->outer_loop = son;
    son->depth = 0;
  }

#ifdef DEBUG_libfirm
  son->loop_nr = get_irp_new_node_nr();
  son->link = NULL;
#endif

  current_loop = son;
  return father;
}

/**********************************************************************/
/* Constructing and destructing the loop/backedge information.       **/
/**********************************************************************/

/* Initialization steps. **********************************************/

static INLINE void
init_scc (void) {
  int i;
  current_dfn = 1;
  loop_node_cnt = 0;
  init_stack();
  for (i = 0; i < get_irp_n_irgs(); ++i) {
    set_irg_link(get_irp_irg(i), new_scc_info());
    get_irp_irg(i)->callgraph_recursion_depth = 0;
    get_irp_irg(i)->callgraph_loop_depth = 0;
  }
}

/** Returns true if n is a loop header, i.e., it is a Block node
 *  and has predecessors within the cfloop and out of the cfloop.
 *
 *  @param root: only needed for assertion.
 */
static bool
is_head (ir_graph *n, ir_graph *root)
{
  int i, arity;
  int some_outof_loop = 0, some_in_loop = 0;

  arity = get_irg_n_callees(n);
  for (i = 0; i < arity; i++) {
    ir_graph *pred = get_irg_callee(n, i);
    if (is_irg_callee_backedge(n, i)) continue;
    if (!irg_is_in_stack(pred)) {
      some_outof_loop = 1;
    } else {
      if (get_irg_uplink(pred) < get_irg_uplink(root))  {
	DDMG(pred); DDMG(root);
	assert(get_irg_uplink(pred) >= get_irg_uplink(root));
      }
      some_in_loop = 1;
    }
  }

  return some_outof_loop && some_in_loop;
}
/* Returns true if n is possible loop head of an endless loop.
   I.e., it is a Block, Phi or Filter node and has only predecessors
   within the loop.
   @arg root: only needed for assertion. */
static bool
is_endless_head (ir_graph *n, ir_graph *root)
{
  int i, arity;
  int some_outof_loop = 0, some_in_loop = 0;

  arity = get_irg_n_callees(n);
  for (i = 0; i < arity; i++) {
    ir_graph *pred = get_irg_callee(n, i);
    assert(pred);
    if (is_irg_callee_backedge(n, i)) { continue; }
    if (!irg_is_in_stack(pred)) {
	some_outof_loop = 1;
    } else {
      if(get_irg_uplink(pred) < get_irg_uplink(root)) {
	DDMG(pred); DDMG(root);
	assert(get_irg_uplink(pred) >= get_irg_uplink(root));
      }
      some_in_loop = 1;
    }
  }

  return !some_outof_loop && some_in_loop;
}


/* Check whether there is a parallel edge in the ip control flow.
   Only */
static bool
is_ip_head (ir_graph *n, ir_graph *pred)
{
  int iv_rem = get_interprocedural_view();
  set_interprocedural_view(true);
  ir_node *sblock = get_irg_start_block(n);
  int i, arity = get_Block_n_cfgpreds(sblock);
  int is_be = 0;

  //printf(" edge from "); DDMG(n);
  //printf(" to pred   "); DDMG(pred);
  //printf(" sblock    "); DDMN(sblock);

  for (i = 0; i < arity; i++) {
    ir_node *pred_cfop = skip_Proj(get_Block_cfgpred(sblock, i));
    //printf("  "); DDMN(pred_cfop);
    if (get_irn_op(pred_cfop) == op_CallBegin) {  /* could be Unknown */
      ir_graph *ip_pred = get_irn_irg(pred_cfop);
      //printf("   "); DDMG(ip_pred);
      if ((ip_pred == pred) && is_backedge(sblock, i)) {
	//printf("   found\n");
	is_be = 1;
      }
    }
  }

  set_interprocedural_view(iv_rem);
  return is_be;
}

/* Returns index of the predecessor with the smallest dfn number
   greater-equal than limit. */
static int
smallest_dfn_pred (ir_graph *n, int limit)
{
  int i, index = -2, min = -1;

  int arity = get_irg_n_callees(n);
  for (i = 0; i < arity; i++) {
    ir_graph *pred = get_irg_callee(n, i);
    if (is_irg_callee_backedge(n, i) || !irg_is_in_stack(pred)) continue;
    if (get_irg_dfn(pred) >= limit && (min == -1 || get_irg_dfn(pred) < min)) {
      index = i;
      min = get_irg_dfn(pred);
    }
  }

  return index;
}

/* Returns index of the predecessor with the largest dfn number. */
static int
largest_dfn_pred (ir_graph *n)
{
  int i, index = -2, max = -1;

  int arity = get_irg_n_callees(n);
  for (i = 0; i < arity; i++) {
    ir_graph *pred = get_irg_callee(n, i);
    if (is_irg_callee_backedge (n, i) || !irg_is_in_stack(pred)) continue;
    if (get_irg_dfn(pred) > max) {
      index = i;
      max = get_irg_dfn(pred);
    }
  }

  return index;
}

#if 0
static ir_graph *
find_tail (ir_graph *n) {
  ir_graph *m;
  int i, res_index = -2;

  /*
    if (!icfg && rm_cyclic_phis && remove_cyclic_phis (n)) return NULL;
  */
  m = stack[tos-1];  /* tos = top of stack */
  if (is_head (m, n)) {
    res_index = smallest_dfn_pred(m, 0);
    if ((res_index == -2) &&  /* no smallest dfn pred found. */
    (n == m))
      return NULL;
  } else {
    if (m == n) return NULL;    // Is this to catch Phi - self loops?
    for (i = tos-2; i >= 0; --i) {
      m = stack[i];

      if (is_head (m, n)) {
	res_index = smallest_dfn_pred (m, get_irg_dfn(m) + 1);
	if (res_index == -2)  /* no smallest dfn pred found. */
	  res_index = largest_dfn_pred (m);

	if ((m == n) && (res_index == -2)) {
	  i = -1;
	}
	break;
      }

      /* We should not walk past our selves on the stack:  The upcoming nodes
	 are not in this loop. We assume a loop not reachable from Start. */
      if (m == n) {
	i = -1;
	break;
      }

    }

    if (i < 0) {
      /* A dead loop not reachable from Start. */
      for (i = tos-2; i >= 0; --i) {
	m = stack[i];
	if (is_endless_head (m, n)) {
	  res_index = smallest_dfn_pred (m, get_irg_dfn(m) + 1);
	  if (res_index == -2)  /* no smallest dfn pred found. */
	    res_index = largest_dfn_pred (m);
	  break;
	}
	if (m == n) { break; }  /* It's not an unreachable loop, either. */
      }
      //assert(0 && "no head found on stack");
    }

  }
  assert (res_index > -2);

  set_irg_callee_backedge (m, res_index);
  return get_irg_callee(m, res_index);
}
#else
static ir_graph *
find_tail (ir_graph *n) {
  ir_graph *m;
  int i, res_index = -2;

  ir_graph *in_and_out    = NULL;
  ir_graph *only_in       = NULL;
  ir_graph *ip_in_and_out = NULL;
  ir_graph *ip_only_in    = NULL;

  //printf("find tail for "); DDMG(n);

  for (i = tos-1; i >= 0; --i) {
    ir_graph *pred = (i < tos -1) ? stack[i+1] : n;
    m = stack[i];

    if (is_head (m, n)) {
      //printf(" found 1a! "); DDM;
      in_and_out = m;
      if (is_ip_head(pred, m)) {
	//printf(" found 1b! "); DDM;
	ip_in_and_out = m;
      }
    } else if (!ip_only_in && is_endless_head(m, n)) {
      only_in = m;
      //printf(" found 2a! "); DDM;
      if (is_ip_head(pred, m)) {
	//printf(" found 2b! "); DDM;
	ip_only_in = m;
      }
    } else if (is_ip_head(pred, m)) {
      //printf(" found 3! "); DDM;   This happens for self recursions in the second
      //assert(0);                   scc iteration (the one to flip the loop.)
    }

    if (ip_in_and_out) break;    /* That's what we really want. */

    if (m == n) break;   /* Don't walk past n on the stack! */
  }


  if (!in_and_out && !only_in)
    /* There is no loop */
    return NULL;


  /* Is there a head in the callgraph without a head in the
     ip cf graph? */
  assert(in_and_out || only_in);

  m = (ip_in_and_out) ? ip_in_and_out : ip_only_in;

  if (!m)
    m = (in_and_out) ? in_and_out : only_in;

  //printf("*** head is "); DDMG(m);

  res_index = smallest_dfn_pred (m, get_irg_dfn(m) + 1);
  if (res_index == -2)  /* no smallest dfn pred found. */
    res_index = largest_dfn_pred (m);

  set_irg_callee_backedge (m, res_index);
  ir_graph *res = get_irg_callee(m, res_index);
  //printf("*** tail is "); DDMG(res);
  return res;
}
#endif


/*-----------------------------------------------------------*
 *                   The core algorithm.                     *
 *-----------------------------------------------------------*/


static void cgscc (ir_graph *n) {
  int i;

  if (cg_irg_visited(n)) return;
  mark_cg_irg_visited(n);

  /* Initialize the node */
  set_irg_dfn(n, current_dfn);      /* Depth first number for this node */
  set_irg_uplink(n, current_dfn);   /* ... is default uplink. */
  current_dfn ++;
  push(n);

  int arity = get_irg_n_callees(n);

  for (i = 0; i < arity; i++) {
    ir_graph *m;
    if (is_irg_callee_backedge(n, i)) continue;
    m = get_irg_callee(n, i);

    /** This marks the backedge, but does it guarantee a correct loop tree? */
    //if (m == n) { set_irg_callee_backedge(n, i); continue; }

    cgscc (m);
    if (irg_is_in_stack(m)) {
      /* Uplink of m is smaller if n->m is a backedge.
	 Propagate the uplink to mark the cfloop. */
      if (get_irg_uplink(m) < get_irg_uplink(n))
	set_irg_uplink(n, get_irg_uplink(m));
    }
  }

  if (get_irg_dfn(n) == get_irg_uplink(n)) {
    /* This condition holds for
       1) the node with the incoming backedge.
       That is: We found a cfloop!
       2) Straight line code, because no uplink has been propagated, so the
       uplink still is the same as the dfn.

       But n might not be a proper cfloop head for the analysis. Proper cfloop
       heads are Block and Phi nodes. find_tail searches the stack for
       Block's and Phi's and takes those nodes as cfloop heads for the current
       cfloop instead and marks the incoming edge as backedge. */

    ir_graph *tail = find_tail(n);
    if (tail) {
      /* We have a cfloop, that is no straight line code,
	 because we found a cfloop head!
	 Next actions: Open a new cfloop on the cfloop tree and
	 try to find inner cfloops */


      ir_loop *l = new_loop();

      /* Remove the cfloop from the stack ... */
      pop_scc_unmark_visit (n);

      /* The current backedge has been marked, that is temporarily eliminated,
	 by find tail. Start the scc algorithm
	 anew on the subgraph thats left (the current cfloop without the backedge)
	 in order to find more inner cfloops. */

      cgscc (tail);

      assert (cg_irg_visited(n));
      close_loop(l);
    } else {
      pop_scc_to_loop(n);
    }
  }
}



static void reset_isbe(void) {
  int i, n_irgs = get_irp_n_irgs();

  for (i = 0; i < n_irgs; ++i) {
    int j, n_cs;
    ir_graph *irg = get_irp_irg(i);

    n_cs = get_irg_n_callers(irg);
    for (j = 0; j < n_cs; ++j) {
      irg->caller_isbe[j] = 0;
    }
    n_cs = get_irg_n_callees(irg);
    for (j = 0; j < n_cs; ++j) {
      irg->callee_isbe[j] = 0;
    }
  }
}




/* ----------------------------------------------------------------------------------- */
/* Another algorithm to compute recursion nesting depth                                */
/* Walk the callgraph.  For each crossed edge increase the loop depth by the edge      */
/* weight. Assign graphs the maximal depth.                                            */
/* ----------------------------------------------------------------------------------- */

void compute_loop_depth (ir_graph *irg, void *env) {
  int current_nesting = *(int *) env;
  int old_nesting = irg->callgraph_loop_depth;
  int old_visited = get_cg_irg_visited(irg);
  int i, n_callees;

  //return ;

  if (cg_irg_visited(irg)) return;

  mark_cg_irg_visited(irg);

  //printf(" old: %d new %d master %d", old_visited, get_cg_irg_visited(irg), master_cg_visited); DDMG(irg);


  if (old_nesting < current_nesting)
    irg->callgraph_loop_depth = current_nesting;

  if (current_nesting > irp->max_callgraph_loop_depth)
    irp->max_callgraph_loop_depth = current_nesting;

  if ((old_visited +1 < get_cg_irg_visited(irg)) ||   /* not yet visited */
      (old_nesting < current_nesting)) {        /* propagate larger nesting */
    /* Don't walk the graph, but a tree that is an unfolded graph. */
    n_callees = get_irg_n_callees(irg);
    for (i = 0; i < n_callees; i++) {
      ir_graph *m = get_irg_callee(irg, i);
      *(int *)env += get_irg_callee_loop_depth(irg, i);
      compute_loop_depth(m, env);
      *(int *)env -= get_irg_callee_loop_depth(irg, i);
    }
  }

  set_cg_irg_visited(irg, master_cg_visited-1);
}


/* ----------------------------------------------------------------------------------- */
/* Another algorithm to compute recursion nesting depth                                */
/* Walk the callgraph.  For each crossed loop increase the nesting depth by one.       */
/* Assign graphs the maximal nesting depth.   Don't increas if passing loops more than */
/* once.                                                                               */
/* ----------------------------------------------------------------------------------- */


/* For callees, we want to remember the Call nodes, too. */
struct ana_entry2 {
  ir_loop **loop_stack;
  int tos;
  int recursion_nesting;
};

typedef struct ana_entry2 ana_entry2;

static void push2(ana_entry2 *e, ir_loop *g) {
  if (ARR_LEN(e->loop_stack) == e->tos) {
    ARR_APP1(ir_loop *, e->loop_stack, g);
  } else {
    e->loop_stack[e->tos] = g;
  }
  e->tos ++;
}

static ir_loop *pop2(ana_entry2 *e) {
  e->tos --;
  return e->loop_stack[e->tos+1];
}

static int in_stack(ana_entry2 *e, ir_loop *g) {
  int i;
  for (i = e->tos-1; i >= 0; --i) {
    if (e->loop_stack[i] == g) return 1;
  }
  return 0;
}

void compute_rec_depth (ir_graph *irg, void *env) {
  ana_entry2 *e = (ana_entry2 *)env;
  ir_loop *l = irg->l;
  int depth, old_depth = irg->callgraph_recursion_depth;
  int i, n_callees;
  int pushed = 0;

  if (cg_irg_visited(irg)) return;
  mark_cg_irg_visited(irg);

  /* -- compute and set the new nesting value -- */
  if ((l != irp->outermost_cg_loop) && !in_stack(e, l)) {
    push2(e, l);
    e->recursion_nesting++;
    pushed = 1;
  }
  depth = e->recursion_nesting;

  if (old_depth < depth)
    irg->callgraph_recursion_depth = depth;

  if (depth > irp->max_callgraph_recursion_depth)
    irp->max_callgraph_recursion_depth = depth;

  /* -- spread the nesting value -- */
  if (depth == 0 || old_depth < depth) {
    /* Don't walk the graph, but a tree that is an unfolded graph. */
    n_callees = get_irg_n_callees(irg);
    for (i = 0; i < n_callees; i++) {
      ir_graph *m = get_irg_callee(irg, i);
      compute_rec_depth(m, env);
    }
  }

  /* -- clean up -- */
  if (pushed) {
    pop2(e);
    e->recursion_nesting--;
  }
  set_cg_irg_visited(irg, master_cg_visited-1);
}


/* Compute the backedges that represent recursions. */
void find_callgraph_recursions(void) {
  int i, n_irgs = get_irp_n_irgs();

  reset_isbe();

  /* -- Compute the looptree -- */

  /* The outermost graph.  We start here.  Then we start at all
     functions in irg list that are never called, then at the remaining
     unvisited ones. */
  assert(get_irp_main_irg());
  outermost_ir_graph = get_irp_main_irg();
  init_scc();

  current_loop = NULL;
  new_loop();  /* sets current_loop */

  master_cg_visited++;
  cgscc(outermost_ir_graph);
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if (!cg_irg_visited(irg) && get_irg_n_callers(irg) == 0)
      cgscc(irg);
  }
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if (!cg_irg_visited(irg))
      cgscc(irg);
  }
  irp->outermost_cg_loop = current_loop;

  /* -- Reverse the backedge information. -- */
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    int j, n_callees = get_irg_n_callees(irg);
    for (j = 0; j < n_callees; ++j) {
      if (is_irg_callee_backedge(irg, j))
	set_irg_caller_backedge(get_irg_callee(irg, j), irg);
    }
  }

  /* -- compute the loop depth  -- */
  int current_nesting = 0;
  irp->max_callgraph_loop_depth = 0;
  master_cg_visited += 2;
  //printf (" ** starting at      "); DDMG(get_irp_main_irg());
  compute_loop_depth(get_irp_main_irg(), &current_nesting);
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if ((get_cg_irg_visited(irg) < master_cg_visited-1) &&
	get_irg_n_callers(irg) == 0) {
      compute_loop_depth(irg, &current_nesting);
      //printf (" ** starting at      "); DDMG(irg);
    }
  }
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if (get_cg_irg_visited(irg) < master_cg_visited-1) {
      compute_loop_depth(irg, &current_nesting);
      //printf (" ** starting at      "); DDMG(irg);
    }
  }

  /* -- compute the recursion depth -- */
  ana_entry2 e;
  e.loop_stack = NEW_ARR_F(ir_loop *, 0);
  e.tos = 0;
  e.recursion_nesting = 0;
  irp->max_callgraph_recursion_depth = 0;

  master_cg_visited += 2;
  compute_rec_depth(get_irp_main_irg(), &e);
  //printf (" ++ starting at "); DDMG(get_irp_main_irg());
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if ((get_cg_irg_visited(irg) < master_cg_visited-1) &&
	get_irg_n_callers(irg) == 0) {
      compute_rec_depth(irg, &e);
      //printf (" ++ starting at "); DDMG(irg);
    }
  }
  for (i = 0; i < n_irgs; i++) {
    ir_graph *irg = get_irp_irg(i);
    if (get_cg_irg_visited(irg) < master_cg_visited-1) {
      compute_rec_depth(irg, &e);
      //printf (" ++ starting at "); DDMG(irg);
    }
  }

  DEL_ARR_F(e.loop_stack);

  //dump_callgraph("-with_nesting");

  //dump_callgraph_loop_tree(current_loop, "-after_cons");
  irp->callgraph_state =  irp_callgraph_and_calltree_consistent;
}

/* Maximal loop depth of all paths from an external visible method to
    this irg. */
int       get_irg_loop_depth(ir_graph *irg) {
  assert(irp->callgraph_state == irp_callgraph_consistent ||
	 irp->callgraph_state == irp_callgraph_and_calltree_consistent);
  return  irg->callgraph_loop_depth;
}

/* Maximal recursion depth of all paths from an external visible method to
   this irg. */
int       get_irg_recursion_depth(ir_graph *irg) {
  assert(irp->callgraph_state == irp_callgraph_and_calltree_consistent);
  return irg->callgraph_recursion_depth;
}

void analyse_loop_nesting_depth(void) {
  entity **free_methods = NULL;
  int arr_len;

  /* establish preconditions. */
  if (get_irp_callee_info_state() != irg_callee_info_consistent) {
    cgana(&arr_len, &free_methods);
  }

  if (irp_callgraph_consistent != get_irp_callgraph_state()) {
    compute_callgraph();
  }

  find_callgraph_recursions();
}
