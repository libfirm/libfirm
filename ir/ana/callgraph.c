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

#include "config.h"
#include "callgraph.h"

#include "irloop_t.h"
#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"

#include "array.h"
#include "pmap.h"

#include "irgwalk.h"

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
static void       set_irg_caller_backedge(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callers(irg));
  irg->caller_isbe[pos] = 1;
}

/* The functions called by irg. */
int       get_irg_n_callees(ir_graph *irg) {
  if (irg->callees) return ARR_LEN(irg->callees);
  return -1;
}
ir_graph *get_irg_callee(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  if (irg->callees) return irg->callees[pos];
  return NULL;
}
int       is_irg_callee_backedge(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  return irg->callee_isbe[pos];
}
void       set_irg_callee_backedge(ir_graph *irg, int pos) {
  assert (pos >= 0 && pos < get_irg_n_callees(irg));
  irg->callee_isbe[pos] = 1;
}


static void ana_Call(ir_node *n, void *env) {
  int i, n_callees;
  ir_graph *irg;

  if (get_irn_op(n) != op_Call) return;

  irg = get_irn_irg(n);
  n_callees = get_Call_n_callees(n);
  for (i = 0; i < n_callees; ++i) {
    entity *callee_e = get_Call_callee(n, i);
    if (callee_e) {
      ir_graph *callee = get_entity_irg(callee_e);
      pset_insert((pset *)irg->callees,    callee, (unsigned)callee >> 3);
      pset_insert((pset *)callee->callers, irg,    (unsigned)irg >> 3);
    }
  }
}


/* compare two ir graphs */
static int graph_cmp(const void *elt, const void *key) {
  return elt != key;
}


/* Construct and destruct the callgraph. */
void compute_callgraph(void) {
  int i, n_irgs = get_irp_n_irgs();

  assert(interprocedural_view == 0);  /* Else walking will not reach the Call nodes. */

  /* initialize */
  free_callgraph();
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
    assert(get_irg_callee_info_state(irg) == irg_callee_info_consistent);
    irg->callees = (ir_graph **)new_pset(graph_cmp, 8);
    irg->callers = (ir_graph **)new_pset(graph_cmp, 8);
  }

  /* Compute the call graph */
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
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
}

void free_callgraph(void) {
  int i, n_irgs = get_irp_n_irgs();
  for (i = 0; i < n_irgs; ++i) {
    ir_graph *irg = get_irp_irg(i);
    if (irg->callees) DEL_ARR_F(irg->callees);
    if (irg->callers) DEL_ARR_F(irg->callers);
    if (irg->callee_isbe) DEL_ARR_F(irg->callee_isbe);
    if (irg->caller_isbe) DEL_ARR_F(irg->caller_isbe);
    irg->callees = NULL;
    irg->callers = NULL;
    irg->callee_isbe = NULL;
    irg->caller_isbe = NULL;
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
  return ((scc_info *)n->link)->visited;
}

static INLINE void
mark_cg_irg_visited(ir_graph *n) {
  ((scc_info *)n->link)->visited = 1;
}

static INLINE void
set_cg_irg_visited(ir_graph *n, int i) {
  ((scc_info *)n->link)->visited = i;
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

  /*for (;;) {*/
  do {
    m = pop();
    loop_node_cnt++;
    set_irg_dfn(m, loop_node_cnt);
    add_loop_node(current_loop, (ir_node *)m);
    set_irg_loop(m, current_loop);
    /*    if (m==n) break;*/
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
init_scc (ir_graph *irg) {
  int i;
  current_dfn = 1;
  loop_node_cnt = 0;
  init_stack();
  for (i = 0; i < get_irp_n_irgs(); ++i) {
    set_irg_link(get_irp_irg(i), new_scc_info());
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
    (n ==  m))
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
  set_irg_loop(n, NULL);
  current_dfn ++;
  push(n);

  int arity = get_irg_n_callees(n);

  for (i = 0; i < arity; i++) {
    ir_graph *m;
    if (is_irg_callee_backedge(n, i)) continue;
    m = get_irg_callee(n, i);

    /** This marks the backedge, but does it guarantee a correct loop tree? */
    if (m == n) { set_irg_callee_backedge(n, i); continue; }

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

/* Compute the backedges that represent recursions. */
void find_callgraph_recursions(void) {
  int i, n_irgs = get_irp_n_irgs();

  reset_isbe();

  assert(get_irp_main_irg()); /* The outermost graph.  We start here.  Then we start at all
				 external visible functions in irg list, then at the remaining
				 unvisited ones. */
  outermost_ir_graph = get_irp_main_irg();

  assert(!interprocedural_view &&
     "use construct_ip_backedges");

  init_scc(current_ir_graph);

  current_loop = NULL;
  new_loop();  /* sets current_loop */

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

  /* We can not use the looptree -- it has no real meaning.
     There is a working dumper, but commented out.
     assert(current_loop && current_loop == get_loop_outer_loop(current_loop));
     dump_callgraph_loop_tree(current_loop, ""); */
}
