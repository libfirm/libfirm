/*
 * Project:     libFIRM
 * File name:   ir/ana/irscc.c
 * Purpose:     Compute the strongly connected regions and build
 *              backedge/cfloop datastructures.
 *              A variation on the Tarjan algorithm. See also [Trapp:99],
 *              Chapter 5.2.1.2.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     7.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "irloop_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "array.h"
#include "pmap.h"
#include "irgwalk.h"
#include "irprog_t.h"
#include "irdump.h"

#define NO_CFLOOPS_WITHOUT_HEAD 1

static ir_graph *outermost_ir_graph;      /* The outermost graph the scc is computed
                      for */
static ir_loop *current_loop;      /* Current cfloop construction is working
                      on. */
static int loop_node_cnt = 0;      /* Counts the number of allocated cfloop nodes.
				      Each cfloop node gets a unique number.
				      What for? ev. remove. @@@ */
static int current_dfn = 1;        /* Counter to generate depth first numbering
				      of visited nodes.  */

static int max_loop_depth = 0;

void link_to_reg_end (ir_node *n, void *env);

/**********************************************************************/
/* Node attributes                                                   **/
/**********************************************************************/

/**********************************************************************/
/* Node attributes needed for the construction.                      **/
/**********************************************************************/

typedef struct scc_info {
  bool in_stack;         /* Marks whether node is on the stack. */
  int dfn;               /* Depth first search number. */
  int uplink;            /* dfn number of ancestor. */
} scc_info;

static INLINE scc_info* new_scc_info(void) {
  scc_info *info = obstack_alloc (outermost_ir_graph->obst, sizeof (scc_info));
  memset (info, 0, sizeof (scc_info));
  return info;
}

static INLINE void
mark_irn_in_stack (ir_node *n) {
  assert(get_irn_link(n));
  ((scc_info *)n->link)->in_stack = true;
}

static INLINE void
mark_irn_not_in_stack (ir_node *n) {
  assert(get_irn_link(n));
  ((scc_info *)n->link)->in_stack = false;
}

static INLINE bool
irn_is_in_stack (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)n->link)->in_stack;
}

static INLINE void
set_irn_uplink (ir_node *n, int uplink) {
  assert(get_irn_link(n));
  ((scc_info *)n->link)->uplink = uplink;
}

static INLINE int
get_irn_uplink (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)n->link)->uplink;
}

static INLINE void
set_irn_dfn (ir_node *n, int dfn) {
  assert(get_irn_link(n));
  ((scc_info *)n->link)->dfn = dfn;
}

static INLINE int
get_irn_dfn (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)n->link)->dfn;
}

/**********************************************************************/
/* A stack.                                                          **/
/**********************************************************************/

static ir_node **stack = NULL;
static int tos = 0;                /* top of stack */

static INLINE void init_stack(void) {
  if (stack) {
    ARR_RESIZE (ir_node *, stack, 1000);
  } else {
    stack = NEW_ARR_F (ir_node *, 1000);
  }
  tos = 0;
}


static INLINE void
push (ir_node *n)
{
  if (tos == ARR_LEN (stack)) {
    int nlen = ARR_LEN (stack) * 2;
    ARR_RESIZE (ir_node *, stack, nlen);
  }
  stack [tos++] = n;
  mark_irn_in_stack(n);
}

static INLINE ir_node *
pop (void)
{
  ir_node *n = stack[--tos];
  mark_irn_not_in_stack(n);
  return n;
}

/* The nodes up to n belong to the current loop.
   Removes them from the stack and adds them to the current loop. */
static INLINE void
pop_scc_to_loop (ir_node *n)
{
  ir_node *m;

  /*for (;;) {*/
  do {
    m = pop();
    loop_node_cnt++;
    set_irn_dfn(m, loop_node_cnt);
    add_loop_node(current_loop, m);
    set_irn_loop(m, current_loop);
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
pop_scc_unmark_visit (ir_node *n)
{
  ir_node *m = NULL;

  while (m != n) {
    m = pop();
    set_irn_visited(m, 0);
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
  son->n_sons=0;
  if (father) {
    son->outer_loop = father;
    add_loop_son(father, son);
    son->depth = father->depth+1;
    if (son->depth > max_loop_depth) max_loop_depth = son->depth;
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
init_node (ir_node *n, void *env) {
  if (is_Block(n))
    set_irn_link (n, new_scc_info());
  clear_backedges(n);
}

static INLINE void
init_scc_common (void) {
  current_dfn = 1;
  loop_node_cnt = 0;
  init_stack();
}

static INLINE void
init_scc (ir_graph *irg) {
  init_scc_common();
  irg_walk_graph (irg, init_node, NULL, NULL);
}

static INLINE void
init_ip_scc (void) {
  init_scc_common();
  cg_walk (init_node, NULL, NULL);

#if EXPERIMENTAL_CFLOOP_TREE
  cg_walk (link_to_reg_end, NULL, NULL);
#endif
}

/* Condition for breaking the recursion. */
static bool is_outermost_StartBlock(ir_node *n) {
  /* Test whether this is the outermost Start node.  If so
     recursion must end. */
  assert(is_Block(n));
  if ((get_Block_n_cfgpreds(n) == 1)  &&
      (get_irn_op(skip_Proj(get_Block_cfgpred(n, 0))) == op_Start) &&
      (get_nodes_block(skip_Proj(get_Block_cfgpred(n, 0))) == n)) {
    return true;
  }
  return false;
}

/** Returns true if n is a loop header, i.e., it is a Block node
 *  and has predecessors within the cfloop and out of the cfloop.
 *
 *  @param root: only needed for assertion.
 */
static bool
is_head (ir_node *n, ir_node *root)
{
  int i, arity;
  int some_outof_loop = 0, some_in_loop = 0;

  assert(is_Block(n));

  if (!is_outermost_StartBlock(n)) {
    arity = get_irn_arity(n);
    for (i = 0; i < arity; i++) {
      ir_node *pred = get_nodes_block(skip_Proj(get_irn_n(n, i)));
      if (is_backedge(n, i)) continue;
      if (!irn_is_in_stack(pred)) {
	some_outof_loop = 1;
      } else {
	if (get_irn_uplink(pred) < get_irn_uplink(root))  {
	  DDMN(pred); DDMN(root);
	  assert(get_irn_uplink(pred) >= get_irn_uplink(root));
	}
	some_in_loop = 1;
      }
    }
  }
  return some_outof_loop && some_in_loop;
}


/* Returns true if n is possible loop head of an endless loop.
   I.e., it is a Block, Phi or Filter node and has only predecessors
   within the loop.
   @arg root: only needed for assertion. */
static bool
is_endless_head (ir_node *n, ir_node *root)
{
  int i, arity;
  int some_outof_loop = 0, some_in_loop = 0;

  assert(is_Block(n));
  /* Test for legal loop header: Block, Phi, ... */
  if (!is_outermost_StartBlock(n)) {
    arity = get_irn_arity(n);
    for (i = 0; i < arity; i++) {
      ir_node *pred = get_nodes_block(skip_Proj(get_irn_n(n, i)));
      assert(pred);
      if (is_backedge(n, i)) { continue; }
      if (!irn_is_in_stack(pred)) {
	some_outof_loop = 1; //printf(" some out of loop ");
      } else {
	if(get_irn_uplink(pred) < get_irn_uplink(root)) {
	  DDMN(pred); DDMN(root);
	  assert(get_irn_uplink(pred) >= get_irn_uplink(root));
	}
	some_in_loop = 1;
      }
    }
  }
  return !some_outof_loop && some_in_loop;
}

/* Returns index of the predecessor with the smallest dfn number
   greater-equal than limit. */
static int
smallest_dfn_pred (ir_node *n, int limit)
{
  int i, index = -2, min = -1;

  if (!is_outermost_StartBlock(n)) {
    int arity = get_irn_arity(n);
    for (i = 0; i < arity; i++) {
      ir_node *pred = get_nodes_block(skip_Proj(get_irn_n(n, i)));
      if (is_backedge(n, i) || !irn_is_in_stack(pred)) continue;
      if (get_irn_dfn(pred) >= limit && (min == -1 || get_irn_dfn(pred) < min)) {
	index = i;
	min = get_irn_dfn(pred);
      }
    }
  }
  return index;
}

/* Returns index of the predecessor with the largest dfn number. */
static int
largest_dfn_pred (ir_node *n)
{
  int i, index = -2, max = -1;

  if (!is_outermost_StartBlock(n)) {
    int arity = get_irn_arity(n);
    for (i = 0; i < arity; i++) {
      ir_node *pred = get_nodes_block(skip_Proj(get_irn_n(n, i)));
      if (is_backedge (n, i) || !irn_is_in_stack(pred)) continue;
      if (get_irn_dfn(pred) > max) {
	index = i;
	max = get_irn_dfn(pred);
      }
    }
  }
  return index;
}

/* Searches the stack for possible loop heads.  Tests these for backedges.
   If it finds a head with an unmarked backedge it marks this edge and
   returns the tail of the loop.
   If it finds no backedge returns NULL. */
static ir_node *
find_tail (ir_node *n) {
  ir_node *m;
  int i, res_index = -2;

  m = stack[tos-1];  /* tos = top of stack */
  if (is_head (m, n)) {
    res_index = smallest_dfn_pred(m, 0);
    if ((res_index == -2) &&  /* no smallest dfn pred found. */
	(n ==  m))
      return NULL;
  } else {
    if (m == n) return NULL;
    for (i = tos-2; i >= 0; --i) {

      m = stack[i];
      if (is_head (m, n)) {
	res_index = smallest_dfn_pred (m, get_irn_dfn(m) + 1);
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
	  res_index = smallest_dfn_pred (m, get_irn_dfn(m) + 1);
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

  set_backedge (m, res_index);
  return is_outermost_StartBlock(n) ? NULL : get_nodes_block(skip_Proj(get_irn_n(m, res_index)));
}

INLINE static int
is_outermost_loop(ir_loop *l) {
  return l == get_loop_outer_loop(l);
}

/*-----------------------------------------------------------*
 *                   The core algorithm.                     *
 *-----------------------------------------------------------*/


static void cfscc (ir_node *n) {
  int i;

  assert(is_Block(n));

  if (irn_visited(n)) return;
  mark_irn_visited(n);

  /* Initialize the node */
  set_irn_dfn(n, current_dfn);      /* Depth first number for this node */
  set_irn_uplink(n, current_dfn);   /* ... is default uplink. */
  set_irn_loop(n, NULL);
  current_dfn ++;
  push(n);

  /* AS: get_start_index might return -1 for Control Flow Nodes, and thus a negative
     array index would be passed to is_backedge(). But CFG Nodes dont't have a backedge array,
     so is_backedge does not access array[-1] but correctly returns false! */

  if (!is_outermost_StartBlock(n)) {
    int arity = get_irn_arity(n);

    for (i = 0; i < arity; i++) {
      ir_node *m;
      if (is_backedge(n, i)) continue;
      m = get_nodes_block(skip_Proj(get_irn_n(n, i)));

      cfscc (m);
      if (irn_is_in_stack(m)) {
	/* Uplink of m is smaller if n->m is a backedge.
	   Propagate the uplink to mark the cfloop. */
	if (get_irn_uplink(m) < get_irn_uplink(n))
	  set_irn_uplink(n, get_irn_uplink(m));
      }
    }
  }

  if (get_irn_dfn(n) == get_irn_uplink(n)) {
    /* This condition holds for
       1) the node with the incoming backedge.
          That is: We found a cfloop!
       2) Straight line code, because no uplink has been propagated, so the
          uplink still is the same as the dfn.

       But n might not be a proper cfloop head for the analysis. Proper cfloop
       heads are Block and Phi nodes. find_tail searches the stack for
       Block's and Phi's and takes those nodes as cfloop heads for the current
       cfloop instead and marks the incoming edge as backedge. */

    ir_node *tail = find_tail(n);
    if (tail) {
      /* We have a cfloop, that is no straight line code,
	 because we found a cfloop head!
	 Next actions: Open a new cfloop on the cfloop tree and
	               try to find inner cfloops */

#if NO_CFLOOPS_WITHOUT_HEAD

      /* This is an adaption of the algorithm from fiasco / optscc to
       * avoid cfloops without Block or Phi as first node.  This should
       * severely reduce the number of evaluations of nodes to detect
       * a fixpoint in the heap analysis.
       * Further it avoids cfloops without firm nodes that cause errors
       * in the heap analyses. */

      ir_loop *l;
      int close;
      if ((get_loop_n_elements(current_loop) > 0) || (is_outermost_loop(current_loop))) {
	l = new_loop();
	close = 1;
      } else {
	l = current_loop;
	close = 0;
      }

#else

      ir_loop *l = new_loop();

#endif

      /* Remove the cfloop from the stack ... */
      pop_scc_unmark_visit (n);

      /* The current backedge has been marked, that is temporarily eliminated,
	 by find tail. Start the scc algorithm
	 anew on the subgraph thats left (the current cfloop without the backedge)
	 in order to find more inner cfloops. */

      cfscc (tail);

      assert (irn_visited(n));
#if NO_CFLOOPS_WITHOUT_HEAD
      if (close)
#endif
	close_loop(l);
    }
    else
      {
	/* AS: No cfloop head was found, that is we have straightline code.
	       Pop all nodes from the stack to the current cfloop. */
      pop_scc_to_loop(n);
    }
  }
}

/* Constructs backedge information for irg. */
int construct_cf_backedges(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  ir_loop *head_rem;
  ir_node *end = get_irg_end(irg);
  int i;

  assert(!get_interprocedural_view() &&
     "use construct_ip_backedges");
  max_loop_depth = 0;

  current_ir_graph = irg;
  outermost_ir_graph = irg;

  init_scc(current_ir_graph);

  current_loop = NULL;
  new_loop();  /* sets current_loop */
  head_rem = current_loop; /* Just for assertion */

  inc_irg_visited(current_ir_graph);

  cfscc(get_irg_end_block(current_ir_graph));
  for (i = 0; i < get_End_n_keepalives(end); i++) {
    ir_node *el = get_End_keepalive(end, i);
    if (is_Block(el)) cfscc(el);
  }

  assert(head_rem == current_loop);
  set_irg_loop(current_ir_graph, current_loop);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_cf_consistent);
  assert(get_irg_loop(current_ir_graph)->kind == k_ir_loop);

  current_ir_graph = rem;
  return max_loop_depth;
}


int construct_ip_cf_backedges (void) {
  ir_graph *rem = current_ir_graph;
  int rem_ipv = get_interprocedural_view();
  int i;

  assert(get_irp_ip_view_state() == ip_view_valid);
  max_loop_depth = 0;
  outermost_ir_graph = get_irp_main_irg();

  init_ip_scc();

  current_loop = NULL;
  new_loop();  /* sets current_loop */
  set_interprocedural_view(true);

  inc_max_irg_visited();
  for (i = 0; i < get_irp_n_irgs(); i++)
    set_irg_visited(get_irp_irg(i), get_max_irg_visited());

  /** We have to start the walk at the same nodes as cg_walk. **/
  /* Walk starting at unreachable procedures. Only these
   * have End blocks visible in interprocedural view. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);

    sb = get_irg_start_block(current_ir_graph);

    if ((get_Block_n_cfgpreds(sb) > 1) ||
	(get_nodes_block(get_Block_cfgpred(sb, 0)) != sb)) continue;

    cfscc(get_irg_end_block(current_ir_graph));
  }

  /* Check whether we walked all procedures: there could be procedures
     with cyclic calls but no call from the outside. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);

    /* Test start block: if inner procedure end and end block are not
     * visible and therefore not marked. */
    sb = get_irg_start_block(current_ir_graph);
    if (get_irn_visited(sb) < get_irg_visited(current_ir_graph)) cfscc(sb);
  }

  /* Walk all endless cfloops in inner procedures.
   * We recognize an inner procedure if the End node is not visited. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *e;
    current_ir_graph = get_irp_irg(i);

    e = get_irg_end(current_ir_graph);
    if (get_irn_visited(e) < get_irg_visited(current_ir_graph)) {
      int j;
      /* Don't visit the End node. */
      for (j = 0; j < get_End_n_keepalives(e); j++) {
	ir_node *el = get_End_keepalive(e, j);
	if (is_Block(el)) cfscc(el);
      }
    }
  }

  set_irg_loop(outermost_ir_graph, current_loop);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_cf_ip_consistent);
  assert(get_irg_loop(outermost_ir_graph)->kind == k_ir_loop);

  current_ir_graph = rem;
  set_interprocedural_view(rem_ipv);
  return max_loop_depth;
}


static void reset_backedges(ir_node *n) {
  assert(is_Block(n));
  int rem = get_interprocedural_view();
  set_interprocedural_view(true);
  clear_backedges(n);
  set_interprocedural_view(false);
  clear_backedges(n);
  set_interprocedural_view(rem);
}

static void loop_reset_backedges(ir_loop *l) {
  int i;
  reset_backedges(get_loop_node(l, 0));
  for (i = 0; i < get_loop_n_nodes(l); ++i)
    set_irn_loop(get_loop_node(l, i), NULL);
  for (i = 0; i < get_loop_n_sons(l); ++i) {
    loop_reset_backedges(get_loop_son(l, i));
  }
}

/* Removes all cfloop information.
   Resets all backedges */
void free_cfloop_information(ir_graph *irg) {
  if (get_irg_loop(irg))
    loop_reset_backedges(get_irg_loop(irg));
  set_irg_loop(irg, NULL);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_none);
  /* We cannot free the cfloop nodes, they are on the obstack. */
}


void free_all_cfloop_information (void) {
  int i;
  int rem = get_interprocedural_view();
  set_interprocedural_view(true);  /* To visit all filter nodes */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    free_cfloop_information(get_irp_irg(i));
  }
  set_interprocedural_view(rem);
}
