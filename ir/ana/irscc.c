/* Copyright (C) 2002 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors:  Goetz Lindenmaier
**
** irscc.c  Computing the strongly connected regions and building
** backedge/loop datastructures.
**
*/

/* $Id$ */

#include <string.h>

#include "irloop_t.h"
#include "irnode.h"
#include "irgraph_t.h"
#include "array.h"
#include "xprintf.h"
#include "irgwalk.h"
#include "irprog.h"

ir_graph *outermost_ir_graph;      /* The outermost graph the scc is computed
				      for */
static ir_loop *current_loop;      /* Current loop construction is working
				      on. */
static int loop_node_cnt = 0;      /* Counts the number of allocated loop nodes.
				      Each loop node gets a unique number.
				      What for? ev. remove. @@@ */
static int current_dfn = 1;        /* Counter to generate depth first numbering
				      of visited nodes.  */

/**********************************************************************/
/* Node attributes needed for the construction.                      **/
/**********************************************************************/

typedef struct scc_info {
  bool in_stack;         /* Marks whether node is on the stack. */
  int dfn;               /* Depth first search number. */
  int uplink;            /* dfn number of ancestor. */
  ir_loop *loop;         /* Refers to the containing loop. */
  /*
      struct section *section;
      xset def;
      xset use;
  */
} scc_info;

static INLINE scc_info* new_scc_info() {
  scc_info *info = obstack_alloc (outermost_ir_graph->obst, sizeof (scc_info));
  memset (info, 0, sizeof (scc_info));
  return info;
}

static INLINE void
mark_irn_in_stack (ir_node *n) {
  assert(get_irn_link(n));
  ((scc_info *)get_irn_link(n))->in_stack = true;
}

static INLINE void
mark_irn_not_in_stack (ir_node *n) {
  assert(get_irn_link(n));
  ((scc_info *)get_irn_link(n))->in_stack = false;
}

static INLINE bool
irn_is_in_stack (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)get_irn_link(n))->in_stack;
}

static INLINE void
set_irn_uplink (ir_node *n, int uplink) {
  assert(get_irn_link(n));
  ((scc_info *)get_irn_link(n))->uplink = uplink;
}

static INLINE int
get_irn_uplink (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)get_irn_link(n))->uplink;
}

static INLINE void
set_irn_dfn (ir_node *n, int dfn) {
  if (! get_irn_link(n)) { DDMN(n); DDME(get_irg_ent(current_ir_graph));}
  assert(get_irn_link(n));
  ((scc_info *)get_irn_link(n))->dfn = dfn;
}

static INLINE int
get_irn_dfn (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)get_irn_link(n))->dfn;
}

/* Uses temporary information to set the loop */
static INLINE void
set_irn_loop_tmp (ir_node *n, ir_loop* loop) {
  assert(get_irn_link(n));
  ((scc_info *)get_irn_link(n))->loop = loop;
}

/* Uses temporary information to get the loop */
static INLINE ir_loop *
get_irn_loop_tmp (ir_node *n) {
  assert(get_irn_link(n));
  return ((scc_info *)get_irn_link(n))->loop;
}

ir_loop *find_nodes_loop (ir_node *n, ir_loop *l) {
  int i;
  ir_loop *res = NULL;

  /* Test whether n is contained in this loop. */
  for (i = 0; i < get_loop_n_nodes(l); i++)
    if (n == get_loop_node(l, i)) return l;

  /* Is this a leave in the loop tree? If so loop not found. */
  if (get_loop_n_sons(l) == 0) return NULL;

  /* Else descend in the loop tree. */
  for (i = 0; i < get_loop_n_sons(l); i++) {
    res = find_nodes_loop(n, get_loop_son(l, i));
    if (res) break;
  }
  return res;
}

/* @@@ temporary implementation, costly!!! */
ir_loop * get_irn_loop(ir_node *n) {
  ir_loop *l = get_irg_loop(current_ir_graph);
  l = find_nodes_loop(n, l);
  return l;
}

/**********************************************************************/
/* A stack.                                                          **/
/**********************************************************************/

static ir_node **stack = NULL;
static int tos = 0;                /* top of stack */

static INLINE void init_stack() {
  if (stack) {
    ARR_RESIZE (ir_node *, stack, 1000);
  } else {
    stack = NEW_ARR_F (ir_node *, 1000);
  }
  tos = 0;
}

static INLINE void free_stack() {
  DEL_ARR_F(stack);
  stack = NULL;
  tos = 0;
}

static INLINE void
push (ir_node *n)
{
  //DDMN(n);

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

  for (;;) {
    m = pop();
    set_irn_dfn(m, loop_node_cnt);
    loop_node_cnt++;
    add_loop_node(current_loop, m);
    set_irn_loop_tmp(m, current_loop);
    if (m==n) break;
  }
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
ir_loop *new_loop (void) {
  ir_loop *father, *son;

  father = current_loop;

  son = (ir_loop *) obstack_alloc (outermost_ir_graph->obst, sizeof (ir_loop));
  memset (son, 0, sizeof (ir_loop));
  son->kind = k_ir_loop;
  son->sons = NEW_ARR_F (ir_loop *, 0);
  son->nodes = NEW_ARR_F (ir_node *, 0);
  if (father) {
    son->outer_loop = father;
    add_loop_son(father, son);
    son->depth = father->depth+1;
  } else {  /* The root loop */
    son->outer_loop = son;
    son->depth = 0;
  }

  current_loop = son;
  return father;
}

/* Finishes the datastructures, copies the arrays to the obstack
   of current_ir_graph. */
void mature_loop (ir_loop *loop) {
  ir_loop **new_sons;
  ir_node **new_nods;

  new_sons = NEW_ARR_D (ir_loop *, current_ir_graph->obst, ARR_LEN(loop->sons));
  memcpy (new_sons, loop->sons, sizeof (ir_loop *) * ARR_LEN(loop->sons));
  DEL_ARR_F(loop->sons);
  loop->sons = new_sons;
}

/* Returns outer loop, itself if outermost. */
ir_loop *get_loop_outer_loop (ir_loop *loop) {
  assert(loop && loop->kind == k_ir_loop);
  return loop->outer_loop;
}

/* Returns nesting depth of this loop */
int get_loop_depth (ir_loop *loop) {
  assert(loop); assert(loop->kind == k_ir_loop);
  return loop->depth;
}

/* Returns the number of inner loops */
int      get_loop_n_sons (ir_loop *loop) {
  assert(loop && loop->kind == k_ir_loop);
  return ARR_LEN(loop->sons);
}
ir_loop *get_loop_son (ir_loop *loop, int pos) {
  assert(loop && loop->kind == k_ir_loop);
  return loop->sons[pos];
}
static INLINE void
add_loop_son(ir_loop *loop, ir_loop *son) {
  assert(loop && loop->kind == k_ir_loop);
  ARR_APP1 (ir_loop *, loop->sons, son);
}

/* Returns the number of nodes in the loop */
int      get_loop_n_nodes (ir_loop *loop) {
  assert(loop); assert(loop->kind == k_ir_loop);
  return ARR_LEN(loop->nodes);
}
ir_node *get_loop_node (ir_loop *loop, int pos) {
  assert(loop && loop->kind == k_ir_loop);
  return loop->nodes[pos];
}
static INLINE void
add_loop_node(ir_loop *loop, ir_node *n) {
  assert(loop && loop->kind == k_ir_loop);
  ARR_APP1 (ir_node *, loop->nodes, n);
}

/* The outermost loop is remarked in the surrounding graph. */
void     set_irg_loop(ir_graph *irg, ir_loop *loop) {
  assert(irg);
  irg->loop = loop;
}
ir_loop *get_irg_loop(ir_graph *irg) {
  assert(irg);
  return irg->loop;
}

/**********************************************************************/
/* Constructing and destructing the loop/backedge information.       **/
/**********************************************************************/

/* Initialization steps. **********************************************/

static INLINE void
init_node (ir_node *n, void *env) {
  int i;
  set_irn_link (n, new_scc_info());
  clear_backedges(n);
#if 0
  /* Also init nodes not visible in intraproc_view. */
    /* @@@ init_node is called for too many nodes -- this wastes memory!.
       The mem is not lost as its on the obstack. */
  if (get_irn_op(n) == op_Filter) {
    for (i = 0; i < get_Filter_n_cg_preds(n); i++)
      init_node(get_Filter_cg_pred(n, i), NULL);
  }
  if (get_irn_op(n) == op_Block) {
    for (i = 0; i < get_Block_cg_n_cfgpreds(n); i++) {
      init_node(get_Block_cg_cfgpred(n, i), NULL);
    }
  }
  /* The following pattern matches only after a call from above pattern. */
  if ((get_irn_op(n) == op_Proj) /*&& (get_Proj_proj(n) == 0)*/) {
    /* @@@ init_node is called for every proj -- this wastes memory!.
       The mem is not lost as its on the obstack. */
    ir_node *cb = get_Proj_pred(n);
    if ((get_irn_op(cb) == op_CallBegin) ||
	(get_irn_op(cb) == op_EndReg) ||
	(get_irn_op(cb) == op_EndExcept)) {
      init_node(cb, NULL);
      init_node(get_nodes_Block(cb), NULL);
    }
#endif
}

static INLINE void
init_scc (ir_graph *irg) {
  current_dfn = 1;
  loop_node_cnt = 0;
  init_stack();
  irg_walk_graph (irg, init_node, NULL, NULL);
  /*
  irg_walk (irg, link_to_reg_end, NULL, NULL);
  */
}

static INLINE void
init_ip_scc () {
  current_dfn = 1;
  loop_node_cnt = 0;
  init_stack();
  cg_walk (init_node, NULL, NULL);
}
#if 0
Works, but is inefficient.
static INLINE void
init_ip_scc () {
  int i;
  interprocedural_view = 1;
  current_dfn = 1;
  loop_node_cnt = 0;
  init_stack();
  for (i = 0; i < get_irp_n_irgs(); i++) {
    current_ir_graph = get_irp_irg(i);
    irg_walk_graph (current_ir_graph, init_node, NULL, NULL);
    /* @@@ decrease max_visited to avoide double walks */
  }
}
#endif

/* Condition for breaking the recursion. */
bool is_outermost_Start(ir_node *n) {
  /* Test whether this is the outermost Start node.  If so
     recursion must end. */
  if ((get_irn_op(n) == op_Block)     &&
      (get_Block_n_cfgpreds(n) == 1)  &&
      (get_irn_op(skip_Proj(get_Block_cfgpred(n, 0))) == op_Start) &&
      (get_nodes_Block(skip_Proj(get_Block_cfgpred(n, 0))) == n)) {
    return true;
  }
#if 0
  /*  @@@ Bad condition:
      not possible in interprocedural view as outermost_graph is
      not necessarily the only with a dead-end start block.
      Besides current_ir_graph is not set properly. */
  if ((get_irn_op(n) == op_Block) &&
      (n == get_irg_start_block(current_ir_graph))) {
    if ((!interprocedural_view)  ||
	(current_ir_graph == outermost_ir_graph))
      return true;
  }
#endif
  return false;
}

/* Don't walk from nodes to blocks except for Control flow operations. */
static INLINE int
get_start_index(ir_node *n) {
  if (is_cfop(n) || is_fragile_op(n) || get_irn_op(n) == op_Start)
    return -1;
  else
    return 0;
}

/* Returns current_ir_graph and set it to the irg of predecessor index
   of node n. */
static INLINE ir_graph *
switch_irg (ir_node *n, int index) {
  ir_graph *old_current = current_ir_graph;

  if (interprocedural_view) {
    /* Only Filter and Block nodes can have predecessors in other graphs. */
    if (get_irn_op(n) == op_Filter)
      n = get_nodes_Block(n);
    if (get_irn_op(n) == op_Block) {
      ir_node *cfop = skip_Proj(get_Block_cfgpred(n, index));
      if (is_ip_cfop(cfop)) {
	current_ir_graph = get_irn_irg(cfop);
	set_irg_visited(current_ir_graph, get_max_irg_visited());
      }
    }
  }

  return old_current;
}

/* Walks up the stack passing n and then finding the node
   where we walked into the irg n is contained in.
   Here we switch the irg. */
static ir_graph *
find_irg_on_stack (ir_node *n) {
  ir_node *m;
  ir_graph *old_current = current_ir_graph;
  int i;

  if (interprocedural_view) {
    for (i = tos; i >= 0; i--) {
      if (stack[i] == n) break;
    }
    if (i < 0) i = tos;

    //printf(" Here\n");

    assert (i >= 0);
    for (; i >= 0; i--) {
      m = stack[i];
      //printf(" Visiting %d ", i); DDMN(m);
      if (is_ip_cfop(m)) {
	current_ir_graph = get_irn_irg(m);
	break;
      }
      if (get_irn_op(m) == op_Filter) {
	/* Find the corresponding ip_cfop */
	ir_node *pred = stack[i+1];
	int j;
	for (j = 0; j < get_Filter_n_cg_preds(m); j++)
	  if (get_Filter_cg_pred(m, j) == pred) break;
	if (j >= get_Filter_n_cg_preds(m))
	  /* It is a filter we didn't pass as the predecessors are marked. */
	  continue;
	assert(get_Filter_cg_pred(m, j) == pred);
	switch_irg(m, j);
	break;
      }
    }
  }

  return old_current;
}

static void test(ir_node *pred, ir_node *root, ir_node *this) {
  int i;
  if (get_irn_uplink(pred) >= get_irn_uplink(root)) return;

  printf("this: %d ", get_irn_uplink(this)); DDMN(this);
  printf("pred: %d ", get_irn_uplink(pred)); DDMN(pred);
  printf("root: %d ", get_irn_uplink(root)); DDMN(root);

  printf("tos: %d\n", tos);

  for (i = tos; i >= 0; i--) {
    ir_node *n = stack[i];
    if (!n) continue;
    printf(" uplink: %d, pos: %d ", get_irn_uplink(n), i); DDMN(n);
  }
}

/* Returns true if n is a loop header, i.e., it is a Block, Phi
   or Filter node and has predecessors within the loop and out
   of the loop. */
static bool
is_head (ir_node *n, ir_node *root)
{
  int i;
  int some_outof_loop = 0,  some_in_loop = 0;

  /* Test for legal loop header */
  if (!((get_irn_op(n) == op_Block) ||
	(get_irn_op(n) == op_Phi) ||
	((get_irn_op(n) == op_Filter) && interprocedural_view)))
    return false;

  if (!is_outermost_Start(n)) {
    for (i = get_start_index(n); i < get_irn_arity(n); i++) {
      ir_node *pred = get_irn_n(n, i);
      assert(pred);
      if (is_backedge(n, i)) continue;
      if (!irn_is_in_stack(pred)) {
	some_outof_loop = 1;
      } else {
	assert(get_irn_uplink(pred) >= get_irn_uplink(root));
	some_in_loop = 1;
      }
    }
  }
  return some_outof_loop && some_in_loop;
}

/* Returns index of the predecessor with the smallest dfn number
   greater-equal than limit. */
static int
smallest_dfn_pred (ir_node *n, int limit)
{
  int i, index = -2, min = -1;

  if (!is_outermost_Start(n)) {
    for (i = get_start_index(n); i < get_irn_arity(n); i++) {
      ir_node *pred = get_irn_n(n, i);
      assert(pred);
      if (is_backedge(n, i) || !irn_is_in_stack(pred)) continue;
      if (get_irn_dfn(pred) >= limit
	&& (min == -1 || get_irn_dfn(pred) < min)) {
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

  if (!is_outermost_Start(n)) {
    for (i = get_start_index(n); i < get_irn_arity(n); i++) {
      ir_node *pred = get_irn_n(n, i);
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

  /*
    if (!icfg && rm_cyclic_phis && remove_cyclic_phis (n)) return NULL;
  */

  m = stack[tos-1];
  if (is_head (m, n)) {
    res_index = smallest_dfn_pred(m, 0);
    if ((res_index == -2) &&  /* no smallest dfn pred found. */
	(n == m))
      return NULL;
  } else {
    if (m == n) return NULL;
    for (i = tos-2; ; --i) {
      m = stack[i];
      if (is_head (m, n)) {
	res_index = smallest_dfn_pred (m, get_irn_dfn(m) + 1);
	if (res_index == -2)  /* no smallest dfn pred found. */
	  res_index = largest_dfn_pred (m);
	break;
      }
    }
  }
  assert (res_index > -2);

  set_backedge (m, res_index);
  return is_outermost_Start(n) ? NULL : get_irn_n(m, res_index);
}


/* The core algorithm. *****************************************/

void scc (ir_node *n) {
  int i;
  ir_graph *rem;

  if (irn_visited(n)) return;
  mark_irn_visited(n);
  //printf("mark: %d ", get_irn_visited(n)); DDMN(n);
  //DDME(get_irg_ent(current_ir_graph));

  /* Initialize the node */
  set_irn_dfn(n, current_dfn);      /* Depth first number for this node */
  set_irn_uplink(n, current_dfn);   /* ... is default uplink. */
  set_irn_loop_tmp(n, NULL);
  current_dfn ++;

  /* What's this good for?
  n->ana.scc.section = NULL;
  */

  push(n);

  if (!is_outermost_Start(n)) {
    for (i = get_start_index(n); i < get_irn_arity(n); i++) {
      ir_node *m;
      if (is_backedge(n, i)) continue;

      m = get_irn_n(n, i); //get_irn_ip_pred(n, i);
      if ((!m) || (get_irn_op(m) == op_Unknown)) continue;
      scc (m);
      //return_recur(n, i);

      if (irn_is_in_stack(m)) {
	/* Uplink of m is smaller if n->m is a backedge.
	   Propagate the uplink to mark the loop. */
	if (get_irn_uplink(m) < get_irn_uplink(n))
	  set_irn_uplink(n, get_irn_uplink(m));
      }
    }
  }
  if (get_irn_dfn(n) == get_irn_uplink(n)) {
    /* This condition holds for the node with the incoming backedge. */
    ir_node *tail = find_tail(n);
    if (tail) {
      /* We found a new loop! */
      ir_loop *l = new_loop();
      /* Remove the loop from the stack ... */
      pop_scc_unmark_visit (n);
      /* and recompute it in a better order; and so that it goes into
	 the new loop. */
      rem = find_irg_on_stack(tail);
      scc (tail);
      current_ir_graph = rem;

      assert (irn_visited(n));

      current_loop = l;
    } else {
      pop_scc_to_loop(n);
    }
  }
}

/* Constructs backedge information for irg. In interprocedural view constructs
   backedges for all methods called by irg, too. */
void construct_backedges(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  ir_loop *head_rem;
  int i;

  assert(!interprocedural_view &&
	 "not implemented, use construct_ip_backedges");

  current_ir_graph = irg;
  outermost_ir_graph = irg;

  init_scc(irg);

  current_loop = NULL;
  new_loop();  /* sets current_loop */
  head_rem = current_loop; /* Just for assertion */

  if (interprocedural_view) {
    set_irg_visited(irg, inc_max_irg_visited());
    init_ip_walk ();
  } else {
    inc_irg_visited(irg);
  }

  scc(get_irg_end(irg));
  for (i = 0; i < get_End_n_keepalives(get_irg_end(irg)); i++)
    scc(get_End_keepalive(get_irg_end(irg), i));

  if (interprocedural_view) finish_ip_walk();

  assert(head_rem == current_loop);
  set_irg_loop(irg, current_loop);
  assert(get_irg_loop(irg)->kind == k_ir_loop);
  /*
  irg->loops = current_loop;
  if (icfg == 1) {
    int count = 0;
    int depth = 0;
    count_loop (the_loop, &count, &depth);
    }
  }
  */
  current_ir_graph = rem;
}



void construct_ip_backedges () {
  ir_graph *rem = current_ir_graph;
  int rem_ipv = interprocedural_view;
  int i, j;

  outermost_ir_graph = get_irp_main_irg();

  init_ip_scc();

  current_loop = NULL;
  new_loop();  /* sets current_loop */
  interprocedural_view = 1;

  inc_max_irg_visited();
  for (i = 0; i < get_irp_n_irgs(); i++)
    set_irg_visited(get_irp_irg(i), get_max_irg_visited());

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);
    //DDME(get_irg_ent(current_ir_graph));
    /* Find real entry points */
    sb = get_irg_start_block(current_ir_graph);
    if ((get_Block_n_cfgpreds(sb) > 1) ||
	(get_nodes_Block(get_Block_cfgpred(sb, 0)) != sb)) continue;
    //    printf("running scc for "); DDME(get_irg_ent(current_ir_graph));
    /* Compute scc for this graph */
    outermost_ir_graph = current_ir_graph;
    set_irg_visited(outermost_ir_graph, get_max_irg_visited());
    scc(get_irg_end(current_ir_graph));
    for (j = 0; j < get_End_n_keepalives(get_irg_end(outermost_ir_graph)); j++)
      scc(get_End_keepalive(get_irg_end(outermost_ir_graph), j));
  }

  set_irg_loop(outermost_ir_graph, current_loop);
  assert(get_irg_loop(outermost_ir_graph)->kind == k_ir_loop);

  current_ir_graph = rem;
  interprocedural_view = rem_ipv;
}
