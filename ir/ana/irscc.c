/*
 * Project:     libFIRM
 * File name:   ir/ana/irscc.c
 * Purpose:     Compute the strongly connected regions and build
 *              backedge/loop datastructures.
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

#include <stdlib.h>

#include "irloop_t.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "array.h"
#include "pmap.h"

#include "irdump.h"

/* A variant of the loop tree that avoids loops without head.
   This reduces the depth of the loop tree. */
#define NO_LOOPS_WITHOUT_HEAD 1

static ir_graph *outermost_ir_graph;      /* The outermost graph the scc is computed
					     for */
static ir_loop *current_loop;      /* Current loop construction is working
				      on. */
static int loop_node_cnt = 0;      /* Counts the number of allocated loop nodes.
				      Each loop node gets a unique number.
				      What for? ev. remove. @@@ */
static int current_dfn = 1;        /* Counter to generate depth first numbering
				      of visited nodes.  */

static int max_loop_depth = 0;

void link_to_reg_end (ir_node *n, void *env);
void set_projx_link(ir_node *cb_projx, ir_node *end_projx);
ir_node *get_projx_link(ir_node *cb_projx);

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
  /*  ir_loop *loop;         *//* Refers to the containing loop. */
  /*
      struct section *section;
      xset def;
      xset use;
  */
} scc_info;

static INLINE scc_info* new_scc_info(void) {
  scc_info *info = obstack_alloc (outermost_ir_graph->obst, sizeof (scc_info));
  memset (info, 0, sizeof (scc_info));
  return info;
}

static INLINE void
mark_irn_in_stack (ir_node *n) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  scc->in_stack = true;
}

static INLINE void
mark_irn_not_in_stack (ir_node *n) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  scc->in_stack = false;
}

static INLINE bool
irn_is_in_stack (ir_node *n) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  return scc->in_stack;
}

static INLINE void
set_irn_uplink (ir_node *n, int uplink) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  scc->uplink = uplink;
}

int
get_irn_uplink (ir_node *n) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  return scc->uplink;
}

static INLINE void
set_irn_dfn (ir_node *n, int dfn) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  scc->dfn = dfn;
}

int
get_irn_dfn (ir_node *n) {
  scc_info *scc = get_irn_link(n);
  assert(scc);
  return scc->dfn;
}


void
set_irn_loop (ir_node *n, ir_loop *loop) {
  n->loop = loop;
}

/* Uses temporary information to get the loop */
ir_loop *
get_irn_loop (ir_node *n) {
  return n->loop;
}


#if 0
static ir_loop *find_nodes_loop (ir_node *n, ir_loop *l) {
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
#endif

/**********************************************************************/
/* A stack.                                                          **/
/**********************************************************************/

static ir_node **stack = NULL;
static int tos = 0;                /* top of stack */

/**
 * initializes the stack
 */
static INLINE void init_stack(void) {
  if (stack) {
    ARR_RESIZE (ir_node *, stack, 1000);
  } else {
    stack = NEW_ARR_F (ir_node *, 1000);
  }
  tos = 0;
}

#if 0
static INLINE void free_stack(void) {
  DEL_ARR_F(stack);
  stack = NULL;
  tos = 0;
}
#endif

/**
 * push a node onto the stack
 *
 * @param n  The node to push
 */
static INLINE void
push (ir_node *n)
{
  /*DDMN(n);*/

  if (tos == ARR_LEN (stack)) {
    int nlen = ARR_LEN (stack) * 2;
    ARR_RESIZE (ir_node *, stack, nlen);
  }
  stack [tos++] = n;
  mark_irn_in_stack(n);
}

/**
 * pop a node from the stack
 *
 * @return  The topmost node
 */
static INLINE ir_node *
pop (void)
{
  ir_node *n = stack[--tos];
  mark_irn_not_in_stack(n);
  return n;
}

/**
 * The nodes up to n belong to the current loop.
 * Removes them from the stack and adds them to the current loop.
 */
static INLINE void
pop_scc_to_loop (ir_node *n)
{
  ir_node *m;
  int i = 0;

  do {
    m = pop();

    //printf(" dfn: %d, upl %d upl-new %d ", get_irn_dfn(m), get_irn_uplink(m), loop_node_cnt+1); DDMN(m);

    loop_node_cnt++;
    set_irn_dfn(m, loop_node_cnt);
    add_loop_node(current_loop, m);
    set_irn_loop(m, current_loop);
    i++;

    /*    if (m==n) break;*/
  } while(m != n);

  /* i might be bigger than 1 for dead (and that's why bad) loops */
  /* if(i > 1)
    printf("Mehr als eine Iteration!!!!!!!!!!!!!!!!!!!!!!!!!!!!11111\n");
   */
}

/* GL ??? my last son is my grandson???  Removes loops with no
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

    if (get_kind(gson) == k_ir_loop) {
      loop_element new_last_son;

      gson->outer_loop = l;
      new_last_son.son = gson;
      l->children[last] = new_last_son;
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
ir_loop *new_loop (void) {
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

#if 0
/* Finishes the datastructures, copies the arrays to the obstack
   of current_ir_graph.
   A. Schoesser: Caution: loop -> sons is gone. */
static void mature_loop (ir_loop *loop) {
  ir_loop **new_sons;

  new_sons = NEW_ARR_D (ir_loop *, current_ir_graph->obst, ARR_LEN(loop->sons));
  memcpy (new_sons, loop->sons, sizeof (ir_loop *) * ARR_LEN(loop->sons));
  DEL_ARR_F(loop->sons);
  loop->sons = new_sons;
}
#endif

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
  return(loop -> n_sons);
}

/* Returns the pos`th loop_node-child              *
 * TODO: This method isn`t very efficient !        *
 * Returns NULL if there isnt`t a pos`th loop_node */
ir_loop *get_loop_son (ir_loop *loop, int pos) {
  int child_nr = 0, loop_nr = -1;

  assert(loop && loop->kind == k_ir_loop);
  while(child_nr < ARR_LEN(loop->children))
   {
    if(*(loop -> children[child_nr].kind) == k_ir_loop)
      loop_nr++;
    if(loop_nr == pos)
      return(loop -> children[child_nr].son);
    child_nr++;
   }
  return NULL;
}

/* Use EXCLUSIVELY this function to add sons, otherwise the loop->n_sons
   is invalid! */

void
add_loop_son(ir_loop *loop, ir_loop *son) {
  loop_element lson;
  lson.son = son;
  assert(loop && loop->kind == k_ir_loop);
  assert(get_kind(son) == k_ir_loop);
  ARR_APP1 (loop_element, loop->children, lson);
  loop -> n_sons++;
}

/* Returns the number of nodes in the loop */
int      get_loop_n_nodes (ir_loop *loop) {
  assert(loop); assert(loop->kind == k_ir_loop);
  return loop -> n_nodes;
/*  return ARR_LEN(loop->nodes); */
}

/* Returns the pos`th ir_node-child                *
 * TODO: This method isn`t very efficient !        *
 * Returns NULL if there isnt`t a pos`th ir_node   */
ir_node *get_loop_node (ir_loop *loop, int pos) {
  int child_nr, node_nr = -1;

  assert(loop && loop->kind == k_ir_loop);
  assert(pos < get_loop_n_nodes(loop));

  for (child_nr = 0; child_nr < ARR_LEN(loop->children); child_nr++) {
    if(*(loop -> children[child_nr].kind) == k_ir_node)
      node_nr++;
    if(node_nr == pos)
      return(loop -> children[child_nr].node);
  }
  DDML(loop);
  printf("pos: %d\n", pos);
  assert(0 && "no child at pos found");
  return NULL;
}

/* Use EXCLUSIVELY this function to add nodes, otherwise the loop->n_nodes
   is invalid! */

void
add_loop_node(ir_loop *loop, ir_node *n) {
  loop_element ln;
  ln.node = n;
  assert(loop && loop->kind == k_ir_loop);
  assert(get_kind(n) == k_ir_node || get_kind(n) == k_ir_graph);  /* used in callgraph.c */
  ARR_APP1 (loop_element, loop->children, ln);
  loop->n_nodes++;
}

/** Returns the number of elements contained in loop.  */
int get_loop_n_elements (ir_loop *loop) {
  assert(loop && loop->kind == k_ir_loop);
  return(ARR_LEN(loop->children));
}

/*
 Returns the pos`th loop element.
 This may be a loop_node or a ir_node. The caller of this function has
 to check the *(loop_element.kind) field for "k_ir_node" or "k_ir_loop"
 and then select the apropriate "loop_element.node" or "loop_element.son".
*/

loop_element get_loop_element (ir_loop *loop, int pos) {
  assert(loop && loop->kind == k_ir_loop && pos < ARR_LEN(loop->children));

  return(loop -> children[pos]);
}

int get_loop_element_pos(ir_loop *loop, void *le) {
  int i;
  assert(loop && loop->kind == k_ir_loop);

  for (i = 0; i < get_loop_n_elements(loop); i++)
    if (get_loop_element(loop, i).node == le) return i;
  return -1;
}

int get_loop_loop_nr(ir_loop *loop) {
  assert(loop && loop->kind == k_ir_loop);
#ifdef DEBUG_libfirm
  return loop->loop_nr;
#else
  return (int)loop;
#endif
}


/** A field to connect additional information to a loop.  Only valid
    if libfirm_debug is set. */
void  set_loop_link (ir_loop *loop, void *link) {
  assert(loop && loop->kind == k_ir_loop);
#ifdef DEBUG_libfirm
  loop->link = link;
#endif
}
void *get_loop_link (const ir_loop *loop) {
  assert(loop && loop->kind == k_ir_loop);
#ifdef DEBUG_libfirm
  return loop->link;
#else
  return NULL;
#endif
}

int is_ir_loop(const void *thing) {
  return (get_kind(thing) == k_ir_loop);
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
  /*
  irg_walk (irg, link_to_reg_end, NULL, NULL);
  */
}

static INLINE void
init_ip_scc (void) {
  init_scc_common();
  cg_walk (init_node, NULL, NULL);

#if EXPERIMENTAL_LOOP_TREE
  cg_walk (link_to_reg_end, NULL, NULL);
#endif
}

/* Condition for breaking the recursion. */
static bool is_outermost_Start(ir_node *n) {
  /* Test whether this is the outermost Start node.  If so
     recursion must end. */
  if ((get_irn_op(n) == op_Block)     &&
      (get_Block_n_cfgpreds(n) == 1)  &&
      (get_irn_op(skip_Proj(get_Block_cfgpred(n, 0))) == op_Start) &&
      (get_nodes_block(skip_Proj(get_Block_cfgpred(n, 0))) == n)) {
    return true;
  }
#if 0
  /*  @@@ Bad condition:
      not possible in interprocedural view as outermost_graph is
      not necessarily the only with a dead-end start block.
      Besides current_ir_graph is not set properly. */
  if ((get_irn_op(n) == op_Block) &&
      (n == get_irg_start_block(current_ir_graph))) {
    if ((!get_interprocedural_view())  ||
    (current_ir_graph == outermost_ir_graph))
      return true;
  }
#endif
  return false;
}

/* When to walk from nodes to blocks. Only for Control flow operations? */
static INLINE int
get_start_index(ir_node *n) {
#undef BLOCK_BEFORE_NODE
#define BLOCK_BEFORE_NODE 1

#if BLOCK_BEFORE_NODE

  /* This version assures, that all nodes are ordered absolutely.  This allows
     to undef all nodes in the heap analysis if the block is false, which means
     not reachable.
     I.e., with this code, the order on the loop tree is correct. But a (single)
     test showed the loop tree is deeper.   */
  if (get_irn_op(n) == op_Phi   ||
      get_irn_op(n) == op_Block ||
      (get_irn_op(n) == op_Filter && get_interprocedural_view()) ||
      (get_irg_pinned(get_irn_irg(n)) == op_pin_state_floats &&
       get_irn_pinned(n) == op_pin_state_floats))
    // Here we could test for backedge at -1 which is illegal
    return 0;
  else
    return -1;

#else

  /* This version causes deeper loop trees (at least we verified this
     for Polymor).
     But it guarantees that Blocks are analysed before nodes contained in the
     block.  If so, we can set the value to undef if the block is not \
     executed. */
   if (is_cfop(n) || is_fragile_op(n) || get_irn_op(n) == op_Start)
     return -1;
   else
     return 0;

#endif
}


#if 0
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
#endif

/* Test for legal loop header: Block, Phi, ... */
static INLINE bool is_possible_loop_head(ir_node *n) {
  ir_op *op = get_irn_op(n);
  return ((op == op_Block) ||
	  (op == op_Phi) ||
	  ((op == op_Filter) && get_interprocedural_view()));
}

/* Returns true if n is a loop header, i.e., it is a Block, Phi
   or Filter node and has predecessors within the loop and out
   of the loop.
   @arg root: only needed for assertion. */
static bool
is_head (ir_node *n, ir_node *root)
{
  int i, arity;
  int some_outof_loop = 0, some_in_loop = 0;

  /* Test for legal loop header: Block, Phi, ... */
  if (!is_possible_loop_head(n))
    return false;

  if (!is_outermost_Start(n)) {
    arity = get_irn_arity(n);
    for (i = get_start_index(n); i < arity; i++) {
      ir_node *pred = get_irn_n(n, i);
      assert(pred);
      if (is_backedge(n, i)) continue;
      if (!irn_is_in_stack(pred)) {
	some_outof_loop = 1;
      } else {
	if(get_irn_uplink(pred) < get_irn_uplink(root)) {
	  DDMN(n); DDMN(pred); DDMN(root);
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

  /* Test for legal loop header: Block, Phi, ... */
  if (!is_possible_loop_head(n))
    return false;

  if (!is_outermost_Start(n)) {
    arity = get_irn_arity(n);
    for (i = get_start_index(n); i < arity; i++) {
      ir_node *pred = get_irn_n(n, i);
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

  if (!is_outermost_Start(n)) {
    int arity = get_irn_arity(n);
    for (i = get_start_index(n); i < arity; i++) {
      ir_node *pred = get_irn_n(n, i);
      assert(pred);
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

  if (!is_outermost_Start(n)) {
    int arity = get_irn_arity(n);
    for (i = get_start_index(n); i < arity; i++) {
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

/** Searches the stack for possible loop heads.  Tests these for backedges.
    If it finds a head with an unmarked backedge it marks this edge and
    returns the tail of the loop.
    If it finds no backedge returns NULL.
    ("disable_backedge" in fiasco)
*
*  @param n  A node where uplink == dfn.
**/

static ir_node *
find_tail (ir_node *n) {
  ir_node *m;
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
	res_index = smallest_dfn_pred (m, get_irn_dfn(m) + 1);
	if (res_index == -2)  /* no smallest dfn pred found. */
	  res_index = largest_dfn_pred (m);

	if ((m == n) && (res_index == -2)) {  /* dont walk past loop head. */
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
  if (res_index <= -2) {
    /* It's a completely bad loop: without Phi/Block nodes that can
       be a head. I.e., the code is "dying".  We break the loop by
       setting Bad nodes. */
    int arity = get_irn_arity(n);
    for (i = -1; i < arity; ++i) {
      set_irn_n(n, i, get_irg_bad(get_irn_irg(n)));
    }
    return NULL;
  }
  assert (res_index > -2);

  set_backedge (m, res_index);
  return is_outermost_Start(n) ? NULL : get_irn_n(m, res_index);
}


#if EXPERIMENTAL_LOOP_TREE

/*  ----------------------------------------------------------------
    AS:  This is experimantal code to build loop trees suitable for
    the heap analysis. Does not work correctly right now... :-(


    Search in stack for the corresponding first Call-End-ProjX that
    corresponds to one of the control flow predecessors of the given
    block, that is the possible callers.
    returns: the control predecessor to chose\
    or       -1 if no corresponding Call-End-Node could be found
             on the stack.
    - -------------------------------------------------------------- */

int search_endproj_in_stack(ir_node *start_block)
{
  int i, j;
  assert(is_Block(start_block));
  for(i = tos - 1; i >= 0; --i)
    {
      DDMN(stack[i]);
      if(get_irn_op(stack[i]) == op_Proj && get_irn_mode(stack[i]) == mode_X &&
	 get_irn_op(get_irn_n(stack[i], 0)) == op_EndReg)
	{
	  printf("FOUND PROJ!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
	  ir_node *end_projx = stack[i];

	  int arity = get_irn_arity(start_block);
	  for(j = 0; j < arity; j++)
	    {
	      ir_node *begin_projx = get_Block_cfgpred(get_irg_start_block(get_irn_irg(end_projx)),
						       get_Proj_proj(end_projx));
	      DDMN(begin_projx);
	      if(get_irn_n(start_block, j) == begin_projx)
		{
		  printf("FOUND IT!!!!!!!!!!!!!!!!!!\n");
		  return(j);
		}
	    }
	}
    }
  return(-1);
}


static pmap *projx_link = NULL;

void link_to_reg_end (ir_node *n, void *env) {
  if(get_irn_op(n) == op_Proj &&
     get_irn_mode(n) == mode_X &&
     get_irn_op(get_irn_n(n, 0)) == op_EndReg) {
      /* Reg End Projx -> Find the CallBegin Projx and hash it */
      ir_node *end_projx = n;
      ir_node *begin_projx = get_Block_cfgpred(get_irg_start_block(get_irn_irg(end_projx)),
					       get_Proj_proj(end_projx));
      printf("Linked the following ProjxNodes:\n");
      DDMN(begin_projx);
      DDMN(end_projx);
      set_projx_link(begin_projx, end_projx);
    }
}

void set_projx_link(ir_node *cb_projx, ir_node *end_projx)
{
  if(projx_link == NULL)
    projx_link = pmap_create();
  pmap_insert(projx_link, (void *)cb_projx, (void *)end_projx);
}

ir_node *get_projx_link(ir_node *cb_projx)
{
  return((ir_node *) pmap_get(projx_link, (void *)cb_projx));
}

#endif

static INLINE int
is_outermost_loop(ir_loop *l) {
  return l == get_loop_outer_loop(l);
}


/*-----------------------------------------------------------*
 *                   The core algorithm.                     *
 *-----------------------------------------------------------*/

static void scc (ir_node *n) {
  int i;
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

  if (!is_outermost_Start(n)) {
    int arity = get_irn_arity(n);

    for (i = get_start_index(n); i < arity; i++) {
      ir_node *m;
      if (is_backedge(n, i)) continue;
      m = get_irn_n(n, i); /* get_irn_ip_pred(n, i); */
      /* if ((!m) || (get_irn_op(m) == op_Unknown)) continue; */
      scc (m);
      if (irn_is_in_stack(m)) {
	/* Uplink of m is smaller if n->m is a backedge.
	   Propagate the uplink to mark the loop. */
	if (get_irn_uplink(m) < get_irn_uplink(n))
	  set_irn_uplink(n, get_irn_uplink(m));
      }
    }
  }

  if (get_irn_dfn(n) == get_irn_uplink(n)) {
    /* This condition holds for
       1) the node with the incoming backedge.
          That is: We found a loop!
       2) Straight line code, because no uplink has been propagated, so the
          uplink still is the same as the dfn.

       But n might not be a proper loop head for the analysis. Proper loop
       heads are Block and Phi nodes. find_tail searches the stack for
       Block's and Phi's and takes those nodes as loop heads for the current
       loop instead and marks the incoming edge as backedge. */

    ir_node *tail = find_tail(n);
    if (tail) {
      /* We have a loop, that is no straight line code,
	 because we found a loop head!
	 Next actions: Open a new loop on the loop tree and
	               try to find inner loops */

#if NO_LOOPS_WITHOUT_HEAD
      /* This is an adaption of the algorithm from fiasco / optscc to
       * avoid loops without Block or Phi as first node.  This should
       * severely reduce the number of evaluations of nodes to detect
       * a fixpoint in the heap analysis.
       * Further it avoids loops without firm nodes that cause errors
       * in the heap analyses.
       * But attention:  don't do it for the outermost loop:  This loop
       * is not iterated.  A first block can be a loop head in case of
       * an endless recursion. */

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

      /* Remove the loop from the stack ... */
      pop_scc_unmark_visit (n);

      /* The current backedge has been marked, that is temporarily eliminated,
	 by find tail. Start the scc algorithm
	 anew on the subgraph that is left (the current loop without the backedge)
	 in order to find more inner loops. */
      scc (tail);

      assert (irn_visited(n));
#if NO_LOOPS_WITHOUT_HEAD
      if (close)
#endif
	close_loop(l);
    }
    else
      {
	/* No loop head was found, that is we have straightline code.
	   Pop all nodes from the stack to the current loop. */
      pop_scc_to_loop(n);
    }
  }
}

static void my_scc (ir_node *n) {
  int i;
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

  if (!is_outermost_Start(n)) {
    int arity = get_irn_arity(n);

    for (i = get_start_index(n); i < arity; i++) {
      ir_node *m;
      if (is_backedge(n, i)) continue;
      m = get_irn_n(n, i); /* get_irn_ip_pred(n, i); */
      /* if ((!m) || (get_irn_op(m) == op_Unknown)) continue; */
      my_scc (m);
      if (irn_is_in_stack(m)) {
	/* Uplink of m is smaller if n->m is a backedge.
	   Propagate the uplink to mark the loop. */
	if (get_irn_uplink(m) < get_irn_uplink(n))
	  set_irn_uplink(n, get_irn_uplink(m));
      }
    }
  }

  if (get_irn_dfn(n) == get_irn_uplink(n)) {
    /* This condition holds for
       1) the node with the incoming backedge.
          That is: We found a loop!
       2) Straight line code, because no uplink has been propagated, so the
          uplink still is the same as the dfn.

       But n might not be a proper loop head for the analysis. Proper loop
       heads are Block and Phi nodes. find_tail searches the stack for
       Block's and Phi's and takes those nodes as loop heads for the current
       loop instead and marks the incoming edge as backedge. */

    ir_node *tail = find_tail(n);
    if (tail) {
      /* We have a loop, that is no straight line code,
	 because we found a loop head!
	 Next actions: Open a new loop on the loop tree and
	               try to find inner loops */

#if NO_LOOPS_WITHOUT_HEAD
      /* This is an adaption of the algorithm from fiasco / optscc to
       * avoid loops without Block or Phi as first node.  This should
       * severely reduce the number of evaluations of nodes to detect
       * a fixpoint in the heap analysis.
       * Further it avoids loops without firm nodes that cause errors
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

      /* Remove the loop from the stack ... */
      pop_scc_unmark_visit (n);

      /* The current backedge has been marked, that is temporarily eliminated,
	 by find tail. Start the scc algorithm
	 anew on the subgraph that is left (the current loop without the backedge)
	 in order to find more inner loops. */
      my_scc (tail);

      assert (irn_visited(n));
#if NO_LOOPS_WITHOUT_HEAD
      if (close)
#endif
	close_loop(l);
    }
    else
      {
	/* No loop head was found, that is we have straightline code.
	   Pop all nodes from the stack to the current loop. */
      pop_scc_to_loop(n);
    }
  }
}

/* Constructs backedge information for irg. In interprocedural view constructs
   backedges for all methods called by irg, too. */
int construct_backedges(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  ir_loop *head_rem;

  assert(!get_interprocedural_view() &&
     "not implemented, use construct_ip_backedges");

  max_loop_depth = 0;
  current_ir_graph   = irg;
  outermost_ir_graph = irg;

  init_scc(current_ir_graph);

  current_loop = NULL;
  new_loop();  /* sets current_loop */
  head_rem = current_loop; /* Just for assertion */

  inc_irg_visited(current_ir_graph);

  scc(get_irg_end(current_ir_graph));

  assert(head_rem == current_loop);
  set_irg_loop(current_ir_graph, current_loop);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_consistent);
  assert(get_irg_loop(current_ir_graph)->kind == k_ir_loop);
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

  return max_loop_depth;
}


int construct_ip_backedges (void) {
  ir_graph *rem = current_ir_graph;
  int rem_ipv = get_interprocedural_view();
  int i;

  max_loop_depth = 0;
  assert(get_irp_ip_view_state() == ip_view_valid);

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

    scc(get_irg_end(current_ir_graph));
  }

  /* Check whether we walked all procedures: there could be procedures
     with cyclic calls but no call from the outside. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);

    /* Test start block: if inner procedure end and end block are not
     * visible and therefore not marked. */
    sb = get_irg_start_block(current_ir_graph);
    if (get_irn_visited(sb) < get_irg_visited(current_ir_graph)) scc(sb);
  }

  /* Walk all endless loops in inner procedures.
   * We recognize an inner procedure if the End node is not visited. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *e;
    current_ir_graph = get_irp_irg(i);

    e = get_irg_end(current_ir_graph);
    if (get_irn_visited(e) < get_irg_visited(current_ir_graph)) {
      int j;
      /* Don't visit the End node. */
      for (j = 0; j < get_End_n_keepalives(e); j++) scc(get_End_keepalive(e, j));
    }
  }

  set_irg_loop(outermost_ir_graph, current_loop);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_ip_consistent);
  assert(get_irg_loop(outermost_ir_graph)->kind == k_ir_loop);

  current_ir_graph = rem;
  set_interprocedural_view(rem_ipv);
  return max_loop_depth;
}

void my_construct_ip_backedges (void) {
  ir_graph *rem = current_ir_graph;
  int rem_ipv = get_interprocedural_view();
  int i;

  assert(get_irp_ip_view_state() == ip_view_valid);

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

    my_scc(get_irg_end(current_ir_graph));
  }

  /* Check whether we walked all procedures: there could be procedures
     with cyclic calls but no call from the outside. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *sb;
    current_ir_graph = get_irp_irg(i);

    /* Test start block: if inner procedure end and end block are not
     * visible and therefore not marked. */
    sb = get_irg_start_block(current_ir_graph);
    if (get_irn_visited(sb) < get_irg_visited(current_ir_graph)) scc(sb);
  }

  /* Walk all endless loops in inner procedures.
   * We recognize an inner procedure if the End node is not visited. */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_node *e;
    current_ir_graph = get_irp_irg(i);

    e = get_irg_end(current_ir_graph);
    if (get_irn_visited(e) < get_irg_visited(current_ir_graph)) {
      int j;
      /* Don't visit the End node. */
      for (j = 0; j < get_End_n_keepalives(e); j++) scc(get_End_keepalive(e, j));
    }
  }

  set_irg_loop(outermost_ir_graph, current_loop);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_ip_consistent);
  assert(get_irg_loop(outermost_ir_graph)->kind == k_ir_loop);

  current_ir_graph = rem;
  set_interprocedural_view(rem_ipv);
}

static void reset_backedges(ir_node *n) {
  if (is_possible_loop_head(n)) {
    int rem = get_interprocedural_view();

    set_interprocedural_view(true);
    clear_backedges(n);
    set_interprocedural_view(true);
    clear_backedges(n);
    set_interprocedural_view(rem);
  }
}


/*
static void loop_reset_backedges(ir_loop *l) {
  int i;
  reset_backedges(get_loop_node(l, 0));
  for (i = 0; i < get_loop_n_nodes(l); ++i)
    set_irn_loop(get_loop_node(l, i), NULL);
  for (i = 0; i < get_loop_n_sons(l); ++i) {
    loop_reset_backedges(get_loop_son(l, i));
  }
}
*/

static void loop_reset_node(ir_node *n, void *env) {
  set_irn_loop(n, NULL);
  reset_backedges(n);
}


/** Removes all loop information.
    Resets all backedges */
void free_loop_information(ir_graph *irg) {
  /* We can not use this recursion, as the loop might contain
     illegal nodes by now.  Why else would we throw away the
     representation?
  if (get_irg_loop(irg)) loop_reset_backedges(get_irg_loop(irg));
  */
  irg_walk_graph(irg, loop_reset_node, NULL, NULL);
  set_irg_loop(irg, NULL);
  set_irg_loopinfo_state(current_ir_graph, loopinfo_none);
  /* We cannot free the loop nodes, they are on the obstack. */
}


void free_all_loop_information (void) {
  int i;
  int rem = get_interprocedural_view();
  set_interprocedural_view(true);  /* To visit all filter nodes */
  for (i = 0; i < get_irp_n_irgs(); i++) {
    free_loop_information(get_irp_irg(i));
  }
  set_interprocedural_view(rem);
}





/* Debug stuff *************************************************/

static int test_loop_node(ir_loop *l) {
  int i, has_node = 0, found_problem = 0;
  loop_element le;

  assert(l && l->kind == k_ir_loop);

  if (get_loop_n_elements(l) == 0) {
    printf(" Loop completely empty! "); DDML(l);
    found_problem = 1;
    dump_loop(l, "-ha");
  }

  le = get_loop_element(l, 0);
  if (*(le.kind) != k_ir_node) {
    assert(le.kind && *(le.kind) == k_ir_loop);
    printf(" First loop element is not a node! "); DDML(l);
    printf("                                   "); DDML(le.son);

    found_problem = 1;
    dump_loop(l, "-ha");
  }

  if ((*(le.kind) == k_ir_node) && !is_possible_loop_head(le.node)) {
    printf(" Wrong node as head! "); DDML(l);
    printf("                     "); DDMN(le.node);
    found_problem = 1;
    dump_loop(l, "-ha");
  }

  if ((get_loop_depth(l) != 0) &&
      (*(le.kind) == k_ir_node) && !has_backedges(le.node)) {
    printf(" Loop head has no backedges! "); DDML(l);
    printf("                             "); DDMN(le.node);
    found_problem = 1;
    dump_loop(l, "-ha");
  }

  /* Recur */
  has_node = 0;
  for (i = 0; i < get_loop_n_elements(l); ++i) {
    le = get_loop_element(l, i);
    if (*(le.kind) == k_ir_node)
      has_node++;
    else
      if (test_loop_node(le.son)) found_problem = 1;
  }

  if (has_node == 0) {
    printf(" Loop has no firm node! "); DDML(l);
    found_problem = 1;
    dump_loop(l, "-ha");
  }

  return found_problem;
}

/** Prints all loop nodes that
 *  - do not have any firm nodes, only loop sons
 *  - the header is not a Phi, Block or Filter.
 */
void find_strange_loop_nodes(ir_loop *l) {
  int found_problem = 0;
  printf("\nTesting loop "); DDML(l);
  found_problem = test_loop_node(l);
  printf("Finished Test\n\n");
  if (found_problem) exit(0);

}

/* ------------------------------------------------------------------- */
/* Simple analyses based on the loop information                       */
/* ------------------------------------------------------------------- */

int is_loop_variant(ir_loop *l, ir_loop *b) {
  int i, n_elems;

  if (l == b) return true;

  n_elems = get_loop_n_elements(l);
  for (i = 0; i < n_elems; ++i) {
    loop_element e = get_loop_element(l, i);
    if (is_ir_loop(e.kind))
      if (is_loop_variant(e.son, b))
        return true;
  }

  return false;
}

/* Test whether a value is loop invariant.
 *
 * @param n      The node to be tested.
 * @param block  A block node.  We pass the block, not the loop as we must
 *               start off with a block loop to find all proper uses.
 *
 * Returns true, if the node n is not changed in the loop block
 * belongs to or in inner loops of this blocks loop. */
int is_loop_invariant(ir_node *n, ir_node *block) {
  ir_loop *l = get_irn_loop(block);
  ir_node *b = (is_Block(n)) ? n : get_nodes_block(n);
  return !is_loop_variant(l, get_irn_loop(b));
}
