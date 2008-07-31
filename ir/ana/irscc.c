/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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

/**
 * @file
 * @brief    Compute the strongly connected regions and build
 *              backedge/loop datastructures.
 *              A variation on the Tarjan algorithm. See also [Trapp:99],
 *              Chapter 5.2.1.2.
 * @author   Goetz Lindenmaier
 * @date     7.2002
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "irloop_t.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "array.h"
#include "pmap.h"

/* A variant of the loop tree that avoids loops without head.
   This reduces the depth of the loop tree. */
#define NO_LOOPS_WITHOUT_HEAD 1

/** The outermost graph the scc is computed for. */
static ir_graph *outermost_ir_graph;
/** Current loop construction is working on. */
static ir_loop *current_loop;
/** Counts the number of allocated loop nodes.
 *  Each loop node gets a unique number.
 *  @todo What for? ev. remove.
 */
static int loop_node_cnt = 0;
/** Counter to generate depth first numbering of visited nodes. */
static int current_dfn = 1;

static int max_loop_depth = 0;

void link_to_reg_end(ir_node *n, void *env);
void set_projx_link(ir_node *cb_projx, ir_node *end_projx);
ir_node *get_projx_link(ir_node *cb_projx);

/**********************************************************************/
/* Node attributes                                                   **/
/**********************************************************************/

/**********************************************************************/
/* Node attributes needed for the construction.                      **/
/**********************************************************************/

typedef struct scc_info {
	int in_stack;          /**< Marks whether node is on the stack. */
	int dfn;               /**< Depth first search number. */
	int uplink;            /**< dfn number of ancestor. */
	/*  ir_loop *loop;         *//* Refers to the containing loop. */
	/*
	    struct section *section;
	    xset def;
	    xset use;
	*/
} scc_info;

/**
 * Allocates a new SCC info on the given obstack.
 */
static INLINE scc_info *new_scc_info(struct obstack *obst) {
	scc_info *info = obstack_alloc(obst, sizeof(*info));
	memset(info, 0, sizeof(*info));
	return info;
}

/**
 * Mark node n being on the SCC stack.
 */
static INLINE void mark_irn_in_stack(ir_node *n) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	scc->in_stack = 1;
}

/**
* Mark node n NOT being on the SCC stack.
*/
static INLINE void mark_irn_not_in_stack(ir_node *n) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	scc->in_stack = 0;
}

/**
 * Checks if a node is on the SCC stack.
 */
static INLINE int irn_is_in_stack(ir_node *n) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	return scc->in_stack;
}

/**
 * Sets the uplink number for a node.
 */
static INLINE void set_irn_uplink(ir_node *n, int uplink) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	scc->uplink = uplink;
}

/**
 * Returns the uplink number for a node.
 */
static int get_irn_uplink(ir_node *n) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	return scc->uplink;
}

/**
 * Sets the depth-first-search number for a node.
 */
static INLINE void set_irn_dfn(ir_node *n, int dfn) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	scc->dfn = dfn;
}

/**
 * Returns the depth-first-search number of a node.
 */
static int get_irn_dfn(ir_node *n) {
	scc_info *scc = get_irn_link(n);
	assert(scc);
	return scc->dfn;
}

#if 0
static ir_loop *find_nodes_loop(ir_node *n, ir_loop *l) {
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
		ARR_RESIZE(ir_node *, stack, 1000);
	} else {
		stack = NEW_ARR_F(ir_node *, 1000);
	}
	tos = 0;
}

/**
 * Frees the stack.
 */
static void finish_stack(void) {
	DEL_ARR_F(stack);
	stack = NULL;
}

/**
 * push a node onto the stack
 *
 * @param n  The node to push
 */
static INLINE void push(ir_node *n) {
	if (tos == ARR_LEN(stack)) {
		int nlen = ARR_LEN(stack) * 2;
		ARR_RESIZE(ir_node *, stack, nlen);
	}
	stack [tos++] = n;
	mark_irn_in_stack(n);
}

/**
 * pop a node from the stack
 *
 * @return  The topmost node
 */
static INLINE ir_node *pop(void) {
	ir_node *n = stack[--tos];
	mark_irn_not_in_stack(n);
	return n;
}

/**
 * The nodes up to n belong to the current loop.
 * Removes them from the stack and adds them to the current loop.
 */
static INLINE void pop_scc_to_loop(ir_node *n) {
	ir_node *m;
	int i = 0;

	do {
		m = pop();

		loop_node_cnt++;
		set_irn_dfn(m, loop_node_cnt);
		add_loop_node(current_loop, m);
		set_irn_loop(m, current_loop);
		++i;
	} while (m != n);

	/* i might be bigger than 1 for dead (and that's why bad) loops */
	/* if(i > 1)
		printf("Mehr als eine Iteration!!!!!!!!!!!!!!!!!!!!!!!!!!!!11111\n");
	 */
}

/* GL ??? my last son is my grandson???  Removes loops with no
   ir_nodes in them.  Such loops have only another loop as son. (Why
   can't they have two loops as sons? Does it never get that far? ) */
static void close_loop(ir_loop *l) {
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
static INLINE void pop_scc_unmark_visit(ir_node *n) {
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
static ir_loop *new_loop(void) {
	ir_loop *father = current_loop;
	ir_loop *son    = alloc_loop(father, outermost_ir_graph->obst);

	if (son->depth > max_loop_depth) max_loop_depth = son->depth;
	current_loop = son;
	return father;
}

/**********************************************************************/
/* Constructing and destructing the loop/backedge information.       **/
/**********************************************************************/

/* Initialization steps. **********************************************/

static INLINE void init_node(ir_node *n, void *env) {
	struct obstack *obst = env;
	set_irn_link(n, new_scc_info(obst));
	clear_backedges(n);
}

static INLINE void init_scc_common(void) {
	current_dfn = 1;
	loop_node_cnt = 0;
	init_stack();
}

static INLINE void init_scc(ir_graph *irg, struct obstack *obst) {
	init_scc_common();
	irg_walk_graph(irg, init_node, NULL, obst);
	/*
	irg_walk (irg, link_to_reg_end, NULL, NULL);
	*/
}

static INLINE void finish_scc(void)
{
	finish_stack();
}

#ifdef INTERPROCEDURAL_VIEW
static INLINE void init_ip_scc(struct obstack *obst) {
	init_scc_common();
	cg_walk(init_node, NULL, obst);

#if EXPERIMENTAL_LOOP_TREE
	cg_walk(link_to_reg_end, NULL, NULL);
#endif
}
#endif

/**
 * Check weather a given node represents the outer most Start
 * block. In intra-procedural view this is the start block of the
 * current graph, in interprocedural view it is the start block
 * of the outer most graph.
 *
 * @param n  the node to check
 *
 * This is the condition for breaking the scc recursion.
 */
static int is_outermost_Start(ir_node *n) {
	/* Test whether this is the outermost Start node. */
	if (is_Block(n) && get_Block_n_cfgpreds(n) == 1) {
		ir_node *pred = skip_Proj(get_Block_cfgpred(n, 0));
	    if (is_Start(pred) && get_nodes_block(pred) == n)
			return 1;
	}
	return 0;
}

/* When to walk from nodes to blocks. Only for Control flow operations? */
static INLINE int get_start_index(ir_node *n) {
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

/**
 * Return non-zero if the given node is a legal loop header:
 * Block, Phi, Filter.
 *
 * @param n  the node to check
 */
static INLINE int is_possible_loop_head(ir_node *n) {
	ir_op *op = get_irn_op(n);
	return ((op == op_Block) ||
	        (op == op_Phi) ||
	        ((op == op_Filter) && get_interprocedural_view()));
}

/**
 * Returns non-zero if n is a loop header, i.e., it is a Block, Phi
 * or Filter node and has predecessors within the loop and out
 * of the loop.
 *
 * @param n    the node to check
 * @param root only needed for assertion.
 */
static int is_head(ir_node *n, ir_node *root) {
	int i, arity;
	int some_outof_loop = 0, some_in_loop = 0;

	/* Test for legal loop header: Block, Phi, ... */
	if (!is_possible_loop_head(n))
		return 0;

	if (!is_outermost_Start(n)) {
#ifndef NDEBUG
		int uplink = get_irn_uplink(root);
#endif
		arity = get_irn_arity(n);
		for (i = get_start_index(n); i < arity; i++) {
			ir_node *pred;
			if (is_backedge(n, i))
				continue;
			pred = get_irn_n(n, i);
			if (! irn_is_in_stack(pred)) {
				some_outof_loop = 1;
			} else {
				assert(get_irn_uplink(pred) >= uplink);
				some_in_loop = 1;
			}
		}
	}
	return some_outof_loop & some_in_loop;
}

/**
 * Returns non-zero if n is possible loop head of an endless loop.
 * I.e., it is a Block, Phi or Filter node and has only predecessors
 * within the loop.
 *
 * @param n    the node to check
 * @param root only needed for assertion.
 */
static int is_endless_head(ir_node *n, ir_node *root) {
	int i, arity;
	int none_outof_loop = 1, some_in_loop = 0;

	/* Test for legal loop header: Block, Phi, ... */
	if (!is_possible_loop_head(n))
		return 0;

	if (!is_outermost_Start(n)) {
#ifndef NDEBUG
		int uplink = get_irn_uplink(root);
#endif
		arity = get_irn_arity(n);
		for (i = get_start_index(n); i < arity; i++) {
			ir_node *pred;
			if (is_backedge(n, i))
				continue;
			pred = get_irn_n(n, i);
			if (!irn_is_in_stack(pred)) {
				none_outof_loop = 0;
			} else {
				assert(get_irn_uplink(pred) >= uplink);
				some_in_loop = 1;
			}
		}
	}
	return none_outof_loop & some_in_loop;
}

/** Returns index of the predecessor with the smallest dfn number
    greater-equal than limit. */
static int smallest_dfn_pred(ir_node *n, int limit) {
	int i, index = -2, min = -1;

	if (!is_outermost_Start(n)) {
		int arity = get_irn_arity(n);
		for (i = get_start_index(n); i < arity; i++) {
			ir_node *pred = get_irn_n(n, i);
			if (is_backedge(n, i) || !irn_is_in_stack(pred))
				continue;
			if (get_irn_dfn(pred) >= limit && (min == -1 || get_irn_dfn(pred) < min)) {
				index = i;
				min = get_irn_dfn(pred);
			}
		}
	}
	return index;
}

/**
 * Returns index of the predecessor with the largest dfn number.
 */
static int largest_dfn_pred(ir_node *n) {
	int i, index = -2, max = -1;

	if (!is_outermost_Start(n)) {
		int arity = get_irn_arity(n);
		for (i = get_start_index(n); i < arity; i++) {
			ir_node *pred = get_irn_n(n, i);
			if (is_backedge (n, i) || !irn_is_in_stack(pred))
				continue;
			if (get_irn_dfn(pred) > max) {
				index = i;
				max = get_irn_dfn(pred);
			}
		}
	}
	return index;
}

/**
 * Searches the stack for possible loop heads.  Tests these for backedges.
 * If it finds a head with an unmarked backedge it marks this edge and
 * returns the tail of the loop.
 * If it finds no backedge returns NULL.
 * ("disable_backedge" in fiasco)
 *
 * @param n  A node where uplink == dfn.
 */
static ir_node *find_tail(ir_node *n) {
	ir_node *m;
	int i, res_index = -2;

	/*
	if (!icfg && rm_cyclic_phis && remove_cyclic_phis (n)) return NULL;
	 */
	m = stack[tos-1];  /* tos = top of stack */
	if (is_head(m, n)) {
		res_index = smallest_dfn_pred(m, 0);
		if ((res_index == -2) &&  /* no smallest dfn pred found. */
			(n ==  m))
			return NULL;
	} else {
		if (m == n) return NULL;    // Is this to catch Phi - self loops?
		for (i = tos-2; i >= 0; --i) {
			m = stack[i];

			if (is_head(m, n)) {
				res_index = smallest_dfn_pred(m, get_irn_dfn(m) + 1);
				if (res_index == -2)  /* no smallest dfn pred found. */
					res_index = largest_dfn_pred(m);

				if ((m == n) && (res_index == -2)) {  /* don't walk past loop head. */
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
				if (is_endless_head(m, n)) {
					res_index = smallest_dfn_pred(m, get_irn_dfn(m) + 1);
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
		ir_node *bad = get_irg_bad(get_irn_irg(n));
		for (i = -1; i < arity; ++i) {
			set_irn_n(n, i, bad);
		}
		return NULL;
	}
	assert(res_index > -2);

	set_backedge(m, res_index);
	return is_outermost_Start(n) ? NULL : get_irn_n(m, res_index);
}


#if EXPERIMENTAL_LOOP_TREE

/*  ----------------------------------------------------------------
    AS:  This is experimental code to build loop trees suitable for
    the heap analysis. Does not work correctly right now... :-(


    Search in stack for the corresponding first Call-End-ProjX that
    corresponds to one of the control flow predecessors of the given
    block, that is the possible callers.
    returns: the control predecessor to chose\
    or       -1 if no corresponding Call-End-Node could be found
             on the stack.
    - -------------------------------------------------------------- */

int search_endproj_in_stack(ir_node *start_block) {
	int i, j;
	assert(is_Block(start_block));
	for(i = tos - 1; i >= 0; --i)
	{
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
			set_projx_link(begin_projx, end_projx);
	}
}

void set_projx_link(ir_node *cb_projx, ir_node *end_projx) {
	if(projx_link == NULL)
		projx_link = pmap_create();
	pmap_insert(projx_link, (void *)cb_projx, (void *)end_projx);
}

ir_node *get_projx_link(ir_node *cb_projx) {
	return((ir_node *) pmap_get(projx_link, (void *)cb_projx));
}

#endif

static INLINE int is_outermost_loop(ir_loop *l) {
	return l == get_loop_outer_loop(l);
}


/*-----------------------------------------------------------*
 *                   The core algorithm.                     *
 *-----------------------------------------------------------*/

/**
 * The core algorithm: Find strongly coupled components.
 *
 * @param n  node to start
 */
static void scc(ir_node *n) {
	if (irn_visited(n))
		return;
	mark_irn_visited(n);

	/* Initialize the node */
	set_irn_dfn(n, current_dfn);      /* Depth first number for this node */
	set_irn_uplink(n, current_dfn);   /* ... is default uplink. */
	set_irn_loop(n, NULL);
	++current_dfn;
	push(n);

	/* AS: get_start_index might return -1 for Control Flow Nodes, and thus a negative
	   array index would be passed to is_backedge(). But CFG Nodes dont't have a backedge array,
	   so is_backedge does not access array[-1] but correctly returns false! */

	if (!is_outermost_Start(n)) {
		int i, arity = get_irn_arity(n);

		for (i = get_start_index(n); i < arity; ++i) {
			ir_node *m;
			if (is_backedge(n, i))
				continue;
			m = get_irn_n(n, i);
			scc(m);
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
		   heads are Block and Phi nodes. find_tail() searches the stack for
		   Block's and Phi's and takes those nodes as loop heads for the current
		   loop instead and marks the incoming edge as backedge. */

		ir_node *tail = find_tail(n);
		if (tail != NULL) {
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
			pop_scc_unmark_visit(n);

			/* The current backedge has been marked, that is temporarily eliminated,
			   by find tail. Start the scc algorithm
			   again on the subgraph that is left (the current loop without the backedge)
			   in order to find more inner loops. */
			scc(tail);

			assert(irn_visited(n));
#if NO_LOOPS_WITHOUT_HEAD
			if (close)
#endif
				close_loop(l);
		} else {
			/* No loop head was found, that is we have straight line code.
			   Pop all nodes from the stack to the current loop. */
			pop_scc_to_loop(n);
		}
	}
}

#ifdef INTERPROCEDURAL_VIEW
static void my_scc(ir_node *n) {
	int i;
	if (irn_visited(n))
		return;
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
			my_scc(m);
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
			pop_scc_unmark_visit(n);

			/* The current backedge has been marked, that is temporarily eliminated,
			   by find tail. Start the scc algorithm
			   anew on the subgraph that is left (the current loop without the backedge)
			   in order to find more inner loops. */
			my_scc(tail);

			assert(irn_visited(n));
#if NO_LOOPS_WITHOUT_HEAD
			if (close)
#endif
				close_loop(l);
		} else {
			/* No loop head was found, that is we have straightline code.
			   Pop all nodes from the stack to the current loop. */
			pop_scc_to_loop(n);
		}
	}
}
#endif /* INTERPROCEDURAL_VIEW */

/* Constructs backedge information for irg. In interprocedural view constructs
   backedges for all methods called by irg, too. */
int construct_backedges(ir_graph *irg) {
	ir_graph *rem = current_ir_graph;
	ir_loop *head_rem;
	struct obstack temp;

	assert(!get_interprocedural_view() &&
	       "not implemented, use construct_ip_backedges()");

	max_loop_depth = 0;
	current_ir_graph   = irg;
	outermost_ir_graph = irg;

	obstack_init(&temp);
	init_scc(irg, &temp);

	current_loop = NULL;
	new_loop();  /* sets current_loop */
	head_rem = current_loop; /* Just for assertion */

	inc_irg_visited(irg);

	scc(get_irg_end(irg));

	finish_scc();
	obstack_free(&temp, NULL);

	assert(head_rem == current_loop);
	mature_loops(current_loop, irg->obst);
	set_irg_loop(irg, current_loop);
	set_irg_loopinfo_state(irg, loopinfo_consistent);
	assert(get_irg_loop(irg)->kind == k_ir_loop);
	current_ir_graph = rem;
	return max_loop_depth;
}


#ifdef INTERPROCEDURAL_VIEW
int construct_ip_backedges(void) {
	ir_graph *rem = current_ir_graph;
	int rem_ipv = get_interprocedural_view();
	int i;
	strcut obstack temp;

	max_loop_depth = 0;
	assert(get_irp_ip_view_state() == ip_view_valid);

	outermost_ir_graph = get_irp_main_irg();

	obstack_init(&temp);
	init_ip_scc(&temp);

	current_loop = NULL;
	new_loop();  /* sets current_loop */
	set_interprocedural_view(1);

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
		    (get_nodes_block(get_Block_cfgpred(sb, 0)) != sb))
			continue;

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
			for (j = 0; j < get_End_n_keepalives(e); j++)
				scc(get_End_keepalive(e, j));
		}
	}

	set_irg_loop(outermost_ir_graph, current_loop);
	set_irg_loopinfo_state(current_ir_graph, loopinfo_ip_consistent);
	assert(get_irg_loop(outermost_ir_graph)->kind == k_ir_loop);

	obstack_free(&temp, NULL);
	current_ir_graph = rem;
	set_interprocedural_view(rem_ipv);
	return max_loop_depth;
}

void my_construct_ip_backedges(void) {
	ir_graph *rem = current_ir_graph;
	int rem_ipv = get_interprocedural_view();
	int i;

	assert(get_irp_ip_view_state() == ip_view_valid);

	outermost_ir_graph = get_irp_main_irg();

	init_ip_scc();

	current_loop = NULL;
	new_loop();  /* sets current_loop */
	set_interprocedural_view(1);

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
		if (get_irn_visited(sb) < get_irg_visited(current_ir_graph))
			scc(sb);
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
#endif

static void reset_backedges(ir_node *n) {
	if (is_possible_loop_head(n)) {
#ifdef INTERPROCEDURAL_VIEW
		int rem = get_interprocedural_view();

		set_interprocedural_view(1);
		clear_backedges(n);
		set_interprocedural_view(1);
		clear_backedges(n);
		set_interprocedural_view(rem);
#else
		clear_backedges(n);
#endif
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
	(void) env;
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


void free_all_loop_information(void) {
	int i;
#ifdef INTERPROCEDURAL_VIEW
	int rem = get_interprocedural_view();
	set_interprocedural_view(1);  /* To visit all filter nodes */
#endif
	for (i = 0; i < get_irp_n_irgs(); i++) {
		free_loop_information(get_irp_irg(i));
	}
#ifdef INTERPROCEDURAL_VIEW
	set_interprocedural_view(rem);
#endif
}





/* Debug stuff *************************************************/

static int test_loop_node(ir_loop *l) {
	int i, has_node = 0, found_problem = 0;
	loop_element le;

	assert(l && l->kind == k_ir_loop);

	if (get_loop_n_elements(l) == 0) {
		found_problem = 1;
		dump_loop(l, "-ha");
	}

	le = get_loop_element(l, 0);
	if (*(le.kind) != k_ir_node) {
		assert(le.kind && *(le.kind) == k_ir_loop);

		found_problem = 1;
		dump_loop(l, "-ha");
	}

	if ((*(le.kind) == k_ir_node) && !is_possible_loop_head(le.node)) {
		found_problem = 1;
		dump_loop(l, "-ha");
	}

	if ((get_loop_depth(l) != 0) &&
		(*(le.kind) == k_ir_node) && !has_backedges(le.node)) {
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
	found_problem = test_loop_node(l);
	printf("Finished Test\n\n");
	if (found_problem) exit(0);

}

/* ------------------------------------------------------------------- */
/* Simple analyses based on the loop information                       */
/* ------------------------------------------------------------------- */

int is_loop_variant(ir_loop *l, ir_loop *b) {
	int i, n_elems;

	if (l == b) return 1;

	n_elems = get_loop_n_elements(l);
	for (i = 0; i < n_elems; ++i) {
		loop_element e = get_loop_element(l, i);
		if (is_ir_loop(e.kind))
			if (is_loop_variant(e.son, b))
				return 1;
	}

	return 0;
}

/* Test whether a value is loop invariant.
 *
 * @param n      The node to be tested.
 * @param block  A block node.  We pass the block, not the loop as we must
 *               start off with a block loop to find all proper uses.
 *
 * Returns non-zero, if the node n is not changed in the loop block
 * belongs to or in inner loops of this blocks loop. */
int is_loop_invariant(const ir_node *n, const ir_node *block) {
	ir_loop *l = get_irn_loop(block);
	const ir_node *b = is_Block(n) ? n : get_nodes_block(n);
	return !is_loop_variant(l, get_irn_loop(b));
}
