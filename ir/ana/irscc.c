/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Compute the strongly connected regions and build
 *              backedge/loop datastructures.
 *              A variation on the Tarjan algorithm. See also [Trapp:99],
 *              Chapter 5.2.1.2.
 * @author   Goetz Lindenmaier
 * @date     7.2002
 */
#include <stdlib.h>

#include "irloop_t.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "array.h"
#include "pmap.h"
#include "ircons.h"

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

/**********************************************************************/
/* Node attributes needed for the construction.                      **/
/**********************************************************************/

typedef struct scc_info {
	int in_stack;          /**< Marks whether node is on the stack. */
	int dfn;               /**< Depth first search number. */
	int uplink;            /**< dfn number of ancestor. */
} scc_info;

/**
 * Allocates a new SCC info on the given obstack.
 */
static inline scc_info *new_scc_info(struct obstack *obst)
{
	return OALLOCZ(obst, scc_info);
}

/**
 * Mark node n being on the SCC stack.
 */
static inline void mark_irn_in_stack(ir_node *n)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	scc->in_stack = 1;
}

/**
* Mark node n NOT being on the SCC stack.
*/
static inline void mark_irn_not_in_stack(ir_node *n)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	scc->in_stack = 0;
}

/**
 * Checks if a node is on the SCC stack.
 */
static inline int irn_is_in_stack(ir_node *n)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	return scc->in_stack;
}

/**
 * Sets the uplink number for a node.
 */
static inline void set_irn_uplink(ir_node *n, int uplink)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	scc->uplink = uplink;
}

/**
 * Returns the uplink number for a node.
 */
static int get_irn_uplink(ir_node *n)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	return scc->uplink;
}

/**
 * Sets the depth-first-search number for a node.
 */
static inline void set_irn_dfn(ir_node *n, int dfn)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	scc->dfn = dfn;
}

/**
 * Returns the depth-first-search number of a node.
 */
static int get_irn_dfn(ir_node *n)
{
	scc_info *scc = (scc_info*) get_irn_link(n);
	assert(scc);
	return scc->dfn;
}

/**********************************************************************/
/* A stack.                                                          **/
/**********************************************************************/

static ir_node **stack = NULL;
static size_t tos = 0;                /* top of stack */

/**
 * initializes the stack
 */
static inline void init_stack(void)
{
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
static void finish_stack(void)
{
	DEL_ARR_F(stack);
	stack = NULL;
}

/**
 * push a node onto the stack
 *
 * @param n  The node to push
 */
static inline void push(ir_node *n)
{
	if (tos == ARR_LEN(stack)) {
		size_t nlen = ARR_LEN(stack) * 2;
		ARR_RESIZE(ir_node *, stack, nlen);
	}
	stack[tos++] = n;
	mark_irn_in_stack(n);
}

/**
 * pop a node from the stack
 *
 * @return  The topmost node
 */
static inline ir_node *pop(void)
{
	ir_node *n;

	assert(tos > 0);
	n = stack[--tos];
	mark_irn_not_in_stack(n);
	return n;
}

/**
 * The nodes up to n belong to the current loop.
 * Removes them from the stack and adds them to the current loop.
 */
static inline void pop_scc_to_loop(ir_node *n)
{
	ir_node *m;

	do {
		m = pop();

		loop_node_cnt++;
		set_irn_dfn(m, loop_node_cnt);
		add_loop_node(current_loop, m);
		set_irn_loop(m, current_loop);
	} while (m != n);
}

/* GL ??? my last son is my grandson???  Removes loops with no
   ir_nodes in them.  Such loops have only another loop as son. (Why
   can't they have two loops as sons? Does it never get that far? ) */
static void close_loop(ir_loop *l)
{
	size_t last = get_loop_n_elements(l) - 1;
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
static inline void pop_scc_unmark_visit(ir_node *n)
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
static ir_loop *new_loop(void)
{
	ir_loop *father = current_loop;
	ir_loop *son    = alloc_loop(father, get_irg_obstack(outermost_ir_graph));

	current_loop = son;
	return father;
}

/**********************************************************************/
/* Constructing and destructing the loop/backedge information.       **/
/**********************************************************************/

/* Initialization steps. **********************************************/

static inline void init_node(ir_node *n, void *env)
{
	struct obstack *obst = (struct obstack*) env;
	set_irn_link(n, new_scc_info(obst));
	clear_backedges(n);
}

static inline void init_scc_common(void)
{
	current_dfn = 1;
	loop_node_cnt = 0;
	init_stack();
}

static inline void init_scc(ir_graph *irg, struct obstack *obst)
{
	init_scc_common();
	irg_walk_graph(irg, init_node, NULL, obst);
}

static inline void finish_scc(void)
{
	finish_stack();
}

/**
 * Check whether a given node represents the outermost Start
 * block. In intra-procedural view this is the start block of the
 * current graph, in interprocedural view it is the start block
 * of the outer most graph.
 *
 * @param n  the node to check
 *
 * This is the condition for breaking the scc recursion.
 */
static int is_outermost_Start(ir_node *n)
{
	/* Test whether this is the outermost Start node. */
	if (is_Block(n) && get_Block_n_cfgpreds(n) == 1) {
		ir_node *pred = skip_Proj(get_Block_cfgpred(n, 0));
	    if (is_Start(pred) && get_nodes_block(pred) == n)
			return 1;
	}
	return 0;
}

/* When to walk from nodes to blocks. Only for Control flow operations? */
static inline int get_start_index(ir_node *n)
{
	/* This version assures, that all nodes are ordered absolutely.  This allows
	   to undef all nodes in the heap analysis if the block is false, which
	   means not reachable.
	   I.e., with this code, the order on the loop tree is correct. But a
	   (single) test showed the loop tree is deeper. */
	if (is_Phi(n)   ||
	    is_Block(n) ||
	    (get_irg_pinned(get_irn_irg(n)) == op_pin_state_floats &&
	      get_irn_pinned(n)              == op_pin_state_floats))
		// Here we could test for backedge at -1 which is illegal
		return 0;
	else
		return -1;
}

/**
 * Return non-zero if the given node is a legal loop header:
 * Block, Phi
 *
 * @param n  the node to check
 */
static inline int is_possible_loop_head(ir_node *n)
{
	return is_Block(n) || is_Phi(n);
}

/**
 * Returns non-zero if n is a loop header, i.e., it is a Block or Phi
 * node and has predecessors within the loop and out of the loop.
 *
 * @param n    the node to check
 * @param root only needed for assertion.
 */
static int is_head(ir_node *n, ir_node *root)
{
	int i, arity;
	int some_outof_loop = 0, some_in_loop = 0;

	/* Test for legal loop header: Block, Phi, ... */
	if (!is_possible_loop_head(n))
		return 0;

	if (!is_outermost_Start(n)) {
#ifndef NDEBUG
		int uplink = get_irn_uplink(root);
#else
		(void) root;
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
 * I.e., it is a Block or Phi node and has only predecessors
 * within the loop.
 *
 * @param n    the node to check
 * @param root only needed for assertion.
 */
static int is_endless_head(ir_node *n, ir_node *root)
{
	int i, arity;
	int none_outof_loop = 1, some_in_loop = 0;

	/* Test for legal loop header: Block, Phi, ... */
	if (!is_possible_loop_head(n))
		return 0;

	if (!is_outermost_Start(n)) {
#ifndef NDEBUG
		int uplink = get_irn_uplink(root);
#else
		(void) root;
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
static int smallest_dfn_pred(ir_node *n, int limit)
{
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
static int largest_dfn_pred(ir_node *n)
{
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
static ir_node *find_tail(ir_node *n)
{
	ir_node *m;
	int i, res_index = -2;

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
				/* It's not an unreachable loop, either. */
				if (m == n)
					break;
			}
			//panic("no head found on stack");
		}

	}
	if (res_index <= -2) {
		/* It's a completely bad loop: without Phi/Block nodes that can
		   be a head. I.e., the code is "dying".  We break the loop by
		   setting Bad nodes. */
		ir_graph *irg   = get_irn_irg(n);
		ir_mode  *mode  = get_irn_mode(n);
		ir_node  *bad   = new_r_Bad(irg, mode);
		int       arity = get_irn_arity(n);
		for (i = -1; i < arity; ++i) {
			set_irn_n(n, i, bad);
		}
		return NULL;
	}
	assert(res_index > -2);

	set_backedge(m, res_index);
	return is_outermost_Start(n) ? NULL : get_irn_n(m, res_index);
}

static inline int is_outermost_loop(ir_loop *l)
{
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
static void scc(ir_node *n)
{
	if (irn_visited_else_mark(n))
		return;

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

			/* Remove the loop from the stack ... */
			pop_scc_unmark_visit(n);

			/* The current backedge has been marked, that is temporarily eliminated,
			   by find tail. Start the scc algorithm
			   again on the subgraph that is left (the current loop without the backedge)
			   in order to find more inner loops. */
			scc(tail);

			assert(irn_visited(n));
			if (close)
				close_loop(l);
		} else {
			/* No loop head was found, that is we have straight line code.
			   Pop all nodes from the stack to the current loop. */
			pop_scc_to_loop(n);
		}
	}
}

void construct_backedges(ir_graph *irg)
{
	struct obstack temp;

	outermost_ir_graph = irg;

	obstack_init(&temp);
	init_scc(irg, &temp);

	current_loop = NULL;
	new_loop();  /* sets current_loop */

#ifndef NDEBUG
	ir_loop *head_rem = current_loop; /* Just for assertion */
#endif

	inc_irg_visited(irg);

	scc(get_irg_end(irg));

	finish_scc();
	obstack_free(&temp, NULL);

	assert(head_rem == current_loop);
	mature_loops(current_loop, get_irg_obstack(irg));
	set_irg_loop(irg, current_loop);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
	assert(get_irg_loop(irg)->kind == k_ir_loop);
}

static void reset_backedges(ir_node *n)
{
	if (is_possible_loop_head(n)) {
		clear_backedges(n);
	}
}

static void loop_reset_node(ir_node *n, void *env)
{
	(void) env;
	set_irn_loop(n, NULL);
	reset_backedges(n);
}

void free_loop_information(ir_graph *irg)
{
	irg_walk_graph(irg, loop_reset_node, NULL, NULL);
	set_irg_loop(irg, NULL);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
	/* We cannot free the loop nodes, they are on the obstack. */
}

void free_all_loop_information(void)
{
	foreach_irp_irg(i, irg) {
		free_loop_information(irg);
	}
}

/* ------------------------------------------------------------------- */
/* Simple analyses based on the loop information                       */
/* ------------------------------------------------------------------- */

static int is_loop_variant(ir_loop *l, ir_loop *b)
{
	size_t i, n_elems;

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

int is_loop_invariant(const ir_node *n, const ir_node *block)
{
	ir_loop *l = get_irn_loop(block);
	const ir_node *b = is_Block(n) ? n : get_nodes_block(n);
	return !is_loop_variant(l, get_irn_loop(b));
}
