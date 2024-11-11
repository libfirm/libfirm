/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Compute the strongly connected regions and build backedge/cfloop
 *            data structures. A variation on the Tarjan algorithm. See also
 *            [Trapp:99], Chapter 5.2.1.2.
 * @author    Goetz Lindenmaier
 * @date      7.2002
 */
#include "array.h"
#include "ircons_t.h"
#include "irdump.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "pmap.h"

/** The outermost graph the scc is computed for */
static ir_graph *outermost_ir_graph;
/** Current cfloop construction is working on. */
static ir_loop *current_loop;
/** Counts the number of allocated cfloop nodes.
 * Each cfloop node gets a unique number.
 * @todo What for? ev. remove.
 */
static int loop_node_cnt = 0;
/** Counter to generate depth first numbering of visited nodes. */
static int current_dfn = 1;

/**********************************************************************/
/* Node attributes needed for the construction.                      **/
/**********************************************************************/

/**
 * The SCC info. Additional fields for an ir-node needed for the
 * construction.
 */
typedef struct scc_info {
	bool in_stack;          /**< Marks whether node is on the stack. */
	int  dfn;               /**< Depth first search number. */
	int  uplink;            /**< dfn number of ancestor. */
} scc_info;

/** Allocate a new scc_info on the given obstack */
static inline scc_info *new_scc_info(struct obstack *obst)
{
	return OALLOCZ(obst, scc_info);
}

/**
 * Marks the node n to be on the stack.
 */
static inline void mark_irn_in_stack(ir_node *n)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	info->in_stack = true;
}

/**
 * Marks the node n to be not on the stack.
 */
static inline void mark_irn_not_in_stack(ir_node *n)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	info->in_stack = false;
}

/**
 * Returns whether node n is on the stack.
 */
static inline bool irn_is_in_stack(const ir_node *n)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	return info->in_stack;
}

/**
 * Sets node n uplink value.
 */
static inline void set_irn_uplink(ir_node *n, int uplink)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	info->uplink = uplink;
}

/**
 * Return node n uplink value.
 */
static inline int get_irn_uplink(ir_node *n)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	return info->uplink;
}

/**
 * Sets node n dfn value.
 */
static inline void set_irn_dfn(ir_node *n, int dfn)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	info->dfn = dfn;
}

/**
 * Returns node n dfn value.
 */
static inline int get_irn_dfn(ir_node *n)
{
	scc_info *info = (scc_info*) get_irn_link(n);
	return info->dfn;
}

/**********************************************************************/
/* A stack.                                                          **/
/**********************************************************************/

/** An IR-node stack */
static ir_node **stack = NULL;
/** The top (index) of the IR-node stack */
static size_t    tos = 0;

/**
 * Initializes the IR-node stack
 */
static inline void init_stack(void)
{
	stack = NEW_ARR_F(ir_node *, 100);
	tos = 0;
}

static void finish_stack(void)
{
	DEL_ARR_F(stack);
	stack = NULL;
}

/**
 * Push a node n onto the IR-node stack.
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
 * Pop a node from the IR-node stack and return it.
 */
static inline ir_node *pop(void)
{
	ir_node *n = stack[--tos];
	mark_irn_not_in_stack(n);
	return n;
}

/**
 * The nodes from tos up to n belong to the current loop.
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

/* GL ??? my last son is my grandson???  Removes cfloops with no
   ir_nodes in them.  Such loops have only another loop as son. (Why
   can't they have two loops as sons? Does it never get that far? ) */
static void close_loop(ir_loop *l)
{
	size_t       last     = get_loop_n_elements(l) - 1;
	loop_element lelement = get_loop_element(l, last);
	ir_loop     *last_son = lelement.son;

	if (get_kind(last_son) == k_ir_loop && get_loop_n_elements(last_son) == 1) {
		lelement = get_loop_element(last_son, 0);
		ir_loop *gson = lelement.son;
		if (get_kind(gson) == k_ir_loop) {
			loop_element new_last_son;

			gson->outer_loop = l;
			new_last_son.son = gson;
			l->children[last] = new_last_son;

			/* the loop last_son is dead now, recover at least some memory */
			DEL_ARR_F(last_son->children);
		}
	}

	current_loop = l;
}

/**
 * Removes and unmarks all nodes up to n from the stack.
 * The nodes must be visited once more to assign them to a scc.
 */
static inline void pop_scc_unmark_visit(ir_node *n)
{
	ir_node *m;
	do {
		m = pop();
		set_irn_visited(m, 0);
	} while (m != n);
}

/**********************************************************************/
/* The loop data structure.                                          **/
/**********************************************************************/

/**
 * Allocates a new loop as son of current_loop.  Sets current_loop
 * to the new loop and returns its father.
 * The loop is allocated on the outermost_ir_graphs's obstack.
 */
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

/**
 * Allocates a scc_info for every Block node n.
 * Clear the backedges for all nodes.
 * Called from a walker.
 */
static inline void init_node(ir_node *n, void *env)
{
	struct obstack *obst = (struct obstack*) env;
	if (is_Block(n))
		set_irn_link(n, new_scc_info(obst));
	clear_backedges(n);
}

/**
 * Initializes the common global settings for the scc algorithm
 */
static inline void init_scc_common(void)
{
	current_dfn   = 1;
	loop_node_cnt = 0;
	init_stack();
}

/**
 * Initializes the scc algorithm for the intraprocedural case.
 * Add scc info to every block node.
 */
static inline void init_scc(ir_graph *irg, struct obstack *obst)
{
	init_scc_common();
	irg_walk_graph(irg, init_node, NULL, obst);
}

static inline void finish_scc(void)
{
	finish_stack();
}

/** Returns non-zero if n is a loop header, i.e., it is a Block node
 *  and has predecessors within the cfloop and out of the cfloop.
 *
 *  @param n     the block node to check
 *  @param root  only needed for assertion.
 */
static int is_head(ir_node *n, ir_node *root)
{
	(void)root;
	bool some_outof_loop = false;
	bool some_in_loop    = false;
	for (int i = 0, arity = get_Block_n_cfgpreds(n); i < arity; i++) {
		ir_node *pred = get_Block_cfgpred_block(n, i);
		/* ignore Bad control flow: it cannot happen */
		if (pred == NULL)
			continue;
		if (is_backedge(n, i))
			continue;
		if (!irn_is_in_stack(pred)) {
			some_outof_loop = true;
		} else {
			assert(get_irn_uplink(pred) >= get_irn_uplink(root));
			some_in_loop = true;
		}
	}
	return some_outof_loop && some_in_loop;
}


/**
 * Returns non-zero if n is possible loop head of an endless loop.
 * I.e., it is a Block node and has only predecessors
 * within the loop.
 *
 * @param n     the block node to check
 * @param root  only needed for assertion.
 */
static int is_endless_head(ir_node *n, ir_node *root)
{
	(void)root;

	/* Test for legal loop header: Block, Phi, ... */
	bool none_outof_loop = true;
	bool some_in_loop    = false;
	for (int i = 0, arity = get_Block_n_cfgpreds(n); i < arity; i++) {
		ir_node *pred = get_Block_cfgpred_block(n, i);
		/* ignore Bad control flow: it cannot happen */
		if (pred == NULL)
			continue;
		if (is_backedge(n, i))
			continue;
		if (!irn_is_in_stack(pred)) {
			none_outof_loop = false;
		} else {
			assert(get_irn_uplink(pred) >= get_irn_uplink(root));
			some_in_loop = true;
		}
	}
	return none_outof_loop && some_in_loop;
}

/**
 * Returns index of the predecessor with the smallest dfn number
 * greater-equal than limit.
 */
static int smallest_dfn_pred(ir_node *n, int limit)
{
	int index = -2;
	int min   = -1;
	for (int i = 0, arity = get_Block_n_cfgpreds(n); i < arity; i++) {
		ir_node *pred = get_Block_cfgpred_block(n, i);
		/* ignore Bad control flow: it cannot happen */
		if (pred == NULL)
			continue;
		if (is_backedge(n, i) || !irn_is_in_stack(pred))
			continue;
		if (get_irn_dfn(pred) >= limit && (min == -1 || get_irn_dfn(pred) < min)) {
			index = i;
			min   = get_irn_dfn(pred);
		}
	}
	return index;
}

/**
 * Returns index of the predecessor with the largest dfn number.
 */
static int largest_dfn_pred(ir_node *n)
{
	int index = -2;
	int max   = -1;
	for (int i = 0, arity = get_Block_n_cfgpreds(n); i < arity; i++) {
		ir_node *pred = get_Block_cfgpred_block(n, i);
		/* ignore Bad control flow: it cannot happen */
		if (pred == NULL)
			continue;
		if (is_backedge(n, i) || !irn_is_in_stack(pred))
			continue;
		if (get_irn_dfn(pred) > max) {
			index = i;
			max = get_irn_dfn(pred);
		}
	}
	return index;
}

/**
 * Searches the stack for possible loop heads.  Tests these for backedges.
 * If it finds a head with an unmarked backedge it marks this edge and
 * returns the tail of the loop.
 * If it finds no backedge returns NULL.
 */
static ir_node *find_tail(ir_node *n)
{
	int res_index = -2;

	ir_node *m = stack[tos - 1];  /* tos = top of stack */
	if (is_head(m, n)) {
		res_index = smallest_dfn_pred(m, 0);
		if ((res_index == -2) &&  /* no smallest dfn pred found. */
			(n ==  m))
			return NULL;
	} else {
		if (m == n)
			return NULL;
		size_t i;
		for (i = tos - 1; i != 0;) {
			m = stack[--i];
			if (is_head(m, n)) {
				res_index = smallest_dfn_pred(m, get_irn_dfn(m) + 1);
				if (res_index == -2)  /* no smallest dfn pred found. */
					res_index = largest_dfn_pred(m);

				if ((m == n) && (res_index == -2)) {
					i = (size_t)-1;
				}
				break;
			}


			/* We should not walk past our selves on the stack:  The upcoming nodes
			   are not in this loop. We assume a loop not reachable from Start. */
			if (m == n) {
				i = (size_t)-1;
				break;
			}
		}

		if (i == (size_t)-1) {
			/* A dead loop not reachable from Start. */
			for (i = tos - 1; i != 0;) {
				m = stack[--i];
				if (is_endless_head(m, n)) {
					res_index = smallest_dfn_pred (m, get_irn_dfn(m) + 1);
					if (res_index == -2)  /* no smallest dfn pred found. */
						res_index = largest_dfn_pred(m);
					break;
				}
				if (m == n) break;   /* It's not an unreachable loop, either. */
			}
			//panic("no head found on stack");
		}
	}
	assert(res_index > -2);

	set_backedge(m, res_index);
	return get_Block_cfgpred_block(m, res_index);
}

/**
 * returns non.zero if l is the outermost loop.
 */
inline static bool is_outermost_loop(const ir_loop *l)
{
	return l == get_loop_outer_loop(l);
}

/*-----------------------------------------------------------*
 *                   The core algorithm.                     *
 *-----------------------------------------------------------*/

/**
 * Walks over all blocks of a graph
 */
static void cfscc(ir_node *n)
{
	assert(is_Block(n));
	if (irn_visited_else_mark(n))
		return;

	/* Initialize the node */
	set_irn_dfn(n, current_dfn);      /* Depth first number for this node */
	set_irn_uplink(n, current_dfn);   /* ... is default uplink. */
	set_irn_loop(n, NULL);
	++current_dfn;
	push(n);

	for (int i = 0, arity = get_Block_n_cfgpreds(n); i < arity; i++) {
		if (is_backedge(n, i))
			continue;
		ir_node *m = get_Block_cfgpred_block(n, i);
		/* ignore Bad control flow: it cannot happen */
		if (m == NULL)
			continue;

		cfscc(m);
		if (irn_is_in_stack(m)) {
			/* Uplink of m is smaller if n->m is a backedge.
			   Propagate the uplink to mark the cfloop. */
			if (get_irn_uplink(m) < get_irn_uplink(n))
				set_irn_uplink(n, get_irn_uplink(m));
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

			/* This is an adaption of the algorithm from fiasco / optscc to
			 * avoid cfloops without Block or Phi as first node.  This should
			 * severely reduce the number of evaluations of nodes to detect
			 * a fixpoint in the heap analysis.
			 * Further it avoids cfloops without firm nodes that cause errors
			 * in the heap analyses. */

			ir_loop *l;
			bool     close;
			if ((get_loop_n_elements(current_loop) > 0) || (is_outermost_loop(current_loop))) {
				l = new_loop();
				close = true;
			} else {
				l = current_loop;
				close = false;
			}

			/* Remove the cfloop from the stack ... */
			pop_scc_unmark_visit(n);

			/* The current backedge has been marked, that is temporarily eliminated,
			   by find tail. Start the scc algorithm
			   anew on the subgraph thats left (the current cfloop without the backedge)
			   in order to find more inner cfloops. */

			cfscc(tail);

			assert(irn_visited(n));
			if (close)
				close_loop(l);
		} else {
			/* AS: No cfloop head was found, that is we have straight line code.
			       Pop all nodes from the stack to the current cfloop. */
			pop_scc_to_loop(n);
		}
	}
}

/**
 * Return non-zero if the given node is a legal loop header:
 * Block, Phi
 *
 * @param n  the node to check
 */
static inline bool is_possible_loop_head(ir_node *n)
{
	return is_Block(n) || is_Phi(n);
}

static void reset_backedges(ir_node *n)
{
	if (is_possible_loop_head(n))
		clear_backedges(n);
}

static void loop_reset_node(ir_node *n, void *env)
{
	(void)env;
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

void construct_cf_backedges(ir_graph *irg)
{
	outermost_ir_graph = irg;

	struct obstack temp;
	obstack_init(&temp);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	init_scc(irg, &temp);

	current_loop = NULL;
	new_loop();  /* sets current_loop */

#ifndef NDEBUG
	ir_loop *head_rem = current_loop; /* Just for assertion */
#endif

	inc_irg_visited(irg);

	/* walk over all blocks of the graph, including keep alives */
	cfscc(get_irg_end_block(irg));
	ir_node *end = get_irg_end(irg);
	for (int i = get_End_n_keepalives(end); i-- > 0; ) {
		ir_node *el = get_End_keepalive(end, i);
		if (is_Block(el))
			cfscc(el);
	}
	finish_scc();
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	obstack_free(&temp, NULL);

	assert(head_rem == current_loop);
	mature_loops(current_loop, get_irg_obstack(irg));
	set_irg_loop(irg, current_loop);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
}

void assure_loopinfo(ir_graph *irg)
{
	if (irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO))
		return;
	construct_cf_backedges(irg);
}
