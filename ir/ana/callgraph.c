/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Representation and computation of the callgraph.
 * @author      Goetz Lindenmaier
 * @date        21.7.2004
 */
#include "callgraph.h"

#include "array.h"
#include "cgana.h"
#include "hashptr.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "panic.h"
#include "pmap.h"
#include "raw_bitset.h"
#include "util.h"
#include <stdlib.h>

static ir_visited_t master_cg_visited = 0;

irp_callgraph_state get_irp_callgraph_state(void)
{
	return irp->callgraph_state;
}

void set_irp_callgraph_state(irp_callgraph_state s)
{
	irp->callgraph_state = s;
}

size_t get_irg_n_callers(const ir_graph *irg)
{
	assert(irg->callers != NULL);
	return irg->callers ? ARR_LEN(irg->callers) : 0;
}

ir_graph *get_irg_caller(const ir_graph *irg, size_t pos)
{
	assert(pos < get_irg_n_callers(irg));
	return irg->callers ? irg->callers[pos] : NULL;
}

int is_irg_caller_backedge(const ir_graph *irg, size_t pos)
{
	assert(pos < get_irg_n_callers(irg));
	return irg->caller_isbe != NULL ? rbitset_is_set(irg->caller_isbe, pos) : 0;
}

/** Search the caller in the list of all callers and set its backedge property. */
static void set_irg_caller_backedge(ir_graph *irg, const ir_graph *caller)
{
	/* allocate a new array on demand */
	size_t n_callers = get_irg_n_callers(irg);
	if (irg->caller_isbe == NULL)
		irg->caller_isbe = rbitset_malloc(n_callers);
	for (size_t i = 0; i < n_callers; ++i) {
		if (get_irg_caller(irg, i) == caller) {
			rbitset_set(irg->caller_isbe, i);
			break;
		}
	}
}

int has_irg_caller_backedge(const ir_graph *irg)
{
	if (irg->caller_isbe != NULL) {
		for (size_t i = 0, n_callers = get_irg_n_callers(irg);
		     i < n_callers; ++i)
			if (rbitset_is_set(irg->caller_isbe, i))
				return 1;
	}
	return 0;
}

/**
 * Find the reversion position of a caller.
 * Given the position pos_caller of an caller of irg, return
 * irg's callee position on that caller.
 */
static size_t reverse_pos(const ir_graph *callee, size_t pos_caller)
{
	ir_graph *caller = get_irg_caller(callee, pos_caller);
	/* search the other relation for the corresponding edge. */
	for (size_t i = 0, n_callees = get_irg_n_callees(caller);
	     i < n_callees; ++i) {
		if (get_irg_callee(caller, i) == callee) {
			return i;
		}
	}
	panic("reverse_pos() did not find position");
}

size_t get_irg_caller_loop_depth(const ir_graph *irg, size_t pos)
{
	ir_graph *caller     = get_irg_caller(irg, pos);
	size_t    pos_callee = reverse_pos(irg, pos);
	return get_irg_callee_loop_depth(caller, pos_callee);
}

size_t get_irg_n_callees(const ir_graph *irg)
{
	assert(irg->callees != NULL);
	return ARR_LEN(irg->callees);
}

ir_graph *get_irg_callee(const ir_graph *irg, size_t pos)
{
	assert(pos < get_irg_n_callees(irg));
	return irg->callees ? irg->callees[pos]->irg : NULL;
}

int is_irg_callee_backedge(const ir_graph *irg, size_t pos)
{
	assert(pos < get_irg_n_callees(irg));
	return irg->callee_isbe != NULL ? rbitset_is_set(irg->callee_isbe, pos) : 0;
}

int has_irg_callee_backedge(const ir_graph *irg)
{
	if (irg->callee_isbe != NULL) {
		for (size_t i = 0, n_callees = get_irg_n_callees(irg);
		     i < n_callees; ++i)
			if (rbitset_is_set(irg->callee_isbe, i))
				return 1;
	}
	return 0;
}

/**
 * Mark the callee at position pos as a backedge.
 */
static void set_irg_callee_backedge(ir_graph *irg, size_t pos)
{
	/* allocate a new array on demand */
	size_t n = get_irg_n_callees(irg);
	if (irg->callee_isbe == NULL)
		irg->callee_isbe = rbitset_malloc(n);
	assert(pos < n);
	rbitset_set(irg->callee_isbe, pos);
}

size_t get_irg_callee_loop_depth(const ir_graph *irg, size_t pos)
{
	assert(pos < get_irg_n_callees(irg));
	return irg->callees ? irg->callees[pos]->max_depth : 0;
}


/**
 * Pre-Walker called by compute_callgraph(), analyses all Call nodes.
 */
static void ana_Call(ir_node *n, void *env)
{
	(void)env;
	if (!is_Call(n))
		return;

	ir_graph *irg = get_irn_irg(n);
	for (size_t i = 0, n_callees = cg_get_call_n_callees(n); i < n_callees;
	     ++i) {
		ir_entity *callee_e = cg_get_call_callee(n, i);
		ir_graph  *callee   = get_entity_linktime_irg(callee_e);

		if (callee) {
			cg_callee_entry buf;
			buf.irg = callee;
			pset_insert((pset *)callee->callers, irg, hash_ptr(irg));
			cg_callee_entry *found = (cg_callee_entry*) pset_find((pset *)irg->callees, &buf, hash_ptr(callee));
			if (found) {  /* add Call node to list, compute new nesting. */
				ir_node **arr = found->call_list;
				ARR_APP1(ir_node *, arr, n);
				found->call_list = arr;
			} else { /* New node, add Call node and init nesting. */
				found = OALLOC(get_irg_obstack(irg), cg_callee_entry);
				found->irg = callee;
				found->call_list = NEW_ARR_F(ir_node *, 1);
				found->call_list[0] = n;
				found->max_depth = 0;
				pset_insert((pset *)irg->callees, found, hash_ptr(callee));
			}
			unsigned depth = get_loop_depth(get_irn_loop(get_nodes_block(n)));
			found->max_depth = MAX(found->max_depth, depth);
		}
	}
}

/** compare two ir graphs in a cg_callee_entry */
static int cg_callee_entry_cmp(const void *elt, const void *key)
{
	const cg_callee_entry *e1 = (const cg_callee_entry*) elt;
	const cg_callee_entry *e2 = (const cg_callee_entry*) key;
	return e1->irg != e2->irg;
}

/** compare two ir graphs for pointer identity */
static int graph_cmp(const void *elt, const void *key)
{
	const ir_graph *e1 = (const ir_graph*) elt;
	const ir_graph *e2 = (const ir_graph*) key;
	return e1 != e2;
}

void compute_callgraph(void)
{
	/* initialize */
	free_callgraph();

	foreach_irp_irg(i, irg) {
		assert(get_irg_callee_info_state(irg) == irg_callee_info_consistent);
		irg->callees = (cg_callee_entry **)new_pset(cg_callee_entry_cmp, 8);
		irg->callers = (ir_graph **)new_pset(graph_cmp, 8);
		//construct_cf_backedges(irg);
	}

	/* Compute the call graph */
	foreach_irp_irg(i, irg) {
		construct_cf_backedges(irg);   // We also find the maximal loop depth of a call.
		irg_walk_graph(irg, ana_Call, NULL, NULL);
	}

	/* Change the sets to arrays. */
	foreach_irp_irg(i, irg) {
		pset *callee_set = (pset *)irg->callees;
		size_t count = pset_count(callee_set);
		irg->callees = NEW_ARR_F(cg_callee_entry *, count);
		irg->callee_isbe = NULL;
		size_t j = 0;
		foreach_pset(callee_set, cg_callee_entry, callee) {
			irg->callees[j++] = callee;
		}
		del_pset(callee_set);
		assert(j == count);

		pset *caller_set = (pset *)irg->callers;
		count = pset_count(caller_set);
		irg->callers = NEW_ARR_F(ir_graph *, count);
		irg->caller_isbe =  NULL;
		j = 0;
		foreach_pset(caller_set, ir_graph, c) {
			irg->callers[j++] = c;
		}
		del_pset(caller_set);
		assert(j == count);
	}
	set_irp_callgraph_state(irp_callgraph_consistent);
}

void free_callgraph(void)
{
	foreach_irp_irg(i, irg) {
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

/**
 * Returns non-zero if a graph was already visited.
 */
static inline int cg_irg_visited(ir_graph *irg)
{
	return irg->self_visited >= master_cg_visited;
}

/**
 * Marks a graph as visited.
 */
static inline void mark_cg_irg_visited(ir_graph *irg)
{
	irg->self_visited = master_cg_visited;
}

static void do_walk(ir_graph *irg, callgraph_walk_func *pre,
                    callgraph_walk_func *post, void *env)
{
	if (cg_irg_visited(irg))
		return;
	mark_cg_irg_visited(irg);

	if (pre != NULL)
		pre(irg, env);

	for (size_t i = 0, n_callees = get_irg_n_callees(irg); i < n_callees; i++) {
		ir_graph *m = get_irg_callee(irg, i);
		do_walk(m, pre, post, env);
	}

	if (post != NULL)
		post(irg, env);
}

void callgraph_walk(callgraph_walk_func *pre, callgraph_walk_func *post, void *env)
{
	++master_cg_visited;

	/* roots are methods which have no callers in the current program */
	foreach_irp_irg(i, irg) {
		if (get_irg_n_callers(irg) == 0)
			do_walk(irg, pre, post, env);
	}

	/* in case of unreachable call loops we haven't visited some irgs yet */
	foreach_irp_irg(i, irg) {
		do_walk(irg, pre, post, env);
	}
}

static ir_graph *outermost_ir_graph;   /**< The outermost graph the scc is computed
                                            for */
static ir_loop *current_loop;      /**< Current cfloop construction is working
                                        on. */
static size_t loop_node_cnt = 0;   /**< Counts the number of allocated cfloop nodes.
                                        Each cfloop node gets a unique number.
                                        What for? ev. remove. @@@ */
static size_t current_dfn = 1;     /**< Counter to generate depth first numbering
                                        of visited nodes.  */

typedef struct scc_info {
	size_t dfn;            /**< Depth first search number. */
	size_t uplink;         /**< dfn number of ancestor. */
	ir_visited_t visited;  /**< visited counter */
	int in_stack;          /**< Marks whether node is on the stack. */
} scc_info;

/**
 * allocates a new scc_info on the obstack
 */
static inline scc_info *new_scc_info(struct obstack *obst)
{
	return OALLOCZ(obst, scc_info);
}

/**
 * Set a graphs visited flag to i.
 */
static inline void set_cg_irg_visited(ir_graph *irg, ir_visited_t i)
{
	irg->self_visited = i;
}

static inline void mark_irg_in_stack(ir_graph *irg)
{
	scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	info->in_stack = 1;
}

static inline void mark_irg_not_in_stack(ir_graph *irg)
{
	scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	info->in_stack = 0;
}

static inline int irg_is_in_stack(const ir_graph *irg)
{
	scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	return info->in_stack;
}

static inline void set_irg_uplink(ir_graph *irg, size_t uplink)
{
	scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	info->uplink = uplink;
}

static inline size_t get_irg_uplink(const ir_graph *irg)
{
	const scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	return info->uplink;
}

static inline void set_irg_dfn(ir_graph *irg, size_t dfn)
{
	scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	info->dfn = dfn;
}

static inline size_t get_irg_dfn(const ir_graph *irg)
{
	const scc_info *info = (scc_info*) get_irg_link(irg);
	assert(info != NULL);
	return info->dfn;
}

static ir_graph **stack = NULL;
static size_t tos = 0;                /**< top of stack */

/**
 * Initialize the irg stack.
 */
static inline void init_stack(void)
{
	if (stack) {
		ARR_RESIZE(ir_graph *, stack, 1000);
	} else {
		stack = NEW_ARR_F(ir_graph *, 1000);
	}
	tos = 0;
}

/**
 * push a graph on the irg stack
 * @param n the graph to be pushed
 */
static inline void push(ir_graph *irg)
{
	if (tos == ARR_LEN(stack)) {
		size_t nlen = ARR_LEN(stack) * 2;
		ARR_RESIZE(ir_graph*, stack, nlen);
	}
	stack[tos++] = irg;
	mark_irg_in_stack(irg);
}

/**
 * return the topmost graph on the stack and pop it
 */
static inline ir_graph *pop(void)
{
	assert(tos > 0);
	ir_graph *irg = stack[--tos];
	mark_irg_not_in_stack(irg);
	return irg;
}

/**
 * The nodes up to irg belong to the current loop.
 * Removes them from the stack and adds them to the current loop.
 */
static inline void pop_scc_to_loop(ir_graph *irg)
{
	ir_graph *m;
	do {
		m = pop();
		++loop_node_cnt;
		set_irg_dfn(m, loop_node_cnt);
		add_loop_irg(current_loop, m);
		m->l = current_loop;
	} while (m != irg);
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
		}
	}
	current_loop = l;
}

/**
 * Removes and unmarks all nodes up to n from the stack.
 * The nodes must be visited once more to assign them to a scc.
 */
static inline void pop_scc_unmark_visit(ir_graph *n)
{
	ir_graph *m = NULL;
	while (m != n) {
		m = pop();
		set_cg_irg_visited(m, 0);
	}
}

/**
 * Allocates a new loop as son of current_loop.  Sets current_loop
 * to the new loop and returns the father.
 */
static ir_loop *new_loop(void)
{
	ir_loop *father = current_loop;
	ir_loop *son    = alloc_loop(father, get_irg_obstack(outermost_ir_graph));

	current_loop = son;
	return father;
}


static void init_scc(struct obstack *obst)
{
	current_dfn   = 1;
	loop_node_cnt = 0;
	init_stack();

	foreach_irp_irg(i, irg) {
		set_irg_link(irg, new_scc_info(obst));
	}
}

/** Returns non-zero if n is a loop header, i.e., it is a Block node
 *  and has predecessors within the cfloop and out of the cfloop.
 *
 *  @param root: only needed for assertion.
 */
static int is_head(const ir_graph *n, const ir_graph *root)
{
	bool some_outof_loop = false;
	bool some_in_loop    = false;

	for (size_t i = 0, n_callees = get_irg_n_callees(n); i < n_callees; ++i) {
		const ir_graph *pred = get_irg_callee(n, i);
		if (is_irg_callee_backedge(n, i)) continue;
		if (!irg_is_in_stack(pred)) {
			some_outof_loop = true;
		} else {
			if (get_irg_uplink(pred) < get_irg_uplink(root))  {
				assert(get_irg_uplink(pred) >= get_irg_uplink(root));
			}
			some_in_loop = true;
		}
	}

	return some_outof_loop && some_in_loop;
}

/**
 * Returns non-zero if n is possible loop head of an endless loop.
 * I.e., it is a Block or Phi node and has only predecessors
 * within the loop.
 * @arg root: only needed for assertion.
 */
static int is_endless_head(const ir_graph *n, const ir_graph *root)
{
	bool some_outof_loop = false;
	bool some_in_loop    = false;

	for (size_t i = 0, n_calless = get_irg_n_callees(n); i < n_calless; ++i) {
		const ir_graph *pred = get_irg_callee(n, i);
		assert(pred != NULL);
		if (is_irg_callee_backedge(n, i))
			continue;
		if (!irg_is_in_stack(pred)) {
			some_outof_loop = true;
		} else {
			if (get_irg_uplink(pred) < get_irg_uplink(root)) {
				assert(get_irg_uplink(pred) >= get_irg_uplink(root));
			}
			some_in_loop = true;
		}
	}
	return !some_outof_loop && some_in_loop;
}

/**
 * Finds index of the predecessor with the smallest dfn number
 * greater-equal than limit.
 */
static bool smallest_dfn_pred(const ir_graph *n, size_t limit, size_t *result)
{
	size_t index = 0;
	size_t min   = 0;
	bool   found = false;
	for (size_t i = 0, n_callees = get_irg_n_callees(n); i < n_callees; ++i) {
		const ir_graph *pred = get_irg_callee(n, i);
		if (is_irg_callee_backedge(n, i) || !irg_is_in_stack(pred))
			continue;
		if (get_irg_dfn(pred) >= limit && (!found || get_irg_dfn(pred) < min)) {
			index = i;
			min   = get_irg_dfn(pred);
			found = true;
		}
	}

	*result = index;
	return found;
}

/** Finds index of the predecessor with the largest dfn number. */
static bool largest_dfn_pred(const ir_graph *n, size_t *result)
{
	size_t index = 0;
	size_t max   = 0;
	bool   found = false;

	for (size_t i = 0, n_callees = get_irg_n_callees(n); i < n_callees; ++i) {
		const ir_graph *pred = get_irg_callee(n, i);
		if (is_irg_callee_backedge (n, i) || !irg_is_in_stack(pred))
			continue;
		/* Note: dfn is always > 0 */
		if (get_irg_dfn(pred) > max) {
			index = i;
			max   = get_irg_dfn(pred);
			found = true;
		}
	}

	*result = index;
	return found;
}

static ir_graph *find_tail(const ir_graph *n)
{
	/*
	if (!icfg && rm_cyclic_phis && remove_cyclic_phis (n)) return NULL;
	*/
	bool      found     = false;
	size_t    res_index = 0;
	ir_graph *m         = stack[tos - 1];  /* tos = top of stack */
	if (is_head(m, n)) {
		found = smallest_dfn_pred(m, 0, &res_index);
		if (!found &&  /* no smallest dfn pred found. */
			n == m)
			return NULL;
	} else {
		if (m == n)
			return NULL;    // Is this to catch Phi - self loops?
		for (size_t i = tos - 1; i > 0;) {
			m = stack[--i];

			if (is_head(m, n)) {
				found = smallest_dfn_pred(m, get_irg_dfn(m) + 1, &res_index);
				if (!found)  /* no smallest dfn pred found. */
					found = largest_dfn_pred(m, &res_index);

				break;
			}

			/* We should not walk past our selves on the stack:  The upcoming nodes
			   are not in this loop. We assume a loop not reachable from Start. */
			if (m == n) {
				found = false;
				break;
			}

		}

		if (!found) {
			/* A dead loop not reachable from Start. */
			for (size_t i = tos-1; i > 0;) {
				m = stack[--i];
				if (is_endless_head(m, n)) {
					found = smallest_dfn_pred(m, get_irg_dfn(m) + 1, &res_index);
					if (!found)  /* no smallest dfn pred found. */
						found = largest_dfn_pred(m, &res_index);
					break;
				}
				/* It's not an unreachable loop, either. */
				if (m == n)
					break;
			}
		}

	}
	assert(found);

	set_irg_callee_backedge(m, res_index);
	return get_irg_callee(m, res_index);
}

static void cgscc(ir_graph *n)
{
	if (cg_irg_visited(n))
		return;
	mark_cg_irg_visited(n);

	/* Initialize the node */
	set_irg_dfn(n, current_dfn);      /* Depth first number for this node */
	set_irg_uplink(n, current_dfn);   /* ... is default uplink. */
	++current_dfn;
	push(n);

	for (size_t i = 0, n_callees = get_irg_n_callees(n); i < n_callees; ++i) {
		if (is_irg_callee_backedge(n, i))
			continue;
		ir_graph *m = get_irg_callee(n, i);

		/** This marks the backedge, but does it guarantee a correct loop tree? */
		//if (m == n) { set_irg_callee_backedge(n, i); continue; }

		cgscc(m);
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
			pop_scc_unmark_visit(n);

			/* The current backedge has been marked, that is temporarily eliminated,
			   by find tail. Start the scc algorithm
			   anew on the subgraph thats left (the current cfloop without the backedge)
			   in order to find more inner cfloops. */

			cgscc(tail);

			assert(cg_irg_visited(n));
			close_loop(l);
		} else {
			pop_scc_to_loop(n);
		}
	}
}


/**
 * reset the backedge information for all callers in all irgs
 */
static void reset_isbe(void)
{
	foreach_irp_irg(i, irg) {
		free(irg->caller_isbe);
		irg->caller_isbe = NULL;

		free(irg->callee_isbe);
		irg->callee_isbe = NULL;
	}
}

void find_callgraph_recursions(void)
{
	reset_isbe();

	/* -- compute the looptree. -- */

	/* The outermost graph.  We start here.  Then we start at all
	functions in irg list that are never called, then at the remaining
	unvisited ones. The third step is needed for functions that are not
	reachable from the outermost graph, but call themselves in a cycle. */
	assert(get_irp_main_irg());
	outermost_ir_graph = get_irp_main_irg();
	struct obstack temp;
	obstack_init(&temp);
	init_scc(&temp);

	current_loop = NULL;
	new_loop();  /* sets current_loop */

	++master_cg_visited;
	cgscc(outermost_ir_graph);
	foreach_irp_irg(i, irg) {
		if (!cg_irg_visited(irg) && get_irg_n_callers(irg) == 0)
			cgscc(irg);
	}
	foreach_irp_irg(i, irg) {
		if (!cg_irg_visited(irg))
			cgscc(irg);
	}
	obstack_free(&temp, NULL);

	irp->outermost_cg_loop = current_loop;
	mature_loops(current_loop, get_irg_obstack(outermost_ir_graph));

	/* -- Reverse the backedge information. -- */
	foreach_irp_irg(i, irg) {
		for (size_t j = 0, n_callees = get_irg_n_callees(irg); j < n_callees; ++j) {
			if (is_irg_callee_backedge(irg, j))
				set_irg_caller_backedge(get_irg_callee(irg, j), irg);
		}
	}

	irp->callgraph_state = irp_callgraph_and_calltree_consistent;
}

void analyse_loop_nesting_depth(void)
{
	/* establish preconditions. */
	if (get_irp_callee_info_state() != irg_callee_info_consistent) {
		ir_entity **free_methods = NULL;

		cgana(&free_methods);
		free(free_methods);
	}

	if (irp_callgraph_consistent != get_irp_callgraph_state()) {
		compute_callgraph();
	}

	find_callgraph_recursions();

	set_irp_loop_nesting_depth_state(loop_nesting_depth_consistent);
}

loop_nesting_depth_state get_irp_loop_nesting_depth_state(void)
{
	return irp->lnd_state;
}

void set_irp_loop_nesting_depth_state(loop_nesting_depth_state s)
{
	irp->lnd_state = s;
}

void set_irp_loop_nesting_depth_state_inconsistent(void)
{
	if (irp->lnd_state == loop_nesting_depth_consistent)
		irp->lnd_state = loop_nesting_depth_inconsistent;
}
