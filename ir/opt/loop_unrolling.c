/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Karlsruhe Institute of Technology
 */

/**
 * @file
 * @brief   loop unrolling using LCSSA form
 * @author  Elias Aebi
 */
#include "debug.h"
#include "irnode_t.h"
#include "irtools.h"
#include "lcssa_t.h"
#include "xmalloc.h"
#include <assert.h>
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static void add_edge(ir_node *const node, ir_node *const pred)
{
	int       const arity = get_irn_arity(node);
	ir_node **const in    = ALLOCAN(ir_node *, arity + 1);
	for (int i = 0; i < arity; ++i) {
		in[i] = get_irn_n(node, i);
	}
	in[arity] = pred;
	set_irn_in(node, arity + 1, in);
}

static bool is_inner_loop(ir_loop *const outer_loop, ir_loop *inner_loop)
{
	ir_loop *old_inner_loop;
	do {
		old_inner_loop = inner_loop;
		inner_loop = get_loop_outer_loop(inner_loop);
	} while (inner_loop != old_inner_loop && inner_loop != outer_loop);
	return inner_loop != old_inner_loop;
}

static bool block_is_inside_loop(ir_node *const block, ir_loop *const loop)
{
	ir_loop *const block_loop = get_irn_loop(block);
	if (block_loop == NULL)
		return false;
	return block_loop == loop || is_inner_loop(loop, block_loop);
}

static bool block_dominates_loop(ir_node *const block, ir_loop *const loop)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			assert(is_Block(element.node));
			if (!block_dominates(block, element.node))
				return false;
		} else if (*element.kind == k_ir_loop) {
			if (!block_dominates_loop(block, element.son))
				return false;
		}
	}
	return true;
}

// returns the block that dominates all blocks in the loop or NULL
static ir_node *get_loop_header(ir_loop *const loop)
{
	// pick a random block
	ir_node *header = NULL;
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			header = element.node;
			break;
		}
	}
	assert(header && is_Block(header));

	// walk up the dominance tree
	ir_node *idom = get_Block_idom(header);
	while (idom && block_is_inside_loop(idom, loop)) {
		header = idom;
		idom = get_Block_idom(header);
	}

	return block_dominates_loop(header, loop) ? header : NULL;
}

static ir_node *duplicate_node(ir_node *const node, ir_node *const new_block)
{
	ir_node *const new_node = exact_copy(node);
	if (!is_Block(new_node))
		set_nodes_block(new_node, new_block);
	// link the original node and the most recent copy to the new node
	ir_node *const link = get_irn_link(node);
	if (link)
		set_irn_link(link, new_node);
	set_irn_link(node, new_node);
	set_irn_link(new_node, node);
	DB((dbg, LEVEL_3, "\tduplicating node %N (%n), new node %N\n", node, node, new_node));
	return new_node;
}

static void rewire_successor_block(ir_node *const block, int n)
{
	ir_node *const node     = get_irn_n(block, n);
	ir_node *const new_node = get_irn_link(node);
	assert(new_node);
	add_edge(block, new_node);

	// rewire phis inside the block
	unsigned const n_outs = get_irn_n_outs(block);
	for (unsigned i = 0; i < n_outs; ++i) {
		ir_node *const phi = get_irn_out(block, i);
		if (is_Phi(phi)) {
			ir_node *const pred     = get_irn_n(phi, n);
			ir_node       *new_pred = get_irn_link(pred);
			if (new_pred == NULL) {
				new_pred = pred;
			}
			add_edge(phi, new_pred);
		}
	}
}

static void rewire_node(ir_node *const node, ir_node *const header)
{
	ir_node *const new_node = get_irn_link(node);
	assert(new_node);
	assert(get_irn_arity(node) == get_irn_arity(new_node));

	// rewire the successors outside the loop
	unsigned const n_outs = get_irn_n_outs(node);
	for (unsigned i = 0; i < n_outs; ++i) {
		int n;
		ir_node *const succ = get_irn_out_ex(node, i, &n);
		if (get_irn_link(succ) == NULL && is_Block(succ)) {
			rewire_successor_block(succ, n);
		} else if (is_End(succ)) {
			assert(get_irn_link(succ) == NULL);
			add_End_keepalive(succ, new_node);
		}
	}

	// loop header block
	if (node == header) {
		assert(is_Block(node));
		int const arity = get_irn_arity(node);
		int new_arity = 0;
		for (int i = 0; i < arity; ++i) {
			ir_node *const pred     = get_irn_n(header, i);
			ir_node *const new_pred = get_irn_link(pred);
			if (new_pred) {
				++new_arity;
			}
		}
		ir_node **const in = ALLOCAN(ir_node *, new_arity);
		for (int i = 0, j = 0; i < arity; ++i) {
			ir_node *const pred     = get_irn_n(header, i);
			ir_node *const new_pred = get_irn_link(pred);
			if (new_pred) {
				// jump to the old node from outside and from the new node
				set_irn_n(node, i, new_pred);
				// jump to the new node only from the old node
				in[j++] = pred;
			}
		}
		set_irn_in(new_node, new_arity, in);
		return;
	}

	// phi node inside loop header
	if (is_Phi(node) && get_nodes_block(node) == header) {
		int const arity = get_irn_arity(node);
		assert(arity == get_irn_arity(header));
		int new_arity = 0;
		for (int i = 0; i < arity; ++i) {
			if (get_irn_link(get_irn_n(header, i))) {
				++new_arity;
			}
		}
		ir_node **const in = ALLOCAN(ir_node *, new_arity);
		for (int i = 0, j = 0; i < arity; ++i) {
			if (get_irn_link(get_irn_n(header, i))) {
				ir_node *const pred     = get_irn_n(node, i);
				ir_node *const new_pred = get_irn_link(pred);
				if (new_pred) {
					set_irn_n(node, i, new_pred);
				}
				in[j++] = pred;
			}
		}
		set_irn_in(new_node, new_arity, in);
		return;
	}

	int const arity = get_irn_arity(new_node);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred     = get_irn_n(new_node, i);
		ir_node *const new_pred = get_irn_link(pred);
		assert(!is_backedge(node, i));
		if (new_pred != NULL) {
			set_irn_n(new_node, i, new_pred);
		}
	}
}

static ir_node *duplicate_block(ir_node *const block)
{
	ir_node *const new_block = duplicate_node(block, NULL);
	unsigned const n_outs = get_irn_n_outs(block);
	for (unsigned i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		if (get_nodes_block(node) != block)
			continue;
		duplicate_node(node, new_block);
	}
	return new_block;
}

static void rewire_block(ir_node *const block, ir_node *const header)
{
	rewire_node(block, header);
	unsigned const n_outs = get_irn_n_outs(block);
	for (unsigned i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		if (get_nodes_block(node) != block)
			continue;
		rewire_node(node, header);
	}
}

static unsigned find_optimal_factor(unsigned long number, unsigned max) {
	if (number <= max) {
		// loop can be unrolled completely
		return (unsigned) number;
	}
	unsigned i;
	for (i = 2; i <= number / 2; i++) {
		if (number % i == 0) {
			// found a small divisor i -> number/i is a large divisor of number
			if ((number / i) <= max) {
				unsigned candidate = (unsigned) number / i;
				// limit to powers of two for now
				if ((candidate != 0) && ((candidate & (candidate - 1)) == 0)) {
					return candidate;
				}
			}
		}
	}
	return 0;
}

enum Op { ADD, SUB, MUL };
#define Op enum Op

static bool binop_to_op(Op *op, ir_node *bin_op)
{
	assert(is_binop(bin_op));
	if (!is_Add(bin_op) && !is_Sub(bin_op) && !is_Mul(bin_op))
		return false;
	if (is_Add(bin_op)) {
		*op = ADD;
	} else if (is_Sub(bin_op)) {
		*op = SUB;
	} else if (is_Mul(bin_op)) {
		*op = MUL;
	}
	return true;
}

struct linear_unroll_info {
	Op op;
	ir_loop *loop;
	ir_node **i;
	unsigned i_size;
	ir_node *cmp;
	ir_relation rel;
	ir_node *incr;
	ir_node *phi;
	ir_node *bound;
	ir_node *header;
};

#define linear_unroll_info struct linear_unroll_info

void free_lin_unroll_info(linear_unroll_info *info)
{
	if (info->i) {
		free(info->i);
	}
}
struct alias_list {
	ir_node *node;
	const ir_node *addr;
	ir_type *type;
	unsigned size;
	struct alias_list *next;
};
static struct alias_list *alias_candidates;

static bool is_aliased(ir_node *node)
{
	DB((dbg, LEVEL_4, "Checking aliasing of %+F\n", node));

	const ir_node *addr;
	ir_type *type;

	if (is_Load(node)) {
		addr = get_Load_ptr(node);
		type = get_Load_type(node);
	} else if (is_Store(node)) {
		addr = get_Store_ptr(node);
		type = get_Store_type(node);
	} else if (is_Call(node)) {
		addr = get_Call_ptr(node);
		type = get_Call_type(node);
	} else {
		DB((dbg, LEVEL_4, "%+F neither, load, nor store, nor call\n",
		    node));
		return false;
	}
	for (struct alias_list *curr = alias_candidates; curr;
	     curr = curr->next) {
		if (get_alias_relation(curr->addr, curr->type, curr->size, addr,
				       type,
				       get_type_size(type)) != ir_no_alias) {
			DB((dbg, LEVEL_4, "found  aliasing with %+F\n",
			    curr->node));
			return true;
		}
	}
	DB((dbg, LEVEL_4, "found no aliasing\n"));
	return false;
}
static void walk_call_for_aliases(ir_node *call);
static bool stored;

void check_for_store(ir_node *node, ir_loop *loop)
{
	assert(!is_Block(node));
	if (is_Call(node)) {
		walk_call_for_aliases(node);
	}
	if (!is_Store(node)) {
		return;
	}
	DB((dbg, LEVEL_4, "Found store %+F\n", node));
	ir_node *block = get_block(node);
	if (loop && !block_is_inside_loop(block, loop)) {
		DB((dbg, LEVEL_4, "Store %+F in wrong loop - skipping\n",
		    node));
		DB((dbg, LEVEL_4,
		    "Expected loop: %+F (or inner loops of it), was %+F\n",
		    loop, get_irn_loop(node)));
		return;
	}
	ir_type *type = get_Store_type(node);
	ir_node *addr = get_Store_ptr(node);
	unsigned int size = get_type_size(type);
	struct alias_list list = { .type = type,
				   .addr = addr,
				   .node = node,
				   .size = size,
				   .next = alias_candidates };
	DB((dbg, LEVEL_4, "Adding store to potential alias list\n"));
	struct alias_list *list_ptr =
		(struct alias_list *)malloc(sizeof(struct alias_list));
	*list_ptr = list;
	alias_candidates = list_ptr;
}
static void walk_graph_aliasing(ir_node *block, void *_)
{
	DB((dbg, LEVEL_5, "Inspecting block in call graph: %+F\n", block));
	if (!is_Block(block)) {
		return;
	}
	for (unsigned i = 0; i < get_irn_n_outs(block); ++i) {
		ir_node *node = get_irn_out(block, i);
		check_for_store(node, NULL);
	}
}

static void walk_call_for_aliases(ir_node *call)
{
	DB((dbg, LEVEL_4, "Found call: %+F\n", call));
	ir_entity *callee_entity = get_Call_callee(call);
	ir_graph *callee_graph = get_entity_linktime_irg(callee_entity);
	if (!callee_graph) {
		// TODO: Library doing things? Should this be a straight "no unroll"-scenario
		// TODO: This code is highly dangerous - definitely should be thoroughly tested and reviewed
		DB((dbg, LEVEL_4, "Unknown call found!\n"));
		for (int i = 0; i < get_Call_n_params(call); i++) {
			ir_node *param = get_Call_param(call, i);
			DB((dbg, LEVEL_4, "Has param %+F of type!\n", param));
			struct alias_list list = { .addr = NULL,
						   .next = alias_candidates };
			if (is_Proj(param)) {
				ir_node *pre_proj = get_Proj_pred(param);
				if (is_Load(pre_proj)) {
					list.addr = get_Load_ptr(pre_proj);
					ir_type *type = get_Load_type(pre_proj);
					list.type = type;
					list.size = get_type_size(type);
					list.node = pre_proj;
				} else if (is_Proj(pre_proj)) {
					ir_node *pre_pre_proj =
						get_Proj_pred(pre_proj);
					if (is_Call(pre_pre_proj)) {
						list.addr = get_Call_ptr(
							pre_pre_proj);
						ir_type *type = get_Call_type(
							pre_pre_proj);
						list.type = type;
						list.size = get_type_size(type);
						list.node = pre_proj;
					}
				}
			}
			if (list.addr) {
				DB((dbg, LEVEL_4,
				    "Adding store to potential alias list\n"));
				struct alias_list *list_ptr =
					(struct alias_list *)malloc(
						sizeof(struct alias_list));
				*list_ptr = list;
				alias_candidates = list_ptr;
			}
		}
		return;
	}
	if (callee_graph->reserved_resources & IR_RESOURCE_IRN_VISITED) {
		DB((dbg, LEVEL_4,
		    "Already visited target of call %+F - recursive\n", call));
		return;
	}
	DB((dbg, LEVEL_4, "Walking graph %+F of call for aliases\n",
	    callee_graph));
	irg_walk_blkwise_graph(callee_graph, walk_graph_aliasing, NULL, NULL);
}

static void clear_all_stores()
{
	DB((dbg, LEVEL_4, "Clearing existing stores\n"));
	for (struct alias_list *curr = alias_candidates; curr;) {
		struct alias_list *tmp = curr;
		curr = curr->next;
		free(tmp);
	}
}

static void get_all_stores(ir_loop *loop)
{
	long n = get_loop_n_elements(loop);

	DB((dbg, LEVEL_4, "Finding all stores in loop %+F\n", loop));
	alias_candidates = NULL;
	for (long i = 0; i < n; ++i) {
		loop_element element = get_loop_element(loop, i);
		if (*element.kind == k_ir_loop) {
			DB((dbg, LEVEL_4,
			    "\t Found child loop %+F; digging in\n",
			    element.son));
			get_all_stores(element.son);
			continue;
		} else if (*element.kind != k_ir_node) {
			continue;
		}
		ir_node *node = element.node;
		assert(is_Block(node));
		DB((dbg, LEVEL_5,
		    "\t Block %+F in loop %+F... looking for stores\n", node,
		    loop));
		unsigned m = get_irn_n_outs(node);
		for (unsigned j = 0; j < m; ++j) {
			check_for_store(get_irn_out(node, j), loop);
		}
	}
	DB((dbg, LEVEL_4, "Found all stores in loop %+F\n", loop));
}
struct irn_stack {
	ir_node *el;
	struct irn_stack *next;
};

static void add_to_stack(ir_node *node, struct irn_stack **stack)
{
	struct irn_stack new_top = { .next = *stack, .el = node };
	*stack = malloc(sizeof(struct irn_stack));
	**stack = new_top;
}

static bool is_in_stack(ir_node *query, struct irn_stack *head)
{
	for (struct irn_stack *curr = head; curr; curr = curr->next) {
		if (curr->el == query) {
			return true;
		}
	}
	return false;
}

static void free_stack(struct irn_stack *head)
{
	for (struct irn_stack *curr = head; curr;) {
		struct irn_stack *to_free = curr;
		curr = curr->next;
		free(to_free);
	}
}

static struct irn_stack *visited_base;

static bool is_valid_base_(ir_node *node, ir_loop *loop)
{
	DB((dbg, LEVEL_4, "Checking if %+F is a valid base\n", node));
	// Const
	if (is_in_stack(node, visited_base)) {
		return false;
	}
	add_to_stack(node, &visited_base);
	if (is_Const(node)) {
		DB((dbg, LEVEL_4, "Node is const. Valid base.\n"));
		return true;
	}

	// Load
	if (is_Proj(node)) {
		DB((dbg, LEVEL_5, "Node is proj; looking further\n"));
		ir_node *pre_proj = get_Proj_pred(node);
		if (is_Proj(pre_proj)) {
			DB((dbg, LEVEL_5, "Found 2nd proj layer\n"));
			ir_node *proj_call = get_Proj_pred(pre_proj);
			if (!is_Call(proj_call)) {
				DB((dbg, LEVEL_4,
				    "2nd proj layer does not point to call\n"));
				return false;
			}
			ir_entity *callee = get_Call_callee(proj_call);
			mtp_additional_properties properties =
				get_entity_additional_properties(callee);
			if (!(properties & mtp_property_pure)) {
				DB((dbg, LEVEL_4, "Call is not pure\n"));
				return false; // TODO: Comment out for Unroll_library_function_modifying_incr.invalid.c testcase
			}
			unsigned n = get_Call_n_params(proj_call);
			for (unsigned i = 0; i < n; ++i) {
				ir_node *call_param =
					get_Call_param(proj_call, i);
				if (!is_valid_base_(call_param, loop)) {
					DB((dbg, LEVEL_4,
					    "Call param %d %+F is not pure\n",
					    i, call_param));
					return false;
				}
			}
			DB((dbg, LEVEL_4,
			    "Checking for aliasing on call then returning\n"));
			return !is_aliased(proj_call);
		} else if (is_Load(pre_proj)) {
			ir_node *pre_load = get_Load_ptr(pre_proj);
			if (is_Proj(pre_load)) {
				DB((dbg, LEVEL_4,
				    "Load points further to %+F. Investigating further\n",
				    pre_load));
				if (!is_valid_base_(pre_load, loop)) {
					return false;
				}
			}
			DB((dbg, LEVEL_4, "Load; Checking on aliasing\n"));
			return !is_aliased(pre_proj);
		}
	}
	if (is_Phi(node)) {
		unsigned n = get_Phi_n_preds(node);
		DB((dbg, LEVEL_4,
		    "Node is phi; Checking all %u inputs are bases\n", n));
		unsigned pointing_into_loop = 0;
		for (unsigned i = 0; i < n; i++) {
			ir_node *phi_pred = get_Phi_pred(node, i);
			ir_node *pred_block = get_block(phi_pred);
			if (loop && block_is_inside_loop(pred_block, loop)) {
				pointing_into_loop++;
				DB((dbg, LEVEL_4,
				    "\tPhi pred %u (%+F) inside loop\n", n,
				    phi_pred));
			}
			if (!is_valid_base_(phi_pred, loop)) {
				DB((dbg, LEVEL_4,
				    "\tPhi pred %u (%+F) was not a valid base. Phi is not a valid "
				    "base\n",
				    i, phi_pred));
				return false;
			}
		}
		if (loop && pointing_into_loop > 1) {
			DB((dbg, LEVEL_4,
			    "Phi has multiple ends in loop => Cannot unroll\n"));
			return false;
		}
		DB((dbg, LEVEL_4,
		    "Phi is valid base: All phi preds were valid bases\n"));
		return true;
	}
	if (is_Conv(node)) { // Cast
		ir_node *conved = get_Conv_op(node);
		DB((dbg, LEVEL_4, "Found cast. Checking target of cast (%+F)\n",
		    conved));
		return is_valid_base_(conved, loop);
	}
	return false;
}

static bool is_valid_base(ir_node *node, ir_loop *loop)
{
	visited_base = NULL;
	bool ret = is_valid_base_(node, loop);
	free_stack(visited_base);
	return ret;
}

static ir_node *climb_single_phi(ir_node *phi)
{
	if (!is_Phi(phi))
		return phi;
	if (get_Phi_n_preds(phi) != 1)
		return phi;
	return climb_single_phi(get_Phi_pred(phi, 0));
}

static void phi_cycle_dfs(ir_node *curr, ir_node *searched, ir_loop *loop,
			  bool *foundCycle, bool *valid, ir_node **outside,
			  struct irn_stack **stack_head)
{
	assert(is_Phi(curr));
	struct irn_stack stack_el = { .next = *stack_head, .el = curr };
	*stack_head = (struct irn_stack *)malloc(sizeof(struct irn_stack));
	**stack_head = stack_el;
	unsigned n = get_Phi_n_preds(curr);
	DB((dbg, LEVEL_5, "Querying %+F for phi cycle check\n", curr));
	if (n == 0) {
		DB((dbg, LEVEL_5, "%+F has no preds. Can't be right\n", curr));
		*valid = false;
		return;
	}
	for (unsigned i = 0; i < n; ++i) {
		ir_node *w = get_Phi_pred(curr, i);
		DB((dbg, LEVEL_5, "\tChecking edge (%+F,%+F)\n", curr, w));
		if (w == searched) {
			DB((dbg, LEVEL_5, "\t\tEdge to searched (%+F)\n",
			    searched));
			*foundCycle = true;
		}
		if (!is_Phi(w)) {
			DB((dbg, LEVEL_5, "\t\tEdge to outside\n"));
			if (!*outside) {
				*outside = curr;
			} else if (*outside != curr) {
				DB((dbg, LEVEL_5,
				    "\t\t\t Found 2nd edge to outside. Not valid\n"));
				*valid = false;
			}
			continue;
		}
		if (is_in_stack(w, *stack_head)) {
			DB((dbg, LEVEL_5, "\t\tAlready visited %+F. Skipping\n",
			    w));
			return;
		}
		phi_cycle_dfs(w, searched, loop, foundCycle, valid, outside,
			      stack_head);
	}
	if (*outside != curr && !block_is_inside_loop(get_block(curr), loop)) {
		DB((dbg, LEVEL_5,
		    "\tBlock is neither leading out nor in loop\n", searched));
		*valid = false;
	}
}
static ir_node *check_cycle_and_find_exit(ir_node *initial_phi,
					  ir_node *searched, ir_loop *loop)
{
	if (!is_Phi(initial_phi)) {
		return initial_phi;
	}
	struct irn_stack *stack = NULL;
	bool valid = true;
	bool foundCycle = false;
	ir_node *outside = NULL;
	phi_cycle_dfs(initial_phi, searched, loop, &foundCycle, &valid,
		      &outside, &stack);
	struct irn_stack *curr = stack;
	while (curr) {
		struct irn_stack *clear = curr;
		curr = curr->next;
		free(clear);
	}
	if (outside && valid && foundCycle)
		return outside;
	return NULL;
}

static bool is_valid_incr(linear_unroll_info *unroll_info, ir_node *node)
{
	DB((dbg, LEVEL_4, "Checking if increment\n"));

	if (!is_binop(node)) {
		DB((dbg, LEVEL_4, "Did not find increment: Not binop\n"));
		return false;
	}
	if (!binop_to_op(&(unroll_info->op), node)) {
		DB((dbg, LEVEL_4, "Invalid binary op\n"));
		return false;
	}
	ir_node *left = climb_single_phi(get_binop_left(node));
	ir_node *right = climb_single_phi(get_binop_right(node));
	ir_node *node_to_check = NULL;
	DB((dbg, LEVEL_5,
	    "\tLooking for phi (%+F) in left (%+F) and right (%+F)\n",
	    unroll_info->phi, left, right));
	if (!is_Phi(left) && !is_Phi(right)) {
		DB((dbg, LEVEL_4, "No phis found in incr. Can't be right\n"));
		return false;
	}
	/* TODO: Find out why not working -> Unroll_simple_incr_const.c */
	if (left == unroll_info->phi) {
		DB((dbg, LEVEL_5, "\tLeft is correct Phi\n"));
		node_to_check = right;
	}
	if (right == unroll_info->phi) {
		DB((dbg, LEVEL_5, "\tRight is correct Phi\n"));
		node_to_check = left;
	}
	if (!node_to_check) {
		// Assume it is a cycle:
		ir_node *left_c = check_cycle_and_find_exit(
			get_binop_left(node), node, unroll_info->loop);
		ir_node *right_c = check_cycle_and_find_exit(
			get_binop_right(node), node, unroll_info->loop);
		if (left_c == unroll_info->phi) {
			DB((dbg, LEVEL_5, "\tLeft leads to correct Phi\n"));
			node_to_check = right;
		}
		if (right_c == unroll_info->phi) {
			DB((dbg, LEVEL_5, "\tRight leads to correct Phi\n"));
			node_to_check = left;
		}
		if (!node_to_check) {
			DB((dbg, LEVEL_4, "Phi not found in incr\n"));
			return false;
		}
	}
	if (unroll_info->op == MUL && !is_Const(node_to_check)) {
		DB((dbg, LEVEL_1,
		    "Mul currently only supports const addition\n"));
		return false;
	}
	if (!is_valid_base(node_to_check,
			   get_irn_loop(get_block(node_to_check)))) {
		DB((dbg, LEVEL_4,
		    "Incr does not have valid base, but has correct Phi\n"));
		return false;
	}
	DB((dbg, LEVEL_4, "Valid incr found %+F\n", node_to_check));
	unroll_info->incr = node_to_check;
	return true;
}
static bool check_phi(linear_unroll_info *unroll_info, ir_loop *loop)
{
	ir_node *phi = unroll_info->phi;
	assert(is_Phi(phi));
	unsigned phi_preds = get_Phi_n_preds(phi);
	if (phi_preds < 2) {
		DB((dbg, LEVEL_4, "Phi has %u preds. Too few!F\n", phi_preds));
		return false;
	}
	// check for static beginning: neither in loop, nor aliased and for valid linear increment
	clear_all_stores();
	get_all_stores(loop);
	long long incr_pred_index = -1;
	ir_node **is = malloc((phi_preds - 1) * sizeof(ir_node *));
	int is_size = 0;
	for (unsigned i = 0; i < phi_preds; ++i) {
		ir_node *curr = get_Phi_pred(phi, i);
		DB((dbg, LEVEL_5, "\tChecking for valid incr %+F\n", curr));
		if (is_valid_incr(unroll_info, curr)) {
			DB((dbg, LEVEL_5, "\tFound valid incr %+F\n", curr));
			if (incr_pred_index != -1) {
				incr_pred_index = -1;
				break;
			}
			incr_pred_index = i;
			continue;
		}
		if (is_size < phi_preds - 1) {
			is[is_size] = curr;
			is_size++;
		}
	}
	if (incr_pred_index == -1) {
		return false;
	}
	if (!unroll_info->i) {
		unroll_info->i_size = is_size;
		unroll_info->i = is;
	}
	DB((dbg, LEVEL_5, "\tFound %u Is:\n", unroll_info->i_size));
	for (int i = 0; i < unroll_info->i_size; i++) {
		DB((dbg, LEVEL_5, "\t\tI_src[%u]: %+F\n", i, is[i]));
		DB((dbg, LEVEL_5, "\t\tI[%u]: %+F\n", i, unroll_info->i[i]));
	}
	DB((dbg, LEVEL_5, "\tFound one phi incr and (%u-1) inputs. Phi valid\n",
	    phi_preds));
	return true;
}

static bool determine_lin_unroll_info(linear_unroll_info *unroll_info,
				      ir_loop *loop)
{
	unroll_info->i = NULL;
	unroll_info->loop = loop;
	DB((dbg, LEVEL_4, "\tDetermining info for loop %+F\n", loop));
	ir_node *header = get_loop_header(loop);
	unsigned outs = get_irn_n_outs(header);
	for (int i = 0; i < outs; ++i) {
		ir_node *node = get_irn_out(header, i);
		DB((dbg, LEVEL_4,
		    "Assessing node %+F for check being compare\n", loop));
		if (get_nodes_block(node) != header) {
			DB((dbg, LEVEL_5,
			    "\tNode is in wrong block -] skipping\n"));
			continue;
		}
		if (!is_Cmp(node)) {
			DB((dbg, LEVEL_5,
			    "\tNode is not compare - skipping\n"));
			continue;
		}
		ir_relation rel = get_Cmp_relation(node);
		if (rel != ir_relation_greater_equal &&
		    rel != ir_relation_greater &&
		    rel != ir_relation_less_equal && rel != ir_relation_less) {
			DB((dbg, LEVEL_5,
			    "\tRelation is wrong compare symbol\n"));
			continue;
		}
		DB((dbg, LEVEL_4,
		    "Found compare: %+F - investigating further\n", node));
		unroll_info->rel = rel;
		unroll_info->cmp = node;
		ir_node *left = get_Cmp_left(node);
		ir_node *right = get_Cmp_right(node);
		if (!is_Phi(left) && !is_Phi(right)) {
			DB((dbg, LEVEL_5,
			    "\tCouldn't find a phi in compare\n"));
			return false;
		}
		bool ret = false;
		if (is_Phi(left)) {
			unroll_info->phi = left;
			unroll_info->bound = right;
			DB((dbg, LEVEL_4, "Checking Phi left %+F\n", left));
			ret |= check_phi(unroll_info, loop);
		}
		if (is_Phi(right)) {
			unroll_info->phi = right;
			unroll_info->bound = left;
			DB((dbg, LEVEL_4, "Checking Phi right %+F\n", right));
			ret |= check_phi(unroll_info, loop);
		}
		DB((dbg, LEVEL_4, "Checking bound %+F\n", unroll_info->bound));
		if (!is_valid_base(unroll_info->bound, loop)) {
			DB((dbg, LEVEL_4, "Bound %+F is not valid base\n",
			    unroll_info->bound));
			ret = false;
		}
		DEBUG_ONLY(if (!ret) {
			DB((dbg, LEVEL_4,
			    "Cannot unroll: phi checks failed %+F\n", loop));
		} else { DB((dbg, LEVEL_4, "Can unroll %+F\n", loop)); })
		return ret;
	}
	DB((dbg, LEVEL_4, "Cannot unroll: Didn't find valid compare%+F\n",
	    loop));
	return false;
}

/**
 * walk trivial phis (with only one input) until another node is found
 */
static ir_node *skip_trivial_phis(ir_node *start)
{
	if (is_Phi(start) && get_Phi_n_preds(start) == 1) {
		return skip_trivial_phis(get_Phi_pred(start, 0));
	}
	return start;
}

/**
 * Analyzes loop and decides whether it should be unrolled or not and chooses a suitable unroll factor.
 *
 * Currently only loops featuring a counter variable with constant start, step and limit known at compile time
 * are considered for unrolling.
 * Tries to find a divisor of the number of loop iterations which is smaller than the maximum unroll factor
 * and is a power of two. In this case, additional optimizations are possible.
 *
 * @param header loop header
 * @param max max allowed unroll factor
 * @param fully_unroll pointer to where the decision to fully unroll the loop is
 * stored
 * @return unroll factor to use fot this loop; 0 if loop should not be unrolled
 */
static unsigned find_suitable_factor(ir_node *const header, unsigned max,
				     bool *fully_unroll)
{
	unsigned const DONT_UNROLL = 0;
	unsigned const n_outs = get_irn_n_outs(header);
	unsigned factor = 1;
	return 0;
	for (unsigned i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(header, i);
		assert(!is_Block(node));
		if (get_nodes_block(node) != header)
			continue;

		if (is_Cmp(node)) {

			ir_relation cmp_rel = get_Cmp_relation(node);
			if (cmp_rel == ir_relation_less_greater || cmp_rel == ir_relation_equal || cmp_rel & ir_relation_unordered) {
				return DONT_UNROLL;
			}

			ir_tarval *tv_init = NULL;
			ir_tarval *tv_step = NULL;
			ir_tarval *tv_limit = NULL;

			ir_node *header_phi;
			ir_node *cmp_right = get_Cmp_right(node);
			if (is_Const(cmp_right) && mode_is_int(get_irn_mode(cmp_right))) {
				if (!is_Phi(get_Cmp_left(node))) {
					return DONT_UNROLL;
				}
				// found Cmp(?, const)
				header_phi = get_Cmp_left(node);
				tv_limit = get_Const_tarval(get_Cmp_right(node));
			} else {
				return DONT_UNROLL;
			}
			int phi_preds = get_Phi_n_preds(header_phi);
			ir_node *cnt_add = NULL;
			for (int j = 0; j < phi_preds; j++) {
				ir_node *phi_pred = get_Phi_pred(header_phi, j);
				if (is_Const(phi_pred) && mode_is_int(get_irn_mode(cmp_right))) {
					// found constant init for (possible) counter
					ir_tarval *const_tv = get_Const_tarval(phi_pred);
					if (tv_init == NULL || tarval_cmp(tv_init, const_tv) == ir_relation_equal) {
						tv_init = const_tv;
						continue;
					}
				}
				phi_pred = skip_trivial_phis(phi_pred);
				// is_binop() would find more cases, but we currently can only optimize further if we have an Add here
				if (is_Add(phi_pred) && cnt_add == NULL) {
					cnt_add = phi_pred;
					ir_node *left = get_binop_left(phi_pred);
					ir_node *right = get_binop_right(phi_pred);
					if (is_Const(right) && is_Phi(left)) {
						// found Add(phi, const)

						bool found_constant_step = false;
						// LCSSA construction builds additional phi nodes
						do {
							if (left == header_phi) {
								found_constant_step = true;
								tv_step = get_Const_tarval(right);
								break;
							}
							left = get_Phi_pred(left, 0);

						} while (is_Phi(left) && (get_Phi_n_preds(left) == 1 || left == header_phi));

						if (found_constant_step) {
							continue;
						}
					}
					return DONT_UNROLL;
				}
				// multiple uses of the same loop counter increment/decrement
				if (phi_pred == cnt_add) {
					continue;
				} else {
					return DONT_UNROLL;
				}
			}

			assert(tv_limit != NULL && tv_init != NULL && tv_step != NULL);

			// normalize: use less or less_equal as relation
			if (cmp_rel & ir_relation_greater) {
				ir_tarval *tmp = tv_init;
				tv_init = tv_limit;
				tv_limit = tmp;
				tv_step = tarval_neg(tv_step);
				cmp_rel = get_inversed_relation(cmp_rel);
			}

			ir_tarval *tv_interval = tarval_sub(tv_limit, tv_init);
			if (tarval_is_negative(tv_interval) || tarval_is_negative(tv_step)) {
				return DONT_UNROLL;
			}

			ir_tarval *tv_one = new_tarval_from_long(1, get_tarval_mode(tv_interval));
			// normalize: use less_equal as relation
			if (!(cmp_rel & ir_relation_equal)) {
				// interval -= 1
				tarval_sub(tv_interval, tv_one);
			}

			assert(!tarval_is_null(tv_step));
			// calculate loop iterations; add one iteration to count the first iteration
			ir_tarval *tv_loop_count = (tarval_add(tarval_div(tv_interval, tv_step), tv_one));
			long loop_count = get_tarval_long(tv_loop_count);
			if (loop_count <= 0) {
				return DONT_UNROLL;
			}

#ifdef DEBUG_libfirm
			long limit = get_tarval_long(tv_limit);
			long step = get_tarval_long(tv_step);
			long init = get_tarval_long(tv_init);
			DB((dbg, LEVEL_3 , "\tinit: %ld, step: %ld, limit: %ld, loop count: %ld\n", init, step, limit, loop_count));
#endif
			factor = find_optimal_factor((unsigned long) loop_count, max);
			if (factor == (unsigned long) loop_count) {
				*fully_unroll = true;
			}
			break;
		}
	}
	return factor;
}

/**
 * Remove block input with given index.
 */
static void remove_block_input(ir_node *block, int idx)
{
	int i, j, n = get_Block_n_cfgpreds(block) - 1;
	unsigned k;
	ir_node *phi;

	ir_node **ins = ALLOCAN(ir_node*, n);

	if (n == 1) {
		/* all Phis will be deleted */

		for (k = 0; k < get_irn_n_outs(block); k++) {
			phi = get_irn_out(block, k);
			if (is_Phi(phi)) {
				if (get_Phi_loop(phi)) {
					remove_keep_alive(phi);
					set_Phi_loop(phi, false);
				}
				exchange(phi, get_Phi_pred(phi, idx ^ 1));
			}
		}
	} else {
		for (k = 0; k < get_irn_n_outs(block); k++) {
			phi = get_irn_out(block, k);
			if (is_Phi(phi)) {
				for (i = j = 0; i <= n; ++i) {
					if (i != idx)
						ins[j++] = get_Phi_pred(phi, i);
				}
				set_irn_in(phi, n, ins);
			}
		}
	}
	for (i = j = 0; i <= n; ++i) {
		if (i != idx)
			ins[j++] = get_Block_cfgpred(block, i);
	}
	set_irn_in(block, n, ins);
}

static void rewire_fully_unrolled(ir_loop *const loop, ir_node *header, unsigned const factor) {
	int n_header_preds = get_irn_arity(header);

	ir_node *after_loop = NULL;
	int n_after = 0;
	// 1. search for the after_loop block
	unsigned const header_n_outs = get_irn_n_outs(header);
	for (unsigned i = 0; i < header_n_outs; ++i) {
		int n;
		ir_node *const succ = get_irn_out_ex(header, i, &n);
		if (is_Proj(succ) && get_irn_mode(succ) == mode_X) {
			unsigned proj_outs = get_irn_n_outs(succ);
			assert(proj_outs == 1);
			for (unsigned j = 0; j < proj_outs; j++) {
				ir_node *cf_succ = get_irn_out_ex(succ, j, &n_after);
				if (get_irn_link(cf_succ) == NULL && is_Block(cf_succ) && !block_is_inside_loop(cf_succ, loop)) {
					// found block after loop
					after_loop = cf_succ;
				}
			}
		}
	}

	if (after_loop == NULL) {
		return;
	}

	for (int i = 0; i < n_header_preds; i++) {
		// 2. find loop body blocks which jump back into the loop header
		ir_node *pred_block = get_nodes_block(get_irn_n(header, i));
		if ((get_irn_link(pred_block) == NULL && factor > 1) || !block_is_inside_loop(pred_block, loop)) {
			continue;
		}

		// 3. jump from such loop body block into block after_loop instead
		ir_node *old_jump = get_irn_n(header, i);
		add_edge(after_loop, old_jump);

		// 4. add inputs to phis inside the after_loop block
		unsigned const n_outs = get_irn_n_outs(after_loop);
		for (unsigned j = 0; j < n_outs; ++j) {
			ir_node *const phi = get_irn_out(after_loop, j);
			if (is_Phi(phi)) {
				ir_node *const pred = get_irn_n(phi, n_after);
				ir_node *new_pred = NULL;
				if (is_Phi(pred)) {
					// case: pred is phi in loop header. use input i of loop header phi
					new_pred = get_irn_n(pred, i);
				} else if (get_irn_mode(phi) == mode_M) {
					// case: memory phi in after_loop: search memory phi in loop
					// header note: if there are no nodes except the phi on the
					// memory path within the loop header, the case above
					// already handled the memory phi correctly.
					new_pred = pred;
					// walk memory path until phi is found
					while (!is_Phi(new_pred)) {
						new_pred = is_memop(new_pred) ? get_memop_mem(new_pred) : get_irn_n(new_pred, 0);
					}
					assert(is_Phi(new_pred));
					// use input i of loop header memory phi
					new_pred = get_irn_n(new_pred, i);
				} else {
					// case: pred was copied during loop unrolling
					new_pred = get_irn_link(pred);
				}
				if (new_pred == NULL) {
					// case: pred was defined outside of the loop
					new_pred = pred;
				}
				add_edge(phi, new_pred);
			}
		}
		// 5. remove input of loop header which represents jump from the last loop iteration
		remove_block_input(header, i);
		n_header_preds--;
		i--;
	}

	// 6. cleanup keepalives
	remove_End_Bads_and_doublets(get_irg_end(get_irn_irg(header)));
	DB((dbg, LEVEL_2, "fully unrolled loop %+F\n", loop));
}

static unsigned n_loops_unrolled = 0;

static struct irn_stack *unrolled_headers;
static struct irn_stack *unrolled_nodes;

static void rewire_loop(ir_loop *loop, ir_node *header, unsigned factor)
{
	ir_graph *irg = get_irn_irg(header);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			add_to_stack(element.node, &unrolled_nodes);
		}
	}
	for (unsigned j = 1; j < factor; ++j) {
		// step 1: duplicate blocks
		for (size_t i = 0; i < n_elements; ++i) {
			loop_element const element = get_loop_element(loop, i);
			if (*element.kind == k_ir_node) {
				assert(is_Block(element.node));
				ir_node *dup = duplicate_block(element.node);
				add_to_stack(dup, &unrolled_nodes);
				if (element.node == header) {
					DB((dbg, LEVEL_2,
					    " Duplicated header to %+F\n",
					    dup));
					add_to_stack(dup, &unrolled_headers);
				}
			}
		}
		// step 2: rewire the edges
		for (size_t i = 0; i < n_elements; ++i) {
			loop_element const element = get_loop_element(loop, i);
			if (*element.kind == k_ir_node) {
				assert(is_Block(element.node));
				rewire_block(element.node, header);
			}
		}
	}
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}

static bool is_Proj_attached_to_Cmp(ir_node *const proj)
{
	assert(is_Proj(proj));
	ir_node *post_proj = get_Proj_pred(proj);
	if (!is_Cond(post_proj)) {
		return false;
	}
	ir_node *pre_Cond = get_Cond_selector(post_proj);
	return is_Cmp(pre_Cond);
}
static void prune_block(ir_node *block, ir_node *header)
{
	assert(is_Block(block));
	assert(is_Block(header));
	if (block == header) {
		return;
	}
	for (unsigned i = 0; i < get_irn_n_outs(block); ++i) {
		ir_node *phi = get_irn_out(block, i);
		if (!is_Phi(phi)) {
			continue;
		}
		unsigned phi_n_preds = get_irn_arity(phi);
		ir_node **phi_preds = get_irn_in(phi);
		DB((dbg, LEVEL_5, "\t\t\tPruning phi %+F with links to ", phi));
		for (unsigned j = 0; j < phi_n_preds - 1; j++) {
			DB((dbg, LEVEL_5, "(%+F), ", phi_preds[j]));
		}
		if (phi_n_preds > 0) {
			DB((dbg, LEVEL_5, "(%+F).",
			    phi_preds[phi_n_preds - 1]));
		}
		DB((dbg, LEVEL_4, "\n"));
		for (int j = 0; j < get_irn_n_outs(phi); ++j) {
			ir_node *target = get_irn_out(phi, j);
			ir_node *target_block = get_block(target);
			if (is_in_stack(target_block, unrolled_headers)) {
				continue;
			}
			if (!is_in_stack(target_block, unrolled_nodes)) {
				DB((dbg, LEVEL_5,
				    "\t\t\t\t%+F is outside and links to pruned node\n",
				    target));
				unsigned target_arity = get_irn_arity(target);
				ir_node **new_in =
					ALLOCAN(ir_node *, target_arity - 1);
				for (unsigned k = 0, index = 0;
				     k < target_arity; k++) {
					ir_node *in = get_irn_n(target, k);
					if (in == phi) {
						DB((dbg, LEVEL_5,
						    "\t\t\t\t\t Removing link to %+F\n",
						    phi));
						continue;
					}
					DB((dbg, LEVEL_5,
					    "\t\t\t\t\t Keeping link to %+F\n",
					    in));
					assert(index < target_arity - 1);
					new_in[index] = in;
					index++;
				}
				set_irn_in(target, target_arity - 1, new_in);
				continue;
			}
			ir_node **nodes = ALLOCAN(ir_node *, 1);
			DB((dbg, LEVEL_5,
			    "\t\t\t\t%+F will now have input: ", target));
			for (unsigned k = 0; k < phi_n_preds; k++) {
				ir_node *curr_node = get_irn_n(phi, k);
				ir_node *curr_block = get_block(curr_node);
				if (!is_in_stack(curr_block,
						 unrolled_headers) &&
				    is_in_stack(curr_block, unrolled_nodes)) {
					nodes[0] = curr_node;
					DB((dbg, LEVEL_5, "%+F.", curr_node));
				}
			}
			set_irn_in(target, 1, nodes);
			DB((dbg, LEVEL_5, ".\n", target));
		}

		set_irn_in(phi, 0, NULL);
	}
	remove_keep_alive(block);
}

static void get_false_and_true_targets(ir_node *header,
				       ir_node **in_loop_target,
				       ir_node **out_of_loop_target)
{
	unsigned n = get_irn_n_outs(header);
	DB((dbg, LEVEL_4, "\tSearching targets of %+F\n", header));
	*in_loop_target = NULL;
	*out_of_loop_target = NULL;
	for (unsigned i = 0; i < n; ++i) {
		ir_node *curr = get_irn_out(header, i);
		if (!is_Proj(curr) || !is_Proj_attached_to_Cmp(curr)) {
			continue;
		}
		if (get_Proj_num(curr) == pn_Cond_true ||
		    get_Proj_num(curr) == pn_Cond_false) {
			assert(get_irn_n_outs(curr) == 1);
			ir_node *post_proj = get_irn_out(curr, 0);
			DEBUG_ONLY(ir_node *post_proj_block =
					   get_block(post_proj);)
			if (block_is_inside_loop(get_block(post_proj),
						 get_irn_loop(header))) {
				DB((dbg, LEVEL_4,
				    "\t\tIn loop tgt %+F, in block %+F\n",
				    post_proj, post_proj_block));
				*in_loop_target = post_proj;
			} else {
				DB((dbg, LEVEL_4,
				    "\t\tOut of loop tgt %+F, in block %+F\n",
				    post_proj, post_proj_block));
				*out_of_loop_target = post_proj;
			}
		}
	}
}

static void remove_excess_headers(linear_unroll_info *info,
				  ir_node *const header)
{
	for (struct irn_stack *curr = unrolled_headers; curr;
	     curr = curr->next) {
		ir_node *linked_header = curr->el;
		if (linked_header == header) {
			continue;
		}
		prune_block(linked_header, header);
	}
	for (struct irn_stack *curr = unrolled_headers; curr;
	     curr = curr->next) {
		ir_node *linked_header = curr->el;
		if (linked_header == header) {
			continue;
		}
		// TODO: Use unroll info to  find link to cmd rewire in to header  to out of proj true
		assert(linked_header);
		assert(is_Block(linked_header));
		DB((dbg, LEVEL_2, "Link to header %+F\n", linked_header));

		assert(info);
		void *v = linked_header->o.out;
		assert(v);
		ir_node *in_loop_target;
		ir_node *out_of_loop_target;
		get_false_and_true_targets(linked_header, &in_loop_target,
					   &out_of_loop_target);
		assert(in_loop_target);
		assert(out_of_loop_target);

		unsigned const in_loop_n_preds =
			get_Block_n_cfgpreds(linked_header);
		ir_node **const in_loop_preds = get_irn_in(linked_header);
		for (unsigned i = 0; i < in_loop_n_preds; ++i) {
			DB((dbg, LEVEL_4,
			    "\tRewire %+F (arity: %u, outs: %u) to be pointed to by %+F\n",
			    in_loop_preds[i], get_irn_arity(in_loop_preds[i]),
			    get_irn_n_outs(in_loop_preds[i]), in_loop_target));
			for (unsigned j = 0;
			     j < get_irn_arity(in_loop_preds[i]); ++j) {
				DB((dbg, LEVEL_5,
				    "\t\tCurrently %+F points to: %+F\n",
				    in_loop_preds[i],
				    get_irn_n(in_loop_preds[i], j)));
			}
		}
		set_irn_in(linked_header, 0, NULL);
		set_irn_in(in_loop_target, in_loop_n_preds, in_loop_preds);
		ir_node *out_of_loop_block = get_block(out_of_loop_target);
		unsigned const out_of_loop_n_preds =
			get_Block_n_cfgpreds(out_of_loop_block);
		assert(out_of_loop_n_preds > 0);
		ir_node **const out_of_loop_preds =
			ALLOCAN(ir_node *, out_of_loop_n_preds - 1);
		DB((dbg, LEVEL_4,
		    "\tRemove end block (%+F, with %u connections); linked header (%+F)\n",
		    out_of_loop_block, out_of_loop_n_preds, linked_header));
		for (unsigned i = 0, j = 0; i < out_of_loop_n_preds; ++i) {
			ir_node *pred = get_Block_cfgpred(out_of_loop_block, i);
			ir_node *pred_block = get_block(pred);
			DB((dbg, LEVEL_4, "\t\tAssessing for prune %+F\n",
			    pred_block));
			if (pred_block == linked_header) {
				DB((dbg, LEVEL_4, "\t\tRemove and prune %+F\n",
				    pred_block));
				continue;
			}
			assert(j < out_of_loop_n_preds - 1);
			DB((dbg, LEVEL_4, "\t\tKeep %+F\n", pred));
			out_of_loop_preds[j] = pred;
			j++;
		}
		set_irn_in(out_of_loop_block, out_of_loop_n_preds - 1,
			   out_of_loop_preds);
	}
}

static void recursive_copy_in_loop(ir_node *node, ir_node *header)
{
	for (unsigned i = 0; i < get_irn_arity(node); ++i) {
		ir_node *to_cpy = get_irn_n(node, i);
		ir_node *to_cpy_block = get_block(to_cpy);
		if (!is_in_stack(to_cpy_block, unrolled_nodes)) {
			continue;
		}
		duplicate_node(to_cpy, header);
		recursive_copy_in_loop(to_cpy, header);
	}
}

static void recursive_rewire_in_loop(ir_node *node)
{
	unsigned arity = get_irn_arity(node);
	ir_node **new_in = ALLOCAN(ir_node *, arity);
	for (unsigned i = 0; i < arity; ++i) {
		ir_node *next = get_irn_n(node, i);
		ir_node *next_block = get_block(next);
		if (!is_in_stack(next_block, unrolled_nodes)) {
			new_in[i] = next;
			continue;
		}
		new_in[i] = get_irn_link(next);
		recursive_rewire_in_loop(next);
	}
	set_irn_in(get_irn_link(node), arity, new_in);
}
enum Side { LEFT, RIGHT };
#define Side enum Side

static ir_node *update_header_condition_add(linear_unroll_info *info,
					    ir_node *header, ir_node *N,
					    ir_node *c_cpy,
					    ir_node *factor_const)
{
	ir_node *new_N_minus_c;
	ir_node *mul = new_r_Mul(header, c_cpy, factor_const);
	DB((dbg, LEVEL_4, "\tc * factor: %+F\n", mul));
	if (info->op == ADD) {
		new_N_minus_c = new_r_Sub(header, N, mul);
	} else if (info->op == SUB) {
		new_N_minus_c = new_r_Add(header, N, mul);
	} else {
		assert(false);
	}
	ir_node *new_N = new_r_Add(header, new_N_minus_c, c_cpy);
	DB((dbg, LEVEL_4,
	    "\tAttaching c + (N %s (c * factor)): %+F + (%+F %s (%+F * %+F) = %+F",
	    info->op == ADD ? "-" : "+", c_cpy, N, info->op == ADD ? "-" : "+",
	    c_cpy, factor_const, new_N));
	return new_N;
}
static ir_node *create_r_pow(ir_node *block, ir_node *base,
			     unsigned long long exp)
{
	if (exp == 0) {
		assert(false);
	}
	if (exp == 1) {
		return base;
	}
	return new_r_Mul(block, base, create_r_pow(block, base, exp - 1));
}
static ir_node *update_header_condition_mul(linear_unroll_info *info,
					    ir_node *header, ir_node *N,
					    ir_node *c_cpy,
					    ir_node *factor_const)
{
	ir_graph *irg = get_irn_irg(header);
	assert(is_Const(c_cpy));
	ir_node *pow =
		create_r_pow(header, factor_const, get_Const_long(c_cpy));
	DB((dbg, LEVEL_4, "\tc * factor: %+F\n", pow));
	ir_node *div = new_r_DivRL(header, new_r_Pin(header, new_r_NoMem(irg)),
				   N, pow, op_pin_state_pinned);
	ir_mode *mode;
	ir_mode *N_mode = get_irn_mode(N);
	ir_mode *div_mode = get_irn_mode(div);
	if (larger_mode(N_mode, div_mode)) {
		mode = N_mode;
	} else {
		mode = div_mode;
	}
	ir_node *div_proj = new_r_Proj(div, mode, pn_Div_res);
	ir_node *new_N_div_c = new_r_Mul(header, div_proj, c_cpy);
	ir_node *new_N = new_r_Mul(header, new_N_div_c, c_cpy);
	DB((dbg, LEVEL_4,
	    "\tAttaching c * (N / (factor ^ c)): %+F * (%+F / (%+F ^ %+F) = %+F * (%+F / %+F) = %+F",
	    c_cpy, N, factor_const, c_cpy, c_cpy, N, pow, new_N));
	return new_N;
}

static void update_header_condition(linear_unroll_info *info, unsigned factor)
{
	ir_node *cmp = info->cmp;
	DB((dbg, LEVEL_3, "Changing condition and compare %+F\n", cmp));
	ir_node *header = info->header;
	ir_node *left = get_Cmp_left(cmp);
	ir_node *right = get_Cmp_right(cmp);
	ir_node *N;
	Side side;
	if (left == info->phi) {
		N = right;
		side = RIGHT;
	} else if (right == info->phi) {
		N = left;
		side = LEFT;
	} else {
		assert(false);
	}
	DB((dbg, LEVEL_4, "\tN: %+F\n", N));
	ir_node *c_cpy;
	if (is_Const(info->incr)) {
		c_cpy = exact_copy(info->incr);
	} else {
		c_cpy = duplicate_node(info->incr, header);
		recursive_copy_in_loop(c_cpy, header);
		recursive_rewire_in_loop(info->incr);
	}
	DB((dbg, LEVEL_4, "\tcopied c: %+F\n", c_cpy));
	ir_node *factor_const = new_r_Const_long(get_irn_irg(header),
						 get_irn_mode(c_cpy), factor);
	ir_node *new_N;
	switch (info->op) {
	case ADD:
	case SUB:
		new_N = update_header_condition_add(info, header, N, c_cpy,
						    factor_const);
		break;
	case MUL:
		new_N = update_header_condition_mul(info, header, N, c_cpy,
						    factor_const);
		break;
	default:
		assert(false);
	}
	DB((dbg, LEVEL_4, "to %+F on the ", cmp));
	if (side == LEFT) {
		DB((dbg, LEVEL_4, "left side\n"));
		set_Cmp_left(cmp, new_N);
	} else {
		DB((dbg, LEVEL_4, "right side\n"));
		set_Cmp_right(cmp, new_N);
	}
}
static void unroll_loop_duff(ir_loop *const loop, unsigned factor,
			     linear_unroll_info *info)
{
	DB((dbg, LEVEL_3, "\tTrying to unroll %N\n", loop));
	ir_node *const header = get_loop_header(loop);
	if (header == NULL)
		return;
	info->header = header;
	unrolled_headers = NULL;
	unrolled_nodes = NULL;
	ir_graph *irg = get_irn_irg(header);
	rewire_loop(loop, header, factor);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	assure_irg_properties(irg,
			      IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
				      IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES |
				      IR_GRAPH_PROPERTY_NO_BADS);
	assert(unrolled_headers);
	DEBUG_ONLY(dump_ir_graph(irg, "duff_0"));
	remove_excess_headers(info, header);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	assure_irg_properties(irg,
			      IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
				      IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES |
				      IR_GRAPH_PROPERTY_NO_BADS);
	info->loop = get_irn_loop(header);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	update_header_condition(info, factor);
	DEBUG_ONLY(dump_ir_graph(irg, "duff_1"));
	++n_loops_unrolled;
	// TODO: Change main header
	// TODO: Fixup
}

static void unroll_loop(ir_loop *const loop, unsigned factor)
{
	DB((dbg, LEVEL_3, "\tTrying to unroll %N\n", loop));
	ir_node *const header = get_loop_header(loop);
	if (header == NULL)
		return;
	DB((dbg, LEVEL_3, "\tfound loop header %N\n", header));

	bool fully_unroll = false;
	factor = find_suitable_factor(header, factor, &fully_unroll);
	if (factor < 1 || (factor == 1 && !fully_unroll)) {
		DB((dbg, LEVEL_3,
		    "\tCan't unroll %+F, factor is %u, fully unroll: %u\n",
		    loop, factor, fully_unroll));
		return;
	}
	DB((dbg, LEVEL_2, "unroll loop %+F\n", loop));
	DB((dbg, LEVEL_3, "\tuse %d as unroll factor\n", factor));
	rewire_loop(loop, header, factor);
	++n_loops_unrolled;
	// fully unroll: remove control flow loop
	if (fully_unroll) {
		rewire_fully_unrolled(loop, header, factor);
	}
}

static size_t count_nodes(ir_loop *const loop)
{
	size_t       n_nodes    = 0;
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			n_nodes += get_irn_n_outs(element.node);
		} else if (*element.kind == k_ir_loop) {
			n_nodes += count_nodes(element.son);
		}
	}
	return n_nodes;
}

static unsigned determine_unroll_factor(ir_loop *const loop, unsigned const factor, unsigned const maxsize)
{
	return count_nodes(loop) < maxsize ? factor : 0;
}

#define DUFF_FACTOR 4
static void duplicate_innermost_loops(ir_loop *const loop,
				      unsigned const factor,
				      unsigned const maxsize,
				      bool const outermost)
{
	bool innermost = true;
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_loop) {
			duplicate_innermost_loops(element.son, factor, maxsize,
						  false);
			innermost = false;
		}
	}
	/*
	if (innermost && !outermost) {
		unsigned const actual_factor =
			determine_unroll_factor(loop, factor, maxsize);
		if (actual_factor > 0) {
			unroll_loop(loop, actual_factor, false);
		}
	}
	*/
	linear_unroll_info info;
	unsigned depth = get_loop_depth(loop);
	DB((dbg, LEVEL_2, "DUFF: Checking if %+F (depth: %u) is unrollable\n",
	    loop, depth));
	if (depth == 0) {
		DB((dbg, LEVEL_2, "Skipping loop with depth 0\n"));
		return;
	}
	for (unsigned i = 0; i < get_loop_n_elements(loop); ++i) {
		DB((dbg, LEVEL_3, "\tContaining: %+F\n",
		    get_loop_element(loop, i)));
	}
	DB((dbg, LEVEL_3, "-------------\n", loop));
	if (determine_lin_unroll_info(&info, loop)) {
		DB((dbg, LEVEL_2, "DUFF: Can unroll! (loop: %+F)\n", loop));
		unroll_loop_duff(loop, DUFF_FACTOR, &info);
	} else {
		DB((dbg, LEVEL_2, "DUFF: Cannot unroll! (loop: %+F)\n", loop));
	}
	free_lin_unroll_info(&info);
	DB((dbg, LEVEL_2, "--------------------------------------------\n",
	    loop));
}

void unroll_loops(ir_graph *const irg, unsigned factor, unsigned maxsize)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.loop-unrolling");
	n_loops_unrolled = 0;
	assure_lcssa(irg);
	assure_irg_properties(irg,
			      IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
				      IR_GRAPH_PROPERTY_NO_BADS |
				      IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	dump_ir_graph(irg, "lcssa");
	duplicate_innermost_loops(get_irg_loop(irg), factor, maxsize, true);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	DB((dbg, LEVEL_1, "%+F: %d loops unrolled\n", irg, n_loops_unrolled));
}
