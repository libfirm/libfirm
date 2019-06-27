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
	size_t depth;
	struct irn_stack *next;
};

#define iterate_stack(stack)                                                   \
	for (struct irn_stack *curr = stack; curr; curr = curr->next)

static size_t stack_size(struct irn_stack *stack)
{
	return stack->depth;
}

static void add_to_stack(ir_node *node, struct irn_stack **stack)
{
	struct irn_stack new_top = { .next = *stack,
				     .el = node,
				     .depth =
					     *stack ? (*stack)->depth + 1 : 0 };
	*stack = malloc(sizeof(struct irn_stack));
	**stack = new_top;
}

static bool is_in_stack(ir_node *query, struct irn_stack *head)
{
	iterate_stack(head)
	{
		if (curr->el == query) {
			return true;
		}
	}
	return false;
}

static void free_stack(struct irn_stack **head)
{
	for (struct irn_stack *curr = *head; curr;) {
		struct irn_stack *to_free = curr;
		curr = curr->next;
		free(to_free);
	}
	*head = NULL;
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
	if (loop && !block_is_inside_loop(get_block(node), loop)) {
		DB((dbg, LEVEL_4, "Node %+F not in loop -> ok\n", node));
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
			if (loop &&
			    !block_is_inside_loop(get_block(proj_call), loop)) {
				DB((dbg, LEVEL_4,
				    "Call %+F not in loop -> ok\n", proj_call));
				return true;
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
	free_stack(&visited_base);
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
static struct irn_stack *fixup_phis;
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
		if (get_irn_mode(phi) == mode_M) {
			assert(phi_n_preds == 0);
			continue;
		}
		ir_node **phi_preds = get_irn_in(phi);
		DB((dbg, LEVEL_5, "\t\t\tPruning phi %+F with links to ", phi));
		if (phi_n_preds > 0) {
			for (unsigned j = 0; j < phi_n_preds - 1; j++) {
				DB((dbg, LEVEL_5, "(%+F), ", phi_preds[j]));
			}
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
			if (is_in_stack(target, fixup_phis)) {
				continue;
			}
			if (!is_in_stack(target_block, unrolled_nodes)) {
				int target_arity = get_irn_arity(target);
				ir_node **new_in =
					ALLOCAN(ir_node *, target_arity - 1);
				DB((dbg, LEVEL_5,
				    "\t\t\t\t%+F (arity: %u) is outside and links to pruned node\n",
				    target, target_arity));
				for (int k = 0, index = 0; k < target_arity;
				     k++) {
					ir_node *in = get_irn_n(target, k);
					if (in == phi) {
						DB((dbg, LEVEL_5,
						    "\t\t\t\t\t Removing link to %+F (index: %u)\n",
						    phi, k));
						if (is_End(target)) {
							remove_End_n(target, k);
							break;
						}
						continue;
					}
					DB((dbg, LEVEL_5,
					    "\t\t\t\t\t Keeping link to %+F\n",
					    in));
					assert(index < target_arity - 1);
					new_in[index] = in;
					index++;
				}
				if (!is_End(target)) {
					set_irn_in(target, target_arity - 1,
						   new_in);
				}

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

static void remove_node_from_succ_ins(ir_node *node)
{
	DB((dbg, LEVEL_4, "\t\t\tPruning successors of %+F\n", node));
	for (unsigned i = 0; i < get_irn_n_outs(node); ++i) {
		ir_node *succ = get_irn_out(node, i);
		if (is_End(succ)) {
			DB((dbg, LEVEL_4, "\t\t\t\tRemoving KA\n"));
			remove_keep_alive(node);
			continue;
		}
		unsigned arity = get_irn_arity(succ);
		assert(arity > 0);
		ir_node **new_ins = ALLOCAN(ir_node *, arity - 1);
		DB((dbg, LEVEL_4, "\t\t\t\tPruning %+F\n", succ));
		for (unsigned j = 0, k = 0; j < arity; j++) {
			ir_node *tgt = get_irn_n(succ, j);
			if (tgt == node) {
				DB((dbg, LEVEL_4,
				    "\t\t\t\t\tRemoving %+F from ins of %+F\n",
				    tgt, succ));
				continue;
			}
			DB((dbg, LEVEL_4,
			    "\t\t\t\t\tKeeping %+F in ins of %+F\n", tgt,
			    succ));
			new_ins[k] = tgt;
			k++;
		}
		set_irn_in(succ, arity - 1, new_ins);
	}
}
static void rewire_memory_of_execess_header(ir_node *const linked_header,
					    ir_node *const in_loop_target)
{
	DB((dbg, LEVEL_4, "\t\t\tRewiring memory of %+F\n", linked_header));
	ir_node *target = NULL;
	for (unsigned i = 0; i < get_irn_n_outs(in_loop_target); ++i) {
		ir_node *out = get_irn_out(in_loop_target, i);
		if (get_block(out) != in_loop_target) {
			continue;
		}
		if (!is_Phi(out)) {
			continue;
		}
		if (get_irn_mode(out) != mode_M) {
			continue;
		}
		target = out;
	}
	DB((dbg, LEVEL_4, "\t\t\t\tMemory target is %+F\n", target));
	for (unsigned i = 0; i < get_irn_n_outs(linked_header); ++i) {
		ir_node *out = get_irn_out(linked_header, i);
		if (get_block(out) != linked_header) {
			continue;
		}
		if (!is_Phi(out)) {
			continue;
		}
		if (get_irn_mode(out) != mode_M) {
			continue;
		}
		remove_node_from_succ_ins(out);
		unsigned arity = get_irn_arity(out);
		DB((dbg, LEVEL_4, "\t\t\t\tMemory source is %+F (arity: %u)\n",
		    out, arity));
		assert(target);
		assert(arity > 0);
		for (unsigned j = 0; j < arity; ++j) {
			DB((dbg, LEVEL_4,
			    "\t\t\t\t\tWiring memory %+F to %+F\n", target,
			    get_irn_n(out, j)));
		}
		set_irn_in(target, arity, get_irn_in(out));
		set_irn_in(out, 0, NULL);
	}
}
static void remove_excess_headers(linear_unroll_info *info,
				  ir_node *const header)
{
	iterate_stack(unrolled_headers)
	{
		ir_node *linked_header = curr->el;
		ir_node *in_loop_target, *out_of_loop_target;
		get_false_and_true_targets(linked_header, &in_loop_target,
					   &out_of_loop_target);
		rewire_memory_of_execess_header(linked_header, in_loop_target);
	}
	iterate_stack(unrolled_headers)
	{
		ir_node *linked_header = curr->el;
		if (linked_header == header) {
			continue;
		}
		prune_block(linked_header, header);
	}
	iterate_stack(unrolled_headers)
	{
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
	confirm_irg_properties(get_irn_irg(header), IR_GRAPH_PROPERTIES_NONE);
}

static void recursive_copy_in_loop(ir_node *node, ir_node *header)
{
	for (unsigned i = 0; i < get_irn_arity(node); ++i) {
		ir_node *to_cpy = get_irn_n(node, i);
		ir_node *to_cpy_block = get_block(to_cpy);
		if (to_cpy_block == header ||
		    block_dominates(to_cpy_block, header) > 0) {
			continue;
		}
		if (get_irn_mode(to_cpy) != mode_M) {
			duplicate_node(to_cpy, header);
			recursive_copy_in_loop(to_cpy, header);
		}
	}
}

static void recursive_rewire_in_loop(ir_node *node, ir_node *header,
				     ir_node *phi_M)
{
	unsigned arity = get_irn_arity(node);
	ir_node **new_in = ALLOCAN(ir_node *, arity);
	for (unsigned i = 0; i < arity; ++i) {
		ir_node *next = get_irn_n(node, i);
		ir_node *next_block = get_block(next);
		if (block_dominates(next_block, header) > 0 ||
		    next_block == header) {
			new_in[i] = next;
			continue;
		}
		if (get_irn_mode(next) == mode_M) {
			new_in[i] = phi_M;
		} else {
			new_in[i] = get_irn_link(next);
			recursive_rewire_in_loop(next, header, phi_M);
		}
	}
	set_irn_in(get_irn_link(node), arity, new_in);
}

static ir_node *create_abs(ir_node *node, ir_node *block)
{
	/*
	 * Compile the following with O3 to get this form:
	 * int abs(int n) { return n < 0 ? -n : n }
	 */
	assert(block_dominates(block, get_block(node)) >= 0);
	ir_mode *mode = get_irn_mode(node);
	if (!mode_is_signed(mode)) {
		return node;
	}
	ir_graph *irg = get_irn_irg(block);
	ir_node *shrs = new_r_Shrs(
		block, node,
		new_r_Const_long(irg, mode_Iu, get_mode_size_bits(mode) - 1));
	ir_node *eor = new_r_Eor(block, shrs, node);
	ir_node *sub = new_r_Sub(block, eor, shrs);
	return sub;
}

enum Side { LEFT, RIGHT };
#define Side enum Side

static ir_node *update_header_condition_add(linear_unroll_info *info,
					    ir_node *header, ir_node *N,
					    ir_node *c_cpy,
					    ir_node *factor_const, bool less)
{
	ir_node *c_abs = create_abs(c_cpy, header);
	DB((dbg, LEVEL_4, "\t(|c|,c) = (%+F,%+F)\n", c_abs, c_cpy));
	ir_node *one_const =
		new_r_Const_long(get_irn_irg(header), get_irn_mode(c_abs), 1);
	ir_node *factor_offset = new_r_Sub(header, factor_const, one_const);
	ir_node *mul = new_r_Mul(header, c_abs, factor_offset);
	ir_node *new_N =
		less ? new_r_Sub(header, N, mul) : new_r_Add(header, N, mul);
	DEBUG_ONLY(char *symb_fac = less ? "+" : "-";
		   char *symb_N = less ? "-" : "+";)
	DB((dbg, LEVEL_4, "\t(|c|) * (factor %s 1): %+F\n", symb_fac, mul));
	DB((dbg, LEVEL_4,
	    "\tAttaching (N %s (|c|* (factor %s 1))): (%+F %s (%+F * %+F)  = %+F %s %+F = %+F",
	    symb_N, symb_fac, N, symb_N, c_abs, factor_offset, N, symb_N, mul,
	    new_N));
	return new_N;
}
static ir_node *create_r_pow(ir_node *block, ir_node *base,
			     unsigned long long exp)
{
	if (exp == 0) {
		return new_r_Const_long(get_irn_irg(block), get_irn_mode(base),
					1);
	}
	if (exp == 1) {
		return base;
	}
	return new_r_Mul(block, base, create_r_pow(block, base, exp - 1));
}
static ir_node *update_header_condition_mul(linear_unroll_info *info,
					    ir_node *header, ir_node *N,
					    ir_node *c_cpy,
					    ir_node *factor_const, bool less)
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
	ir_node *new_N =
		less ? new_r_Mul(header, new_N_div_c, c_cpy) :
		       new_Proj(new_r_DivRL(header,
					    new_r_Pin(header, new_r_NoMem(irg)),
					    new_N_div_c, c_cpy,
					    op_pin_state_pinned),
				mode, pn_Div_res);
	DB((dbg, LEVEL_4,
	    "\tAttaching c * (N / (factor ^ c)): %+F * (%+F / (%+F ^ %+F) = %+F * (%+F / %+F) = %+F",
	    c_cpy, N, factor_const, c_cpy, c_cpy, N, pow, new_N));
	return new_N;
}
static ir_node *copy_and_rewire(ir_node *node, ir_node *target_block,
				ir_node *phi_M)
{
	if (is_Const(node)) {
		return exact_copy(node);
	}
	ir_node *cpy = duplicate_node(node, target_block);
	recursive_copy_in_loop(cpy, target_block);
	recursive_rewire_in_loop(node, target_block, phi_M);
	return cpy;
}
static bool is_less(linear_unroll_info *info)
{
	bool less = info->rel == ir_relation_less ||
		    info->rel == ir_relation_less_equal;
	bool inverted = info->phi == get_Cmp_right(info->cmp);
	return less ^ inverted;
}

static void update_header_condition(linear_unroll_info *info, unsigned factor)
{
	ir_node *cmp = info->cmp;
	ir_node *header = info->header;
	ir_node *left = get_Cmp_left(cmp);
	ir_node *right = get_Cmp_right(cmp);
	DB((dbg, LEVEL_3,
	    "Changing condition and compare %+F (comparing %+F to %+F)\n", cmp,
	    left, right));
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
	ir_node *phi_M = NULL;
	for (unsigned i = 0; i < get_irn_n_outs(header); ++i) {
		ir_node *curr = get_irn_out(header, i);
		if (get_block(curr) == header && is_Phi(curr) &&
		    get_irn_mode(curr) == mode_M) {
			phi_M = curr;
			break;
		}
	}
	assert(phi_M);
	c_cpy = copy_and_rewire(info->incr, header, phi_M);
	DB((dbg, LEVEL_4, "\tcopied c: %+F\n", c_cpy));
	ir_node *factor_const = new_r_Const_long(get_irn_irg(header),
						 get_irn_mode(c_cpy), factor);
	ir_node *new_N;
	bool less = is_less(info);
	switch (info->op) {
	case ADD:
	case SUB:
		new_N = update_header_condition_add(info, header, N, c_cpy,
						    factor_const, less);
		break;
	case MUL:
		new_N = update_header_condition_mul(info, header, N, c_cpy,
						    factor_const, less);
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

static void duplicate_original_loop(ir_loop *const loop, ir_graph *irg)
{
	DB((dbg, LEVEL_4, "Duplicating loop %+F\n", loop));
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	for (size_t i = 0; i < get_loop_n_elements(loop); ++i) {
		loop_element el = get_loop_element(loop, i);
		if (*el.kind != k_ir_node) {
			continue;
		}
		assert(is_Block(el.node));
		DEBUG_ONLY(ir_node *dup =) duplicate_block(el.node);
		DB((dbg, LEVEL_4, "\tCreated %+F\n", dup));
	}
}

static void rewire_ins_linked(ir_node *node)
{
	ir_node *link = get_irn_link(node);
	assert(link);
	DB((dbg, LEVEL_5, "\t\tRewiring link of %+F (%+F)\n", node, link));
	unsigned arity = get_irn_arity(node);
	ir_node **new_ins = ALLOCAN(ir_node *, arity);
	for (unsigned i = 0; i < arity; ++i) {
		ir_node *irn_n = get_irn_n(node, i);
		ir_node *linked_irn_n = get_irn_link(irn_n);
		ir_node *target = linked_irn_n ? linked_irn_n : irn_n;
		DB((dbg, LEVEL_5,
		    "\t\t\tGetting in %+F (link: %+F, original: %+F)\n", target,
		    linked_irn_n, irn_n));
		new_ins[i] = target;
	}
	set_irn_in(link, arity, new_ins);
}

static void rewire_duplicated_block(ir_node *node, ir_loop *loop,
				    ir_node *header, linear_unroll_info *info)
{
	ir_node *const new_node = get_irn_link(node);
	// rewire the successors outside the loop
	DB((dbg, LEVEL_5, "\tRewiring block %+F (link of %+F)\n", new_node,
	    node));
	unsigned const n_outs = get_irn_n_outs(node);
	for (unsigned j = 0; j < n_outs; ++j) {
		int index;
		ir_node *const curr = get_irn_out_ex(node, j, &index);
		DB((dbg, LEVEL_5, "\t\tAssessing %+F\n", curr));
		if (is_Block(curr)) {
			continue;
		} else if (is_End(curr)) {
			DB((dbg, LEVEL_5,
			    "\t\t\tAdding keep alive from %+F to %+F (link of %+F)\n",
			    curr, new_node, node));
			add_End_keepalive(curr, new_node);
			continue;
		}
		rewire_ins_linked(curr);
		ir_node *const curr_link = get_irn_link(curr);
		if (!curr_link) {
			continue;
		}
		for (unsigned k = 0; k < get_irn_n_outs(curr); ++k) {
			int index_out;
			ir_node *const out =
				get_irn_out_ex(curr, k, &index_out);
			if (is_End(out)) {
				DB((dbg, LEVEL_5,
				    "\t\t\tAdding keep alive from %+F to %+F (link of %+F)\n",
				    out, curr_link, curr));
				add_End_keepalive(out, curr_link);
			} else if (!block_is_inside_loop(get_block(out),
							 loop)) {
				if (get_block(node) == header) {
					DB((dbg, LEVEL_5,
					    "\t\t\tRewiring out of loop link starting at %+F to now point to %+F instead of link %+F\n",
					    out, curr_link, curr));
					set_irn_n(out, index_out, curr_link);
				} else {
					unsigned arity = get_irn_arity(out);
					ir_node **ins = get_irn_in(out);
					ir_node **new_ins =
						ALLOCAN(ir_node *, arity + 1);
					for (unsigned i = 0; i < arity; ++i) {
						new_ins[i] = ins[i];
					}
					new_ins[arity] = curr_link;
					set_irn_in(out, arity + 1, new_ins);
					DB((dbg, LEVEL_5,
					    "\t\t\tRewiring out of loop link starting at %+F to now also point to %+F\n",
					    out, curr_link));
				}
			}
		}
	}
	rewire_ins_linked(node);
}
static void rewire_duplicated_header(ir_node *header, ir_loop *loop,
				     linear_unroll_info *info)
{
	rewire_duplicated_block(header, loop, header, info);
	DB((dbg, LEVEL_5, "\t\tNode is header\n"));
	ir_node *linked_header = get_irn_link(header);
	unsigned header_arity = get_irn_arity(header);
	ir_node **header_new_ins = malloc(sizeof(ir_node *) * header_arity);
	ir_node *cond = get_irn_out(info->cmp, 0);
	DB((dbg, LEVEL_5, "\t\t\tRewiring Condition %+F\n", cond));
	for (unsigned j = 0; j < get_irn_n_outs(cond); ++j) {
		ir_node *proj = get_irn_out(cond, j);
		DB((dbg, LEVEL_5, "\t\t\t\t Checking proj attached %+F", proj));
		int index;
		ir_node *target = get_irn_out_ex(proj, 0, &index);
		DB((dbg, LEVEL_5, " that points to %+F\n", target));
		if (!block_is_inside_loop(get_block(target), loop)) {
			ir_node *linked_proj = get_irn_link(proj);
			set_irn_n(target, index, linked_proj);
			DB((dbg, LEVEL_5,
			    "\t\t\t\t\tPost loop %+F in wired to %+F (link of %+F) \n",
			    target, linked_proj, proj));
			header_new_ins[0] = proj;
		}
	}
	unsigned i = 1;
	for (unsigned j = 0; j < header_arity; ++j) {
		ir_node *pre = get_irn_n(header, j);
		ir_node *pre_link = get_irn_link(pre);
		if (pre_link) {
			header_new_ins[i] = pre_link;
			i++;
		}
	}
	DB((dbg, LEVEL_5, "\t\t\t\t\tnew arity of linked header: %u\n", i));
	ir_node **header_new_ins_all = ALLOCAN(ir_node *, i);
	for (int j = 0; j < i; ++j) {
		header_new_ins_all[j] = header_new_ins[j];
	}
	free(header_new_ins);
	for (int j = 0; j < i; ++j) {
		DB((dbg, LEVEL_5, "\t\t\t\t\tLinked header in wired to %+F\n",
		    header_new_ins_all[j]));
	}
	set_irn_in(linked_header, i, header_new_ins_all);
	unsigned const n_outs = get_irn_n_outs(header);
	DB((dbg, LEVEL_5, "\t\t\tRewiring phis\n"));
	for (unsigned j = 0; j < n_outs; ++j) {
		ir_node *const out = get_irn_out(header, j);
		if (is_Phi(out)) {
			ir_node *linked = get_irn_link(out);
			unsigned arity = get_irn_arity(out);
			ir_node **new_ins = malloc(sizeof(ir_node *) * arity);
			bool link_to_out = false;
			unsigned l = 0;
			for (unsigned k = 0; k < arity; ++k) {
				ir_node *curr = get_irn_n(out, k);
				if (block_is_inside_loop(get_block(curr),
							 loop)) {
					DB((dbg, LEVEL_5,
					    "\t\t\t\tRewiring %+F (link of %+F) to keep %+F, link of %+F\n",
					    linked, out, get_irn_link(curr),
					    curr));
					new_ins[l] = get_irn_link(curr);
					l++;
				}
				if (!block_is_inside_loop(get_block(curr),
							  loop) ||
				    get_block(curr) == header) {
					DB((dbg, LEVEL_5,
					    "\t\t\t\tRewiring %+F (link of %+F) to have input %+F instead of %+F\n",
					    linked, out, out,
					    get_irn_n(out, k)));
					link_to_out = true;
				}
			}
			if (link_to_out) {
				new_ins[l] = out;
				l++;
			}
			ir_node **new_ins_all = ALLOCAN(ir_node *, l);
			new_ins_all[0] = new_ins[l - 1];
			for (unsigned m = 0; m < l - 1; ++m) {
				new_ins_all[m + 1] = new_ins[m];
			}
			free(new_ins);
			set_irn_in(linked, l, new_ins_all);
		}
	}
}

static void rewire_duplicated(ir_loop *loop, linear_unroll_info *info)
{
	// Rewire jump into header, jump from last block(s)
	// Rewire header and last block phis
	DB((dbg, LEVEL_4, "Rewiring loop %+F fixup\n", loop));
	ir_node *header = get_loop_header(loop);
	for (size_t i = 0; i < get_loop_n_elements(loop); ++i) {
		loop_element el = get_loop_element(loop, i);
		if (*el.kind != k_ir_node) {
			continue;
		}
		if (el.node == header) {
			continue;
		}
		rewire_duplicated_block(el.node, loop, header, info);
	}
	rewire_duplicated_header(header, loop, info);
}

static void create_fixup_loop(ir_loop *const loop, ir_graph *irg,
			      linear_unroll_info *info)
{
	duplicate_original_loop(loop, irg);
	rewire_duplicated(loop, info);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}
static ir_node *get_phi_M(ir_node *block)
{
	assert(is_Block(block));
	for (unsigned i = 0; i < get_irn_n_outs(block); ++i) {
		ir_node *curr = get_irn_out(block, i);
		if (get_block(curr) == block && is_Phi(curr) &&
		    get_irn_mode(curr) == mode_M) {
			return curr;
		}
	}
	return NULL;
}
static ir_node *create_fixup_switch_header(ir_loop *const loop, ir_graph *irg,
					   unsigned factor,
					   ir_node **target_blocks,
					   ir_node *after_loop,
					   linear_unroll_info *info)
{
	DB((dbg, LEVEL_4, "\tCreating switch fixup header\n"));
	ir_node *header = get_loop_header(loop);
	ir_node **in = ALLOCAN(ir_node *, 1);
	unsigned after_index = 0;
	for (; after_index < get_irn_arity(after_loop); after_index++) {
		ir_node *curr = get_irn_n(after_loop, after_index);
		if (get_block(curr) == header) {
			in[0] = curr;
			break;
		}
	}
	ir_node *switch_header = new_r_Block(irg, 1, in);
	set_irn_n(after_loop, after_index, switch_header);
	ir_node *phi_M = get_phi_M(header);
	assert(phi_M);
	ir_node *c_cpy = copy_and_rewire(info->incr, switch_header, phi_M);
	ir_node *N_cpy = copy_and_rewire(info->bound, switch_header, phi_M);
	ir_mode *phi_mode = get_irn_mode(info->i[0]);
	for (unsigned i = 1; i < info->i_size; i++) {
		ir_mode *m = get_irn_mode(info->i[i]);
		if (larger_mode(m, phi_mode)) {
			phi_mode = m;
		}
	}

	bool less = is_less(info);
	ir_node **tmp_ins = ALLOCAN(ir_node *, get_irn_arity(header));

	for (unsigned i = 0; i < get_irn_arity(header); i++) {
		ir_node *target = get_irn_n(info->phi, i);
		tmp_ins[i] = get_irn_n(info->phi, i);
		if (block_is_inside_loop(get_block(target), loop)) {
			tmp_ins[i] = new_r_Bad(irg, get_irn_mode(target));
		}
	}

	ir_node *I_cpy =
		new_r_Phi(header, get_irn_arity(info->phi), tmp_ins, phi_mode);

	ir_node **new_phi_ins = ALLOCAN(ir_node *, get_irn_arity(header));

	for (unsigned i = 0; i < get_irn_arity(header); i++) {
		ir_node *target = get_irn_n(info->phi, i);
		new_phi_ins[i] = target;
		if (block_is_inside_loop(get_block(target), loop)) {
			new_phi_ins[i] = I_cpy;
		}
	}

	set_irn_in(I_cpy, get_irn_arity(I_cpy), new_phi_ins);
	add_to_stack(I_cpy, &fixup_phis);
	ir_node *c_abs = create_abs(c_cpy, switch_header);
	ir_node *one_const = new_r_Const_long(irg, get_irn_mode(c_abs), 1);
	ir_node *N_minus_I = create_abs(new_r_Sub(switch_header, I_cpy, N_cpy),
					switch_header);
	if (info->rel == ir_relation_less_equal) {
		N_minus_I = new_r_Add(switch_header, N_minus_I, one_const);
	} else if (info->rel == ir_relation_greater_equal) {
		N_minus_I = new_r_Sub(switch_header, N_minus_I, one_const);
	}
	DB((dbg, LEVEL_4, "\t\tCreated %+F = |(N - I)| = |%+F - %+F|\n",
	    N_minus_I, N_cpy, I_cpy));
	ir_node *c_one = new_r_Sub(switch_header, c_abs, one_const);
	ir_node *numerator = new_r_Add(switch_header, N_minus_I, c_one);
	DB((dbg, LEVEL_4, "\t\tCreated %+F = (N - I) + (|c| - 1) = %+F + %+F\n",
	    numerator, N_minus_I, c_one));
	ir_node *pin = new_r_Pin(switch_header, new_r_NoMem(irg));
	ir_node *denominator = less ? c_abs : new_r_Minus(switch_header, c_abs);
	ir_node *div = new_r_DivRL(switch_header, pin, numerator, denominator,
				   op_pin_state_pinned);
	ir_node *div_proj =
		new_r_Proj(div, get_irn_mode(numerator), pn_Div_res);
	DB((dbg, LEVEL_4, "\t\tCreated %+F -> %+F\n", div_proj, div));
	ir_node *duff_factor_const =
		new_r_Const_long(irg, get_irn_mode(div_proj), factor);
	ir_node *mod = new_r_Mod(switch_header, pin, div_proj,
				 duff_factor_const, op_pin_state_pinned);
	ir_node *mod_Proj = new_r_Proj(mod, get_irn_mode(div_proj), pn_Mod_res);
	DB((dbg, LEVEL_4, "\t\tCreated Proj %+F to %+F = %+F %s %+F\n",
	    mod_Proj, mod, div_proj, "%", duff_factor_const));
	ir_switch_table *switch_table = ir_new_switch_table(irg, factor);
	for (unsigned i = 0; i < factor - 1; ++i) {
		ir_tarval *tv = new_tarval_from_long(factor - 1 - i,
						     get_irn_mode(div_proj));
		ir_switch_table_set(switch_table, i, tv, tv, i + 1);
	}
	ir_switch_table_set(switch_table, factor - 1, NULL, NULL,
			    pn_Switch_default);
	ir_node *switch_mod =
		new_r_Switch(switch_header, mod_Proj, factor, switch_table);
	DB((dbg, LEVEL_4, "\t\tCreated %+F that switches over %+F -> %+F\n",
	    switch_mod, mod_Proj, mod));
	ir_node **ins_first = ALLOCAN(ir_node *, 1);
	ins_first[0] = new_r_Proj(switch_mod, mode_X, 1);
	set_irn_in(target_blocks[0], 1, ins_first);
	DB((dbg, LEVEL_4, "\t\tSetting in of %+F to %+F\n", target_blocks[0],
	    ins_first[0]));
	// TODO: I side effect  => loop
	for (unsigned i = 1; i < factor - 1; ++i) {
		ir_node **ins = ALLOCAN(ir_node *, 2);
		ins[0] = new_r_Proj(switch_mod, mode_X, i + 1);
		ins[1] = new_r_Jmp(target_blocks[i - 1]);
		set_irn_in(target_blocks[i], 2, ins);
		DB((dbg, LEVEL_4, "\t\tSetting in of %+F to %+F and %+F\n",
		    target_blocks[i], ins[0], ins[1]));
	}
	ir_node **ins = ALLOCAN(ir_node *, 2);
	ins[0] = new_r_Proj(switch_mod, mode_X, pn_Switch_default);
	ins[1] = new_r_Jmp(target_blocks[factor - 1 - 1]);
	set_irn_in(after_loop, 2, ins);
	DB((dbg, LEVEL_4, "\t\tSetting in of %+F to %+F and %+F\n", after_loop,
	    ins[0], ins[1]));
	return switch_header;
}
static ir_node *find_out_block_exit_(ir_node *node, ir_node *block,
				     ir_mode *mode)
{
	assert(mode != mode_T);
	DB((dbg, LEVEL_5, "\tQuerying %+F\n", node));

	if (get_irn_mode(node) != mode && get_irn_mode(node) != mode_T) {
		return NULL;
	}
	if (get_irn_n_outs(node) == 0 && get_irn_mode(node) == mode) {
		DB((dbg, LEVEL_5, "\t\tFound %+F: node with no exits", node));
		return node;
	}
	bool any_same_mode = false;
	unsigned n_outs = get_irn_n_outs(node);
	for (int i = 0; i < n_outs; ++i) {
		DB((dbg, LEVEL_5, "\t\tNode has child: %+F\n",
		    get_irn_out(node, i)));
	}
	for (int i = 0; i < n_outs; ++i) {
		ir_node *curr = get_irn_out(node, i);
		DB((dbg, LEVEL_5, "\t\tQuerying out %+F\n", curr));

		if (get_irn_mode(curr) == mode) {
			any_same_mode = true;
		}
		if (get_block(curr) != block && get_irn_mode(curr) == mode) {
			DB((dbg, LEVEL_5, "\tFound %+F: Out of block node\n",
			    node));
			return node;
		}
		ir_node *find = find_out_block_exit_(curr, block, mode);
		if (find) {
			DB((dbg, LEVEL_5, "\t\tReturning %+F\n", find));
			return find;
		}
	}
	if (!any_same_mode) {
		DB((dbg, LEVEL_5,
		    "\t\tFound %+F: none of the %u exits were the right mode\n",
		    node, n_outs));
		return node;
	}
	DB((dbg, LEVEL_5, "\t\tNothing found", node));
	return NULL;
}
#define find_block_exit(node)                                                  \
	find_out_block_exit_(node, get_block(node), get_irn_mode(node))        \
		DEBUG_ONLY(; DB((dbg, LEVEL_5, "Looking for exit of %+F\n",    \
				 node)))

static ir_node *find_in_header_phi(ir_node *node, ir_node *header)
{
	assert(node);
	assert(!is_Block(node));
	for (unsigned k = 0; k < get_irn_arity(node); k++) {
		ir_node *curr = get_irn_n(node, k);
		if (get_block(curr) == header && is_Phi(curr)) {
			return curr;
		}
	}
	return NULL;
}
static ir_node *find_into_loop_phi(ir_node *header_phi, ir_loop *loop)
{
	assert(header_phi);
	for (unsigned k = 0; k < get_irn_arity(header_phi); k++) {
		ir_node *phi_in = get_irn_n(header_phi, k);
		DB((dbg, LEVEL_4, "\t\t\t\tHeader phi %+F points to %+F\n",
		    header_phi, phi_in));
		if (block_is_inside_loop(get_block(phi_in), loop)) {
			DB((dbg, LEVEL_4, "\t\t\t\tLast in: Link of %+F\n",
			    phi_in));
			return phi_in;
		}
	}
	return NULL;
}

static ir_node *get_link_in(ir_node *link, ir_node *block)
{
	assert(is_Block(block));
	assert(!is_Block(link));
	clear_irg_properties(get_irn_irg(link),
			     IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	assure_irg_properties(get_irn_irg(link),
			      IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	DB((dbg, LEVEL_5, "Looking for node linked to %+F in %+F\n", link,
	    block));
	for (unsigned i = 0; i < get_irn_n_outs(block); i++) {
		ir_node *curr = get_irn_out(block, i);
		ir_node *curr_link = get_irn_link(curr);
		DB((dbg, LEVEL_5, "\tChecking %+F with link to %+F\n", curr,
		    curr_link));
		if (get_block(curr) != block) {
			continue;
		}
		if (curr_link == link) {
			return curr;
		}
	}
	return NULL;
}

struct irn_stack *kas = NULL;
static void rewire_link(ir_node *node, ir_loop *loop, ir_node *header,
			ir_node *prev, ir_node *after_loop, bool last)
{
	// If points into loop create phi
	// If points into header link to switch if
	if (is_Jmp(node)) {
		return;
	}
	ir_node *block = get_block(node);
	assert(block != header);
	ir_node *link = get_irn_link(node);
	add_End_keepalive(get_irg_end(get_irn_irg(node)), link);
	add_to_stack(link, &kas);
	ir_node *link_block = get_block(link);
	assert(link);
	unsigned arity = get_irn_arity(link);
	unsigned new_arity = arity;
	ir_node **new_ins = ALLOCAN(ir_node *, arity);
	for (unsigned j = 0, i = 0; j < arity; j++, i++) {
		ir_node *in = get_irn_n(link, j);
		ir_node *in_block = get_block(in);
		ir_node *in_link = get_irn_link(in);
		if (!block_is_inside_loop(in_block, loop)) {
			assert(in);
			new_ins[i] = in;
		} else if (in_block != block && prev) {
			ir_node **phi_ins;
			unsigned phi_arity = 2;
			ir_node *same_link_in_prev = get_link_in(link, prev);
			assert(same_link_in_prev);
			ir_node *prev_block_usage =
				find_block_exit(same_link_in_prev);
			assert(prev_block_usage);
			if (is_Phi(link)) {
				new_arity++;
				ir_node **new_ins_larger =
					ALLOCAN(ir_node *, new_arity);
				for (unsigned k = 0; k < j; ++k) {
					new_ins_larger[k] = new_ins[j];
				}
				new_ins = new_ins_larger;
				new_ins[i] = in;
				new_ins[i + 1] = prev_block_usage;
				DB((dbg, LEVEL_4,
				    "\t\t\t\t\t%+F pointing to %+F and %+F\n",
				    node, new_ins[i], new_ins[i + 1]));
				i++;
			} else {
				phi_ins = ALLOCAN(ir_node *, phi_arity);
				phi_ins[0] = in;

				phi_ins[1] = prev_block_usage;
				ir_node *phi =
					new_r_Phi(link_block, phi_arity,
						  phi_ins, get_irn_mode(in));
				DB((dbg, LEVEL_4, "\t\t\t\tCreating %+F\n",
				    phi));
				DEBUG_ONLY(for (unsigned i = 0; i < phi_arity;
						++i) {
					DB((dbg, LEVEL_4,
					    "\t\t\t\t\t%+F pointing to %+F\n",
					    phi, phi_ins[i]));
				})

				new_ins[i] = phi;
			}
		} else if (in_block != block) {
			new_ins[i] = in;
		} else {
			assert(in_link);
			new_ins[i] = in_link;
		}
		if (last && in_block == header) {
			for (unsigned k = 0; k < get_irn_n_outs(in); k++) {
				ir_node *out = get_irn_out(in, k);
				if (is_End(out)) {
					continue;
				}
				if (get_block(out) == after_loop) {
					DB((dbg, LEVEL_4,
					    "\t\t\tRewiring post loop block %+F\n",
					    out));
					unsigned out_arity = get_irn_arity(out);
					ir_node **new_ins_out = ALLOCAN(
						ir_node *, out_arity + 1);
					for (unsigned k = 0; k < arity; k++) {
						ir_node *new_in =
							get_irn_n(out, k);
						new_ins_out[k] = new_in;
					}
					new_ins_out[out_arity] =
						get_irn_link(find_into_loop_phi(
							find_in_header_phi(
								out, header),
							loop));

					for (unsigned k = 0; k < arity + 1;
					     k++) {
						DB((dbg, LEVEL_4,
						    "\t\t\t\t%+F pointing to %+F\n",
						    out, new_ins_out[k]));
					}
					set_irn_in(out, out_arity + 1,
						   new_ins_out);
				}
			}
		}
		DB((dbg, LEVEL_4, "\t\t\t%+F pointing to %+F\n", link,
		    new_ins[j]));
	}
	set_irn_in(link, new_arity, new_ins);
}
static ir_node *duplicate_rewire_loop_body(ir_loop *const loop, ir_node *header,
					   ir_graph *irg, ir_node *after_loop,
					   ir_node *prev, bool last)
{
	struct irn_stack *copied = NULL;
	ir_node *first = NULL;
	for (size_t i = 0; i < get_loop_n_elements(loop); i++) {
		loop_element el = get_loop_element(loop, i);
		if (*el.kind != k_ir_node)
			continue;
		ir_node *block = el.node;
		if (block == header)
			continue;
		ir_node *curr = duplicate_block(block);
		if (!first) {
			first = curr;
		}
		add_to_stack(block, &copied);
	}
	iterate_stack(copied)
	{
		ir_node *block = curr->el;
		DB((dbg, LEVEL_4, "\tRewiring %+F (with pred %+F)\n", block,
		    prev));
		for (unsigned i = 0; i < get_irn_n_outs(block); ++i) {
			ir_node *node = get_irn_out(block, i);
			if (is_Block(node))
				continue;
			DB((dbg, LEVEL_4, "\t\tRewiring %+F in %+F\n",
			    get_irn_link(node), get_irn_link(block)));
			rewire_link(node, loop, header, prev, after_loop, last);
		}
	}
	free_stack(&copied);
	assert(first);
	return first;
}
static void create_fixup_switch(ir_loop *const loop, ir_graph *irg,
				unsigned factor, linear_unroll_info *info)
{
	fixup_phis = NULL;
	DB((dbg, LEVEL_4, "Creating switch-case fixup\n"));
	ir_node *header = get_loop_header(loop);
	ir_node *in_loop_target, *out_of_loop_target;
	get_false_and_true_targets(header, &in_loop_target,
				   &out_of_loop_target);
	ir_node **dups = malloc(sizeof(ir_node *) * factor);
	DB((dbg, LEVEL_4, "Duplicating blocks for switch-case fixup\n"));
	kas = NULL;
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	for (unsigned i = 0; i < factor - 1; ++i) {
		dups[i] = duplicate_rewire_loop_body(
			loop, header, irg, out_of_loop_target,
			i == 0 ? NULL : dups[i - 1], i == factor - 2);
	}
	DEBUG_ONLY(dump_ir_graph(irg, "duff-fixup-pre-switch-header-1"));

	ir_node *end = get_irg_end(irg);
	iterate_stack(kas)
	{
		remove_End_keepalive(end, curr->el);
	}
	free_stack(&kas);
	// Cleared when removing KAs
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-fixup-pre-switch-header-2"));
	create_fixup_switch_header(loop, irg, factor, dups, out_of_loop_target,
				   info);
	free(dups);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}

static void unroll_loop_duff(ir_loop *const loop, unsigned factor,
			     linear_unroll_info *info)
{
	DB((dbg, LEVEL_3, "\tTrying to unroll %N\n", loop));
	ir_node *const header = get_loop_header(loop);
	if (!header)
		return;
	info->header = header;
	unrolled_headers = NULL;
	unrolled_nodes = NULL;
	ir_graph *irg = get_irn_irg(header);
	create_fixup_loop(loop, irg, info);
	//create_fixup_switch(loop, irg, factor, info);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-fixup-pre-fix-graph"));
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	assure_irg_properties(
		irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
			     IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
			     IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE |
			     IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
			     IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES |
			     IR_GRAPH_PROPERTY_NO_BADS);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-fixup"));
	assure_lcssa(irg);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	assure_irg_properties(
		irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
			     IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
			     IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE |
			     IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
			     IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES |
			     IR_GRAPH_PROPERTY_NO_BADS);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-fixup-lcssa"));
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	rewire_loop(loop, header, factor);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	assure_irg_properties(
		irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
			     IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
			     IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE |
			     IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
			     IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	assert(unrolled_headers);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-unroll"));
	remove_excess_headers(info, header);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-no-excess-header-pre-fix-graph"));
	assure_irg_properties(irg,
			      IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO |
				      IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUTS |
				      IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES |
				      IR_GRAPH_PROPERTY_NO_BADS);
	info->loop = get_irn_loop(header);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-no-excess-header"));
	update_header_condition(info, factor);
	DEBUG_ONLY(dump_ir_graph(irg, "duff-updated-header-condition"));
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
static int load_duff_factor()
{
	char *factor = getenv("DUFF_FACTOR");
	if (!factor) {
		return 0;
	}
	return atoi(factor);
}
#define DUFF_DEFAULT_FACTOR 4
#define DUFF_FACTOR                                                            \
	load_duff_factor() ? load_duff_factor() : DUFF_DEFAULT_FACTOR
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
	if (!innermost) {
		DB((dbg, LEVEL_2, "DUFF: %+F not innermost\n", loop));
		return;
	}
	ir_node *header = get_loop_header(loop);
	if (!header) {
		DB((dbg, LEVEL_2,
		    "DUFF: Cannot unroll! (Missing header) (loop: %+F)\n",
		    loop));
		return;
	}
	ir_loop *curr_loop = get_irn_loop(header);
	linear_unroll_info info;
	unsigned depth = get_loop_depth(curr_loop);
	DB((dbg, LEVEL_2, "DUFF: Checking if %+F (depth: %u) is unrollable\n",
	    loop, depth));
	if (depth == 0) {
		DB((dbg, LEVEL_2, "Skipping loop with depth 0\n"));
		return;
	}
	for (unsigned i = 0; i < get_loop_n_elements(curr_loop); ++i) {
		DB((dbg, LEVEL_3, "\tContaining: %+F\n",
		    get_loop_element(loop, i)));
	}
	DB((dbg, LEVEL_3, "-------------\n", loop));
	if (determine_lin_unroll_info(&info, curr_loop)) {
		DB((dbg, LEVEL_2, "DUFF: Can unroll! (loop: %+F)\n", loop));
		unroll_loop_duff(curr_loop, DUFF_FACTOR, &info);
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
