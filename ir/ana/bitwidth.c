/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Bitwidth analyses of a graph
 * @author  Marcel Hollerbach
 */
#include "bitwidth.h"

#include "debug.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "iropt.h"
#include "irhooks.h"
#include "ircons.h"
#include "callgraph.h"
#include "cgana.h"
#include "nodes.h"
#include "pqueue.h"
#include "util.h"
#include "irouts.h"
#include "scalar_replace.h"
#include "math.h"
#include "irconsconfirm.h"
#include "iroptimize.h"
#include "irgraph_t.h"
#include "pset_new.h"

#include <assert.h>

typedef struct{
	bitwidth bitwidth;
	unsigned int true_counter;
	unsigned int false_counter;
} Cmp_Custom_Information;


#define MAXIMUM_NUMBER(op) (pow(2, bitwidth_used_bits(op)))

static void evalulate_node(ir_node *node, pqueue_t *queue);

unsigned long
bitwidth_upper_bound(const ir_node *const node)
{
	ir_mode *mode;

	if (is_Const(node)) {
		return get_Const_long(node);
	} else {
		mode = get_irn_mode(node);
		return ((unsigned long)1 << (unsigned long)(bitwidth_used_bits(node) - (mode_is_signed(mode) ? 1 : 0))) - 1;
	}
}

unsigned int
bitwidth_used_bits(const ir_node *const node)
{
	bitwidth *b = bitwidth_fetch_bitwidth(node);
	ir_mode *mode = get_irn_mode(node);

	if (b) {
		return get_mode_size_bits(mode) - b->stable_digits;
	} else {
		return get_mode_size_bits(mode);
	}
}


bitwidth*
bitwidth_fetch_bitwidth(const ir_node *const node)
{
	ir_graph *g = get_irn_irg(node);

	if (!g->bitwidth.infos.data) {
		return NULL;
	} else {
		return ir_nodemap_get(bitwidth, &g->bitwidth.infos, node);
	}
}

static bool
is_meaningfull(ir_node *n)
{
	ir_mode *mode = get_irn_mode(n);

	if (!mode_is_int(mode))
		return false;

	return true;
}

static void
create_node(ir_node *node, void *data)
{
	(void)data;
	ir_graph *g = get_irn_irg(node);
	ir_mode *mode = get_irn_mode(node);
	bitwidth *info;

	if (is_Cmp(node)) {
		info = (bitwidth*)XMALLOC(Cmp_Custom_Information);
		Cmp_Custom_Information *i = (Cmp_Custom_Information*) info;
		i->true_counter = 0;
		i->false_counter = 0;
	} else {
		info = XMALLOC(bitwidth);
	}

	info->valid = is_meaningfull(node);
	info->stable_digits = 0;
	//if the mode is valid, try to compute the best stable digit number you can think of
	if (info->valid){
		switch (get_irn_opcode(node)) {
			case iro_Const: {
				//const can be calculated directly
				long value = get_Const_long(node);
				unsigned word_length = get_mode_size_bits(mode);
				unsigned required_bits;

				if (value > 0)
				  required_bits = log2_floor(value) + 1;
				else if (value < 0)
					required_bits = log2_floor(-value) + 1;
				else
					required_bits = 1;

				//how many bits are required to represent the value
				assert(required_bits <= word_length);
				info->stable_digits = word_length - required_bits;

				//setting the not negative bit
				if (value < 0)
					info->is_positive = false;
				else
					info->is_positive = true;

				break;
			}
			case iro_Builtin: {
				info->stable_digits = 0;
				info->is_positive = false;
				//FIXME this can be made better depending on the buildin type.
				//however, for now this is not implemented
			}
			case iro_Member:
			case iro_Sel:
			case iro_Proj:
			case iro_Address: {
				info->stable_digits = 0;
				info->is_positive = false;
				break;
			}
			case iro_Size: {
				ir_type *t = get_Size_type(node);
				info->stable_digits = get_type_size(t);
				info->is_positive = false;
				break;
			}
			default: {
				info->stable_digits = get_mode_size_bits(mode);
				info->is_positive = true;
			}
		}
	}

	ir_nodemap_insert(&g->bitwidth.infos, node, info);
}

static void
add_node(ir_node *node, void *data)
{
	pqueue_t *queue = data;

	pqueue_put(queue, node, 0);
}

static void
refit_children(ir_node *node, pqueue_t *queue)
{
	foreach_out_edge(node, edge) {
		ir_node *successor = get_edge_src_irn(edge);
		pqueue_put(queue, successor, 0);
	}
}

static long
generate_min_abs_value(ir_node *n)
{
	if (get_irn_opcode_(n) == iro_Const) {
		return get_Const_long(n);
	} else {
		return 0;
	}
}

static unsigned
compute_bitwidth_relation(bitwidth *value, bitwidth *bound, ir_relation relation)
{
	switch(relation) {
		case ir_relation_less_equal:
		case ir_relation_less:
			return MAX(value->stable_digits, bound->stable_digits);
		case ir_relation_equal:
			return bound->stable_digits;
		case ir_relation_greater:
		case ir_relation_greater_equal:
		case ir_relation_false:
			return 0;
		default:
			return 0;
	}
}

typedef enum {
   SMALLER = 0,
   EQUAL = 1,
   BIGGER = 2
} Compare_result;

static Compare_result
cmp_bitwidth(bitwidth *a, bitwidth *b)
{
	if (a->stable_digits < b->stable_digits)
		return SMALLER;
	if (a->stable_digits > b->stable_digits)
		return BIGGER;

	if (a->is_positive == b->is_positive) {
		return EQUAL;
	} else if (a->is_positive) {
		return BIGGER;
	} else { //if (!a->not_negative)
		return SMALLER;
	}

}

static bool
op_one_unstable(ir_node *n, ir_node **unstable)
{
	*unstable = NULL;
	//iterate over all children
	for (int i = 0; i < get_irn_arity(n); ++i) {
		ir_node *in = get_irn_n(n, i);
		//if not const it is unstable
		if (!is_Const(in)) {
			if (!*unstable) {
			  *unstable = in;
			} else {
				//error out, we only allow one unstable node
				*unstable = NULL;
				return false;
			}
		}
	}

	return true;
}

static void
handle_true_blocks(ir_node *cmp, ir_node *op, pqueue_t *queue)
{
	ir_node *original_block;
	ir_nodemap map, hitmap;

	ir_nodemap_init(&map, get_irn_irg(cmp));
	ir_nodemap_init(&hitmap, get_irn_irg(cmp));

	original_block = get_block(cmp);

	//collect the blocks that are positive
	foreach_out_edge(cmp, to_cond) {
		//these are cond nodes
		ir_node *const cond = get_edge_src_irn(to_cond);

		if (!is_Cond(cond)) continue;

		foreach_out_edge(cond, to_proj) {
			ir_node *const proj = get_edge_src_irn(to_proj);

			assert(is_Proj(proj));

			//find the true proj node of the cmp construct
			if (get_Proj_num(proj) == 1 ) {

				ir_node *goal = get_irn_out(proj, 0);

				assert(is_Block(goal));

				if (block_dominates(original_block, goal)) {
					ir_nodemap_insert(&map, goal, ((void*)(intptr_t)true));
				}
			}
		}
	}
	//walk up from the not const node
	while(op && get_block(op) == original_block) {
		ir_node *tmp;

		if (ir_nodemap_get(ir_node, &hitmap, op)) break;

		ir_nodemap_insert(&hitmap, op, ((void*)(intptr_t)true));

		//check if someone depended on this
		foreach_out_edge_safe(op, edge) {
			ir_node *const succ = get_edge_src_irn(edge);
			int pos = get_edge_src_pos(edge);
			ir_node *const blk = get_block(succ);

			if (blk != original_block && ir_nodemap_get(ir_node, &map, blk)) { //FIXME check that get_block(succ) is in block
				//bitwidth *op_b = bitwidth_fetch_bitwidth(op);
				ir_node *konst, *confirm;

				konst = new_r_Const_long(get_irn_irg(cmp), get_irn_mode(op), bitwidth_upper_bound(op));
				create_node(konst, NULL);
				confirm = new_r_Confirm(get_block(cmp), op, konst, ir_relation_less_equal);
				create_node(confirm, NULL);
				evalulate_node(confirm, queue);
				set_irn_n(succ, pos, confirm);

				//printf("%d-%s <-> %d-%s (%d)\n", op->node_idx, op->op->name, succ->node_idx, succ->op->name, get_block(succ)->node_idx);
			}
		}

		//now skip up one row in the hirachy
		if (op_one_unstable(op, &tmp)) {
			op = tmp;
		} else {
			op = NULL;
		}

		//ensure that we only walk up to integer modes, a proj could be ahead
		if (op && !mode_is_int(get_irn_mode(op)))
			op = NULL;
	}
}

static void
bitwidth_handle_cmp(ir_node *node, pqueue_t *queue)
{
	Cmp_Custom_Information *custom;
	ir_node *op, *obj_a, *obj_b;
	ir_relation required_relation, rel;
	long value = 0;

	rel = get_Cmp_relation(node);
	custom = (Cmp_Custom_Information*)bitwidth_fetch_bitwidth(node);
	obj_a = get_Cmp_left(node);
	obj_b = get_Cmp_right(node);

	//we only do that for modes we understand
	if (!mode_is_int(get_irn_mode(obj_a)) || !mode_is_int(get_irn_mode(obj_b)))
		return;

	if (is_Const(obj_a)) {
		required_relation = ir_relation_greater;
		value = get_Const_long(obj_a);
		op = obj_b;
	} else if (is_Const(obj_b)) {
		required_relation = ir_relation_less;
		value = get_Const_long(obj_b);
		op = obj_a;
	} else {
		required_relation = 0;
	}

	if (required_relation && rel & required_relation) {
		//we have what we want
		if (MAXIMUM_NUMBER(op) <= value) {
			custom->true_counter ++;
		} else {
			custom->false_counter ++;
		}

		//printf("%d %d %i\n", custom->false_counter, custom->true_counter, (int)MAXIMUM_NUMBER(op));

		if (custom->false_counter == 1 && custom->true_counter > 0) {
			//we hit the false part the first time.
			//this means we should insert confirm nodes.
			handle_true_blocks(node, op, queue);
		}
	}
}

static void
evalulate_node(ir_node *node, pqueue_t *queue)
{
	bitwidth *info = bitwidth_fetch_bitwidth(node);
	ir_mode *mode = get_irn_mode(node);
	ir_node *obj_a, *obj_b;

	if (is_Cmp(node)) {
		bitwidth_handle_cmp(node, queue);
	}

	if (!info->valid) return;

	bitwidth new = *info;
	switch(get_irn_opcode(node)) {
		case iro_Add: {
			// stable digits are defininig some max value of this data word,
			// both max values added and transformed back to stable digits
			obj_a = get_Add_left(node);
			obj_b = get_Add_right(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a), *b = bitwidth_fetch_bitwidth(obj_b);
			int bigger_bitwidth = MIN(a->stable_digits, b->stable_digits);
			new.stable_digits = MAX((int)bigger_bitwidth - 1, 0);
			if (a->is_positive && b->is_positive && new.stable_digits != 0)
				new.is_positive = true;
			else
				new.is_positive = false;
			break;
		}
		case iro_Sub: {
			//we invert the right node - bitwidth stays the same
			//we do + 1 - bitwidth gets one werse
			//we do a + *from-before* - bitwidth gets one worse
			obj_a = get_Sub_left(node);
			obj_b = get_Sub_right(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a), *b = bitwidth_fetch_bitwidth(obj_b);
			int bigger_bitwidth = MIN(a->stable_digits, b->stable_digits);
			new.stable_digits = MAX((int)bigger_bitwidth - 1, 0);
			new.is_positive = false;
			break;
		}
		case iro_Minus: {
			//we invert the right node - bitwidth stays the same
			//we do + 1 - bitwidth gets one werse
			obj_a = get_Minus_op(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a);
			new.stable_digits = MAX((int)a->stable_digits - 1, 0);
			new.is_positive = false;
			break;
		}
		// after those nodes the number of stable digits is the smallest number of stable digits from the input nodes
		case iro_Mux:
		case iro_Phi:
		case iro_And:
		case iro_Eor:
		case iro_Or: {
			ir_node **ins = get_irn_in(node);
			unsigned min_of_nodes = INT_MAX;
			bool is_positive = true;
			for (int i = 0; i < get_irn_arity(node); ++i) {
				ir_node *c = ins[i];
				bitwidth *bc = bitwidth_fetch_bitwidth(c);

				min_of_nodes = MIN(min_of_nodes, bc->stable_digits);
				is_positive &= bc->is_positive;
			}
			new.stable_digits = min_of_nodes;
			new.is_positive = is_positive;
			break;
		}
		case iro_Conv: {
			ir_node *op = get_Conv_op(node);
			bitwidth *op_bitwidth = bitwidth_fetch_bitwidth(op);
			ir_mode *old_mode = get_irn_mode(op);

			int delta = (int)get_mode_size_bits(mode) - (int)get_mode_size_bits(old_mode);

			new.stable_digits = MAX((int)op_bitwidth->stable_digits + (int)delta, 0);

			if (mode_is_signed(old_mode) && !op_bitwidth->is_positive && !mode_is_signed(mode))
			  {
				// we cannot really track anything here anymore,
				// negatives are doing rollover, so we can reach
				// the full bandwith of numbers now
				new.stable_digits = 0;
			  }

			if (op_bitwidth->is_positive && new.stable_digits > 0)
			  new.is_positive = true;
			else
				new.is_positive = false;
			break;
		}
		case iro_Mod: {//TODO: Code seems wrong, test it
			obj_a = get_Mod_right(node);
			obj_b = get_Mod_right(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a);//, *b = bitwidth_fetch_bitwidth(obj_b);
			unsigned max_val = bitwidth_upper_bound(obj_b);

			new.stable_digits = log2_floor(max_val) + 1;
			new.is_positive = a->is_positive;
			break;
		}
		case iro_Shl: {
			//shift left makes the number of stable digits lower by the amount of min of the right node
			ir_node *obj_a = get_Shl_left(node) , *obj_b = get_Shl_right(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a);
			long max_val = bitwidth_upper_bound(obj_b);

			if (get_irn_opcode(obj_b) == iro_Const) {
				new.stable_digits = a->stable_digits - max_val;
			} else {
				//if we dont know how much we shift, we should take the worst case, which is 0
				new.stable_digits = 0;
			}

			if (new.stable_digits > 0)
				new.is_positive = a->is_positive;
			else
				new.is_positive = false;
			break;
		}
		case iro_Not: {
			obj_a = get_Not_op(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a);
			new.stable_digits = a->stable_digits;
			new.is_positive = !a->is_positive;
			break;
		}
		case iro_Div: { //WC: X / -1
			ir_mode *mode_right;
			obj_a = get_Div_left(node);
			obj_b = get_Div_right(node);
			mode_right = get_irn_mode(obj_b);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a), *b = bitwidth_fetch_bitwidth(obj_b);
			if (!mode_is_signed(mode_right))
				new.stable_digits = a->stable_digits;
			else
				new.stable_digits = MAX((int)a->stable_digits - 1, 0);

			if (a->is_positive && b->is_positive && new.stable_digits > 0)
				new.is_positive = true;
			else
				new.is_positive = false;
			break;
		}
		case iro_Shr: { // the worst case is the maximum number
			obj_a = get_Shr_left(node);
			obj_b = get_Shr_right(node);
			ir_mode *mode_a = get_irn_mode(obj_a);

			if (mode_is_signed(mode_a)) {
				//Worst case, we shift a value < 0 one value to the right, then we are at 1.
				new.stable_digits = 1;
			} else {
				bitwidth *a = bitwidth_fetch_bitwidth(obj_a);
				long min_val = generate_min_abs_value(obj_b);
				//Worst case, we shift right by the amount from b
				new.stable_digits = MAX((int)a->stable_digits + min_val, 0);
			}
			new.is_positive = true;
			break;
		}
		case iro_Shrs: {
			obj_a = get_Shrs_left(node);
			obj_b = get_Shrs_right(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a);
			long min_val = generate_min_abs_value(obj_b);

			new.stable_digits = MAX((int)a->stable_digits + min_val, 0);
			new.is_positive = a->is_positive;
			break;
		}
		case iro_Mul: {
			ir_mode *mode = get_irn_mode(node);
			obj_a = get_Mul_left(node);
			obj_b = get_Mul_right(node);
			long used_a = bitwidth_used_bits(obj_a);
			long used_b = bitwidth_used_bits(obj_b);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a), *b = bitwidth_fetch_bitwidth(obj_b);

			new.stable_digits = MAX((int)get_mode_size_bits(mode) - (used_b + used_a), 0);
			if (a->is_positive && b->is_positive && new.stable_digits > 0)
				new.is_positive = true;
			else
				new.is_positive = false;
			break;
		}
		case iro_Mulh: {
			ir_mode *mode = get_irn_mode(node);
			obj_a = get_Mulh_left(node);
			obj_b = get_Mulh_right(node);
			long used_a = bitwidth_used_bits(obj_a);
			long used_b = bitwidth_used_bits(obj_b);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a), *b = bitwidth_fetch_bitwidth(obj_b);

			new.stable_digits = MAX((used_b + used_a) - (int)get_mode_size_bits(mode), 0);
			/* FIXME THIS COULD BE WEIRD */
			if (a->is_positive && b->is_positive && new.stable_digits > 0)
				new.is_positive = true;
			else
				new.is_positive = false;
			break;
		}
		case iro_Confirm: {
			ir_node *obj_value = get_Confirm_value(node);
			ir_node *obj_bound = get_Confirm_bound(node);
			bitwidth *value = bitwidth_fetch_bitwidth(obj_value);
			bitwidth *bound = bitwidth_fetch_bitwidth(obj_bound);

			ir_relation relation = get_Confirm_relation(node);

			new.stable_digits = compute_bitwidth_relation(value, bound, relation);
			new.is_positive = value->is_positive;
			break;
		}
		case iro_Bitcast: {
			obj_a = get_Bitcast_op(node);
			bitwidth *a = bitwidth_fetch_bitwidth(obj_a);
			new.stable_digits = a->stable_digits;
			new.is_positive = true;
			break;
		}
		case iro_Offset: {
			//FIXME try to fetch the real offset in this entity
			break;
		}
		//nodes that stay the same
		case iro_Address:
		case iro_Const:
		case iro_Align:
		case iro_Alloc:
		case iro_Anchor:
		case iro_Bad:
		case iro_Block:
		case iro_Call:
		case iro_Cmp:
		case iro_Cond:
		case iro_CopyB:
		case iro_Deleted:
		case iro_Dummy:
		case iro_End:
		case iro_Free:
		case iro_IJmp:
		case iro_Id:
		case iro_Jmp:
		case iro_Load:
		case iro_NoMem:
		case iro_Pin:
		case iro_Proj:
		case iro_Raise:
		case iro_Return:
		case iro_Start:
		case iro_Store:
		case iro_Switch:
		case iro_Sync:
		case iro_Tuple:
		case iro_Unknown:
		case iro_Size:
		case iro_Member:
		case iro_Sel:
		case iro_Builtin:
		break;
	}

	if (cmp_bitwidth(&new, info) == SMALLER) {
		refit_children(node, queue);
		memcpy(info, &new, sizeof(bitwidth));
	}
}

static hook_entry_t bitwidth_dump_hook;

static void
dump_bitwidth_info(void *ctx, FILE *F, const ir_node *node)
{
	bitwidth *b = bitwidth_fetch_bitwidth(node);
	(void)ctx;

	if (b) {
		if (b->valid) {
			fprintf(F, "bitwidth-stable-digits %d\n", b->stable_digits);
			fprintf(F, "bitwidth-is-positive %d\n", b->is_positive);
		} else {
			fprintf(F, "bitwidth-stable-digits 'invalid'\n");
		}
	}
}

void
compute_bitwidth_info(ir_graph *irg)
{
	pqueue_t *queue;

	if (bitwidth_dump_hook.hook._hook_node_info == NULL) {
		bitwidth_dump_hook.hook._hook_node_info = dump_bitwidth_info;
		register_hook(hook_node_info, &bitwidth_dump_hook);
	}

	//init inital state
	remove_confirms(irg);
	construct_confirms_only(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	ir_nodemap_init(&irg->bitwidth.infos, irg);
	queue = new_pqueue();

	//phase 1 init all nodes that are meaningfull
	irg_walk_graph(irg, create_node, add_node, queue);

	//phase 2 walk down the queue reevalulating each child if things are changing
	while (!pqueue_empty(queue)) {
		ir_node *node = pqueue_pop_front(queue);
		evalulate_node(node, queue);
	}
	optimize_cf(irg);
	remove_confirms(irg);
	del_pqueue(queue);

	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_BITWIDTH_INFO);
}

void
free_bitwidth_info(ir_graph *irg)
{
	size_t len = ARR_LEN(irg->bitwidth.infos.data);
	for (size_t i = 0; i < len; ++i) {
		bitwidth *info = irg->bitwidth.infos.data[i];

		free(info);
	}
	ir_nodemap_destroy(&irg->bitwidth.infos);

	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_BITWIDTH_INFO);
}

void
assure_bitwidth_info(ir_graph *irg)
{
	if (!irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_BITWIDTH_INFO)) {
		compute_bitwidth_info(irg);
	}
}

struct max_param_bw_helper {
	ir_entity *entity;
	bitwidth **max_bitwidth;
};

static void compute_max_param_bitwidth(ir_node *node, void *env) {
	struct max_param_bw_helper *bw_helper = env;
	if(get_irn_opcode(node) != iro_Call)
		return;
	ir_entity *callee = get_Call_callee(node);
	if(callee == bw_helper->entity) {
		bitwidth **max_bws = bw_helper->max_bitwidth;
		for(int i = 0; i < get_Call_n_params(node); i++) {
			bitwidth *param_bw = bitwidth_fetch_bitwidth(get_Call_param(node, i));
			if(max_bws[i] == NULL || param_bw->stable_digits < max_bws[i]->stable_digits) {
				max_bws[i] = param_bw;
			}
		}
	}
}

/* Bitwidth analysis which also looks at caller graphs to narrow down bitwidths of parameters */
void compute_bitwidth_for_si(ir_graph *irg) {
	ir_entity *ent = get_irg_entity(irg);
	if (!(mtp_special_instruction & get_entity_additional_properties(ent)))
		return;
	ir_type *entity_type = get_entity_type(ent);
	size_t param_n = get_method_n_params(entity_type);
	if (param_n == 0) {
		compute_bitwidth_info(irg);
		return;
	}
	/* Callgraph is needed */
	if (get_irp_callee_info_state() != irg_callee_info_consistent) {
		ir_entity **free_methods = NULL;

		cgana(&free_methods);
		free(free_methods);
	}
	if (irp_callgraph_consistent != get_irp_callgraph_state()) {
		compute_callgraph();
	}
	bitwidth *max_param_bitwidth[param_n];
	for (size_t i = 0; i < param_n; i++) {
		max_param_bitwidth[i] = NULL;
	}
	struct max_param_bw_helper helper;
	helper.entity = ent;
	helper.max_bitwidth = max_param_bitwidth;
	for (size_t i = 0; i < get_irg_n_callers(irg); i++) {
		ir_graph *caller = get_irg_caller(irg, i);
		compute_bitwidth_info(caller);
		irg_walk_graph(caller, compute_max_param_bitwidth, NULL, &helper);
	}

	/*Quite the copy of compute_bitwidth_info*/
	pqueue_t *queue;

	if (bitwidth_dump_hook.hook._hook_node_info == NULL) {
		bitwidth_dump_hook.hook._hook_node_info = dump_bitwidth_info;
		register_hook(hook_node_info, &bitwidth_dump_hook);
	}

	//init inital state
	remove_confirms(irg);
	construct_confirms_only(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	ir_nodemap_init(&irg->bitwidth.infos, irg);
	queue = new_pqueue();

	//phase 1 init all nodes that are meaningfull
	irg_walk_graph(irg, create_node, add_node, queue);
	//Set parameter bitwidth here
	ir_node *args_proj = get_irg_args(irg);
	assert(is_Proj(args_proj));
	for (size_t i=0; i < get_irn_n_outs(args_proj); i++) {
		ir_node *arg_proj = get_irn_out(args_proj, i);
		assert(is_Proj(args_proj));
		bitwidth *old_info = ir_nodemap_get(bitwidth, &irg->bitwidth.infos, arg_proj);
		bitwidth *new_info = max_param_bitwidth[get_Proj_num(arg_proj)];
		memcpy(old_info, new_info, sizeof(bitwidth));
	}


	//phase 2 walk down the queue reevalulating each child if things are changing
	while (!pqueue_empty(queue)) {
		ir_node *node = pqueue_pop_front(queue);
		evalulate_node(node, queue);
	}
	optimize_cf(irg);
	remove_confirms(irg);
	del_pqueue(queue);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_BITWIDTH_INFO);

	free_callgraph();
	free_irp_callee_info();
}
