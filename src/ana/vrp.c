/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   analyze graph to provide value range information
 * @author  Jonas Fietz
 */
#include "vrp.h"

#include "bitset.h"
#include "debug.h"
#include "iredges_t.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnodemap.h"
#include "iroptimize.h"
#include "irouts_t.h"
#include "irprintf.h"
#include "pdeq.h"
#include "tv.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct vrp_env_t {
	deq_t        workqueue;
	bitset_t    *visited;
	ir_vrp_info *info;
} vrp_env_t;

static vrp_attr *vrp_get_or_set_info(ir_vrp_info *info, const ir_node *node)
{
	vrp_attr *attr = ir_nodemap_get(vrp_attr, &info->infos, node);
	if (attr == NULL) {
		ir_mode *mode = get_irn_mode(node);
		assert(mode_is_int(mode));

		attr = OALLOCZ(&info->obst, vrp_attr);
		attr->range_type   = VRP_UNDEFINED;
		attr->bits_set     = get_mode_null(mode);
		attr->bits_not_set = get_mode_all_one(mode);
		attr->range_bottom = tarval_bad;
		attr->range_top    = tarval_bad;

		ir_nodemap_insert(&info->infos, node, attr);
	}
	return attr;
}

vrp_attr *vrp_get_info(const ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	if (irg->vrp.infos.data == NULL)
		return NULL;
	return ir_nodemap_get(vrp_attr, &irg->vrp.infos, node);
}

static int vrp_update_node(ir_vrp_info *info, ir_node *node)
{
	ir_tarval       *new_bits_set      = get_tarval_bad();
	ir_tarval       *new_bits_not_set  = get_tarval_bad();
	ir_tarval       *new_range_bottom  = get_tarval_bad();
	ir_tarval       *new_range_top     = get_tarval_bad();
	enum range_types new_range_type    = VRP_UNDEFINED;
	bool             something_changed = false;
	if (!mode_is_int(get_irn_mode(node))) {
		return 0; /* we don't optimize for non-int-nodes*/
	}

	vrp_attr *vrp = vrp_get_or_set_info(info, node);

	/* TODO: Check if all predecessors have valid VRP information*/

	switch (get_irn_opcode(node)) {
	case iro_Const: {
		ir_tarval *tv = get_Const_tarval(node);
		new_bits_set = tv;
		new_bits_not_set = tv;
		new_range_bottom = tv;
		new_range_top = tv;
		new_range_type = VRP_RANGE;
		break;
	}
	case iro_And: {
		const vrp_attr *vrp_left, *vrp_right;
		const ir_node *left, *right;

		left = get_And_left(node);
		right = get_And_right(node);
		vrp_left = vrp_get_or_set_info(info, left);
		vrp_right = vrp_get_or_set_info(info, right);
		new_bits_set = tarval_and(vrp_left->bits_set, vrp_right->bits_set);
		new_bits_not_set = tarval_and(vrp_left->bits_not_set, vrp_right->bits_not_set);

		break;
	}

	case iro_Add: {
		const vrp_attr *vrp_left  = vrp_get_or_set_info(info, get_Add_left(node));
		const vrp_attr *vrp_right = vrp_get_or_set_info(info, get_Add_right(node));

		if (vrp_left->range_type == VRP_UNDEFINED
		 || vrp_right->range_type == VRP_UNDEFINED
		 || vrp_left->range_type == VRP_VARYING
		 || vrp_right->range_type == VRP_VARYING) {
			return 0;
		}

		if (vrp_left->range_type == VRP_RANGE
		 && vrp_right->range_type == VRP_RANGE) {
			int old_wrap_on_overflow = tarval_get_wrap_on_overflow();
			tarval_set_wrap_on_overflow(false);
			ir_tarval *new_top
				= tarval_add(vrp_left->range_top, vrp_right->range_top);
			ir_tarval *new_bottom
				= tarval_add(vrp_left->range_bottom, vrp_right->range_bottom);
			tarval_set_wrap_on_overflow(old_wrap_on_overflow);

			if (new_top != tarval_bad && new_bottom != tarval_bad) {
				new_range_bottom = new_bottom;
				new_range_top    = new_top;
				new_range_type   = VRP_RANGE;
			} else {
				/* TODO Implement overflow handling*/
				new_range_type = VRP_UNDEFINED;
			}
		}
		break;
	}

	case iro_Sub: {
		ir_node *left  = get_Sub_left(node);
		ir_node *right = get_Sub_right(node);

		if (!mode_is_int(get_irn_mode(left)))
			return 0;

		const vrp_attr *vrp_left  = vrp_get_or_set_info(info, left);
		const vrp_attr *vrp_right = vrp_get_or_set_info(info, right);

		if (vrp_left->range_type == VRP_UNDEFINED
		 || vrp_right->range_type == VRP_UNDEFINED
		 || vrp_left->range_type == VRP_VARYING
		 || vrp_right->range_type == VRP_VARYING) {
			return 0;
		}

		if (vrp_left->range_type == VRP_RANGE
		 && vrp_right->range_type == VRP_RANGE) {
			int old_wrap_on_overflow = tarval_get_wrap_on_overflow();
			tarval_set_wrap_on_overflow(false);
			ir_tarval *new_top = tarval_sub(vrp_left->range_top, vrp_right->range_bottom);
			ir_tarval *new_bottom = tarval_sub(vrp_left->range_bottom, vrp_right->range_top);
			tarval_set_wrap_on_overflow(old_wrap_on_overflow);

			if (new_top != tarval_bad && new_bottom != tarval_bad) {
				new_range_bottom = new_bottom;
				new_range_top = new_top;
				new_range_type = VRP_RANGE;
			} else {
				/* TODO Implement overflow handling*/
				new_range_type = VRP_UNDEFINED;
			}
		}
		break;
	}

	case iro_Or: {
		const vrp_attr *vrp_left, *vrp_right;

		vrp_left = vrp_get_or_set_info(info, get_Or_left(node));
		vrp_right = vrp_get_or_set_info(info, get_Or_right(node));

		new_bits_set = tarval_or(vrp_left->bits_set, vrp_right->bits_set);
		new_bits_not_set = tarval_or(vrp_left->bits_not_set, vrp_right->bits_not_set);

		break;
	}

	case iro_Shl: {
		const vrp_attr *vrp_left;
		const ir_node *right = get_Shl_right(node);
		vrp_left = vrp_get_or_set_info(info, get_Shl_left(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			new_bits_set = tarval_shl(vrp_left->bits_set, get_Const_tarval(right));
			new_bits_not_set = tarval_shl(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Shr: {
		const vrp_attr *vrp_left;
		const ir_node *right = get_Shr_right(node);

		vrp_left = vrp_get_or_set_info(info, get_Shr_left(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			new_bits_set = tarval_shr(vrp_left->bits_set, get_Const_tarval(right));
			new_bits_not_set = tarval_shr(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Shrs: {
		const vrp_attr *vrp_left;
		const ir_node *right = get_Shrs_right(node);

		vrp_left = vrp_get_or_set_info(info, get_Shrs_left(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			new_bits_set = tarval_shrs(vrp_left->bits_set, get_Const_tarval(right));
			new_bits_not_set = tarval_shrs(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Eor: {
		const vrp_attr *vrp_left, *vrp_right;

		vrp_left = vrp_get_or_set_info(info, get_Eor_left(node));
		vrp_right = vrp_get_or_set_info(info, get_Eor_right(node));

		new_bits_set = tarval_or(tarval_and(vrp_left->bits_set, tarval_not(vrp_right->bits_not_set)),
		                         tarval_and(tarval_not(vrp_left->bits_not_set), vrp_right->bits_set));

		new_bits_not_set = tarval_not(tarval_or(tarval_and(vrp_left->bits_set,vrp_right->bits_set),
		                                        tarval_and(tarval_not(vrp_left->bits_not_set),
		                                                   tarval_not(vrp_right->bits_not_set))));

		break;
	}

	case iro_Id: {
		const vrp_attr *vrp_pred = vrp_get_or_set_info(info, get_Id_pred(node));
		new_bits_set = vrp_pred->bits_set;
		new_bits_not_set = vrp_pred->bits_not_set;
		new_range_top = vrp_pred->range_top;
		new_range_bottom = vrp_pred->range_bottom;
		new_range_type = vrp_pred->range_type;
		break;
	}

	case iro_Not: {
		const vrp_attr *vrp_pred = vrp_get_or_set_info(info, get_Not_op(node));
		new_bits_set = tarval_not(vrp_pred->bits_not_set);
		new_bits_not_set = tarval_not(vrp_pred->bits_set);
		break;
	}

	case iro_Conv: {
		const ir_node *pred = get_Conv_op(node);
		ir_mode *old_mode = get_irn_mode(pred);
		const vrp_attr *vrp_pred;

		ir_mode *new_mode;

		if (!mode_is_int(old_mode))
			return 0;

		vrp_pred = vrp_get_or_set_info(info, pred);
		new_mode = get_irn_mode(node);

		/* The second and is needed if target type is smaller*/
		new_bits_not_set = tarval_convert_to(get_mode_all_one(old_mode), new_mode);
		new_bits_not_set = tarval_and(new_bits_not_set, tarval_convert_to(vrp_pred->bits_not_set, new_mode));
		new_bits_set = tarval_and(new_bits_not_set,
		                          tarval_convert_to(vrp_pred->bits_set, new_mode));

		/* Matze: TODO, BUGGY, tarval_cmp never returns ir_relation_less_equal */
		if (tarval_cmp(vrp_pred->range_top, get_mode_max(new_mode)) == ir_relation_less_equal) {
			vrp->range_top = vrp_pred->range_top;
		}

		/* Matze: TODO, BUGGY, tarval_cmp never returns ir_relation_greater_equal */
		if (tarval_cmp(vrp_pred->range_bottom, get_mode_min(new_mode)) == ir_relation_greater_equal) {
			vrp->range_bottom = vrp_pred->range_bottom;
		}
		break;
	}

	case iro_Confirm: {
		const ir_relation relation = get_Confirm_relation(node);
		const ir_node    *bound    = get_Confirm_bound(node);


		if (relation == ir_relation_less_greater) {
			/** @todo: Handle non-Const bounds */
			if (is_Const(bound)) {
				new_range_type = VRP_ANTIRANGE;
				new_range_top = get_Const_tarval(bound);
				new_range_bottom = get_Const_tarval(bound);
			}
		} else if (relation == ir_relation_less_equal) {
			if (is_Const(bound)) {
				new_range_type = VRP_RANGE;
				new_range_top = get_Const_tarval(bound);
				new_range_bottom = get_mode_min(get_irn_mode(node));
			}
		}
		break;
	}

	case iro_Phi: {
		/* combine all ranges*/
		const ir_node *pred = get_Phi_pred(node,0);
		const vrp_attr *vrp_pred = vrp_get_or_set_info(info, pred);
		new_range_top    = vrp_pred->range_top;
		new_range_bottom = vrp_pred->range_bottom;
		new_range_type   = vrp_pred->range_type;
		new_bits_set     = vrp_pred->bits_set;
		new_bits_not_set = vrp_pred->bits_not_set;

		for (int i = 1, num = get_Phi_n_preds(node); i < num; i++) {
			pred = get_Phi_pred(node, i);
			vrp_pred = vrp_get_or_set_info(info, pred);
			if (new_range_type == VRP_RANGE && vrp_pred->range_type == VRP_RANGE) {
				ir_relation relation = tarval_cmp(new_range_top, vrp_pred->range_top);
				if (relation == ir_relation_less) {
					new_range_top = vrp_pred->range_top;
				}
				relation = tarval_cmp(new_range_bottom, vrp_pred->range_bottom);
				if (relation == ir_relation_greater) {
					new_range_bottom = vrp_pred->range_bottom;
				}
			} else {
				new_range_type = VRP_VARYING;
			}
			new_bits_set = tarval_and(new_bits_set, vrp_pred->bits_set);
			new_bits_not_set = tarval_or(new_bits_not_set,
			                             vrp_pred->bits_not_set);
		}

		break;
	}
	default:
		/* unhandled, therefore never updated */
		break;
	}



	/* TODO: Check, if there can be information derived from any of these:
	is_Abs(node) is_Address(node) is_Alloc(node) is_Anchor(node) is_Borrow(node)
	is_Bound(node) is_Break(node) is_Builtin(node) is_Call(node)
	is_Carry(node) is_Cmp(node) is_Cond(node)
	is_CopyB(node) is_Div(node) is_Dummy(node)
	is_End(node) is_Free(node)
	is_IJmp(node) is_Jmp(node) is_Load(node) is_Minus(node) is_Member(node)
	is_Mod(node) is_Mul(node) is_Mulh(node) is_Mux(node) is_NoMem(node)
	is_Offset(node) is_Pin(node) is_Proj(node)
	is_Raise(node) is_Return(node) is_Sel(node) is_Start(node) is_Store(node)
	is_Sync(node) is_Tuple(node) is_TypeConst(node)
	*/

	/* @todo: At this place, we check if the mode of the variable changed. A
	 * better place for this might be in the convopt.c file
	 */

	if (new_bits_set != tarval_bad && get_tarval_mode(new_bits_set) != get_tarval_mode(vrp->bits_set)) {
		vrp->bits_set = tarval_convert_to(vrp->bits_set, get_irn_mode(node));
	}
	if (new_bits_not_set != tarval_bad && get_tarval_mode(new_bits_not_set) != get_tarval_mode(vrp->bits_not_set)) {
		vrp->bits_not_set = tarval_convert_to(vrp->bits_not_set, get_irn_mode(node));
	}

	if (vrp->range_type != VRP_UNDEFINED && new_range_type != VRP_UNDEFINED && get_tarval_mode(new_range_top) != get_tarval_mode(vrp->range_top)) {
		/* @todo: We might be able to preserve this range information if it
		 * fits in */
		vrp->range_type = VRP_VARYING;
	}

	/* Merge the newly calculated values with those that might already exist*/
	if (new_bits_set != tarval_bad) {
		new_bits_set = tarval_or(new_bits_set, vrp->bits_set);
		if (new_bits_set != vrp->bits_set) {
			something_changed = true;
			vrp->bits_set     = new_bits_set;
		}
	}
	if (new_bits_not_set != tarval_bad) {
		new_bits_not_set = tarval_and(new_bits_not_set, vrp->bits_not_set);

		if (new_bits_not_set != vrp->bits_not_set) {
			something_changed = true;
			vrp->bits_not_set = new_bits_not_set;
		}
	}

	if (vrp->range_type == VRP_UNDEFINED &&
	    new_range_type != VRP_UNDEFINED) {
		something_changed = true;
		vrp->range_type = new_range_type;
		vrp->range_bottom = new_range_bottom;
		vrp->range_top = new_range_top;

	} else if (vrp->range_type == VRP_RANGE) {
		if (new_range_type == VRP_RANGE) {
			if (tarval_cmp(vrp->range_bottom, new_range_bottom) == ir_relation_less) {
				something_changed = true;
				vrp->range_bottom = new_range_bottom;
			}
			if (tarval_cmp(vrp->range_top, new_range_top) == ir_relation_greater) {
				something_changed = true;
				vrp->range_top    = new_range_top;
			}
		}

		if (new_range_type == VRP_ANTIRANGE) {
			/* if they are overlapping, cut the range.*/
			/* TODO: Maybe we can preserve more information here*/
			if (tarval_cmp(vrp->range_bottom, new_range_top) == ir_relation_greater &&
			    tarval_cmp(vrp->range_bottom, new_range_bottom) == ir_relation_greater) {
				something_changed = true;
				vrp->range_bottom = new_range_top;

			} else if (tarval_cmp(vrp->range_top, new_range_bottom) == ir_relation_greater &&
			           tarval_cmp(vrp->range_top, new_range_top) == ir_relation_less) {
				something_changed = true;
				vrp->range_top    = new_range_bottom;
			}

			/* We can not handle the case where the anti range is in the*/
			/* range*/

		}
	} else if (vrp->range_type == VRP_ANTIRANGE) {
		if (new_range_type == VRP_ANTIRANGE) {
			if (tarval_cmp(vrp->range_bottom, new_range_bottom) == ir_relation_greater) {
				something_changed = true;
				vrp->range_bottom = new_range_bottom;
			}
			if (tarval_cmp(vrp->range_top, new_range_top) == ir_relation_less) {
				something_changed = true;
				vrp->range_top    = new_range_top;
			}
		}

		if (new_range_type == VRP_RANGE) {
			if (tarval_cmp(vrp->range_bottom, new_range_top) == ir_relation_greater) {
				something_changed = true;
				vrp->range_bottom = new_range_top;
			}
			if (tarval_cmp(vrp->range_top, new_range_bottom) == ir_relation_less) {
				something_changed = true;
				vrp->range_top    = new_range_bottom;
			}
		}
	}

	assert(tarval_is_null(tarval_and(vrp->bits_set, tarval_not(vrp->bits_not_set))));
	return something_changed;
}

static void vrp_first_pass(ir_node *n, void *e)
{
	if (is_Block(n))
		return;

	vrp_env_t *env = (vrp_env_t*) e;
	bitset_set(env->visited, get_irn_idx(n));

	vrp_update_node(env->info, n);

	foreach_irn_out_r(n, i, succ) {
		if (bitset_is_set(env->visited, get_irn_idx(succ))) {
			/* we found a loop*/
			deq_push_pointer_right(&env->workqueue, succ);
		}
	}
}

static void dump_vrp_info(void *ctx, FILE *F, const ir_node *node)
{
	(void)ctx;
	if (!mode_is_int(get_irn_mode(node)))
		return;

	vrp_attr *vrp = vrp_get_info(node);
	if (vrp == NULL)
		return;

	fprintf(F, "vrp range type: %d\n", (int) vrp->range_type);
	if (vrp->range_type == VRP_RANGE || vrp->range_type == VRP_ANTIRANGE) {
		ir_fprintf(F, "vrp range bottom: %T\n",vrp->range_bottom);
		ir_fprintf(F, "vrp range top: %T\n", vrp->range_top);
	}
	ir_fprintf(F, "vrp bits set: %T\n", vrp->bits_set);
	ir_fprintf(F, "vrp bits not set: %T\n", vrp->bits_not_set);
}

static hook_entry_t dump_hook;

void set_vrp_data(ir_graph *irg)
{
	if (irg->vrp.infos.data != NULL)
		free_vrp_data(irg);

	FIRM_DBG_REGISTER(dbg, "ir.ana.vrp");

	assure_irg_outs(irg); /* ensure that out edges are consistent*/
	ir_nodemap_init(&irg->vrp.infos, irg);
	obstack_init(&irg->vrp.obst);
	ir_vrp_info *info = &irg->vrp;

	if (dump_hook.hook._hook_node_info == NULL) {
		dump_hook.hook._hook_node_info = dump_vrp_info;
		register_hook(hook_node_info, &dump_hook);
	}

	vrp_env_t *env = OALLOCZ(&irg->vrp.obst, vrp_env_t);
	env->info      = info;
	deq_init(&env->workqueue);

	env->visited = bitset_malloc(get_irg_last_idx(irg));
	irg_walk_graph(irg, NULL, vrp_first_pass, env);
	free(env->visited);

	/* while there are entries in the worklist, continue*/
	while (!deq_empty(&env->workqueue)) {
		ir_node *node = deq_pop_pointer_left(ir_node, &env->workqueue);

		if (vrp_update_node(info, node)) {
			/* if something changed, add successors to worklist*/
			foreach_irn_out_r(node, i, succ) {
				deq_push_pointer_right(&env->workqueue, succ);
			}
		}
	}
	deq_free(&env->workqueue);
}

void free_vrp_data(ir_graph *irg)
{
	if (irg->vrp.infos.data == NULL)
		return;
	obstack_free(&irg->vrp.obst, NULL);
	ir_nodemap_destroy(&irg->vrp.infos);
}

ir_relation vrp_cmp(const ir_node *left, const ir_node *right)
{
	if (!mode_is_int(get_irn_mode(left)))
		return ir_relation_true;

	vrp_attr *vrp_left  = vrp_get_info(left);
	vrp_attr *vrp_right = vrp_get_info(right);
	if (vrp_left == NULL || vrp_right == NULL)
		return ir_relation_true;

	if (vrp_left->range_type == VRP_RANGE && vrp_right->range_type == VRP_RANGE) {
		if (tarval_cmp(vrp_left->range_top, vrp_right->range_bottom) == ir_relation_less) {
			return ir_relation_less;
		}
		if (tarval_cmp(vrp_left->range_bottom, vrp_right->range_top) == ir_relation_greater) {
			return ir_relation_greater;
		}
	}

	if (!tarval_is_null(tarval_and(vrp_left->bits_set, tarval_not(vrp_right->bits_not_set))) ||
	    !tarval_is_null(tarval_and(tarval_not(vrp_left->bits_not_set), vrp_right->bits_set))) {
		return ir_relation_less_greater;
	}

	/* TODO: We can get way more information here*/
	return ir_relation_true;
}
