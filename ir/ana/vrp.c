/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief	analyze graph to provide value range information
 * @author 	Jonas Fietz
 * @version	$Id$
 */
#include "config.h"

#include "irtypes.h"
#include "vrp.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irgraph_t.h"
#include "irgopt.h"
#include "irpass.h"
#include "irgwalk.h"
#include "iredges.h"
#include "tv.h"
#include "irop.h"
#include "pdeq.h"
#include "irphase_t.h"
#include "debug.h"

static char v;
static void *VISITED = &v;

struct vrp_env_t {
	waitq *workqueue;
	DEBUG_ONLY(firm_dbg_module_t *dbg);
};

static vrp_attr *get_vrp_attr(const ir_node *node)
{
	return (vrp_attr*) get_or_set_irn_phase_info(node, PHASE_VRP);
}

static int vrp_update_node(ir_node *node)
{
	tarval *new_bits_set = get_tarval_bad();
	tarval *new_bits_not_set = get_tarval_bad();
	tarval *new_range_bottom = get_tarval_bad();
	tarval *new_range_top = get_tarval_bad();
	enum range_types new_range_type = VRP_UNDEFINED;
	int something_changed = 0;
	vrp_attr *vrp;

	if (!mode_is_int(get_irn_mode(node))) {
		return 0; /* we don't optimize for non-int-nodes*/
	}

	vrp = get_vrp_attr(node);

	/* TODO: Check if all predecessors have valid VRP information*/

	switch (get_irn_opcode(node)) {
	case iro_Const: {
		tarval *tv = get_Const_tarval(node);
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
		vrp_left = get_vrp_attr(left);
		vrp_right = get_vrp_attr(right);
		new_bits_set = tarval_and(vrp_left->bits_set, vrp_right->bits_set);
		new_bits_not_set = tarval_and(vrp_left->bits_not_set, vrp_right->bits_not_set);

		break;
	}

	case iro_Add: {
		int overflow_top, overflow_bottom;
		tarval *new_top, *new_bottom;
		const vrp_attr *vrp_left, *vrp_right;
		vrp_left = get_vrp_attr(get_Add_left(node));
		vrp_right = get_vrp_attr(get_Add_right(node));

		if (vrp_left->range_type == VRP_UNDEFINED || vrp_right->range_type ==
				VRP_UNDEFINED || vrp_left->range_type == VRP_VARYING ||
				vrp_right->range_type == VRP_VARYING) {
			return 0;
		}

		new_top = tarval_add(vrp_left->range_top, vrp_right->range_top);
		overflow_top = tarval_carry();
		new_bottom = tarval_add(vrp_left->range_bottom, vrp_right->range_bottom);
		overflow_bottom = tarval_carry();

		if (!overflow_top && !overflow_bottom && vrp_left->range_type == VRP_RANGE
				&&vrp_right->range_type == VRP_RANGE) {
			new_range_bottom = new_bottom;
			new_range_top = new_top;
			new_range_type = VRP_RANGE;
		}

		if (overflow_top || overflow_bottom) {
			/* TODO Implement overflow handling*/
			new_range_type = VRP_UNDEFINED;
		}
		break;
	}

	case iro_Sub: {
		int overflow_top, overflow_bottom;
		tarval *new_top, *new_bottom;
		const vrp_attr *vrp_left, *vrp_right;
		vrp_left = get_vrp_attr(get_Sub_left(node));
		vrp_right = get_vrp_attr(get_Sub_right(node));

		if (vrp_left->range_type == VRP_UNDEFINED || vrp_right->range_type ==
				VRP_UNDEFINED || vrp_left->range_type == VRP_VARYING ||
				vrp_right->range_type == VRP_VARYING) {
			return 0;
		}

		new_top = tarval_sub(vrp_left->range_top, vrp_right->range_top, NULL);
		overflow_top = tarval_carry();
		new_bottom = tarval_sub(vrp_left->range_bottom, vrp_right->range_bottom, NULL);
		overflow_bottom = tarval_carry();

		if (!overflow_top && !overflow_bottom && vrp_left->range_type == VRP_RANGE
				&&vrp_right->range_type == VRP_RANGE) {
			new_range_bottom = new_bottom;
			new_range_top = new_top;
			new_range_type = VRP_RANGE;
		}

		if (overflow_top || overflow_bottom) {
			/* TODO Implement overflow handling*/
		}
		break;
	}

	case iro_Or: {
		const vrp_attr *vrp_left, *vrp_right;

		vrp_left = get_vrp_attr(get_Or_left(node));
		vrp_right = get_vrp_attr(get_Or_right(node));

		new_bits_set = tarval_or(vrp_left->bits_set, vrp_right->bits_set);
		new_bits_not_set = tarval_or(vrp_left->bits_not_set, vrp_right->bits_not_set);

		break;
	}

	case iro_Rotl: {
		const vrp_attr *vrp_left, *vrp_right;
		const ir_node *right = get_Rotl_right(node);

		vrp_left = get_vrp_attr(get_Rotl_left(node));
		vrp_right = get_vrp_attr(get_Rotl_right(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			tarval *bits_set, *bits_not_set;
			bits_set = tarval_rotl(vrp_left->bits_set, get_Const_tarval(right));
			bits_not_set = tarval_rotl(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Shl: {
		const vrp_attr *vrp_left, *vrp_right;
		const ir_node *right = get_Shl_right(node);
		vrp_left = get_vrp_attr(get_Shl_left(node));
		vrp_right = get_vrp_attr(get_Shl_right(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			new_bits_set = tarval_shl(vrp_left->bits_set, get_Const_tarval(right));
			new_bits_not_set = tarval_shl(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Shr: {
		const vrp_attr *vrp_left, *vrp_right;
		const ir_node *right = get_Shr_right(node);

		vrp_left = get_vrp_attr(get_Shr_left(node));
		vrp_right = get_vrp_attr(get_Shr_right(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			new_bits_set = tarval_shr(vrp_left->bits_set, get_Const_tarval(right));
			new_bits_not_set = tarval_shr(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Shrs: {
		const vrp_attr *vrp_left, *vrp_right;
		const ir_node *right = get_Shrs_right(node);

		vrp_left = get_vrp_attr(get_Shrs_left(node));
		vrp_right = get_vrp_attr(get_Shrs_right(node));

		/* We can only compute this if the right value is a constant*/
		if (is_Const(right)) {
			new_bits_set = tarval_shrs(vrp_left->bits_set, get_Const_tarval(right));
			new_bits_not_set = tarval_shrs(vrp_left->bits_not_set, get_Const_tarval(right));
		}
		break;
	}

	case iro_Eor: {
		const vrp_attr *vrp_left, *vrp_right;

		vrp_left = get_vrp_attr(get_Eor_left(node));
		vrp_right = get_vrp_attr(get_Eor_right(node));

		new_bits_set = tarval_or(
						tarval_and(vrp_left->bits_set, tarval_not(vrp_right->bits_not_set)),
						tarval_and(tarval_not(vrp_left->bits_not_set), vrp_right->bits_set));

		new_bits_not_set = tarval_not(tarval_or(
				tarval_and(vrp_left->bits_set,vrp_right->bits_set),
							tarval_and(tarval_not(vrp_left->bits_not_set),
								tarval_not(vrp_right->bits_not_set))));

		break;
	}

	case iro_Id: {
		const vrp_attr *vrp_pred = get_vrp_attr(get_Id_pred(node));
		new_bits_set = vrp_pred->bits_set;
		new_bits_not_set = vrp_pred->bits_not_set;
		new_range_top = vrp_pred->range_top;
		new_range_bottom = vrp_pred->range_bottom;
		new_range_type = vrp_pred->range_type;
		break;
	}

	case iro_Not: {
		const vrp_attr *vrp_pred = get_vrp_attr(get_Not_op(node));
		new_bits_set = tarval_not(vrp_pred->bits_not_set);
		new_bits_not_set = tarval_not(vrp_pred->bits_set);
		break;
	}

	case iro_Conv: {
		const ir_node *pred = get_Conv_op(node);
		ir_mode *old_mode = get_irn_mode(pred);
		const vrp_attr *vrp_pred = get_vrp_attr(pred);

		ir_mode *new_mode;

		if (!mode_is_int(old_mode))
			return 0;

		new_mode = get_irn_mode(node);

		/* The second and is needed if target type is smaller*/
		new_bits_not_set = tarval_convert_to(get_mode_all_one(old_mode), new_mode);
		new_bits_not_set = tarval_and(new_bits_not_set, tarval_convert_to(vrp_pred->bits_not_set, new_mode));
		new_bits_set = tarval_and(
				new_bits_not_set, tarval_convert_to(vrp_pred->bits_set, new_mode));

		if (tarval_cmp(vrp_pred->range_top, get_mode_max(new_mode)) == pn_Cmp_Le) {
			vrp->range_top = vrp_pred->range_top;
		}

		if (tarval_cmp(vrp_pred->range_bottom, get_mode_min(new_mode)) == pn_Cmp_Ge) {
			vrp->range_bottom = vrp_pred->range_bottom;
		}
		break;
	}

	case iro_Confirm: {
		const pn_Cmp cmp = get_Confirm_cmp(node);
		const ir_node *bound = get_Confirm_bound(node);


		if (cmp == pn_Cmp_Lg) {
			/** @todo: Handle non-Const bounds */
			if (is_Const(bound)) {
				new_range_type = VRP_ANTIRANGE;
				new_range_top = get_Const_tarval(bound);
				new_range_bottom = get_Const_tarval(bound);
			}
		} else if (cmp == pn_Cmp_Le) {
			if (is_Const(bound)) {
				new_range_type = VRP_RANGE;
				new_range_top = get_Const_tarval(bound);
				new_range_bottom = get_tarval_min(get_irn_mode(node));
			}
		}
		break;
	}

	case iro_Phi: {
		/* combine all ranges*/

		int num = get_Phi_n_preds(node);
		pn_Cmp cmp;
		int i;

		const ir_node *pred = get_Phi_pred(node,0);
		const vrp_attr *vrp_pred = get_vrp_attr(pred);
		new_range_top = vrp_pred->range_top;
		new_range_bottom = vrp_pred->range_bottom;
		new_range_type = vrp_pred->range_type;
		new_bits_set = vrp_pred->bits_set;
		new_bits_not_set = vrp_pred->bits_not_set;

		assert(num > 0);

		for (i = 1; i < num; i++) {
			pred = get_Phi_pred(node, i);
			vrp_pred = get_vrp_attr(pred);
			if (new_range_type == VRP_RANGE && vrp_pred->range_type ==
					VRP_RANGE) {
				cmp = tarval_cmp(new_range_top, vrp_pred->range_top);
				if (cmp == pn_Cmp_Lt) {
					new_range_top = vrp_pred->range_top;
				}
				cmp = tarval_cmp(new_range_bottom, vrp_pred->range_bottom);
				if (cmp == pn_Cmp_Gt) {
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
	is_Abs(node) is_Alloc(node) is_Anchor(node) is_Borrow(node) is_Bound(node)
	is_Break(node) is_Builtin(node) is_Call(node) is_CallBegin(node)
	is_Carry(node) is_Cast(node) is_Cmp(node) is_Cond(node)
	is_CopyB(node) is_Div(node) is_DivMod(node) is_Dummy(node)
	is_End(node) is_EndExcept(node) is_EndReg(node) is_Filter(node) is_Free(node)
	is_IJmp(node) is_InstOf(node) is_Jmp(node) is_Load(node) is_Minus(node)
	is_Mod(node) is_Mul(node) is_Mulh(node) is_Mux(node) is_NoMem(node)
	is_Pin(node) is_Proj(node) is_Quot(node)
	is_Raise(node) is_Return(node) is_Sel(node) is_Start(node) is_Store(node)
	is_SymConst(node) is_Sync(node) is_Tuple(node)
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
		if (tarval_cmp(new_bits_set, vrp->bits_set) != pn_Cmp_Eq) {
			something_changed = 1;
			vrp->bits_set = new_bits_set;
		}
	}
	if (new_bits_not_set != tarval_bad) {
		new_bits_not_set = tarval_and(new_bits_not_set, vrp->bits_not_set);

		if (tarval_cmp(new_bits_not_set, vrp->bits_not_set) != pn_Cmp_Eq) {
			something_changed = 1;
			vrp->bits_not_set = new_bits_not_set;
		}
	}

	if (vrp->range_type == VRP_UNDEFINED &&
			new_range_type != VRP_UNDEFINED) {
		something_changed = 1;
		vrp->range_type = new_range_type;
		vrp->range_bottom = new_range_bottom;
		vrp->range_top = new_range_top;

	} else if (vrp->range_type == VRP_RANGE) {
		if (new_range_type == VRP_RANGE) {
			if (tarval_cmp(vrp->range_bottom, new_range_bottom) == pn_Cmp_Lt) {
				something_changed = 1;
				vrp->range_bottom = new_range_bottom;
			}
			if (tarval_cmp(vrp->range_top, new_range_top) == pn_Cmp_Gt) {
				something_changed = 1;
				vrp->range_top = new_range_top;
			}
		}

		if (new_range_type == VRP_ANTIRANGE) {
			/* if they are overlapping, cut the range.*/
			/* TODO: Maybe we can preserve more information here*/
			if (tarval_cmp(vrp->range_bottom, new_range_top) == pn_Cmp_Gt &&
					tarval_cmp(vrp->range_bottom, new_range_bottom) == pn_Cmp_Gt) {
				something_changed = 1;
				vrp->range_bottom = new_range_top;

			} else if (tarval_cmp(vrp->range_top, new_range_bottom) == pn_Cmp_Gt &&
					tarval_cmp(vrp->range_top, new_range_top) == pn_Cmp_Lt) {
				something_changed = 1;
				vrp->range_top = new_range_bottom;
			}

			/* We can not handle the case where the anti range is in the*/
			/* range*/

		}
	} else if (vrp->range_type == VRP_ANTIRANGE) {
		if (new_range_type == VRP_ANTIRANGE) {
			if (tarval_cmp(vrp->range_bottom, new_range_bottom) == pn_Cmp_Gt) {
				something_changed = 1;
				vrp->range_bottom = new_range_bottom;
			}
			if (tarval_cmp(vrp->range_top, new_range_top) == pn_Cmp_Lt) {
				something_changed = 1;
				vrp->range_top = new_range_top;
			}
		}

		if (new_range_type == VRP_RANGE) {
			if (tarval_cmp(vrp->range_bottom, new_range_top) == pn_Cmp_Gt) {
				something_changed = 1;
				vrp->range_bottom = new_range_top;
			}
			if (tarval_cmp(vrp->range_top, new_range_bottom) == pn_Cmp_Lt) {
				something_changed = 1;
				vrp->range_top = new_range_bottom;
			}
		}
	}

	assert(tarval_is_null(
				tarval_and(vrp->bits_set, tarval_not(vrp->bits_not_set))));
	return something_changed;
}

static void vrp_first_pass(ir_node *n, void *e)
{
	ir_node *succ;
	int i;
	struct vrp_env_t *env = e;

	if (is_Block(n))
		return;

	set_irn_link(n, VISITED);

	vrp_update_node(n);

	assure_irg_outs(get_current_ir_graph());
	for (i = get_irn_n_outs(n) - 1; i >=0; --i) {
		succ =  get_irn_out(n, i);
		if (get_irn_link(succ) == VISITED) {
			/* we found a loop*/
			waitq_put(env->workqueue, succ);
		}
	}
}

static void *vrp_init_node(ir_phase *phase, const ir_node *n, void *old)
{
	ir_mode *mode;
	vrp_attr *vrp;
	struct vrp_env_t *env = phase->priv;

	DBG((env->dbg, LEVEL_2, "initialized node nr: %d\n", get_irn_node_nr(n)));
	assert(old==NULL && "init called for node already initialized");
	vrp = phase_alloc(phase, sizeof(vrp_attr));

	memset(vrp, 0, sizeof(vrp_attr));
	/* Initialize the vrp information to default */

	mode = get_irn_mode(n);

	vrp->range_type = VRP_UNDEFINED;

	/* TODO: We might be able to optimize space usage if we do not allocate
	 * vrp space for non-int nodes. (currently caught by vrp_update_node)
	 */
	if (mode_is_int(mode)) {
		/* We are assuming that 0 is always represented as this modes null */
		vrp->valid = 1;
		vrp->bits_set = get_mode_null(mode);
		vrp->bits_not_set = get_mode_all_one(mode);
		vrp->range_bottom = get_tarval_top();
		vrp->range_top = get_tarval_top();
	} else {
		vrp->valid = 0;
		vrp->bits_set = get_tarval_bad();
		vrp->bits_not_set = get_tarval_bad();
		vrp->range_bottom = get_tarval_bad();
		vrp->range_top = get_tarval_bad();
	}

	/* TODO: We might be able to set better vrp info at this time, if this is
	 * a node which is newly created in an already initialized irg
	 *
	 * maybe just call vrp_update_node and if it returns one, iterate over
	 * successors
	 */
	return vrp;
}

void set_vrp_data(ir_graph *irg)
{
	ir_node *succ, *node;
	int i;
	struct vrp_env_t *env;
	ir_phase *phase;

	assure_irg_outs(irg); /* ensure that out edges are consistent*/
	phase = irg_get_phase(irg, PHASE_VRP);
	if (phase == NULL) {
		/* this is our first run */
		phase = new_phase(irg, vrp_init_node);
		irg_register_phase(irg, PHASE_VRP, phase);
		env = phase_alloc(phase, sizeof(*env));
		FIRM_DBG_REGISTER(env->dbg, "ir.ana.vrp");
		phase->priv = env;
	} else {
		env = phase->priv;
	}

	env->workqueue = new_waitq();

	irg_walk_graph(irg, NULL, vrp_first_pass, env);

	/* while there are entries in the worklist, continue*/
	while (!waitq_empty(env->workqueue)) {
		node = waitq_get(env->workqueue);

		if (vrp_update_node(node)) {
			/* if something changed, add successors to worklist*/
			for (i = get_irn_n_outs(node) - 1; i >=0; --i) {
				succ =  get_irn_out(node, i);
				waitq_put(env->workqueue, node);
			}
		}
	}
	del_waitq(env->workqueue);
}


ir_graph_pass_t *set_vrp_pass(const char *name)
{
	return def_graph_pass(name ? name : "set_vrp", set_vrp_data);
}

pn_Cmp vrp_cmp(const ir_node *left, const ir_node *right)
{
	vrp_attr *vrp_left, *vrp_right;

	vrp_left = vrp_get_info(left);
	vrp_right = vrp_get_info(right);

	if (!vrp_left || !vrp_right) {
		return pn_Cmp_False;
	}

	if (vrp_left->range_type == VRP_RANGE && vrp_right->range_type == VRP_RANGE) {
		if (tarval_cmp(vrp_left->range_top, vrp_right->range_bottom) == pn_Cmp_Lt) {
			return pn_Cmp_Lt;
		}
		if (tarval_cmp(vrp_left->range_bottom, vrp_right->range_top) == pn_Cmp_Gt) {
			return pn_Cmp_Gt;
		}
	}

	if (!tarval_is_null(tarval_and(vrp_left->bits_set, tarval_not(vrp_right->bits_not_set))) ||
			!tarval_is_null(tarval_and(tarval_not(vrp_left->bits_not_set), vrp_right->bits_set))) {
		return pn_Cmp_Lg;
	}
	/* TODO: We can get way more information here*/

	return pn_Cmp_False;
}

vrp_attr *vrp_get_info(const ir_node *node)
{
	const ir_graph *irg   = get_irn_irg(node);
	const ir_phase *phase = irg_get_phase(irg, PHASE_VRP);
	vrp_attr       *vrp;

	if (phase == NULL) {
		/* phase has not yet been initialized */
		return NULL;
	}

	vrp = phase_get_irn_data(phase, node);
	if (vrp && vrp->valid) {
		return vrp;
	}
	return NULL;
}
