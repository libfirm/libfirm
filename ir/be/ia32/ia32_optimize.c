/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Implements several optimizations for IA32.
 * @author      Matthias Braun, Christian Wuerdig
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "irprog_t.h"
#include "ircons.h"
#include "irtools.h"
#include "firm_types.h"
#include "iredges.h"
#include "tv.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "height.h"
#include "irbitset.h"

#include "../be_t.h"
#include "../beabi.h"
#include "../benode_t.h"
#include "../besched_t.h"
#include "../bepeephole.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_transform.h"
#include "ia32_dbg_stat.h"
#include "ia32_util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef int is_op_func_t(const ir_node *n);
typedef ir_node *load_func_t(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *base, ir_node *index, ir_node *mem);

/**
 * checks if a node represents the NOREG value
 */
static INLINE int be_is_NoReg(ia32_code_gen_t *cg, const ir_node *irn) {
	return irn == cg->noreg_gp || irn == cg->noreg_xmm || irn == cg->noreg_vfp;
}

/********************************************************************************************************
 *  _____                _           _         ____        _   _           _          _   _
 * |  __ \              | |         | |       / __ \      | | (_)         (_)        | | (_)
 * | |__) |__  ___ _ __ | |__   ___ | | ___  | |  | |_ __ | |_ _ _ __ ___  _ ______ _| |_ _  ___  _ __
 * |  ___/ _ \/ _ \ '_ \| '_ \ / _ \| |/ _ \ | |  | | '_ \| __| | '_ ` _ \| |_  / _` | __| |/ _ \| '_ \
 * | |  |  __/  __/ |_) | | | | (_) | |  __/ | |__| | |_) | |_| | | | | | | |/ / (_| | |_| | (_) | | | |
 * |_|   \___|\___| .__/|_| |_|\___/|_|\___|  \____/| .__/ \__|_|_| |_| |_|_/___\__,_|\__|_|\___/|_| |_|
 *                | |                               | |
 *                |_|                               |_|
 ********************************************************************************************************/

/**
 * NOTE: THESE PEEPHOLE OPTIMIZATIONS MUST BE CALLED AFTER SCHEDULING AND REGISTER ALLOCATION.
 */

// only optimize up to 48 stores behind IncSPs
#define MAXPUSH_OPTIMIZE	48

/**
 * Tries to create pushs from IncSP,Store combinations
 */
static void ia32_create_Pushs(ir_node *irn, ia32_code_gen_t *cg) {
	int i;
	int offset;
	ir_node *node;
	ir_node *stores[MAXPUSH_OPTIMIZE];
	ir_node *block = get_nodes_block(irn);
	ir_graph *irg = cg->irg;
	ir_node *curr_sp;
	ir_mode *spmode = get_irn_mode(irn);

	memset(stores, 0, sizeof(stores));

	assert(be_is_IncSP(irn));

	offset = be_get_IncSP_offset(irn);
	if(offset < 4)
		return;

	/*
	 * We first walk the schedule after the IncSP node as long as we find
	 * suitable stores that could be transformed to a push.
	 * We save them into the stores array which is sorted by the frame offset/4
	 * attached to the node
	 */
	for(node = sched_next(irn); !sched_is_end(node); node = sched_next(node)) {
		ir_node *mem;
		int offset;
		int storeslot;

		// it has to be a store
		if(!is_ia32_Store(node))
			break;

		// it has to use our sp value
		if(get_irn_n(node, n_ia32_base) != irn)
			continue;
		// store has to be attached to NoMem
		mem = get_irn_n(node, n_ia32_mem);
		if(!is_NoMem(mem)) {
			continue;
		}

		/* unfortunately we can't support the full AMs possible for push at the
		 * moment. TODO: fix this */
		if(get_ia32_am_scale(node) > 0 || !is_ia32_NoReg_GP(get_irn_n(node, n_ia32_index)))
			break;

		offset = get_ia32_am_offs_int(node);

		storeslot = offset / 4;
		if(storeslot >= MAXPUSH_OPTIMIZE)
			continue;

		// storing into the same slot twice is bad (and shouldn't happen...)
		if(stores[storeslot] != NULL)
			break;

		// storing at half-slots is bad
		if(offset % 4 != 0)
			break;

		stores[storeslot] = node;
	}

	curr_sp = be_get_IncSP_pred(irn);

	// walk the stores in inverse order and create pushs for them
	i = (offset / 4) - 1;
	if(i >= MAXPUSH_OPTIMIZE) {
		i = MAXPUSH_OPTIMIZE - 1;
	}

	for( ; i >= 0; --i) {
		const arch_register_t *spreg;
		ir_node *push;
		ir_node *val, *mem, *mem_proj;
		ir_node *store = stores[i];
		ir_node *noreg = ia32_new_NoReg_gp(cg);

		if(store == NULL || is_Bad(store))
			break;

		val = get_irn_n(store, n_ia32_unary_op);
		mem = get_irn_n(store, n_ia32_mem);
		spreg = arch_get_irn_register(cg->arch_env, curr_sp);

		push = new_rd_ia32_Push(get_irn_dbg_info(store), irg, block, noreg, noreg, mem, val, curr_sp);

		set_ia32_am_support(push, ia32_am_Source, ia32_am_unary);

		sched_add_before(irn, push);

		// create stackpointer proj
		curr_sp = new_r_Proj(irg, block, push, spmode, pn_ia32_Push_stack);
		arch_set_irn_register(cg->arch_env, curr_sp, spreg);

		// create memory proj
		mem_proj = new_r_Proj(irg, block, push, mode_M, pn_ia32_Push_M);

		// use the memproj now
		exchange(store, mem_proj);

		// we can remove the store now
		sched_remove(store);

		offset -= 4;
	}

	be_set_IncSP_offset(irn, offset);

	// can we remove the IncSP now?
	if(offset == 0) {
		const ir_edge_t *edge, *next;

		foreach_out_edge_safe(irn, edge, next) {
			ir_node *arg = get_edge_src_irn(edge);
			int pos = get_edge_src_pos(edge);

			set_irn_n(arg, pos, curr_sp);
		}

		set_irn_n(irn, 0, new_Bad());
		sched_remove(irn);
	} else {
		set_irn_n(irn, 0, curr_sp);
	}
}

/**
 * Tries to optimize two following IncSP.
 */
static void ia32_optimize_IncSP(ir_node *node)
{
	int      pred_offs;
	int      curr_offs;
	int      offs;
	ir_node *pred = be_get_IncSP_pred(node);
	ir_node *predpred;

	if(!be_is_IncSP(pred))
		return;

	if(get_irn_n_edges(pred) > 1)
		return;

	pred_offs = be_get_IncSP_offset(pred);
	curr_offs = be_get_IncSP_offset(node);

	if(pred_offs == BE_STACK_FRAME_SIZE_EXPAND) {
		if(curr_offs != BE_STACK_FRAME_SIZE_SHRINK) {
			return;
		}
		offs = 0;
	} else if(pred_offs == BE_STACK_FRAME_SIZE_SHRINK) {
		if(curr_offs != BE_STACK_FRAME_SIZE_EXPAND) {
			return;
		}
		offs = 0;
	} else if(curr_offs == BE_STACK_FRAME_SIZE_EXPAND
			|| curr_offs == BE_STACK_FRAME_SIZE_SHRINK) {
		return;
	} else {
		offs = curr_offs + pred_offs;
	}

	be_set_IncSP_offset(node, offs);

	/* rewire dependency edges */
	predpred = be_get_IncSP_pred(pred);
	edges_reroute_kind(pred, predpred, EDGE_KIND_DEP, current_ir_graph);

	/* Omit the IncSP */
	be_set_IncSP_pred(node, predpred);
	sched_remove(pred);
	be_kill_node(pred);
}

/**
 * Performs Peephole Optimizations.
 */
static void ia32_peephole_optimize_node(ir_node *node, void *env) {
	ia32_code_gen_t *cg = env;

	if (be_is_IncSP(node)) {
		ia32_optimize_IncSP(node);

		if (cg->opt & IA32_OPT_PUSHARGS)
			ia32_create_Pushs(node, cg);
	}
}

void ia32_peephole_optimization(ir_graph *irg, ia32_code_gen_t *cg)
{
	be_peephole_opt(cg->birg);
	irg_walk_graph(irg, ia32_peephole_optimize_node, NULL, cg);
}

/******************************************************************
 *              _     _                   __  __           _
 *     /\      | |   | |                 |  \/  |         | |
 *    /  \   __| | __| |_ __ ___  ___ ___| \  / | ___   __| | ___
 *   / /\ \ / _` |/ _` | '__/ _ \/ __/ __| |\/| |/ _ \ / _` |/ _ \
 *  / ____ \ (_| | (_| | | |  __/\__ \__ \ |  | | (_) | (_| |  __/
 * /_/    \_\__,_|\__,_|_|  \___||___/___/_|  |_|\___/ \__,_|\___|
 *
 ******************************************************************/

/**
 * Removes node from schedule if it is not used anymore. If irn is a mode_T node
 * all it's Projs are removed as well.
 * @param irn  The irn to be removed from schedule
 */
static INLINE void try_kill(ir_node *node)
{
	if(get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge, *next;
		foreach_out_edge_safe(node, edge, next) {
			ir_node *proj = get_edge_src_irn(edge);
			try_kill(proj);
		}
	}

	if(get_irn_n_edges(node) != 0)
		return;

	if (sched_is_scheduled(node)) {
		sched_remove(node);
	}

	be_kill_node(node);
}

static void optimize_conv_store(ir_node *node)
{
	ir_node *pred;
	ir_mode *conv_mode;
	ir_mode *store_mode;

	if(!is_ia32_Store(node) && !is_ia32_Store8Bit(node))
		return;

	pred = get_irn_n(node, 2);
	if(!is_ia32_Conv_I2I(pred) && !is_ia32_Conv_I2I8Bit(pred))
		return;

	/* the store only stores the lower bits, so we only need the conv
	 * it it shrinks the mode */
	conv_mode  = get_ia32_ls_mode(pred);
	store_mode = get_ia32_ls_mode(node);
	if(get_mode_size_bits(conv_mode) < get_mode_size_bits(store_mode))
		return;

	set_irn_n(node, 2, get_irn_n(pred, 2));
	if(get_irn_n_edges(pred) == 0) {
		be_kill_node(pred);
	}
}

static void optimize_load_conv(ir_node *node)
{
	ir_node *pred, *predpred;
	ir_mode *load_mode;
	ir_mode *conv_mode;

	if (!is_ia32_Conv_I2I(node) && !is_ia32_Conv_I2I8Bit(node))
		return;

	pred = get_irn_n(node, 2);
	if(!is_Proj(pred))
		return;

	predpred = get_Proj_pred(pred);
	if(!is_ia32_Load(predpred))
		return;

	/* the load is sign extending the upper bits, so we only need the conv
	 * if it shrinks the mode */
	load_mode = get_ia32_ls_mode(predpred);
	conv_mode = get_ia32_ls_mode(node);
	if(get_mode_size_bits(conv_mode) < get_mode_size_bits(load_mode))
		return;

	if(get_mode_sign(conv_mode) != get_mode_sign(load_mode)) {
		/* change the load if it has only 1 user */
		if(get_irn_n_edges(pred) == 1) {
			ir_mode *newmode;
			if(get_mode_sign(conv_mode)) {
				newmode = find_signed_mode(load_mode);
			} else {
				newmode = find_unsigned_mode(load_mode);
			}
			assert(newmode != NULL);
			set_ia32_ls_mode(predpred, newmode);
		} else {
			/* otherwise we have to keep the conv */
			return;
		}
	}

	/* kill the conv */
	exchange(node, pred);
}

static void optimize_conv_conv(ir_node *node)
{
	ir_node *pred_proj, *pred, *result_conv;
	ir_mode *pred_mode, *conv_mode;

	if (!is_ia32_Conv_I2I(node) && !is_ia32_Conv_I2I8Bit(node))
		return;

	assert(n_ia32_Conv_I2I_val == n_ia32_Conv_I2I8Bit_val);
	pred_proj = get_irn_n(node, n_ia32_Conv_I2I_val);
	if(is_Proj(pred_proj))
		pred = get_Proj_pred(pred_proj);
	else
		pred = pred_proj;

	if(!is_ia32_Conv_I2I(pred) && !is_ia32_Conv_I2I8Bit(pred))
		return;

	/* we know that after a conv, the upper bits are sign extended
	 * so we only need the 2nd conv if it shrinks the mode */
	conv_mode = get_ia32_ls_mode(node);
	pred_mode = get_ia32_ls_mode(pred);
	/* if 2nd conv is smaller then first conv, then we can always take the 2nd
	 * conv */
	if(get_mode_size_bits(conv_mode) <= get_mode_size_bits(pred_mode)) {
		if(get_irn_n_edges(pred_proj) == 1) {
			result_conv = pred_proj;
			set_ia32_ls_mode(pred, conv_mode);

			/* Argh:We must change the opcode to 8bit AND copy the register constraints */
			if (get_mode_size_bits(conv_mode) == 8) {
				set_irn_op(pred, op_ia32_Conv_I2I8Bit);
				set_ia32_in_req_all(pred, get_ia32_in_req_all(node));
			}
		} else {
			/* TODO: construct syncs/stuff here but we'll probably end up with
			 * 2 statements anyway */
			if(get_irn_mode(pred) == mode_T) {
				return;
			}

			result_conv = exact_copy(pred);
			set_ia32_ls_mode(result_conv, conv_mode);

			/* Argh:We must change the opcode to 8bit AND copy the register constraints */
			if (get_mode_size_bits(conv_mode) == 8) {
				set_irn_op(result_conv, op_ia32_Conv_I2I8Bit);
				set_ia32_in_req_all(result_conv, get_ia32_in_req_all(node));
			}
		}
	} else {
		/* if both convs have the same sign, then we can take the smaller one */
		if(get_mode_sign(conv_mode) == get_mode_sign(pred_mode)) {
			result_conv = pred_proj;
		} else {
			/* no optimisation possible if smaller conv is sign-extend */
			if(mode_is_signed(pred_mode)) {
				return;
			}
			/* we can take the smaller conv if it is unsigned */
			result_conv = pred_proj;
		}
	}

	/* kill the conv */
	exchange(node, result_conv);

	if(get_irn_n_edges(pred) == 0) {
		be_kill_node(pred);
	}
	optimize_conv_conv(result_conv);
}

static void optimize_node(ir_node *node, void *env)
{
	(void) env;

	optimize_load_conv(node);
	optimize_conv_store(node);
	optimize_conv_conv(node);
}

/**
 * Performs conv and address mode optimization.
 */
void ia32_optimize_graph(ia32_code_gen_t *cg)
{
	irg_walk_blkwise_graph(cg->irg, NULL, optimize_node, cg);

	if (cg->dump)
		be_dump(cg->irg, "-opt", dump_ir_block_graph_sched);
}

void ia32_init_optimize(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.optimize");
}
