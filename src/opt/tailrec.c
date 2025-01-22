/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Tail-recursion call optimization.
 * @date    08.06.2004
 * @author  Michael Beck
 */
#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "ircons_t.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irouts_t.h"
#include "irprog_t.h"
#include "panic.h"
#include "scalar_replace.h"
#include "util.h"
#include <assert.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef enum tail_rec_variants {
	TR_DIRECT,  /**< direct return value, i.e. return func(). */
	TR_ADD,     /**< additive return value, i.e. return x +/- func() */
	TR_MUL,     /**< multiplicative return value, i.e. return x * func()
	                 or return -func() */
	TR_BAD,     /**< any other transformation */
	TR_UNKNOWN, /**< during construction */
} tail_rec_variants;

typedef struct tr_env {
	unsigned           n_tail_calls; /**< number of tail calls found */
	size_t             n_ress;       /**< number of return values */
	tail_rec_variants *variants;     /**< return value variants */
	ir_node           *rets;         /**< list of returns that can be
	                                      transformed */
	ir_node           *start_nodes;  /**< nodes that must be in start block*/
	unsigned           n_parameters;
	ir_node          **parameter_projs;
} tr_env;

/**
 * do the graph reconstruction for tail-recursion elimination
 *
 * @param irg  the graph that will reconstructed
 * @param env  tail recursion environment
 */
static void do_opt_tail_rec(ir_graph *irg, tr_env *env)
{
	unsigned n_tail_calls = env->n_tail_calls;
	assert(n_tail_calls > 0);

	/* the old start block will be used as loop header */
	ir_node *loop_head = get_irg_start_block(irg);

	/* build new start block and move start block nodes into it */
	ir_node *const new_start_block = new_r_Block(irg, 0, NULL);
	for (ir_node *node = env->start_nodes; node != NULL;
	     node = (ir_node*)get_irn_link(node)) {
	    set_nodes_block(node, new_start_block);
	}
	set_irg_start_block(irg, new_start_block);
	ir_node *const start_block_jmp = new_r_Jmp(new_start_block);

	/* build a new header block for the loop we create */
	/* allocate in's for phi and block construction */
	ir_node **loop_head_in = ALLOCAN(ir_node*, n_tail_calls + 1);
	unsigned i = 0;
	loop_head_in[i++] = start_block_jmp;

	/* Create a Jmp for every Return */
	for (ir_node *next, *ret = env->rets; ret != NULL; ret = next) {
		next = (ir_node*)get_irn_link(ret);

		ir_node *ret_block = get_nodes_block(ret);
		loop_head_in[i++] = new_r_Jmp(ret_block);

	}
	assert(i == n_tail_calls + 1);

	/* set block inputs of loop header (former start block) */
	set_irn_in(loop_head, i, loop_head_in);
	/* The new loop may be endless so we need to keep the header block. */
	ir_node *end = get_irg_end(irg);
	add_End_keepalive(end, loop_head);

	/* allocate phi's, position 0 contains the memory phi */
	const ir_node *end_block    = get_irg_end_block(irg);
	ir_node       *calls        = (ir_node*)get_irn_link(end_block);
	unsigned       n_parameters = env->n_parameters;
	ir_node      **phi_in       = ALLOCAN(ir_node*, n_tail_calls+1);

	/* build the memory phi */
	unsigned p = 0;
	phi_in[p++] = get_irg_initial_mem(irg);

	for (ir_node *call = calls; call != NULL;
	     call = (ir_node*)get_irn_link(call)) {
		phi_in[p++] = get_Call_mem(call);
	}
	assert(p == n_tail_calls + 1);

	ir_node *start = get_irg_start(irg);

	ir_node **phis      = ALLOCAN(ir_node*, n_parameters + 1);
	ir_node  *phim      = new_r_Phi_loop(loop_head, n_tail_calls+1, phi_in);
	ir_node  *initial_m = get_irg_initial_mem(irg);
	if (phim == initial_m) {
		/* even if the PhiM was optimized away, we need a self-referencing Phi
		 * because we have create a potentially endless loop. */
		phi_in[0] = initial_m;
		ir_node *dummy = new_r_Dummy(irg, mode_M);
		for (unsigned i = 0; i < n_tail_calls; ++i) {
			phi_in[i+1] = dummy;
		}
		phim = new_r_Phi_loop(loop_head, n_tail_calls+1, phi_in);
		for (unsigned i = 0; i < n_tail_calls; ++i) {
			set_Phi_pred(phim, i+1, phim);
		}
	}
	exchange(initial_m, phim);
	ir_node *new_initial_mem = new_r_Proj(start, mode_M, pn_Start_M);
	set_Phi_pred(phim, 0, new_initial_mem);
	set_irg_initial_mem(irg, new_initial_mem);
	phis[0] = phim;

	/* build the data Phi's */
	const ir_entity *ent       = get_irg_entity(irg);
	const ir_type   *method_tp = get_entity_type(ent);
	if (n_parameters > 0) {
		ir_node ***call_params = ALLOCAN(ir_node**, n_tail_calls);

		/* collect all parameters */
		unsigned i = 0;
		for (ir_node *call = calls; call != NULL;
		     call = (ir_node*)get_irn_link(call)) {
			call_params[i++] = get_Call_param_arr(call);
		}
		assert(i == n_tail_calls);

		/* build new Proj's and Phi's */
		ir_node *args = get_irg_args(irg);
		for (unsigned p = 0; p < n_parameters; ++p) {
			ir_node *parameter_proj = env->parameter_projs[p];
			if (parameter_proj == NULL)
				continue;

			ir_mode *mode = get_irn_mode(parameter_proj);

			phi_in[0] = env->parameter_projs[p];
			for (unsigned j = 0; j < n_tail_calls; ++j)
				phi_in[j + 1] = call_params[j][p];

			ir_node *phi = new_r_Phi(loop_head, n_tail_calls + 1, phi_in, mode);
			if (phi != parameter_proj) {
				exchange(parameter_proj, phi);
				ir_node *new_parameter_proj = new_r_Proj(args, mode, p);
				set_Phi_pred(phi, 0, new_parameter_proj);
			}

			phis[p+1] = phi;
		}
	}

	/* check if we need new values */
	unsigned n_locs = 0;
	for (size_t i = 0, n = env->n_ress; i < n; ++i) {
		if (env->variants[i] != TR_DIRECT) {
			++n_locs;
			break;
		}
	}

	if (n_locs > 0) {
		ir_node **in    = ALLOCAN(ir_node*, env->n_ress);
		ir_mode **modes = ALLOCAN(ir_mode*, env->n_ress);

		ssa_cons_start(irg, env->n_ress);

		ir_node *start_block = get_irg_start_block(irg);
		set_r_cur_block(irg, start_block);

		/* set the neutral elements for the iteration start */
		for (size_t i = 0; i < env->n_ress; ++i) {
			ir_type *tp = get_method_res_type(method_tp, i);
			ir_mode *mode = get_type_mode(tp);

			modes[i] = mode;
			if (env->variants[i] == TR_ADD) {
				set_r_value(irg, i, new_r_Const_null(irg, mode));
			} else if (env->variants[i] == TR_MUL) {
				set_r_value(irg, i, new_r_Const_one(irg, mode));
			}
		}
		mature_immBlock(start_block);

		/* kill all returns behind tail calls */
		ir_node *bad = new_r_Bad(irg, mode_X);
		for (ir_node *next, *ret = env->rets; ret != NULL; ret = next) {
			next = (ir_node*)get_irn_link(ret);

			ir_node *block = get_nodes_block(ret);
			set_r_cur_block(irg, block);

			ir_node *const call = skip_Proj(get_Return_mem(ret));
			ir_node *const mem  = get_Call_mem(call);

			for (size_t i = 0; i < env->n_ress; ++i) {
				ir_mode *mode = modes[i];
				if (env->variants[i] != TR_DIRECT) {
					in[i] = get_r_value(irg, i, mode);
				} else {
					in[i] = new_r_Bad(irg, mode);
				}
			}
			/* create a new tuple for the return values */
			ir_node *tuple = new_r_Tuple(block, env->n_ress, in);

			ir_node *in[pn_Call_max+1] = {
				[pn_Call_M]         = mem,
				[pn_Call_T_result]  = tuple
			};
			int n_in = 2;
			assert(pn_Call_M == 0 && pn_Call_T_result == 1);
			if (ir_throws_exception(call)) {
				in[pn_Call_X_regular] = new_r_Jmp(block);
				in[pn_Call_X_except]  = new_r_Bad(irg, mode_X);
				n_in = 4;
			}
			turn_into_tuple(call, n_in, in);

			for (size_t i = 0; i < env->n_ress; ++i) {
				ir_node *res = get_Return_res(ret, i);
				if (env->variants[i] != TR_DIRECT)
					set_r_value(irg, i, res);
			}

			exchange(ret, bad);
		}

		/* finally fix all other returns */
		for (int i = get_Block_n_cfgpreds(end_block); i-- > 0; ) {
			ir_node *ret = get_Block_cfgpred(end_block, i);
			if (!is_Return(ret))
				continue;

			ir_node *pred_block = get_nodes_block(ret);
			set_r_cur_block(irg, pred_block);
			for (size_t r = 0; r < env->n_ress; ++r) {
				ir_node *pred = get_Return_res(ret, r);

				switch (env->variants[r]) {
				case TR_DIRECT:
					continue;

				case TR_ADD: {
					ir_mode *mode = modes[r];
					ir_node *v    = get_r_value(irg, r, mode);
					if (mode_is_reference(mode)) {
						ir_mode *mode_offset = get_reference_offset_mode(mode);
						v = new_r_Conv(pred_block, v, mode_offset);
					}
					ir_node *add = new_r_Add(pred_block, pred, v);
					set_Return_res(ret, r, add);
					continue;
				}

				case TR_MUL: {
					ir_node *v = get_r_value(irg, r, modes[r]);
					ir_node *n = new_r_Mul(pred_block, v, pred);
					set_Return_res(ret, r, n);
					continue;
				}

				case TR_BAD:
				case TR_UNKNOWN:
					break;
				}
				panic("unexpected tail recursion variant");
			}
		}
		ssa_cons_finish(irg);
	} else {
		/* kill all returns */
		ir_node *bad = new_r_Bad(irg, mode_X);
		for (ir_node *next, *ret = env->rets; ret != NULL; ret = next) {
			next = (ir_node*)get_irn_link(ret);
			exchange(ret, bad);
		}
	}
}

/**
 * Check the lifetime of locals in the given graph.
 * Tail recursion can only be done, if we can prove that
 * the lifetime of locals end with the recursive call.
 * We do this by checking that no address of a local variable is
 * stored or transmitted as an argument to a call.
 *
 * @return non-zero if it's ok to do tail recursion
 */
static bool check_lifetime_of_locals(ir_graph *irg)
{
	ir_type *frame_tp  = get_irg_frame_type(irg);
	ir_node *irg_frame = get_irg_frame(irg);

	foreach_irn_out_r(irg_frame, i, succ) {
		if (is_Member(succ)) {
			/* Check if we have compound arguments.
			   For now, we cannot handle them. */
			if (get_entity_owner(get_Member_entity(succ)) != frame_tp)
				return false;

			if (is_address_taken(succ))
				return false;
		}
	}
	return true;
}

/**
 * Examine irn and detect the recursion variant.
 */
static tail_rec_variants find_variant(ir_node *irn, ir_node *call)
{
	if (skip_Proj(skip_Proj(irn)) == call) {
		/* found it */
		return TR_DIRECT;
	}

	switch (get_irn_opcode(irn)) {
	case iro_Add: {
		/* try additive */
		ir_node           *a          = get_Add_left(irn);
		ir_node           *call_block = get_nodes_block(call);
		tail_rec_variants  va;
		if (get_nodes_block(a) != call_block) {
			/* we are outside, ignore */
			va = TR_UNKNOWN;
		} else {
			va = find_variant(a, call);
			if (va == TR_BAD)
				return TR_BAD;
		}
		ir_node           *b  = get_Add_right(irn);
		tail_rec_variants  vb;
		if (get_nodes_block(b) != call_block) {
			/* we are outside, ignore */
			vb = TR_UNKNOWN;
		} else {
			vb = find_variant(b, call);
			if (vb == TR_BAD)
				return TR_BAD;
		}
		tail_rec_variants res = va == vb         ? va :
		                        va == TR_UNKNOWN ? vb :
		                        vb == TR_UNKNOWN ? va :
		                        TR_BAD;
		if (res == TR_DIRECT || res == TR_ADD)
			return TR_ADD;
		/* not compatible */
		return TR_BAD;
	}

	case iro_Sub: {
		/* try additive, but return value must be left */
		ir_node           *a  = get_Sub_left(irn);
		tail_rec_variants  va;
		if (get_nodes_block(a) != get_nodes_block(call)) {
			/* we are outside, ignore */
			va = TR_UNKNOWN;
		} else {
			va = find_variant(a, call);
			if (va == TR_BAD)
				return TR_BAD;
		}
		ir_node *b = get_Sub_right(irn);
		if (get_nodes_block(b) == get_nodes_block(call)) {
			tail_rec_variants vb = find_variant(b, call);
			if (vb != TR_UNKNOWN)
				return TR_BAD;
		}
		tail_rec_variants res = va;
		if (res == TR_DIRECT || res == TR_ADD)
			return res;
		/* not compatible */
		return TR_BAD;
	}

	case iro_Mul: {
		/* try multiplicative */
		ir_node           *a  = get_Mul_left(irn);
		tail_rec_variants  va;
		if (get_nodes_block(a) != get_nodes_block(call)) {
			/* we are outside, ignore */
			va = TR_UNKNOWN;
		} else {
			va = find_variant(a, call);
			if (va == TR_BAD)
				return TR_BAD;
		}
		ir_node           *b  = get_Mul_right(irn);
		tail_rec_variants  vb;
		if (get_nodes_block(b) != get_nodes_block(call)) {
			/* we are outside, ignore */
			vb = TR_UNKNOWN;
		} else {
			vb = find_variant(b, call);
			if (vb == TR_BAD)
				return TR_BAD;
		}
		tail_rec_variants res = va == vb         ? va :
		                        va == TR_UNKNOWN ? vb :
		                        vb == TR_UNKNOWN ? va :
		                        TR_BAD;
		if (res == TR_DIRECT || res == TR_MUL)
			return TR_MUL;
		/* not compatible */
		return TR_BAD;
	}

	case iro_Minus: {
		/* try multiplicative */
		ir_node           *a   = get_Minus_op(irn);
		tail_rec_variants  res = find_variant(a, call);
		if (res == TR_DIRECT)
			return TR_MUL;
		if (res == TR_MUL || res == TR_UNKNOWN)
			return res;
		/* not compatible */
		return TR_BAD;
	}

	default:
		return TR_UNKNOWN;
	}
}

static void collect_start_block_nodes(ir_node *node, void *data)
{
	tr_env *env = (tr_env*)data;

	ir_node *skipped = node;
	if (is_Proj(node)) {
		ir_node *pred = get_Proj_pred(node);
		if (is_Proj(pred)) {
			ir_node *predpred = get_Proj_pred(pred);
			if (is_Start(predpred) && get_Proj_num(pred) == pn_Start_T_args) {
				unsigned pn = get_Proj_num(node);
				assert(pn < env->n_parameters);
				env->parameter_projs[pn] = node;
			}
		}
		skipped = skip_Proj(pred);
	}

	if (is_irn_start_block_placed(skipped)) {
		set_irn_link(node, env->start_nodes);
		env->start_nodes = node;
	}
}

/**
 * Build a list of all nodes which must be in the start block.
 */
static void find_start_block_nodes(tr_env *env, ir_graph *irg)
{
	ir_entity *entity       = get_irg_entity(irg);
	ir_type   *mtp          = get_entity_type(entity);
	unsigned   n_parameters = get_method_n_params(mtp);
	env->n_parameters       = n_parameters;
	env->parameter_projs    = XMALLOCNZ(ir_node*, n_parameters);

	env->start_nodes = NULL;
	irg_walk_anchors(irg, collect_start_block_nodes, NULL, env);
}

static unsigned find_tail_rec_possibilities(tr_env *env, ir_graph *irg)
{
	if (!check_lifetime_of_locals(irg))
		return 0;

	ir_entity *ent      = get_irg_entity(irg);
	ir_type   *mtd_type = get_entity_type(ent);
	size_t     n_ress   = get_method_n_ress(mtd_type);
	env->variants       = NULL;
	env->n_ress         = n_ress;

	if (n_ress > 0) {
		env->variants = XMALLOCN(tail_rec_variants, n_ress);
		for (size_t i = 0; i < n_ress; ++i)
			env->variants[i] = TR_DIRECT;
	}

	ir_node *end_block    = get_irg_end_block(irg);
	unsigned n_tail_calls = 0;
	ir_node *rets         = NULL;
	set_irn_link(end_block, NULL);

	for (int i = get_Block_n_cfgpreds(end_block); i-- > 0; ) {
		ir_node *ret = get_Block_cfgpred(end_block, i);

		/* search all Returns of a block */
		if (!is_Return(ret))
			continue;

		/* check, if it's a Return self() */
		ir_node *call = skip_Proj(get_Return_mem(ret));
		if (!is_Call(call))
			continue;

		/* the call must be in the same block as the return */
		if (get_nodes_block(call) != get_nodes_block(ret))
			continue;

		/* check if it's a recursive call */
		ir_entity *callee = get_Call_callee(call);
		if (callee == NULL || get_entity_linktime_irg(callee) != irg)
			continue;

		/*
		 * Check, that the types match. At least in C
		 * this might fail.
		 */
		ir_type *call_type = get_Call_type(call);

		if (mtd_type != call_type) {
			/*
			 * Hmm, the types did not match, bad.
			 * This can happen in C when no prototype is given
			 * or K&R style is used.
			 */
			DB((dbg, LEVEL_3, "  tail recursion fails because of call type mismatch: %+F != %+F\n", mtd_type, call_type));
			continue;
		}

		/* ok, mem is routed to a recursive call, check return args */
		ir_node **ress = get_Return_res_arr(ret);
		int       j;
		for (j = get_Return_n_ress(ret); j-- > 0; ) {
			tail_rec_variants var = find_variant(ress[j], call);

			/* cannot be transformed */
			if (var >= TR_BAD)
				break;
			if (var == TR_DIRECT) {
				var = env->variants[j];
			} else if (env->variants[j] == TR_DIRECT) {
				env->variants[j] = var;
			}
			if (env->variants[j] != var) {
				/* not compatible */
				DB((dbg, LEVEL_3, "  tail recursion fails for %d return value of %+F\n", j, ret));
				break;
			}
		}
		if (j >= 0)
			continue;

		/* here, we have found a call */
		set_irn_link(call, get_irn_link(end_block));
		set_irn_link(end_block, call);
		++n_tail_calls;

		/* link all returns, we will need this */
		set_irn_link(ret, rets);
		rets = ret;
	}
	env->n_tail_calls = n_tail_calls;
	env->rets         = rets;
	return n_tail_calls;
}

void opt_tail_rec_irg(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.tailrec");
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_MANY_RETURNS
		| IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	tr_env   env;
	memset(&env, 0, sizeof(env));
	unsigned n_tail_calls = find_tail_rec_possibilities(&env, irg);

	/* now, end_block->link contains the list of all tail calls */
	if (n_tail_calls > 0) {
		find_start_block_nodes(&env, irg);
		DB((dbg, LEVEL_2,
		    "  Performing tail recursion for graph %s and %u Calls\n",
		    get_entity_ld_name(get_irg_entity(irg)), n_tail_calls));
		do_opt_tail_rec(irg, &env);
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
		set_irg_callee_info_state(irg, irg_callee_info_inconsistent);
	} else {
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
	}
	free(env.variants);
	free(env.parameter_projs);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}
