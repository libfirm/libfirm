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
#include <assert.h>

#include "debug.h"
#include "error.h"
#include "iroptimize.h"
#include "scalar_replace.h"
#include "array.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irop.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "ircons.h"
#include "irflag.h"
#include "irouts_t.h"
#include "irhooks.h"
#include "ircons_t.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * the environment for collecting data
 */
typedef struct collect_t {
	ir_node *proj_X;      /**< initial exec proj */
	ir_node *block;       /**< old first block */
	int      blk_idx;     /**< cfgpred index of the initial exec in block */
	ir_node *proj_m;      /**< memory from start proj's */
	ir_node *proj_data;   /**< linked list of all parameter access proj's */
} collect_t;

/**
 * walker for collecting data, fills a collect_t environment
 */
static void collect_data(ir_node *node, void *env)
{
	collect_t *const data = (collect_t *)env;

	switch (get_irn_opcode(node)) {
	case iro_Proj: {
		ir_node   *pred   = get_Proj_pred(node);
		ir_opcode  opcode = (ir_opcode)get_irn_opcode(pred);
		if (opcode == iro_Proj) {
			ir_node *start = get_Proj_pred(pred);

			if (is_Start(start) && get_Proj_proj(pred) == pn_Start_T_args) {
				/* found Proj(ProjT(Start)) */
				set_irn_link(node, data->proj_data);
				data->proj_data = node;
			}
		} else if (opcode == iro_Start
		           && get_Proj_proj(node) == pn_Start_X_initial_exec) {
			/* found ProjX(Start) */
			data->proj_X = node;
		}
		break;
	}
	case iro_Block: {
		for (int i = 0, n_pred = get_Block_n_cfgpreds(node); i < n_pred; ++i) {
			if (get_Block_cfgpred(node, i) == data->proj_X) {
				data->block   = node;
				data->blk_idx = i;
				break;
			}
		}
		break;
	}
	default:
		break;
	}
}

typedef enum tail_rec_variants {
	TR_DIRECT,  /**< direct return value, i.e. return func(). */
	TR_ADD,     /**< additive return value, i.e. return x +/- func() */
	TR_MUL,     /**< multiplicative return value, i.e. return x * func()
	                 or return -func() */
	TR_BAD,     /**< any other transformation */
	TR_UNKNOWN  /**< during construction */
} tail_rec_variants;

typedef struct tr_env {
	int               n_tail_calls; /**< number of tail calls found */
	size_t             n_ress;       /**< number of return values */
	tail_rec_variants *variants;    /**< return value variants */
	ir_node           *rets;        /**< list of returns that can be
	                                     transformed */
} tr_env;


/**
 * do the graph reconstruction for tail-recursion elimination
 *
 * @param irg  the graph that will reconstructed
 * @param env  tail recursion environment
 */
static void do_opt_tail_rec(ir_graph *irg, tr_env *env)
{
	assert(env->n_tail_calls > 0);

	/* we must build some new nodes WITHOUT CSE */
	int rem = get_optimize();
	set_optimize(0);

	/* collect needed data */
	collect_t data = { .proj_X    = NULL,
	                   .block     = NULL,
	                   .blk_idx   = -1,
	                   .proj_m    = get_irg_initial_mem(irg),
	                   .proj_data = NULL };
	irg_walk_graph(irg, NULL, collect_data, &data);

	/* check number of arguments */
	const ir_node *end_block = get_irg_end_block(irg);
	ir_node       *calls     = (ir_node *)get_irn_link(end_block);
	int            n_params  = get_Call_n_params(calls);

	assert(data.proj_X && "Could not find initial exec from Start");
	assert(data.block  && "Could not find first block");
	assert(data.proj_m && "Could not find initial memory");
	assert((data.proj_data || n_params == 0) && "Could not find Proj(ProjT(Start)) of non-void function");

	/* allocate in's for phi and block construction */
	ir_node **in = ALLOCAN(ir_node*, env->n_tail_calls + 1);

	/* build a new header block for the loop we create */
	int i = 0;
	in[i++] = data.proj_X;

	/* turn Return's into Jmp's */
	for (ir_node *next, *p = env->rets; p; p = next) {
		next = (ir_node*)get_irn_link(p);

		ir_node *block = get_nodes_block(p);
		in[i++] = new_r_Jmp(block);

		/* we might generate an endless loop, so add
		 * the block to the keep-alive list */
		add_End_keepalive(get_irg_end(irg), block);
	}
	assert(i == env->n_tail_calls + 1);

	/* now create it */
	ir_node *block = new_r_Block(irg, i, in);
	ir_node *jmp   = new_r_Jmp(block);

	/* the old first block is now the second one */
	set_Block_cfgpred(data.block, data.blk_idx, jmp);

	/* allocate phi's, position 0 contains the memory phi */
	ir_node **phis = ALLOCAN(ir_node*, n_params + 1);

	/* build the memory phi */
	i = 0;
	in[i] = new_r_Proj(get_irg_start(irg), mode_M, pn_Start_M);
	set_irg_initial_mem(irg, in[i]);
	++i;

	for (ir_node *call = calls; call != NULL;
	     call = (ir_node*)get_irn_link(call)) {
		in[i++] = get_Call_mem(call);
	}
	assert(i == env->n_tail_calls + 1);

	phis[0] = new_r_Phi(block, env->n_tail_calls + 1, in, mode_M);

	/* build the data Phi's */
	const ir_entity *ent       = get_irg_entity(irg);
	const ir_type   *method_tp = get_entity_type(ent);
	if (n_params > 0) {
		ir_node ***call_params = ALLOCAN(ir_node**, env->n_tail_calls);

		/* collect all parameters */
		int i = 0;
		for (ir_node *call = calls; call != NULL;
		     call = (ir_node*)get_irn_link(call)) {
			call_params[i++] = get_Call_param_arr(call);
		}

		/* build new Proj's and Phi's */
		ir_node *args         = get_irg_args(irg);
		int      n_tail_calls = env->n_tail_calls;
		for (int p = 0; p < n_params; ++p) {
			ir_mode *mode = get_type_mode(get_method_param_type(method_tp, p));

			in[0] = new_r_Proj(args, mode, p);
			for (int j = 0; j < n_tail_calls; ++j)
				in[j + 1] = call_params[j][p];

			phis[p + 1] = new_r_Phi(block, n_tail_calls + 1, in, mode);
		}
	}

	/*
	 * ok, we are here, so we have build and collected all needed Phi's
	 * now exchange all Projs into links to Phi
	 */
	exchange(data.proj_m, phis[0]);
	for (ir_node *next, *p = data.proj_data; p; p = next) {
		next = (ir_node *)get_irn_link(p);

		long proj = get_Proj_proj(p);
		assert(0 <= proj && proj < n_params);
		exchange(p, phis[proj + 1]);
	}

	set_optimize(rem);

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

		/* no: we can kill all returns */
		for (ir_node *next, *p = env->rets; p; p = next) {
			next = (ir_node*)get_irn_link(p);

			ir_node *block = get_nodes_block(p);
			set_r_cur_block(irg, block);

			ir_node *const call = skip_Proj(get_Return_mem(p));
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
				ir_node *res = get_Return_res(p, i);
				if (env->variants[i] != TR_DIRECT) {
					set_r_value(irg, i, res);
				}
			}

			exchange(p, new_r_Bad(irg, mode_X));
		}

		/* finally fix all other returns */
		end_block = get_irg_end_block(irg);
		for (int i = get_Block_n_cfgpreds(end_block); i-- > 0; ) {
			ir_node *ret = get_Block_cfgpred(end_block, i);

			/* search all Returns of a block */
			if (!is_Return(ret))
				continue;

			ir_node *block = get_nodes_block(ret);
			set_r_cur_block(irg, block);
			for (size_t j = 0; j < env->n_ress; ++j) {
				ir_node *pred = get_Return_res(ret, j);

				switch (env->variants[j]) {
				case TR_DIRECT:
					continue;

				case TR_ADD: {
					ir_node *v = get_r_value(irg, j, modes[j]);
					ir_node *n = new_r_Add(block, v, pred, modes[j]);
					set_Return_res(ret, j, n);
					break;
				}

				case TR_MUL: {
					ir_node *v = get_r_value(irg, j, modes[j]);
					ir_node *n = new_r_Mul(block, v, pred, modes[j]);
					set_Return_res(ret, j, n);
					break;
				}

				default:
					panic("unexpected tail recursion variant");
				}
			}
		}
		ssa_cons_finish(irg);
	} else {
		ir_node *bad = new_r_Bad(irg, mode_X);

		/* no: we can kill all returns */
		for (ir_node *next, *p = env->rets; p; p = next) {
			next = (ir_node*)get_irn_link(p);
			exchange(p, bad);
		}
	}

	/* tail recursion was done, all info is invalid */
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	set_irg_callee_info_state(irg, irg_callee_info_inconsistent);
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


/*
 * convert simple tail-calls into loops
 */
void opt_tail_rec_irg(ir_graph *irg)
{
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_MANY_RETURNS
		| IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	FIRM_DBG_REGISTER(dbg, "firm.opt.tailrec");

	if (!check_lifetime_of_locals(irg)) {
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
		return;
	}

	ir_entity *ent      = get_irg_entity(irg);
	ir_type   *mtd_type = get_entity_type(ent);
	size_t     n_ress   = get_method_n_ress(mtd_type);
	tr_env     env      = { .variants = NULL, .n_ress = n_ress };

	if (n_ress > 0) {
		env.variants = ALLOCAN(tail_rec_variants, n_ress);
		for (size_t i = 0; i < n_ress; ++i)
			env.variants[i] = TR_DIRECT;
	}

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	ir_node *end_block    = get_irg_end_block(irg);
	int      n_tail_calls = 0;
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

			if (var >= TR_BAD) {
				/* cannot be transformed */
				break;
			}
			if (var == TR_DIRECT) {
				var = env.variants[j];
			} else if (env.variants[j] == TR_DIRECT) {
				env.variants[j] = var;
			}
			if (env.variants[j] != var) {
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

	/* now, end_block->link contains the list of all tail calls */
	if (n_tail_calls > 0) {
		DB((dbg, LEVEL_2,
		    "  Performing tail recursion for graph %s and %d Calls\n",
		    get_entity_ld_name(get_irg_entity(irg)), n_tail_calls));

		hook_tail_rec(irg, n_tail_calls);

		env.n_tail_calls = n_tail_calls;
		env.rets         = rets;
		do_opt_tail_rec(irg, &env);
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	} else {
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

/*
 * optimize tail recursion away
 */
void opt_tail_recursion(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.tailrec");

	DB((dbg, LEVEL_1, "Performing tail recursion ...\n"));
	foreach_irp_irg(i, irg) {
		opt_tail_rec_irg(irg);
	}
}
