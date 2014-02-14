/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend ABI implementation.
 * @author      Sebastian Hack, Michael Beck
 */
#include <stdbool.h>

#include "obst.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irgopt.h"
#include "iropt_t.h"
#include "irtools.h"
#include "heights.h"
#include "util.h"
#include "raw_bitset.h"
#include "error.h"
#include "pset_new.h"
#include "irmemory_t.h"

#include "be.h"
#include "beabi.h"
#include "beabihelper.h"
#include "bearch.h"
#include "benode.h"
#include "belive_t.h"
#include "besched.h"
#include "beirg.h"
#include "bessaconstr.h"
#include "bemodule.h"
#include "betranshlp.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct be_abi_call_arg_t {
	bool is_res   : 1;  /**< true:  the call argument is a return value.
	                         false: it's a call parameter. */
	bool in_reg   : 1;  /**< true: this argument is transmitted in registers,
	                         false: on stack. */
	bool callee   : 1;  /**< true: someone called us.
	                         false: We call another function */

	int                    pos;
	const arch_register_t *reg;
	ir_entity             *stack_ent;
	ir_mode               *load_mode;
	unsigned               alignment;    /**< stack alignment */
	unsigned               space_before; /**< allocate space before */
	unsigned               space_after;  /**< allocate space after */
} be_abi_call_arg_t;

struct be_abi_call_t {
	be_abi_call_flags_t       flags;  /**< Flags describing the ABI behavior on
	                                       calls */
	int                       pop;    /**< number of bytes the stack frame is
	                                       shrinked by the callee on return. */
	const be_abi_callbacks_t *cb;
	set                      *params;
};

/**
 * The ABI information for the current graph.
 */
struct be_abi_irg_t {
	be_abi_call_t *call;     /**< The ABI call information. */
	ir_node       *init_sp;  /**< The node representing the stack pointer
	                              at the start of the function. */
	pmap          *regs;     /**< A map of all callee-save and ignore regs to
	                              their Projs to the RegParams node. */
	pmap          *keep_map; /**< mapping blocks to keep nodes. */
	ir_node      **calls;    /**< flexible array containing all be_Call nodes */
};

static ir_heights_t *ir_heights;

static ir_node *be_abi_reg_map_get(pmap *map, const arch_register_t *reg)
{
	return pmap_get(ir_node, map, reg);
}

static void be_abi_reg_map_set(pmap *map, const arch_register_t* reg,
                               ir_node *node)
{
	pmap_insert(map, reg, node);
}

/**
 * Check if the given register is callee save, i.e. will be saved by the callee.
 */
static bool arch_register_is_callee_save(const arch_env_t *arch_env,
                                         const arch_register_t *reg)
{
	if (arch_env->impl->register_saved_by)
		return arch_env->impl->register_saved_by(reg, /*callee=*/1);
	return false;
}

/**
 * Check if the given register is caller save, i.e. must be saved by the caller.
 */
static bool arch_register_is_caller_save(const arch_env_t *arch_env,
                                         const arch_register_t *reg)
{
	if (arch_env->impl->register_saved_by)
		return arch_env->impl->register_saved_by(reg, /*callee=*/0);
	return false;
}



/*
     _    ____ ___    ____      _ _ _                _
    / \  | __ )_ _|  / ___|__ _| | | |__   __ _  ___| | _____
   / _ \ |  _ \| |  | |   / _` | | | '_ \ / _` |/ __| |/ / __|
  / ___ \| |_) | |  | |__| (_| | | | |_) | (_| | (__|   <\__ \
 /_/   \_\____/___|  \____\__,_|_|_|_.__/ \__,_|\___|_|\_\___/

  These callbacks are used by the backend to set the parameters
  for a specific call type.
*/

/**
 * Set compare function: compares two ABI call object arguments.
 */
static int cmp_call_arg(const void *a, const void *b, size_t n)
{
	(void) n;
	const be_abi_call_arg_t *p = (const be_abi_call_arg_t*)a;
	const be_abi_call_arg_t *q = (const be_abi_call_arg_t*)b;
	return !(p->is_res == q->is_res && p->pos == q->pos && p->callee == q->callee);
}

/**
 * Get  an ABI call object argument.
 *
 * @param call      the abi call
 * @param is_res    true for call results, false for call arguments
 * @param pos       position of the argument
 * @param callee    context type - if we are callee or caller
 */
static be_abi_call_arg_t *get_call_arg(be_abi_call_t *call, int is_res, int pos,
                                       int callee)
{
	be_abi_call_arg_t arg;
	memset(&arg, 0, sizeof(arg));
	arg.is_res = is_res;
	arg.pos    = pos;
	arg.callee = callee;

	unsigned hash = is_res * 128 + pos;
	return set_find(be_abi_call_arg_t, call->params, &arg, sizeof(arg), hash);
}

/**
 * Set an ABI call object argument.
 */
static void remember_call_arg(be_abi_call_arg_t *arg, be_abi_call_t *call,
                              be_abi_context_t context)
{
	unsigned hash = arg->is_res * 128 + arg->pos;
	if (context & ABI_CONTEXT_CALLEE) {
		arg->callee = 1;
		(void)set_insert(be_abi_call_arg_t, call->params, arg, sizeof(*arg), hash);
	}
	if (context & ABI_CONTEXT_CALLER) {
		arg->callee = 0;
		(void)set_insert(be_abi_call_arg_t, call->params, arg, sizeof(*arg), hash);
	}
}

/* Set the flags for a call. */
void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags,
                           const be_abi_callbacks_t *cb)
{
	call->flags = flags;
	call->cb    = cb;
}

/* Sets the number of bytes the stackframe is shrinked by the callee on return */
void be_abi_call_set_pop(be_abi_call_t *call, int pop)
{
	assert(pop >= 0);
	call->pop = pop;
}

void be_abi_call_param_stack(be_abi_call_t *call, int arg_pos,
                             ir_mode *load_mode, unsigned alignment,
                             unsigned space_before, unsigned space_after,
                             be_abi_context_t context)
{
	assert(alignment > 0 && "Alignment must be greater than 0");
	be_abi_call_arg_t arg;
	memset(&arg, 0, sizeof(arg));
	arg.load_mode    = load_mode;
	arg.alignment    = alignment;
	arg.space_before = space_before;
	arg.space_after  = space_after;
	arg.is_res       = 0;
	arg.pos          = arg_pos;

	remember_call_arg(&arg, call, context);
}

void be_abi_call_param_reg(be_abi_call_t *call, int arg_pos, const arch_register_t *reg, be_abi_context_t context)
{
	be_abi_call_arg_t arg;
	memset(&arg, 0, sizeof(arg));
	arg.in_reg = 1;
	arg.reg    = reg;
	arg.is_res = 0;
	arg.pos    = arg_pos;

	remember_call_arg(&arg, call, context);
}

void be_abi_call_res_reg(be_abi_call_t *call, int arg_pos, const arch_register_t *reg, be_abi_context_t context)
{
	be_abi_call_arg_t arg;
	memset(&arg, 0, sizeof(arg));
	arg.in_reg = 1;
	arg.reg    = reg;
	arg.is_res = 1;
	arg.pos    = arg_pos;

	remember_call_arg(&arg, call, context);
}

/* Get the flags of a ABI call object. */
be_abi_call_flags_t be_abi_call_get_flags(const be_abi_call_t *call)
{
	return call->flags;
}

/**
 * Constructor for a new ABI call object.
 *
 * @return the new ABI call object
 */
static be_abi_call_t *be_abi_call_new(void)
{
	be_abi_call_t *call = XMALLOCZ(be_abi_call_t);
	call->params            = new_set(cmp_call_arg, 16);
	call->cb                = NULL;
	call->flags.try_omit_fp = be_options.omit_fp;

	return call;
}

/**
 * Destructor for an ABI call object.
 */
static void be_abi_call_free(be_abi_call_t *call)
{
	del_set(call->params);
	free(call);
}

/**
 * Initializes the frame layout from parts
 *
 * @param frame     the stack layout that will be initialized
 * @param args      the stack argument layout type
 * @param between   the between layout type
 * @param locals    the method frame type
 *
 * @return the initialized stack layout
 */
static be_stack_layout_t *stack_frame_init(be_stack_layout_t *frame, ir_type *args,
                                           ir_type *between, ir_type *locals)
{
	frame->arg_type       = args;
	frame->between_type   = between;
	frame->frame_type     = locals;
	frame->initial_offset = 0;
	frame->initial_bias   = 0;
	frame->order[1]       = between;

	/* typical decreasing stack: locals have the
	 * lowest addresses, arguments the highest */
	frame->order[0] = locals;
	frame->order[2] = args;
	return frame;
}

/*
   ____      _ _
  / ___|__ _| | |___
 | |   / _` | | / __|
 | |__| (_| | | \__ \
  \____\__,_|_|_|___/

  Adjustment of the calls inside a graph.

*/

/**
 * Transform a call node into a be_Call node.
 *
 * @param env The ABI environment for the current irg.
 * @param irn The call node.
 * @param curr_sp The stack pointer node to use.
 * @return The stack pointer after the call.
 */
static ir_node *adjust_call(be_abi_irg_t *env, ir_node *irn, ir_node *curr_sp)
{
	ir_graph         *const irg       = get_irn_irg(irn);
	const arch_env_t *const arch_env  = be_get_irg_arch_env(irg);
	ir_type                *call_tp   = get_Call_type(irn);
	ir_node                *call_ptr  = get_Call_ptr(irn);
	size_t            const n_params  = get_method_n_params(call_tp);
	ir_node          *      curr_mem  = get_Call_mem(irn);
	ir_node          *const bl        = get_nodes_block(irn);
	const arch_register_t  *sp        = arch_env->sp;
	be_abi_call_t    *const call      = be_abi_call_new();
	ir_mode          *const mach_mode = sp->reg_class->mode;
	size_t            const n_res     = get_method_n_ress(call_tp);

	/* Let the isa fill out the abi description for that call node. */
	arch_env_get_call_abi(arch_env, call_tp, call);

	/* Insert code to put the stack arguments on the stack. */
	assert((size_t)get_Call_n_params(irn) == n_params);
	int *const stack_param_idx = ALLOCAN(int, n_params);
	int        stack_size      = 0;
	int        n_stack_params  = 0;
	for (size_t p = 0; p < n_params; ++p) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, p, 0);
		assert(arg);
		if (!arg->in_reg) {
			int arg_size = get_type_size_bytes(get_method_param_type(call_tp, p));

			stack_size += round_up2(arg->space_before, arg->alignment);
			stack_size += round_up2(arg_size, arg->alignment);
			stack_size += round_up2(arg->space_after, arg->alignment);

			stack_param_idx[n_stack_params++] = p;
		}
	}

	/* Collect all arguments which are passed in registers. */
	int *const reg_param_idxs = ALLOCAN(int, n_params);
	int        n_reg_params   = 0;
	for (size_t p = 0; p < n_params; ++p) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, p, 0);
		if (arg && arg->in_reg) {
			reg_param_idxs[n_reg_params++] = p;
		}
	}

	/*
	 * If the stack is decreasing and we do not want to store sequentially,
	 * or someone else allocated the call frame
	 * we allocate as much space on the stack all parameters need, by
	 * moving the stack pointer along the stack's direction.
	 *
	 * Note: we also have to do this for stack_size == 0, because we may have
	 * to adjust stack alignment for the call.
	 */
	curr_sp = be_new_IncSP(sp, bl, curr_sp, stack_size, 1);

	dbg_info *dbgi = get_irn_dbg_info(irn);
	/* If there are some parameters which shall be passed on the stack. */
	if (n_stack_params > 0) {
		int       curr_ofs = 0;
		ir_node **in       = ALLOCAN(ir_node*, n_stack_params+1);
		unsigned  n_in     = 0;

		curr_mem = get_Call_mem(irn);
		in[n_in++] = curr_mem;

		for (int i = 0; i < n_stack_params; ++i) {
			int p                  = stack_param_idx[i];
			be_abi_call_arg_t *arg = get_call_arg(call, 0, p, 0);
			ir_node *param         = get_Call_param(irn, p);
			ir_node *addr          = curr_sp;
			ir_node *mem           = NULL;
			ir_type *param_type    = get_method_param_type(call_tp, p);
			int param_size         = get_type_size_bytes(param_type) + arg->space_after;

			/*
			 * If we wanted to build the arguments sequentially,
			 * the stack pointer for the next must be incremented,
			 * and the memory value propagated.
			 */
			curr_ofs += arg->space_before;
			curr_ofs =  round_up2(curr_ofs, arg->alignment);

			/* Make the expression to compute the argument's offset. */
			if (curr_ofs > 0) {
				ir_mode *constmode = mach_mode;
				if (mode_is_reference(mach_mode)) {
					constmode = mode_Is;
				}
				addr = new_r_Const_long(irg, constmode, curr_ofs);
				addr = new_r_Add(bl, curr_sp, addr, mach_mode);
			}

			/* Insert a store for primitive arguments. */
			if (is_atomic_type(param_type)) {
				ir_node *nomem     = get_irg_no_mem(irg);
				ir_node *mem_input = nomem;
				ir_node *store     = new_rd_Store(dbgi, bl, mem_input, addr, param, cons_none);
				mem   = new_r_Proj(store, mode_M, pn_Store_M);
			} else {
				/* Make a mem copy for compound arguments. */
				assert(mode_is_reference(get_irn_mode(param)));

				bool is_volatile = is_partly_volatile(addr) ||
					is_partly_volatile(param);

				mem = new_rd_CopyB(dbgi, bl, curr_mem, addr, param, param_type, is_volatile ? cons_volatile : cons_none);
			}

			curr_ofs += param_size;

			in[n_in++] = mem;
		}

		/* We need the sync only, if we didn't build the stores sequentially. */
		if (n_stack_params >= 1) {
			curr_mem = new_r_Sync(bl, n_in, in);
		} else {
			curr_mem = get_Call_mem(irn);
		}
	}

	/* Put caller save into the destroyed set and state registers in the states
	 * set */
	const arch_register_t **states = NEW_ARR_F(const arch_register_t*, 0);
	const arch_register_t **destroyed_regs = NEW_ARR_F(const arch_register_t*, 0);
	for (int i = 0, n = arch_env->n_register_classes; i < n; ++i) {
		const arch_register_class_t *cls = &arch_env->register_classes[i];
		for (unsigned j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = arch_register_for_index(cls, j);

			/* even if destroyed all is specified, neither SP nor FP are
			 * destroyed (else bad things will happen) */
			if (reg == arch_env->sp || reg == arch_env->bp)
				continue;

			if (reg->type & arch_register_type_state) {
				ARR_APP1(const arch_register_t*, destroyed_regs, reg);
				ARR_APP1(const arch_register_t*, states, reg);
				/* we're already in the destroyed set so no need for further
				 * checking */
				continue;
			}
			if (arch_register_is_caller_save(arch_env, reg))
				ARR_APP1(const arch_register_t*, destroyed_regs, reg);
		}
	}

	/* search the largest result proj number */
	ir_node **const res_projs = ALLOCANZ(ir_node*, n_res);
	ir_node  *      res_proj  = NULL;
	foreach_out_edge(irn, edge) {
		ir_node *irn = get_edge_src_irn(edge);

		if (!is_Proj(irn) || get_Proj_proj(irn) != pn_Call_T_result)
			continue;

		foreach_out_edge(irn, res_edge) {
			ir_node *const res  = get_edge_src_irn(res_edge);
			long     const proj = get_Proj_proj(res);
			assert(proj < (long)n_res);
			assert(res_projs[proj] == NULL);
			res_projs[proj] = res;
		}
		res_proj = irn;
		break;
	}

	/** TODO: this is not correct for cases where return values are passed
	 * on the stack, but no known ABI does this currently...
	 */
	int n_reg_results = n_res;

	int       n_ins = 0;
	ir_node **in    = ALLOCAN(ir_node*, n_reg_params + ARR_LEN(states));

	/* make the back end call node and set its register requirements. */
	for (int i = 0; i < n_reg_params; ++i) {
		in[n_ins++] = get_Call_param(irn, reg_param_idxs[i]);
	}

	/* add state registers ins */
	for (size_t s = 0; s < ARR_LEN(states); ++s) {
		const arch_register_t       *reg = states[s];
		const arch_register_class_t *cls = reg->reg_class;
		ir_node *regnode = new_r_Unknown(irg, cls->mode);
		in[n_ins++]      = regnode;
	}
	assert(n_ins == (int) (n_reg_params + ARR_LEN(states)));

	/* ins collected, build the call */
	ir_node *low_call;
	if (env->call->flags.call_has_imm && is_Address(call_ptr)) {
		/* direct call */
		low_call = be_new_Call(dbgi, bl, curr_mem, sp->single_req, curr_sp,
		                       sp->single_req, curr_sp,
		                       n_reg_results + pn_be_Call_first_res + ARR_LEN(destroyed_regs),
		                       n_ins, in, get_Call_type(irn));
		be_Call_set_entity(low_call, get_Address_entity(call_ptr));
	} else {
		/* indirect call */
		low_call = be_new_Call(dbgi, bl, curr_mem, sp->single_req, curr_sp,
		                       sp->reg_class->class_req, call_ptr,
		                       n_reg_results + pn_be_Call_first_res + ARR_LEN(destroyed_regs),
		                       n_ins, in, get_Call_type(irn));
	}
	int throws_exception = ir_throws_exception(irn);
	ir_set_throws_exception(low_call, throws_exception);
	be_Call_set_pop(low_call, call->pop);

	/* put the call into the list of all calls for later processing */
	ARR_APP1(ir_node *, env->calls, low_call);

	/* create new stack pointer */
	curr_sp = new_r_Proj(low_call, get_irn_mode(curr_sp), pn_be_Call_sp);
	be_set_constr_single_reg_out(low_call, pn_be_Call_sp, sp,
			arch_register_req_type_ignore | arch_register_req_type_produces_sp);
	arch_set_irn_register(curr_sp, sp);

	/* now handle results */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node           *proj = res_projs[i];
		be_abi_call_arg_t *arg  = get_call_arg(call, 1, i, 0);

		/* returns values on stack not supported yet */
		assert(arg->in_reg);

		/*
			shift the proj number to the right, since we will drop the
			unspeakable Proj_T from the Call. Therefore, all real argument
			Proj numbers must be increased by pn_be_Call_first_res
		*/
		long pn = i + pn_be_Call_first_res;

		if (proj == NULL) {
			ir_type *res_type = get_method_res_type(call_tp, i);
			ir_mode *mode     = get_type_mode(res_type);
			proj              = new_r_Proj(low_call, mode, pn);
			res_projs[i]      = proj;
		} else {
			set_Proj_pred(proj, low_call);
			set_Proj_proj(proj, pn);
		}

		if (arg->in_reg) {
			/* remove register from destroyed regs */
			for (size_t j = 0, n = ARR_LEN(destroyed_regs); j < n; ++j) {
				if (destroyed_regs[j] == arg->reg) {
					destroyed_regs[j] = destroyed_regs[n-1];
					ARR_SHRINKLEN(destroyed_regs,n-1);
					break;
				}
			}
		}
	}

	DBG((dbg, LEVEL_3, "\tcreated backend call %+F\n", low_call));

	/* Set the register classes and constraints of the Call parameters. */
	for (int i = 0; i < n_reg_params; ++i) {
		int index = reg_param_idxs[i];
		be_abi_call_arg_t *arg = get_call_arg(call, 0, index, 0);
		assert(arg->reg != NULL);

		be_set_constr_single_reg_in(low_call, n_be_Call_first_arg + i,
		                            arg->reg, arch_register_req_type_none);
	}

	/* Set the register constraints of the results. */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node                 *proj = res_projs[i];
		const be_abi_call_arg_t *arg  = get_call_arg(call, 1, i, 0);
		int                      pn   = get_Proj_proj(proj);

		assert(arg->in_reg);
		be_set_constr_single_reg_out(low_call, pn, arg->reg,
		                             arch_register_req_type_none);
		arch_set_irn_register(proj, arg->reg);
	}
	exchange(irn, low_call);

	/* kill the ProjT node */
	if (res_proj != NULL) {
		kill_node(res_proj);
	}

	/* Make additional projs for the caller save registers
	   and the Keep node which keeps them alive. */
	int       max_keep_ins = ARR_LEN(destroyed_regs) + n_reg_results + 1;
	ir_node **keep_in      = ALLOCAN(ir_node *, max_keep_ins);
	int       n            = 0;

	/* also keep the stack pointer */
	set_irn_link(curr_sp, (void*) sp);
	keep_in[n++] = curr_sp;

	int curr_res_proj = pn_be_Call_first_res + n_reg_results;
	for (size_t d = 0; d < ARR_LEN(destroyed_regs); ++d) {
		const arch_register_t *reg = destroyed_regs[d];
		ir_node *proj = new_r_Proj(low_call, reg->reg_class->mode, curr_res_proj);

		/* memorize the register in the link field. we need afterwards to set the register class of the keep correctly. */
		be_set_constr_single_reg_out(low_call, curr_res_proj, reg,
									 arch_register_req_type_none);
		arch_set_irn_register(proj, reg);

		set_irn_link(proj, (void*) reg);
		keep_in[n++] = proj;
		++curr_res_proj;
	}

	for (int i = 0; i < n_reg_results; ++i) {
		ir_node *proj = res_projs[i];
		const arch_register_t *reg = arch_get_irn_register(proj);
		set_irn_link(proj, (void*) reg);
		keep_in[n++] = proj;
	}
	assert(n <= max_keep_ins);

	/* create the Keep for the caller save registers */
	ir_node *keep = be_new_Keep(bl, n, keep_in);
	for (int i = 0; i < n; ++i) {
		const arch_register_t *reg = (const arch_register_t*)get_irn_link(keep_in[i]);
		be_node_set_reg_class_in(keep, i, reg->reg_class);
	}

	/* Clean up the stack. */
	assert(stack_size >= call->pop);
	stack_size -= call->pop;

	if (stack_size > 0) {
		ir_node *mem_proj = NULL;

		foreach_out_edge(low_call, edge) {
			ir_node *irn = get_edge_src_irn(edge);
			if (is_Proj(irn) && get_Proj_proj(irn) == pn_Call_M) {
				mem_proj = irn;
				break;
			}
		}

		if (mem_proj == NULL) {
			mem_proj = new_r_Proj(low_call, mode_M, pn_be_Call_M);
			keep_alive(mem_proj);
		}
	}
	/* Clean up the stack frame or revert alignment fixes if we allocated it */
	curr_sp = be_new_IncSP(sp, bl, curr_sp, -stack_size, 0);

	be_abi_call_free(call);

	DEL_ARR_F(states);
	DEL_ARR_F(destroyed_regs);

	return curr_sp;
}

/**
 * Adjust the size of a node representing a stack alloc or free for the minimum stack alignment.
 *
 * @param alignment  the minimum stack alignment
 * @param size       the node containing the non-aligned size
 * @param block      the block where new nodes are allocated on
 * @param dbg        debug info for new nodes
 *
 * @return a node representing the aligned size
 */
static ir_node *adjust_alloc_size(unsigned stack_alignment, ir_node *size,
                                  ir_node *block, dbg_info *dbg)
{
	assert(is_po2(stack_alignment));
	if (stack_alignment <= 1)
		return size;

	ir_mode   *mode = get_irn_mode(size);
	ir_tarval *tv   = new_tarval_from_long(stack_alignment-1, mode);
	ir_graph  *irg  = get_Block_irg(block);
	ir_node   *mask = new_r_Const(irg, tv);
	size = new_rd_Add(dbg, block, size, mask, mode);
	tv   = new_tarval_from_long(-(long)stack_alignment, mode);
	mask = new_r_Const(irg, tv);
	size = new_rd_And(dbg, block, size, mask, mode);
	return size;
}
/**
 * Adjust an alloca.
 * The alloca is transformed into a back end alloca node and connected to the
 * stack nodes.
 */
static ir_node *adjust_alloc(be_abi_irg_t *env, ir_node *alloc, ir_node *curr_sp)
{
	ir_node          *block     = get_nodes_block(alloc);
	ir_graph         *irg       = get_Block_irg(block);
	const arch_env_t *arch_env  = be_get_irg_arch_env(irg);

	ir_node *alloc_mem = NULL;
	ir_node *alloc_res = NULL;
	foreach_out_edge(alloc, edge) {
		ir_node *irn = get_edge_src_irn(edge);

		switch (get_Proj_proj(irn)) {
		case pn_Alloc_M:
			alloc_mem = irn;
			break;
		case pn_Alloc_res:
			alloc_res = irn;
			break;
		default:
			break;
		}
	}

	/* Beware: currently Alloc nodes without a result might happen,
	   only escape analysis kills them and this phase runs only for object
	   oriented source. We kill the Alloc here. */
	if (alloc_res == NULL && alloc_mem) {
		exchange(alloc_mem, get_Alloc_mem(alloc));
		return curr_sp;
	}

	dbg_info *dbg  = get_irn_dbg_info(alloc);
	ir_node  *size = get_Alloc_size(alloc);

	/* The stack pointer will be modified in an unknown manner.
	   We cannot omit it. */
	env->call->flags.try_omit_fp = 0;

	unsigned stack_alignment = 1 << arch_env->stack_alignment;
	size = adjust_alloc_size(stack_alignment, size, block, dbg);
	ir_node *new_alloc = be_new_AddSP(arch_env->sp, block, curr_sp, size);
	set_irn_dbg_info(new_alloc, dbg);

	if (alloc_mem != NULL) {
		ir_node *addsp_mem = new_r_Proj(new_alloc, mode_M, pn_be_AddSP_M);

		/* We need to sync the output mem of the AddSP with the input mem
		   edge into the alloc node. */
		ir_node *ins[] = { get_Alloc_mem(alloc), addsp_mem };
		ir_node *sync  = new_r_Sync(block, ARRAY_SIZE(ins), ins);

		exchange(alloc_mem, sync);
	}

	exchange(alloc, new_alloc);

	/* fix projnum of alloca res */
	set_Proj_proj(alloc_res, pn_be_AddSP_res);

	curr_sp = new_r_Proj(new_alloc,  get_irn_mode(curr_sp), pn_be_AddSP_sp);

	return curr_sp;
}

/**
 * Adjust a Free.
 * The Free is transformed into a back end free node and connected to the stack nodes.
 */
static ir_node *adjust_free(be_abi_irg_t *env, ir_node *free, ir_node *curr_sp)
{
#if 0
	/* we might need to multiply the size with the element size */
	ir_type  *type  = get_Free_type(free);
	ir_node  *block = get_nodes_block(free);
	ir_graph *irg   = get_irn_irg(free);
	dbg_info *dbg   = get_irn_dbg_info(free);
	ir_node  *size;
	if (!is_unknown_type(type) && get_type_size_bytes(type) != 1) {
		ir_tarval *tv   = new_tarval_from_long(get_type_size_bytes(type), mode_Iu);
		ir_node   *cnst = new_rd_Const(dbg, irg, tv);
		ir_node   *mul  = new_rd_Mul(dbg, block, get_Free_count(free),
		                             cnst, mode_Iu);
		size = mul;
	} else {
		size = get_Free_count(free);
	}

	const arch_env_t *arch_env        = be_get_irg_arch_env(irg);
	unsigned          stack_alignment = 1 << arch_env->stack_alignment;
	size = adjust_alloc_size(stack_alignment, size, block, dbg);

	/* The stack pointer will be modified in an unknown manner.
	   We cannot omit it. */
	env->call->flags.try_omit_fp = 0;
	ir_node *subsp = be_new_SubSP(arch_env->sp, block, curr_sp, size);
	set_irn_dbg_info(subsp, dbg);

	ir_node *mem     = new_r_Proj(subsp, mode_M, pn_be_SubSP_M);
	ir_mode *sp_mode = arch_env->sp->reg_class->mode;
	ir_node *res     = new_r_Proj(subsp, sp_mode, pn_be_SubSP_sp);

	/* we need to sync the memory */
	ir_node *in[] = { get_Free_mem(free), mem };
	ir_node *sync = new_r_Sync(block, ARRAY_SIZE(in), in);

	/* and make the AddSP dependent on the former memory */
	add_irn_dep(subsp, get_Free_mem(free));

	/* kill the free */
	exchange(free, sync);
	curr_sp = res;
	return curr_sp;
#endif
	(void)env;
	(void)free;
	(void)curr_sp;
	panic("beabi: Free nodes do not work properly yet");
}

/**
 * Check if a node is somehow data dependent on another one.
 * both nodes must be in the same basic block.
 * @param n1 The first node.
 * @param n2 The second node.
 * @return 1, if n1 is data dependent (transitively) on n2, 0 if not.
 */
static bool dependent_on(ir_node *n1, ir_node *n2)
{
	assert(get_nodes_block(n1) == get_nodes_block(n2));
	return heights_reachable_in_block(ir_heights, n1, n2);
}

static int cmp_call_dependency(const void *c1, const void *c2)
{
	/* Classical qsort() comparison function behavior:
	 *  0 if both elements are equal
	 *  1 if second is "smaller" that first
	 * -1 if first is "smaller" that second
	 */
	ir_node *n1 = *(ir_node **) c1;
	ir_node *n2 = *(ir_node **) c2;
	if (dependent_on(n1, n2))
		return -1;

	if (dependent_on(n2, n1))
		return 1;

	/* The nodes have no depth order, but we need a total order because qsort()
	 * is not stable.
	 *
	 * Additionally, we need to respect transitive dependencies. Consider a
	 * Call a depending on Call b and an independent Call c.
	 * We MUST NOT order c > a and b > c. */
	unsigned h1 = get_irn_height(ir_heights, n1);
	unsigned h2 = get_irn_height(ir_heights, n2);
	if (h1 < h2) return -1;
	if (h1 > h2) return  1;
	/* Same height, so use a random (but stable) order */
	return get_irn_idx(n1) - get_irn_idx(n2);
}

/**
 * Walker: links all Call/Alloc/Free nodes to the Block they are contained.
 */
static void link_ops_in_block_walker(ir_node *irn, void *data)
{
	be_abi_irg_t *env  = (be_abi_irg_t*)data;
	unsigned      code = get_irn_opcode(irn);

	if (code == iro_Call || code == iro_Alloc || code == iro_Free) {
		ir_node *bl       = get_nodes_block(irn);
		void *save        = get_irn_link(bl);

		set_irn_link(irn, save);
		set_irn_link(bl, irn);
	}

	if (code == iro_Builtin && get_Builtin_kind(irn) == ir_bk_return_address) {
		ir_node       *param = get_Builtin_param(irn, 0);
		ir_tarval     *tv    = get_Const_tarval(param);
		unsigned long  value = get_tarval_long(tv);
		/* use ebp, so the climbframe algo works... */
		if (value > 0) {
			env->call->flags.try_omit_fp = 0;
		}
	}
}

/**
 * Block-walker:
 * Process all Call/Alloc/Free nodes inside a basic block.
 * Note that the link field of the block must contain a linked list of all
 * nodes inside the Block. We first order this list according to data dependency
 * and that connect the nodes together.
 */
static void process_ops_in_block(ir_node *bl, void *data)
{
	int n_nodes = 0;
	for (ir_node *irn = (ir_node*)get_irn_link(bl); irn != NULL;
	     irn = (ir_node*)get_irn_link(irn)) {
		++n_nodes;
	}

	ir_node **nodes = ALLOCAN(ir_node*, n_nodes);
	int       n     = 0;
	for (ir_node *irn = (ir_node*)get_irn_link(bl); irn != NULL;
	     irn = (ir_node*)get_irn_link(irn)) {
		nodes[n++] = irn;
	}

	/* If there were call nodes in the block. */
	be_abi_irg_t *env     = (be_abi_irg_t*)data;
	ir_node      *curr_sp = env->init_sp;
	if (n > 0) {
		ir_node *keep;
		int i;

		/* order the call nodes according to data dependency */
		qsort(nodes, n_nodes, sizeof(nodes[0]), cmp_call_dependency);

		for (i = n_nodes - 1; i >= 0; --i) {
			ir_node *irn = nodes[i];

			DBG((dbg, LEVEL_3, "\tprocessing call %+F\n", irn));
			switch (get_irn_opcode(irn)) {
			case iro_Call:
				curr_sp = adjust_call(env, irn, curr_sp);
				break;
			case iro_Alloc:
				curr_sp = adjust_alloc(env, irn, curr_sp);
				break;
			case iro_Free:
				curr_sp = adjust_free(env, irn, curr_sp);
				break;
			default:
				panic("invalid call");
			}
		}

		/* Keep the last stack state in the block by tying it to Keep node,
		 * the proj from calls is already kept */
		if (curr_sp != env->init_sp &&
		    !(is_Proj(curr_sp) && be_is_Call(get_Proj_pred(curr_sp)))) {
			nodes[0] = curr_sp;
			keep     = be_new_Keep(bl, 1, nodes);
			pmap_insert(env->keep_map, bl, keep);
		}
	}

	set_irn_link(bl, curr_sp);
}

/**
 * Adjust all call nodes in the graph to the ABI conventions.
 */
static void process_calls(ir_graph *const irg, be_abi_irg_t *const abi)
{
	irg_walk_graph(irg, firm_clear_link, link_ops_in_block_walker, abi);

	ir_heights = heights_new(irg);
	irg_block_walk_graph(irg, NULL, process_ops_in_block, abi);
	heights_free(ir_heights);
}

/**
 * Computes the stack argument layout type.
 * Changes a possibly allocated value param type by moving
 * entities to the stack layout type.
 *
 * @param call          the current call ABI
 * @param method_type   the method type
 *
 * @return the stack argument layout type
 */
static ir_type *compute_arg_type(ir_graph *irg, be_abi_call_t *call,
								 ir_type *method_type)
{
	struct obstack *obst = be_get_be_obst(irg);
	ir_type   *frame_type      = get_irg_frame_type(irg);
	size_t     n_params        = get_method_n_params(method_type);
	ir_entity *va_start_entity = NULL;

	ir_entity **map = OALLOCNZ(obst, ir_entity*, n_params);
	ir_type    *res = new_type_struct(new_id_from_chars("arg_type", 8));

	/* collect existing entities for value_param_types */
	for (size_t f = get_compound_n_members(frame_type); f-- > 0; ) {
		ir_entity *entity = get_compound_member(frame_type, f);
		set_entity_link(entity, NULL);
		if (!is_parameter_entity(entity))
			continue;

		size_t num = get_entity_parameter_number(entity);
		if (num == IR_VA_START_PARAMETER_NUMBER) {
			/* move entity to new arg_type */
			set_entity_owner(entity, res);
			va_start_entity = entity;
			continue;
		}
		assert(num < n_params);
		if (map[num] != NULL)
			panic("multiple entities for parameter %u in %+F found", f, irg);

		if (num != n_params && get_call_arg(call, 0, num, 1)->in_reg) {
			/* don't move this entity */
			continue;
		}

		map[num] = entity;
		/* move entity to new arg_type */
		set_entity_owner(entity, res);
	}

	int ofs = 0;
	for (size_t i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg        = get_call_arg(call, 0, i, 1);
		ir_type           *param_type = get_method_param_type(method_type, i);
		if (arg->in_reg)
			continue;

		ir_entity *entity = map[i];
		if (entity == NULL) {
			/* create a new entity */
			entity = new_parameter_entity(res, i, param_type);
		}
		ofs += arg->space_before;
		ofs = round_up2(ofs, arg->alignment);
		set_entity_offset(entity, ofs);
		ofs += arg->space_after;
		ofs += get_type_size_bytes(param_type);
		arg->stack_ent = entity;
	}
	if (va_start_entity != NULL) {
		set_entity_offset(va_start_entity, ofs);
	}
	set_type_size_bytes(res, ofs);
	set_type_state(res, layout_fixed);

	return res;
}

static int cmp_regs(const void *a, const void *b)
{
	arch_register_t const *const p = *(arch_register_t const**)a;
	arch_register_t const *const q = *(arch_register_t const**)b;

	return p->reg_class == q->reg_class
		? p->index - q->index
		: p->reg_class < q->reg_class ? -1 : +1;
}

static void reg_map_to_arr(arch_register_t const **const res,
                           pmap *const reg_map)
{
	size_t i = 0;
	foreach_pmap(reg_map, ent) {
		res[i++] = (arch_register_t const*)ent->key;
	}

	size_t n = pmap_count(reg_map);
	qsort(res, n, sizeof(res[0]), cmp_regs);
}

/**
 * Creates a be_Return for a Return node.
 *
 * @param @env  the abi environment
 * @param irn   the Return node
 */
static ir_node *create_be_return(be_abi_irg_t *const env, ir_node *const irn)
{
	ir_node          *const bl = get_nodes_block(irn);
	be_abi_call_t    *call     = env->call;
	ir_graph         *irg      = get_Block_irg(bl);
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);

	/* get the valid stack node in this block.
	 * If we had a call in that block there is a Keep constructed by
	 * process_calls() which points to the last stack modification in that
	 * block. we'll use it then. Else we use the stack from the start block
	 * and let the ssa construction fix the usage. */
	ir_node *stack = be_abi_reg_map_get(env->regs, arch_env->sp);
	ir_node *keep  = pmap_get(ir_node, env->keep_map, bl);
	if (keep) {
		stack = get_irn_n(keep, 0);
		kill_node(keep);
		remove_End_keepalive(get_irg_end(irg), keep);
	}

	/* Insert results for Return into the register map. */
	int const n_res   = get_Return_n_ress(irn);
	pmap     *reg_map = pmap_create();
	for (int i = 0; i < n_res; ++i) {
		ir_node *res           = get_Return_res(irn, i);
		be_abi_call_arg_t *arg = get_call_arg(call, 1, i, 1);
		assert(arg->in_reg && "return value must be passed in register");
		pmap_insert(reg_map, (void *) arg->reg, res);
	}

	/* Add uses of the callee save registers. */
	foreach_pmap(env->regs, ent) {
		const arch_register_t *reg = (const arch_register_t*)ent->key;
		if (arch_register_is_callee_save(arch_env, reg))
			pmap_insert(reg_map, ent->key, ent->value);
	}

	be_abi_reg_map_set(reg_map, arch_env->sp, stack);

	/*
		Maximum size of the in array for Return nodes is
		return args + callee save/ignore registers + memory + stack pointer
	*/
	size_t in_max = pmap_count(reg_map) + n_res + 2;

	ir_node **in = ALLOCAN(ir_node*,               in_max);
	const arch_register_t **regs = ALLOCAN(arch_register_t const*, in_max);

	in[0]   = get_Return_mem(irn);
	in[1]   = be_abi_reg_map_get(reg_map, arch_env->sp);
	regs[0] = NULL;
	regs[1] = arch_env->sp;
	int n   = 2;

	/* clear SP entry, since it has already been grown. */
	pmap_insert(reg_map, (void *) arch_env->sp, NULL);
	for (int i = 0; i < n_res; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 1, i, 1);

		in[n]     = be_abi_reg_map_get(reg_map, arg->reg);
		regs[n++] = arg->reg;

		/* Clear the map entry to mark the register as processed. */
		be_abi_reg_map_set(reg_map, arg->reg, NULL);
	}

	/* grow the rest of the stuff. */
	foreach_pmap(reg_map, ent) {
		if (ent->value) {
			in[n]     = (ir_node*)ent->value;
			regs[n++] = (const arch_register_t*)ent->key;
		}
	}

	/* The in array for the new back end return is now ready. */
	dbg_info *const dbgi = get_irn_dbg_info(irn);
	ir_node  *const ret  = be_new_Return(dbgi, bl, n_res, call->pop, n, in);

	/* Set the register classes of the return's parameter accordingly. */
	for (int i = 0; i < n; ++i) {
		if (regs[i] == NULL)
			continue;

		be_set_constr_single_reg_in(ret, i, regs[i],
		                            arch_register_req_type_none);
	}

	/* Free the space of the Epilog's in array and the register <-> proj map. */
	pmap_destroy(reg_map);

	return ret;
}

typedef struct lower_frame_sels_env_t {
	ir_node      *frame;                     /**< the current frame */
	const arch_register_class_t *sp_class;   /**< register class of the stack pointer */
} lower_frame_sels_env_t;

/**
 * Walker: Replaces Sels of frame type and
 * value param type entities by FrameAddress.
 * Links all used entities.
 */
static void lower_frame_sels_walker(ir_node *irn, void *data)
{
	lower_frame_sels_env_t *ctx = (lower_frame_sels_env_t*)data;

	if (is_Sel(irn)) {
		ir_node *ptr = get_Sel_ptr(irn);

		if (ptr == ctx->frame) {
			ir_entity *ent = get_Sel_entity(irn);
			ir_node   *bl  = get_nodes_block(irn);
			ir_node   *nw
				= be_new_FrameAddr(ctx->sp_class, bl, ctx->frame, ent);
			exchange(irn, nw);
		}
	}
}

/**
 * The start block has no jump, instead it has an initial exec Proj.
 * The backend wants to handle all blocks the same way, so we replace
 * the out cfg edge with a real jump.
 */
static void fix_start_block(ir_graph *irg)
{
	ir_node *initial_X   = get_irg_initial_exec(irg);
	ir_node *start_block = get_irg_start_block(irg);
	ir_node *jmp         = new_r_Jmp(start_block);

	assert(is_Proj(initial_X));
	exchange(initial_X, jmp);
	set_irg_initial_exec(irg, new_r_Bad(irg, mode_X));

	/* merge start block with successor if possible */
	foreach_out_edge(jmp, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		if (!is_Block(succ))
			continue;

		if (get_irn_arity(succ) == 1) {
			exchange(succ, start_block);
		}
		break;
	}
}

/**
 * Modify the irg itself and the frame type.
 */
static void modify_irg(ir_graph *const irg, be_abi_irg_t *const env)
{
	be_abi_call_t         *call         = env->call;
	const arch_env_t      *arch_env     = be_get_irg_arch_env(irg);
	const arch_register_t *sp           = arch_env->sp;
	ir_type               *method_type  = get_entity_type(get_irg_entity(irg));
	be_irg_t              *birg         = be_birg_from_irg(irg);
	struct obstack        *obst         = be_get_be_obst(irg);
	be_stack_layout_t     *stack_layout = be_get_irg_stack_layout(irg);

	DBG((dbg, LEVEL_1, "introducing abi on %+F\n", irg));

	ir_node *const old_mem = get_irg_initial_mem(irg);

	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	ir_type *const arg_type = compute_arg_type(irg, call, method_type);

	/* Convert the Sel nodes in the irg to frame addr nodes: */
	lower_frame_sels_env_t ctx;
	ctx.frame    = get_irg_frame(irg);
	ctx.sp_class = arch_env->sp->reg_class;

	ir_type *const frame_tp = get_irg_frame_type(irg);
	/* layout the stackframe now */
	if (get_type_state(frame_tp) == layout_undefined) {
		default_layout_compound_type(frame_tp);
	}

	/* align stackframe */
	unsigned const alignment  = 1U << arch_env->stack_alignment;
	unsigned const frame_size = round_up2(get_type_size_bytes(frame_tp),
	                                      alignment);
	set_type_size_bytes(frame_tp, frame_size);

	env->regs  = pmap_create();

	size_t    const n_params = get_method_n_params(method_type);
	ir_node **const args     = OALLOCNZ(obst, ir_node*, n_params);

	be_add_parameter_entity_stores(irg);

	irg_walk_graph(irg, lower_frame_sels_walker, NULL, &ctx);

	irp_free_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* Fill the argument vector */
	ir_node *const arg_tuple = get_irg_args(irg);
	foreach_out_edge(arg_tuple, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		if (! is_Anchor(irn)) {
			int nr       = get_Proj_proj(irn);
			args[nr]     = irn;
			DBG((dbg, LEVEL_2, "\treating arg: %d -> %+F\n", nr, irn));
		}
	}

	stack_layout->sp_relative = call->flags.try_omit_fp;
	ir_type *const bet_type = call->cb->get_between_type(irg);
	stack_frame_init(stack_layout, arg_type, bet_type,
	                 get_irg_frame_type(irg));

	/* Count the register params and add them to the number of Projs for the RegParams node */
	for (size_t i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i, 1);
		if (arg->in_reg && args[i]) {
			assert(arg->reg != sp && "cannot use stack pointer as parameter register");
			assert((int)i == get_Proj_proj(args[i]));

			/* For now, associate the register with the old Proj from Start representing that argument. */
			pmap_insert(env->regs, (void *) arg->reg, args[i]);
			DBG((dbg, LEVEL_2, "\targ #%d -> reg %s\n", i, arg->reg->name));
		}
	}

	/* Collect all callee-save registers */
	for (size_t i = 0, n = arch_env->n_register_classes; i < n; ++i) {
		const arch_register_class_t *cls = &arch_env->register_classes[i];
		for (unsigned j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = &cls->regs[j];
			if ((reg->type & arch_register_type_state) || arch_register_is_callee_save(arch_env, reg)) {
				pmap_insert(env->regs, (void *) reg, NULL);
			}
		}
	}

	const arch_register_t *fp_reg
		= call->flags.try_omit_fp ? arch_env->sp : arch_env->bp;
	rbitset_clear(birg->allocatable_regs, fp_reg->global_index);

	/* handle start block here (place a jump in the block) */
	fix_start_block(irg);

	pmap_insert(env->regs, (void *) sp, NULL);
	pmap_insert(env->regs, (void *) arch_env->bp, NULL);
	ir_node *const start_bl = get_irg_start_block(irg);
	ir_node *const start
		= be_new_Start(NULL, start_bl, pmap_count(env->regs) + 1);
	set_irg_start(irg, start);

	/*
	 * make proj nodes for the callee save registers.
	 * memorize them, since Return nodes get those as inputs.
	 *
	 * Note, that if a register corresponds to an argument, the regs map
	 * contains the old Proj from start for that argument.
	 */
	arch_register_t const **const regs
		= ALLOCAN(arch_register_t const*, pmap_count(env->regs));
	reg_map_to_arr(regs, env->regs);
	for (size_t i = 0, n = pmap_count(env->regs); i < n; ++i) {
		const arch_register_t    *reg      = regs[i];
		ir_mode                  *mode     = reg->reg_class->mode;
		long                      nr       = i;
		arch_register_req_type_t  add_type = arch_register_req_type_none;

		if (reg == sp)
			add_type |= arch_register_req_type_produces_sp;
		if (!rbitset_is_set(birg->allocatable_regs, reg->global_index)) {
			add_type |= arch_register_req_type_ignore;
		}

		assert(nr >= 0);
		ir_node *const proj = new_r_Proj(start, mode, nr + 1);
		pmap_insert(env->regs, (void *) reg, proj);
		be_set_constr_single_reg_out(start, nr + 1, reg, add_type);
		arch_set_irn_register(proj, reg);

		DBG((dbg, LEVEL_2, "\tregister save proj #%d -> reg %s\n", nr, reg->name));
	}

	/* create a new initial memory proj */
	assert(is_Proj(old_mem));
	arch_set_irn_register_req_out(start, 0, arch_no_register_req);
	ir_node *const mem = new_r_Proj(start, mode_M, 0);
	set_irg_initial_mem(irg, mem);

	env->init_sp = be_abi_reg_map_get(env->regs, sp);

	/* set new frame_pointer */
	ir_node *const frame_pointer = be_abi_reg_map_get(env->regs, fp_reg);
	set_irg_frame(irg, frame_pointer);

	/* rewire old mem users to new mem */
	exchange(old_mem, mem);

	/* keep the mem (for functions with an endless loop = no return) */
	keep_alive(mem);

	set_irg_initial_mem(irg, mem);

	/* Now, introduce stack param nodes for all parameters passed on the stack */
	for (size_t i = 0; i < n_params; ++i) {
		ir_node *arg_proj = args[i];
		if (arg_proj == NULL)
			continue;

		int nr = get_Proj_proj(arg_proj);
		nr     = MIN(nr, (int)n_params);
		be_abi_call_arg_t *arg = get_call_arg(call, 0, nr, 1);
		ir_type *const param_type = get_method_param_type(method_type, nr);

		ir_node *repl;
		if (arg->in_reg) {
			repl = pmap_get(ir_node, env->regs, arg->reg);
		} else {
			ir_node *addr = be_new_FrameAddr(sp->reg_class, start_bl, frame_pointer, arg->stack_ent);

			/* For atomic parameters which are actually used, we create a Load
			 * node. */
			if (is_atomic_type(param_type) && get_irn_n_edges(args[i]) > 0) {
				ir_mode *mode      = get_type_mode(param_type);
				ir_mode *load_mode = arg->load_mode;
				ir_node *nomem     = get_irg_no_mem(irg);

				ir_node *load = new_r_Load(start_bl, nomem, addr, load_mode, cons_floats);
				repl = new_r_Proj(load, load_mode, pn_Load_res);

				if (mode != load_mode) {
					repl = new_r_Conv(start_bl, repl, mode);
				}
			} else {
				/* The stack parameter is not primitive (it is a struct or
				 * array), we thus will create a node representing the
				 * parameter's address on the stack. */
				repl = addr;
			}
		}

		assert(repl != NULL);

		/* Beware: the mode of the register parameters is always the mode of
		 * the register class which may be wrong. Add Conv's then. */
		ir_mode *mode = get_irn_mode(args[i]);
		if (mode != get_irn_mode(repl)) {
			repl = new_r_Conv(get_nodes_block(repl), repl, mode);
		}
		exchange(args[i], repl);
	}

	/* the arg proj is not needed anymore now and should be only used by the
	 * anchor */
	assert(get_irn_n_edges(arg_tuple) == 1);
	kill_node(arg_tuple);
	set_irg_args(irg, new_r_Bad(irg, mode_T));

	/* All Return nodes hang on the End node, so look for them there. */
	ir_node *const end = get_irg_end_block(irg);
	for (size_t i = 0, n = get_Block_n_cfgpreds(end); i < n; ++i) {
		ir_node *irn = get_Block_cfgpred(end, i);

		if (is_Return(irn)) {
			ir_node *const ret = create_be_return(env, irn);
			exchange(irn, ret);
		}
	}

	/* if we have endless loops here, n might be <= 0. Do NOT create a
	 * be_Return then, the code is dead and will never be executed. */
}

/** Fix the state inputs of calls that still hang on unknowns */
static void fix_call_state_inputs(ir_graph *const irg, be_abi_irg_t *const env)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);
	arch_register_t **stateregs = NEW_ARR_F(arch_register_t*, 0);

	/* Collect caller save registers */
	for (int i = 0, n = arch_env->n_register_classes; i < n; ++i) {
		const arch_register_class_t *cls = &arch_env->register_classes[i];
		for (unsigned j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = arch_register_for_index(cls, j);
			if (reg->type & arch_register_type_state) {
				ARR_APP1(arch_register_t*, stateregs, (arch_register_t *)reg);
			}
		}
	}

	size_t n_states = ARR_LEN(stateregs);
	for (size_t i = 0, n = ARR_LEN(env->calls); i < n; ++i) {
		ir_node *call  = env->calls[i];
		int      arity = get_irn_arity(call);

		/* the state reg inputs are the last n inputs of the calls */
		for (size_t s = 0; s < n_states; ++s) {
			int                    inp = arity - n_states + s;
			const arch_register_t *reg = stateregs[s];
			ir_node *regnode = be_abi_reg_map_get(env->regs, reg);

			set_irn_n(call, inp, regnode);
		}
	}

	DEL_ARR_F(stateregs);
}

void be_abi_introduce(ir_graph *irg)
{
	be_timer_push(T_ABI);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	ir_node          *const old_frame   = get_irg_frame(irg);
	const arch_env_t *const arch_env    = be_get_irg_arch_env(irg);
	ir_entity        *const entity      = get_irg_entity(irg);
	ir_type          *const method_type = get_entity_type(entity);
	be_irg_t         *const birg        = be_birg_from_irg(irg);
	struct obstack   *const obst        = &birg->obst;

	/* determine allocatable registers */
	assert(birg->allocatable_regs == NULL);
	birg->allocatable_regs = rbitset_obstack_alloc(obst, arch_env->n_registers);
	for (unsigned r = 0; r < arch_env->n_registers; ++r) {
		const arch_register_t *reg = &arch_env->registers[r];
		if ( !(reg->type & arch_register_type_ignore)) {
			rbitset_set(birg->allocatable_regs, r);
		}
	}

	/* Break here if backend provides a custom API. */

	be_abi_irg_t env;
	env.keep_map     = pmap_create();
	env.call         = be_abi_call_new();
	arch_env_get_call_abi(arch_env, method_type, env.call);

	ir_node *const dummy = new_r_Dummy(irg, arch_env->sp->reg_class->mode);
	env.init_sp = dummy;
	env.calls   = NEW_ARR_F(ir_node*, 0);

	/* Lower all call nodes in the IRG. */
	process_calls(irg, &env);

	/* Process the IRG */
	modify_irg(irg, &env);

	/* fix call inputs for state registers */
	fix_call_state_inputs(irg, &env);

	be_abi_call_free(env.call);

	/* We don't need the keep map anymore. */
	pmap_destroy(env.keep_map);

	/* calls array is not needed anymore */
	DEL_ARR_F(env.calls);

	/* reroute the stack origin of the calls to the true stack origin. */
	exchange(dummy, env.init_sp);
	exchange(old_frame, get_irg_frame(irg));

	pmap_destroy(env.regs);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);

	be_timer_pop(T_ABI);
}

unsigned be_get_n_allocatable_regs(const ir_graph *irg,
                                   const arch_register_class_t *cls)
{
	unsigned *const bs = rbitset_alloca(cls->n_regs);
	be_get_allocatable_regs(irg, cls, bs);
	return rbitset_popcount(bs, cls->n_regs);
}

void be_get_allocatable_regs(ir_graph const *const irg, arch_register_class_t const *const cls, unsigned *const raw_bitset)
{
	be_irg_t *birg             = be_birg_from_irg(irg);
	unsigned *allocatable_regs = birg->allocatable_regs;

	rbitset_clear_all(raw_bitset, cls->n_regs);
	for (unsigned i = 0; i < cls->n_regs; ++i) {
		const arch_register_t *reg = &cls->regs[i];
		if (rbitset_is_set(allocatable_regs, reg->global_index))
			rbitset_set(raw_bitset, i);
	}
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_abi)
void be_init_abi(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.abi");
}
