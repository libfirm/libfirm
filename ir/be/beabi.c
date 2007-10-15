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
 * @brief       Backend ABI implementation.
 * @author      Sebastian Hack, Michael Beck
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "obst.h"
#include "offset.h"

#include "irgopt.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irprintf_t.h"
#include "irgopt.h"
#include "irbitset.h"
#include "height.h"
#include "pdeq.h"
#include "irtools.h"
#include "raw_bitset.h"
#include "error.h"

#include "be.h"
#include "beabi.h"
#include "bearch_t.h"
#include "benode_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirg_t.h"
#include "bessaconstr.h"

typedef struct _be_abi_call_arg_t {
	unsigned is_res   : 1;  /**< 1: the call argument is a return value. 0: it's a call parameter. */
	unsigned in_reg   : 1;  /**< 1: this argument is transmitted in registers. */
	unsigned on_stack : 1;	/**< 1: this argument is transmitted on the stack. */

	int                    pos;
	const arch_register_t *reg;
	ir_entity             *stack_ent;
	ir_mode               *load_mode;
	unsigned               alignment;    /**< stack alignment */
	unsigned               space_before; /**< allocate space before */
	unsigned               space_after;  /**< allocate space after */
} be_abi_call_arg_t;

struct _be_abi_call_t {
	be_abi_call_flags_t          flags;
	int                          pop;
	const be_abi_callbacks_t    *cb;
	ir_type                     *between_type;
	set                         *params;
	const arch_register_class_t *cls_addr;
};

struct _be_abi_irg_t {
	struct obstack       obst;
	be_stack_layout_t    *frame;        /**< The stack frame model. */
	be_irg_t             *birg;         /**< The back end IRG. */
	const arch_isa_t     *isa;          /**< The isa. */
	survive_dce_t        *dce_survivor;

	be_abi_call_t        *call;         /**< The ABI call information. */
	ir_type              *method_type;  /**< The type of the method of the IRG. */

	ir_node              *init_sp;      /**< The node representing the stack pointer
	                                         at the start of the function. */

	ir_node              *reg_params;   /**< The reg params node. */
	pmap                 *regs;         /**< A map of all callee-save and ignore regs to
	                                         their Projs to the RegParams node. */

	int                  start_block_bias; /**< The stack bias at the end of the start block. */

	void                 *cb;           /**< ABI Callback self pointer. */

	pmap                 *keep_map;     /**< mapping blocks to keep nodes. */
	pset                 *ignore_regs;  /**< Additional registers which shall be ignored. */

	ir_node             **calls;        /**< flexible array containing all be_Call nodes */

	arch_register_req_t sp_req;
	arch_register_req_t sp_cls_req;

	DEBUG_ONLY(firm_dbg_module_t    *dbg;)          /**< The debugging module. */
};

static heights_t *ir_heights;

/* Flag: if set, try to omit the frame pointer if called by the backend */
static int be_omit_fp = 1;

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
	const be_abi_call_arg_t *p = a, *q = b;
	(void) n;
	return !(p->is_res == q->is_res && p->pos == q->pos);
}

/**
 * Get or set an ABI call object argument.
 *
 * @param call      the abi call
 * @param is_res    true for call results, false for call arguments
 * @param pos       position of the argument
 * @param do_insert true if the argument is set, false if it's retrieved
 */
static be_abi_call_arg_t *get_or_set_call_arg(be_abi_call_t *call, int is_res, int pos, int do_insert)
{
	be_abi_call_arg_t arg;
	unsigned hash;

	memset(&arg, 0, sizeof(arg));
	arg.is_res = is_res;
	arg.pos    = pos;

	hash = is_res * 128 + pos;

	return do_insert
		? set_insert(call->params, &arg, sizeof(arg), hash)
		: set_find(call->params, &arg, sizeof(arg), hash);
}

/**
 * Retrieve an ABI call object argument.
 *
 * @param call      the ABI call object
 * @param is_res    true for call results, false for call arguments
 * @param pos       position of the argument
 */
static INLINE be_abi_call_arg_t *get_call_arg(be_abi_call_t *call, int is_res, int pos)
{
	return get_or_set_call_arg(call, is_res, pos, 0);
}

/* Set the flags for a call. */
void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags, const be_abi_callbacks_t *cb)
{
	call->flags = flags;
	call->cb    = cb;
}

void be_abi_call_set_pop(be_abi_call_t *call, int pop)
{
	assert(pop >= 0);
	call->pop = pop;
}

/* Set register class for call address */
void be_abi_call_set_call_address_reg_class(be_abi_call_t *call, const arch_register_class_t *cls)
{
	call->cls_addr = cls;
}


void be_abi_call_param_stack(be_abi_call_t *call, int arg_pos, ir_mode *load_mode, unsigned alignment, unsigned space_before, unsigned space_after)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 0, arg_pos, 1);
	arg->on_stack     = 1;
	arg->load_mode    = load_mode;
	arg->alignment    = alignment;
	arg->space_before = space_before;
	arg->space_after  = space_after;
	assert(alignment > 0 && "Alignment must be greater than 0");
}

void be_abi_call_param_reg(be_abi_call_t *call, int arg_pos, const arch_register_t *reg)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 0, arg_pos, 1);
	arg->in_reg = 1;
	arg->reg = reg;
}

void be_abi_call_res_reg(be_abi_call_t *call, int arg_pos, const arch_register_t *reg)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 1, arg_pos, 1);
	arg->in_reg = 1;
	arg->reg = reg;
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
static be_abi_call_t *be_abi_call_new(const arch_register_class_t *cls_addr)
{
	be_abi_call_t *call = xmalloc(sizeof(call[0]));
	memset(call, 0, sizeof(call[0]));

	call->flags.val  = 0;
	call->params     = new_set(cmp_call_arg, 16);
	call->cb         = NULL;
	call->cls_addr   = cls_addr;

	call->flags.bits.try_omit_fp = be_omit_fp;

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

/*
  _____                           _   _                 _ _ _
 |  ___| __ __ _ _ __ ___   ___  | | | | __ _ _ __   __| | (_)_ __   __ _
 | |_ | '__/ _` | '_ ` _ \ / _ \ | |_| |/ _` | '_ \ / _` | | | '_ \ / _` |
 |  _|| | | (_| | | | | | |  __/ |  _  | (_| | | | | (_| | | | | | | (_| |
 |_|  |_|  \__,_|_| |_| |_|\___| |_| |_|\__,_|_| |_|\__,_|_|_|_| |_|\__, |
                                                                    |___/

  Handling of the stack frame. It is composed of three types:
  1) The type of the arguments which are pushed on the stack.
  2) The "between type" which consists of stuff the call of the
     function pushes on the stack (like the return address and
	 the old base pointer for ia32).
  3) The Firm frame type which consists of all local variables
     and the spills.
*/

static int get_stack_entity_offset(be_stack_layout_t *frame, ir_entity *ent, int bias)
{
	ir_type *t = get_entity_owner(ent);
	int ofs    = get_entity_offset(ent);

	int i, index;

	/* Find the type the entity is contained in. */
	for(index = 0; index < N_FRAME_TYPES; ++index) {
		if(frame->order[index] == t)
			break;
	}

	/* Add the size of all the types below the one of the entity to the entity's offset */
	for(i = 0; i < index; ++i)
		ofs += get_type_size_bytes(frame->order[i]);

	/* correct the offset by the initial position of the frame pointer */
	ofs -= frame->initial_offset;

	/* correct the offset with the current bias. */
	ofs += bias;

	return ofs;
}

/**
 * Retrieve the entity with given offset from a frame type.
 */
static ir_entity *search_ent_with_offset(ir_type *t, int offset)
{
	int i, n;

	for(i = 0, n = get_compound_n_members(t); i < n; ++i) {
		ir_entity *ent = get_compound_member(t, i);
		if(get_entity_offset(ent) == offset)
			return ent;
	}

	return NULL;
}

static int stack_frame_compute_initial_offset(be_stack_layout_t *frame)
{
	ir_type  *base = frame->stack_dir < 0 ? frame->between_type : frame->frame_type;
	ir_entity *ent = search_ent_with_offset(base, 0);

	frame->initial_offset = ent ? get_stack_entity_offset(frame, ent, 0) : 0;

	return frame->initial_offset;
}

/**
 * Initializes the frame layout from parts
 *
 * @param frame     the stack layout that will be initialized
 * @param args      the stack argument layout type
 * @param between   the between layout type
 * @param locals    the method frame type
 * @param stack_dir the stack direction
 * @param param_map an array mapping method argument positions to the stack argument type
 *
 * @return the initialized stack layout
 */
static be_stack_layout_t *stack_frame_init(be_stack_layout_t *frame, ir_type *args,
                                           ir_type *between, ir_type *locals, int stack_dir,
                                           ir_entity *param_map[])
{
	frame->arg_type       = args;
	frame->between_type   = between;
	frame->frame_type     = locals;
	frame->initial_offset = 0;
	frame->stack_dir      = stack_dir;
	frame->order[1]       = between;
	frame->param_map      = param_map;

	if(stack_dir > 0) {
		frame->order[0] = args;
		frame->order[2] = locals;
	}
	else {
		frame->order[0] = locals;
		frame->order[2] = args;
	}
	return frame;
}

#if 0
/** Dumps the stack layout to file. */
static void stack_layout_dump(FILE *file, be_stack_layout_t *frame)
{
	int i, j, n;

	ir_fprintf(file, "initial offset: %d\n", frame->initial_offset);
	for (j = 0; j < N_FRAME_TYPES; ++j) {
		ir_type *t = frame->order[j];

		ir_fprintf(file, "type %d: %F size: %d\n", j, t, get_type_size_bytes(t));
		for (i = 0, n = get_compound_n_members(t); i < n; ++i) {
			ir_entity *ent = get_compound_member(t, i);
			ir_fprintf(file, "\t%F int ofs: %d glob ofs: %d\n", ent, get_entity_offset_bytes(ent), get_stack_entity_offset(frame, ent, 0));
		}
	}
}
#endif

/**
 * Returns non-zero if the call argument at given position
 * is transfered on the stack.
 */
static INLINE int is_on_stack(be_abi_call_t *call, int pos)
{
	be_abi_call_arg_t *arg = get_call_arg(call, 0, pos);
	return arg && !arg->in_reg;
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
	ir_graph *irg              = env->birg->irg;
	const arch_env_t *arch_env = env->birg->main_env->arch_env;
	const arch_isa_t *isa      = arch_env->isa;
	ir_type *mt                = get_Call_type(irn);
	ir_node *call_ptr          = get_Call_ptr(irn);
	int n_params               = get_method_n_params(mt);
	ir_node *curr_mem          = get_Call_mem(irn);
	ir_node *bl                = get_nodes_block(irn);
	pset *results              = pset_new_ptr(8);
	pset *caller_save          = pset_new_ptr(8);
	pset *states               = pset_new_ptr(2);
	int stack_size             = 0;
	int stack_dir              = arch_isa_stack_dir(isa);
	const arch_register_t *sp  = arch_isa_sp(isa);
	be_abi_call_t *call        = be_abi_call_new(sp->reg_class);
	ir_mode *mach_mode         = sp->reg_class->mode;
	struct obstack *obst       = &env->obst;
	int no_alloc               = call->flags.bits.frame_is_setup_on_call;
	int n_res                  = get_method_n_ress(mt);

	ir_node *res_proj  = NULL;
	int n_reg_params   = 0;
	int n_stack_params = 0;
	int n_ins;

	ir_node *low_call;
	ir_node **in;
	ir_node **res_projs;
	int      n_reg_results = 0;
	const arch_register_t *reg;
	const ir_edge_t *edge;
	int *reg_param_idxs;
	int *stack_param_idx;
	int i, n;

	/* Let the isa fill out the abi description for that call node. */
	arch_isa_get_call_abi(isa, mt, call);

	/* Insert code to put the stack arguments on the stack. */
	assert(get_Call_n_params(irn) == n_params);
	for (i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		assert(arg);
		if (arg->on_stack) {
			int arg_size = get_type_size_bytes(get_method_param_type(mt, i));

			stack_size += round_up2(arg->space_before, arg->alignment);
			stack_size += round_up2(arg_size, arg->alignment);
			stack_size += round_up2(arg->space_after, arg->alignment);
			obstack_int_grow(obst, i);
			++n_stack_params;
		}
	}
	stack_param_idx = obstack_finish(obst);

	/* Collect all arguments which are passed in registers. */
	for (i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		if (arg && arg->in_reg) {
			obstack_int_grow(obst, i);
			++n_reg_params;
		}
	}
	reg_param_idxs = obstack_finish(obst);

	/* If there are some parameters which shall be passed on the stack. */
	if (n_stack_params > 0) {
		int curr_ofs      = 0;
		int do_seq        = call->flags.bits.store_args_sequential && !no_alloc;

		/*
		 * Reverse list of stack parameters if call arguments are from left to right.
		 * We must them reverse again if they are pushed (not stored) and the stack
		 * direction is downwards.
		 */
		if (call->flags.bits.left_to_right ^ (do_seq && stack_dir < 0)) {
			for (i = 0; i < n_stack_params >> 1; ++i) {
				int other  = n_stack_params - i - 1;
				int tmp    = stack_param_idx[i];
				stack_param_idx[i]     = stack_param_idx[other];
				stack_param_idx[other] = tmp;
			}
		}

		/*
		 * If the stack is decreasing and we do not want to store sequentially,
		 * or someone else allocated the call frame
		 * we allocate as much space on the stack all parameters need, by
		 * moving the stack pointer along the stack's direction.
		 */
		if (stack_dir < 0 && !do_seq && !no_alloc) {
			curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, stack_size);
		}

		curr_mem = get_Call_mem(irn);
		if (! do_seq) {
			obstack_ptr_grow(obst, curr_mem);
		}

		for (i = 0; i < n_stack_params; ++i) {
			int p                  = stack_param_idx[i];
			be_abi_call_arg_t *arg = get_call_arg(call, 0, p);
			ir_node *param         = get_Call_param(irn, p);
			ir_node *addr          = curr_sp;
			ir_node *mem           = NULL;
			ir_type *param_type    = get_method_param_type(mt, p);
			int param_size         = get_type_size_bytes(param_type) + arg->space_after;

			/*
			 * If we wanted to build the arguments sequentially,
			 * the stack pointer for the next must be incremented,
			 * and the memory value propagated.
			 */
			if (do_seq) {
				curr_ofs = 0;
				addr = curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, param_size + arg->space_before);
				add_irn_dep(curr_sp, curr_mem);
			}
			else {
				curr_ofs += arg->space_before;
				curr_ofs =  round_up2(curr_ofs, arg->alignment);

				/* Make the expression to compute the argument's offset. */
				if (curr_ofs > 0) {
					ir_mode *constmode = mach_mode;
					if(mode_is_reference(mach_mode)) {
						constmode = mode_Is;
					}
					addr = new_r_Const_long(irg, bl, constmode, curr_ofs);
					addr = new_r_Add(irg, bl, curr_sp, addr, mach_mode);
				}
			}

			/* Insert a store for primitive arguments. */
			if (is_atomic_type(param_type)) {
				ir_node *store;
				ir_node *mem_input = do_seq ? curr_mem : new_NoMem();
				store = new_r_Store(irg, bl, mem_input, addr, param);
				mem = new_r_Proj(irg, bl, store, mode_M, pn_Store_M);
			}

			/* Make a mem copy for compound arguments. */
			else {
				ir_node *copy;

				assert(mode_is_reference(get_irn_mode(param)));
				copy = new_r_CopyB(irg, bl, curr_mem, addr, param, param_type);
				mem = new_r_Proj(irg, bl, copy, mode_M, pn_CopyB_M_regular);
			}

			curr_ofs += param_size;

			if (do_seq)
				curr_mem = mem;
			else
				obstack_ptr_grow(obst, mem);
		}

		in = (ir_node **) obstack_finish(obst);

		/* We need the sync only, if we didn't build the stores sequentially. */
		if (! do_seq) {
			if (n_stack_params >= 1) {
				curr_mem = new_r_Sync(irg, bl, n_stack_params + 1, in);
			} else {
				curr_mem = get_Call_mem(irn);
			}
		}
		obstack_free(obst, in);
	}

	/* Collect caller save registers */
	for (i = 0, n = arch_isa_get_n_reg_class(isa); i < n; ++i) {
		unsigned j;
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		for (j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = arch_register_for_index(cls, j);
			if (arch_register_type_is(reg, caller_save)) {
				pset_insert_ptr(caller_save, (void *) reg);
			}
			if (arch_register_type_is(reg, state)) {
				pset_insert_ptr(caller_save, (void*) reg);
				pset_insert_ptr(states, (void*) reg);
			}
		}
	}

	/* search the greatest result proj number */

	res_projs = alloca(n_res * sizeof(res_projs[0]));
	memset(res_projs, 0, n_res * sizeof(res_projs[0]));

	foreach_out_edge(irn, edge) {
		const ir_edge_t *res_edge;
		ir_node         *irn = get_edge_src_irn(edge);

		if(!is_Proj(irn) || get_Proj_proj(irn) != pn_Call_T_result)
			continue;

		foreach_out_edge(irn, res_edge) {
			int proj;
			ir_node *res = get_edge_src_irn(res_edge);

			assert(is_Proj(res));

			proj = get_Proj_proj(res);
			assert(proj < n_res);
			assert(res_projs[proj] == NULL);
			res_projs[proj] = res;
		}
		res_proj = irn;
		break;
	}

	/** TODO: this is not correct for cases where return values are passed
	 * on the stack, but no known ABI does this currentl...
	 */
	n_reg_results = n_res;

	/* make the back end call node and set its register requirements. */
	for (i = 0; i < n_reg_params; ++i) {
		obstack_ptr_grow(obst, get_Call_param(irn, reg_param_idxs[i]));
	}
	foreach_pset(states, reg) {
		const arch_register_class_t *cls = arch_register_get_class(reg);
#if 0
		ir_node *regnode = be_abi_reg_map_get(env->regs, reg);
		ir_fprintf(stderr, "Adding %+F\n", regnode);
#endif
		ir_node *regnode = new_rd_Unknown(irg, arch_register_class_mode(cls));
		obstack_ptr_grow(obst, regnode);
	}
	n_ins = n_reg_params + pset_count(states);

	in = obstack_finish(obst);

	if (env->call->flags.bits.call_has_imm && is_SymConst(call_ptr)) {
		/* direct call */
		low_call = be_new_Call(get_irn_dbg_info(irn), irg, bl, curr_mem,
		                       curr_sp, curr_sp,
		                       n_reg_results + pn_be_Call_first_res + pset_count(caller_save),
		                       n_ins, in, get_Call_type(irn));
		be_Call_set_entity(low_call, get_SymConst_entity(call_ptr));
	} else {
		/* indirect call */
		low_call = be_new_Call(get_irn_dbg_info(irn), irg, bl, curr_mem,
		                       curr_sp, call_ptr,
		                       n_reg_results + pn_be_Call_first_res + pset_count(caller_save),
		                       n_ins, in, get_Call_type(irn));
	}
	be_Call_set_pop(low_call, call->pop);
	ARR_APP1(ir_node *, env->calls, low_call);

	/* create new stack pointer */
	curr_sp = new_r_Proj(irg, bl, low_call, get_irn_mode(curr_sp),
	                     pn_be_Call_sp);
	be_set_constr_single_reg(low_call, BE_OUT_POS(pn_be_Call_sp), sp);
	arch_set_irn_register(arch_env, curr_sp, sp);
	be_node_set_flags(low_call, BE_OUT_POS(pn_be_Call_sp),
			arch_irn_flags_ignore | arch_irn_flags_modify_sp);

	for(i = 0; i < n_res; ++i) {
		int pn;
		ir_node           *proj = res_projs[i];
		be_abi_call_arg_t *arg  = get_call_arg(call, 1, i);

		/* returns values on stack not supported yet */
		assert(arg->in_reg);

		/*
			shift the proj number to the right, since we will drop the
			unspeakable Proj_T from the Call. Therefore, all real argument
			Proj numbers must be increased by pn_be_Call_first_res
		*/
		pn = i + pn_be_Call_first_res;

		if(proj == NULL) {
			ir_type *res_type = get_method_res_type(mt, i);
			ir_mode *mode     = get_type_mode(res_type);
			proj              = new_r_Proj(irg, bl, low_call, mode, pn);
			res_projs[i]      = proj;
		} else {
			set_Proj_pred(proj, low_call);
			set_Proj_proj(proj, pn);
		}

		if (arg->in_reg) {
			pset_remove_ptr(caller_save, arg->reg);
		}
	}

	/*
		Set the register class of the call address to
		the backend provided class (default: stack pointer class)
	*/
	be_node_set_reg_class(low_call, be_pos_Call_ptr, call->cls_addr);

	DBG((env->dbg, LEVEL_3, "\tcreated backend call %+F\n", low_call));

	/* Set the register classes and constraints of the Call parameters. */
	for (i = 0; i < n_reg_params; ++i) {
		int index = reg_param_idxs[i];
		be_abi_call_arg_t *arg = get_call_arg(call, 0, index);
		assert(arg->reg != NULL);

		be_set_constr_single_reg(low_call, be_pos_Call_first_arg + i, arg->reg);
	}

	/* Set the register constraints of the results. */
	for (i = 0; i < n_res; ++i) {
		ir_node                 *proj = res_projs[i];
		const be_abi_call_arg_t *arg  = get_call_arg(call, 1, i);
		int                      pn   = get_Proj_proj(proj);

		assert(arg->in_reg);
		be_set_constr_single_reg(low_call, BE_OUT_POS(pn), arg->reg);
		arch_set_irn_register(arch_env, proj, arg->reg);
	}
	obstack_free(obst, in);
	exchange(irn, low_call);

	/* kill the ProjT node */
	if (res_proj != NULL) {
		be_kill_node(res_proj);
	}

	/* Make additional projs for the caller save registers
	   and the Keep node which keeps them alive. */
	if (1 || pset_count(caller_save) + n_reg_results > 0) {
		const arch_register_t *reg;
		ir_node               **in, *keep;
		int                   i;
		int                   n = 0;
		int                   curr_res_proj
			= pn_be_Call_first_res + n_reg_results;

		/* also keep the stack pointer */
		++n;
		set_irn_link(curr_sp, (void*) sp);
		obstack_ptr_grow(obst, curr_sp);

		for (reg = pset_first(caller_save); reg; reg = pset_next(caller_save), ++n) {
			ir_node *proj = new_r_Proj(irg, bl, low_call, reg->reg_class->mode,
			                           curr_res_proj);

			/* memorize the register in the link field. we need afterwards to set the register class of the keep correctly. */
			be_set_constr_single_reg(low_call, BE_OUT_POS(curr_res_proj), reg);
			arch_set_irn_register(arch_env, proj, reg);

			/* a call can produce ignore registers, in this case set the flag and register for the Proj */
			if (arch_register_type_is(reg, ignore)) {
				be_node_set_flags(low_call, BE_OUT_POS(curr_res_proj),
				                  arch_irn_flags_ignore);
			}

			set_irn_link(proj, (void*) reg);
			obstack_ptr_grow(obst, proj);
			curr_res_proj++;
		}

		for(i = 0; i < n_reg_results; ++i) {
			ir_node *proj = res_projs[i];
			const arch_register_t *reg = arch_get_irn_register(arch_env, proj);
			set_irn_link(proj, (void*) reg);
			obstack_ptr_grow(obst, proj);
		}
		n += n_reg_results;

		/* create the Keep for the caller save registers */
		in   = (ir_node **) obstack_finish(obst);
		keep = be_new_Keep(NULL, irg, bl, n, in);
		for (i = 0; i < n; ++i) {
			const arch_register_t *reg = get_irn_link(in[i]);
			be_node_set_reg_class(keep, i, reg->reg_class);
		}
		obstack_free(obst, in);
	}

	/* Clean up the stack. */
	assert(stack_size >= call->pop);
	stack_size -= call->pop;

	if (stack_size > 0) {
		ir_node *mem_proj = NULL;

		foreach_out_edge(low_call, edge) {
			ir_node *irn = get_edge_src_irn(edge);
			if(is_Proj(irn) && get_Proj_proj(irn) == pn_Call_M) {
				mem_proj = irn;
				break;
			}
		}

		if (! mem_proj) {
			mem_proj = new_r_Proj(irg, bl, low_call, mode_M, pn_be_Call_M_regular);
			keep_alive(mem_proj);
		}

		 /* Clean up the stack frame if we allocated it */
		if (! no_alloc) {
			curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, -stack_size);
		}
	}

	be_abi_call_free(call);
	obstack_free(obst, stack_param_idx);
	del_pset(results);
	del_pset(states);
	del_pset(caller_save);

	return curr_sp;
}

/**
 * Adjust the size of a node representing a stack alloc or free for the minimum stack alignment.
 *
 * @param alignment  the minimum stack alignment
 * @param size       the node containing the non-aligned size
 * @param irg        the irg where new nodes are allocated on
 * @param irg        the block where new nodes are allocated on
 * @param dbg        debug info for new nodes
 *
 * @return a node representing the aligned size
 */
static ir_node *adjust_alloc_size(unsigned stack_alignment, ir_node *size,
                                  ir_graph *irg, ir_node *block, dbg_info *dbg)
{
	if (stack_alignment > 1) {
		ir_mode *mode = get_irn_mode(size);
		tarval  *tv   = new_tarval_from_long(stack_alignment-1, mode);
		ir_node *mask = new_r_Const(irg, block, mode, tv);

		size = new_rd_Add(dbg, irg, block, size, mask, mode);

		tv   = new_tarval_from_long(-(long)stack_alignment, mode);
		mask = new_r_Const(irg, block, mode, tv);
		size = new_rd_And(dbg, irg, block, size, mask, mode);
	}
	return size;
}
/**
 * Adjust an alloca.
 * The alloca is transformed into a back end alloca node and connected to the stack nodes.
 */
static ir_node *adjust_alloc(be_abi_irg_t *env, ir_node *alloc, ir_node *curr_sp)
{
	ir_node *block;
	ir_graph *irg;
	ir_node *alloc_mem;
	ir_node *alloc_res;
	ir_type *type;
	dbg_info *dbg;

	const ir_edge_t *edge;
	ir_node *new_alloc, *size, *addr, *ins[2];
	unsigned stack_alignment;

	if (get_Alloc_where(alloc) != stack_alloc) {
		assert(0);
		return alloc;
	}

	block = get_nodes_block(alloc);
	irg = get_irn_irg(block);
	alloc_mem = NULL;
	alloc_res = NULL;
	type = get_Alloc_type(alloc);

	foreach_out_edge(alloc, edge) {
		ir_node *irn = get_edge_src_irn(edge);

		assert(is_Proj(irn));
		switch(get_Proj_proj(irn)) {
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

	dbg = get_irn_dbg_info(alloc);

	/* we might need to multiply the size with the element size */
	if(type != get_unknown_type() && get_type_size_bytes(type) != 1) {
		tarval *tv    = new_tarval_from_long(get_type_size_bytes(type),
		                                     mode_Iu);
		ir_node *cnst = new_rd_Const(dbg, irg, block, mode_Iu, tv);
		ir_node *mul  = new_rd_Mul(dbg, irg, block, get_Alloc_size(alloc),
		                           cnst, mode_Iu);
		size = mul;
	} else {
		size = get_Alloc_size(alloc);
	}

	/* The stack pointer will be modified in an unknown manner.
	   We cannot omit it. */
	env->call->flags.bits.try_omit_fp = 0;

	/* FIXME: size must be here round up for the stack alignment, but
	   this must be transmitted from the backend. */
	stack_alignment = 4;
	size            = adjust_alloc_size(stack_alignment, size, irg, block, dbg);
	new_alloc       = be_new_AddSP(env->isa->sp, irg, block, curr_sp, size);
	set_irn_dbg_info(new_alloc, dbg);

	if(alloc_mem != NULL) {
		ir_node *addsp_mem;
		ir_node *sync;

		addsp_mem = new_r_Proj(irg, block, new_alloc, mode_M, pn_be_AddSP_M);

		/* We need to sync the output mem of the AddSP with the input mem
		   edge into the alloc node. */
		ins[0] = get_Alloc_mem(alloc);
		ins[1] = addsp_mem;
		sync = new_r_Sync(irg, block, 2, ins);

		exchange(alloc_mem, sync);
	}

	exchange(alloc, new_alloc);

	/* fix projnum of alloca res */
	set_Proj_proj(alloc_res, pn_be_AddSP_res);

	addr    = alloc_res;
	curr_sp = new_r_Proj(irg, block, new_alloc,  get_irn_mode(curr_sp),
	                     pn_be_AddSP_sp);

	return curr_sp;
}  /* adjust_alloc */

/**
 * Adjust a Free.
 * The Free is transformed into a back end free node and connected to the stack nodes.
 */
static ir_node *adjust_free(be_abi_irg_t *env, ir_node *free, ir_node *curr_sp)
{
	ir_node *block;
	ir_graph *irg;
	ir_node *subsp, *mem, *res, *size, *sync;
	ir_type *type;
	ir_node *in[2];
	ir_mode *sp_mode;
	unsigned stack_alignment;
	dbg_info *dbg;

	if (get_Free_where(free) != stack_alloc) {
		assert(0);
		return free;
	}

	block = get_nodes_block(free);
	irg = get_irn_irg(block);
	type = get_Free_type(free);
	sp_mode = env->isa->sp->reg_class->mode;
	dbg = get_irn_dbg_info(free);

	/* we might need to multiply the size with the element size */
	if(type != get_unknown_type() && get_type_size_bytes(type) != 1) {
		tarval *tv = new_tarval_from_long(get_type_size_bytes(type), mode_Iu);
		ir_node *cnst = new_rd_Const(dbg, irg, block, mode_Iu, tv);
		ir_node *mul = new_rd_Mul(dbg, irg, block, get_Free_size(free),
		                          cnst, mode_Iu);
		size = mul;
	} else {
		size = get_Free_size(free);
	}

	/* FIXME: size must be here round up for the stack alignment, but
	   this must be transmitted from the backend. */
	stack_alignment = 4;
	size = adjust_alloc_size(stack_alignment, size, irg, block, dbg);

	/* The stack pointer will be modified in an unknown manner.
	   We cannot omit it. */
	env->call->flags.bits.try_omit_fp = 0;
	subsp = be_new_SubSP(env->isa->sp, irg, block, curr_sp, size);
	set_irn_dbg_info(subsp, dbg);

	mem = new_r_Proj(irg, block, subsp, mode_M, pn_be_SubSP_M);
	res = new_r_Proj(irg, block, subsp, sp_mode, pn_be_SubSP_sp);

	/* we need to sync the memory */
	in[0] = get_Free_mem(free);
	in[1] = mem;
	sync = new_r_Sync(irg, block, 2, in);

	/* and make the AddSP dependent on the former memory */
	add_irn_dep(subsp, get_Free_mem(free));

	/* kill the free */
	exchange(free, sync);
	curr_sp = res;

	return curr_sp;
}  /* adjust_free */

/* the following function is replaced by the usage of the heights module */
#if 0
/**
 * Walker for dependent_on().
 * This function searches a node tgt recursively from a given node
 * but is restricted to the given block.
 * @return 1 if tgt was reachable from curr, 0 if not.
 */
static int check_dependence(ir_node *curr, ir_node *tgt, ir_node *bl)
{
	int n, i;

	if (get_nodes_block(curr) != bl)
		return 0;

	if (curr == tgt)
		return 1;

	/* Phi functions stop the recursion inside a basic block */
	if (! is_Phi(curr)) {
		for(i = 0, n = get_irn_arity(curr); i < n; ++i) {
			if (check_dependence(get_irn_n(curr, i), tgt, bl))
				return 1;
		}
	}

	return 0;
}
#endif /* if 0 */

/**
 * Check if a node is somehow data dependent on another one.
 * both nodes must be in the same basic block.
 * @param n1 The first node.
 * @param n2 The second node.
 * @return 1, if n1 is data dependent (transitively) on n2, 0 if not.
 */
static int dependent_on(ir_node *n1, ir_node *n2)
{
	assert(get_nodes_block(n1) == get_nodes_block(n2));

	return heights_reachable_in_block(ir_heights, n1, n2);
}

static int cmp_call_dependency(const void *c1, const void *c2)
{
	ir_node *n1 = *(ir_node **) c1;
	ir_node *n2 = *(ir_node **) c2;

	/*
		Classical qsort() comparison function behavior:
		0  if both elements are equal
		1  if second is "smaller" that first
		-1 if first is "smaller" that second
	*/
	if (dependent_on(n1, n2))
		return -1;

	if (dependent_on(n2, n1))
		return 1;

	return 0;
}

/**
 * Walker: links all Call/alloc/Free nodes to the Block they are contained.
 */
static void link_calls_in_block_walker(ir_node *irn, void *data)
{
	ir_opcode code = get_irn_opcode(irn);

	if (code == iro_Call ||
		(code == iro_Alloc && get_Alloc_where(irn) == stack_alloc) ||
		(code == iro_Free && get_Free_where(irn) == stack_alloc)) {
		be_abi_irg_t *env = data;
		ir_node *bl       = get_nodes_block(irn);
		void *save        = get_irn_link(bl);

		if (code == iro_Call)
			env->call->flags.bits.irg_is_leaf = 0;

		set_irn_link(irn, save);
		set_irn_link(bl, irn);
	}
}

/**
 * Block-walker:
 * Process all Call nodes inside a basic block.
 * Note that the link field of the block must contain a linked list of all
 * Call nodes inside the Block. We first order this list according to data dependency
 * and that connect the calls together.
 */
static void process_calls_in_block(ir_node *bl, void *data)
{
	be_abi_irg_t *env = data;
	ir_node *curr_sp  = env->init_sp;
	ir_node *irn;
	int n;

	for(irn = get_irn_link(bl), n = 0; irn; irn = get_irn_link(irn), ++n)
		obstack_ptr_grow(&env->obst, irn);

	/* If there were call nodes in the block. */
	if(n > 0) {
		ir_node *keep;
		ir_node **nodes;
		int i;

		nodes = obstack_finish(&env->obst);

		/* order the call nodes according to data dependency */
		qsort(nodes, n, sizeof(nodes[0]), cmp_call_dependency);

		for(i = n - 1; i >= 0; --i) {
			ir_node *irn = nodes[i];

			DBG((env->dbg, LEVEL_3, "\tprocessing call %+F\n", irn));
			switch(get_irn_opcode(irn)) {
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
				break;
			}
		}

		obstack_free(&env->obst, nodes);

		/* Keep the last stack state in the block by tying it to Keep node,
		 * the proj from calls is already kept */
		if(curr_sp != env->init_sp
				&& !(is_Proj(curr_sp) && be_is_Call(get_Proj_pred(curr_sp)))) {
			nodes[0] = curr_sp;
			keep     = be_new_Keep(env->isa->sp->reg_class, get_irn_irg(bl),
			                       bl, 1, nodes);
			pmap_insert(env->keep_map, bl, keep);
		}
	}

	set_irn_link(bl, curr_sp);
}  /* process_calls_in_block */

/**
 * Adjust all call nodes in the graph to the ABI conventions.
 */
static void process_calls(be_abi_irg_t *env)
{
	ir_graph *irg = env->birg->irg;

	env->call->flags.bits.irg_is_leaf = 1;
	irg_walk_graph(irg, firm_clear_link, link_calls_in_block_walker, env);

	ir_heights = heights_new(env->birg->irg);
	irg_block_walk_graph(irg, NULL, process_calls_in_block, env);
	heights_free(ir_heights);
}

/**
 * Computes the stack argument layout type.
 * Changes a possibly allocated value param type by moving
 * entities to the stack layout type.
 *
 * @param env          the ABI environment
 * @param call         the current call ABI
 * @param method_type  the method type
 * @param param_map    an array mapping method arguments to the stack layout type
 *
 * @return the stack argument layout type
 */
static ir_type *compute_arg_type(be_abi_irg_t *env, be_abi_call_t *call, ir_type *method_type, ir_entity ***param_map)
{
	int dir  = env->call->flags.bits.left_to_right ? 1 : -1;
	int inc  = env->birg->main_env->arch_env->isa->stack_dir * dir;
	int n    = get_method_n_params(method_type);
	int curr = inc > 0 ? 0 : n - 1;
	int ofs  = 0;

	char buf[128];
	ir_type *res;
	int i;
	ir_type *val_param_tp = get_method_value_param_type(method_type);
	ident *id = get_entity_ident(get_irg_entity(env->birg->irg));
	ir_entity **map;

	*param_map = map = obstack_alloc(&env->obst, n * sizeof(ir_entity *));
	res = new_type_struct(mangle_u(id, new_id_from_chars("arg_type", 8)));
	for (i = 0; i < n; ++i, curr += inc) {
		ir_type *param_type    = get_method_param_type(method_type, curr);
		be_abi_call_arg_t *arg = get_call_arg(call, 0, curr);

		map[i] = NULL;
		if (arg->on_stack) {
			if (val_param_tp) {
				/* the entity was already created, move it to the param type */
				arg->stack_ent = get_method_value_param_ent(method_type, i);
				remove_struct_member(val_param_tp, arg->stack_ent);
				set_entity_owner(arg->stack_ent, res);
				add_struct_member(res, arg->stack_ent);
				/* must be automatic to set a fixed layout */
				set_entity_allocation(arg->stack_ent, allocation_automatic);
			}
			else {
				snprintf(buf, sizeof(buf), "param_%d", i);
				arg->stack_ent = new_entity(res, new_id_from_str(buf), param_type);
			}
			ofs += arg->space_before;
			ofs = round_up2(ofs, arg->alignment);
			set_entity_offset(arg->stack_ent, ofs);
			ofs += arg->space_after;
			ofs += get_type_size_bytes(param_type);
			map[i] = arg->stack_ent;
		}
	}
	set_type_size_bytes(res, ofs);
	set_type_state(res, layout_fixed);
	return res;
}

#if 0
static void create_register_perms(const arch_isa_t *isa, ir_graph *irg, ir_node *bl, pmap *regs)
{
	int i, j, n;
	struct obstack obst;

	obstack_init(&obst);

	/* Create a Perm after the RegParams node to delimit it. */
	for(i = 0, n = arch_isa_get_n_reg_class(isa); i < n; ++i) {
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		ir_node *perm;
		ir_node **in;
		int n_regs;

		for(n_regs = 0, j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = &cls->regs[j];
			ir_node *irn = pmap_get(regs, (void *) reg);

			if(irn && !arch_register_type_is(reg, ignore)) {
				n_regs++;
				obstack_ptr_grow(&obst, irn);
				set_irn_link(irn, (void *) reg);
			}
		}

		obstack_ptr_grow(&obst, NULL);
		in = obstack_finish(&obst);
		if(n_regs > 0) {
			perm = be_new_Perm(cls, irg, bl, n_regs, in);
			for(j = 0; j < n_regs; ++j) {
				ir_node *arg = in[j];
				arch_register_t *reg = get_irn_link(arg);
				pmap_insert(regs, reg, arg);
				be_set_constr_single_reg(perm, BE_OUT_POS(j), reg);
			}
		}
		obstack_free(&obst, in);
	}

	obstack_free(&obst, NULL);
}
#endif

typedef struct {
	const arch_register_t *reg;
	ir_node *irn;
} reg_node_map_t;

static int cmp_regs(const void *a, const void *b)
{
	const reg_node_map_t *p = a;
	const reg_node_map_t *q = b;

	if(p->reg->reg_class == q->reg->reg_class)
		return p->reg->index - q->reg->index;
	else
		return p->reg->reg_class - q->reg->reg_class;
}

static reg_node_map_t *reg_map_to_arr(struct obstack *obst, pmap *reg_map)
{
	pmap_entry *ent;
	int n = pmap_count(reg_map);
	int i = 0;
	reg_node_map_t *res = obstack_alloc(obst, n * sizeof(res[0]));

	pmap_foreach(reg_map, ent) {
		res[i].reg = ent->key;
		res[i].irn = ent->value;
		i++;
	}

	qsort(res, n, sizeof(res[0]), cmp_regs);
	return res;
}

/**
 * Creates a barrier.
 */
static ir_node *create_barrier(be_abi_irg_t *env, ir_node *bl, ir_node **mem, pmap *regs, int in_req)
{
	ir_graph *irg = env->birg->irg;
	int n_regs    = pmap_count(regs);
	int n;
	ir_node *irn;
	ir_node **in;
	reg_node_map_t *rm;

	rm = reg_map_to_arr(&env->obst, regs);

	for(n = 0; n < n_regs; ++n)
		obstack_ptr_grow(&env->obst, rm[n].irn);

	if(mem) {
		obstack_ptr_grow(&env->obst, *mem);
		n++;
	}

	in = (ir_node **) obstack_finish(&env->obst);
	irn = be_new_Barrier(irg, bl, n, in);
	obstack_free(&env->obst, in);

	for(n = 0; n < n_regs; ++n) {
		const arch_register_t *reg = rm[n].reg;
		int flags                  = 0;
		int pos                    = BE_OUT_POS(n);
		ir_node *proj;

		proj = new_r_Proj(irg, bl, irn, get_irn_mode(rm[n].irn), n);
		be_node_set_reg_class(irn, n, reg->reg_class);
		if(in_req)
			be_set_constr_single_reg(irn, n, reg);
		be_set_constr_single_reg(irn, pos, reg);
		be_node_set_reg_class(irn, pos, reg->reg_class);
		arch_set_irn_register(env->birg->main_env->arch_env, proj, reg);

		/* if the proj projects a ignore register or a node which is set to ignore, propagate this property. */
		if(arch_register_type_is(reg, ignore) || arch_irn_is(env->birg->main_env->arch_env, in[n], ignore))
			flags |= arch_irn_flags_ignore;

		if(arch_irn_is(env->birg->main_env->arch_env, in[n], modify_sp))
			flags |= arch_irn_flags_modify_sp;

		be_node_set_flags(irn, pos, flags);

		pmap_insert(regs, (void *) reg, proj);
	}

	if(mem) {
		*mem = new_r_Proj(irg, bl, irn, mode_M, n);
	}

	obstack_free(&env->obst, rm);
	return irn;
}

/**
 * Creates a be_Return for a Return node.
 *
 * @param @env    the abi environment
 * @param irn     the Return node or NULL if there was none
 * @param bl      the block where the be_Retun should be placed
 * @param mem     the current memory
 * @param n_res   number of return results
 */
static ir_node *create_be_return(be_abi_irg_t *env, ir_node *irn, ir_node *bl,
		ir_node *mem, int n_res)
{
	be_abi_call_t *call        = env->call;
	const arch_isa_t *isa = env->birg->main_env->arch_env->isa;
	dbg_info *dbgi;
	pmap *reg_map  = pmap_create();
	ir_node *keep  = pmap_get(env->keep_map, bl);
	int in_max;
	ir_node *ret;
	int i, n;
	unsigned pop;
	ir_node **in;
	ir_node *stack;
	const arch_register_t **regs;
	pmap_entry *ent ;

	/*
		get the valid stack node in this block.
		If we had a call in that block there is a Keep constructed by process_calls()
		which points to the last stack modification in that block. we'll use
		it then. Else we use the stack from the start block and let
		the ssa construction fix the usage.
	*/
	stack = be_abi_reg_map_get(env->regs, isa->sp);
	if (keep) {
		stack = get_irn_n(keep, 0);
		be_kill_node(keep);
		remove_End_keepalive(get_irg_end(env->birg->irg), keep);
	}

	/* Insert results for Return into the register map. */
	for(i = 0; i < n_res; ++i) {
		ir_node *res           = get_Return_res(irn, i);
		be_abi_call_arg_t *arg = get_call_arg(call, 1, i);
		assert(arg->in_reg && "return value must be passed in register");
		pmap_insert(reg_map, (void *) arg->reg, res);
	}

	/* Add uses of the callee save registers. */
	pmap_foreach(env->regs, ent) {
		const arch_register_t *reg = ent->key;
		if(arch_register_type_is(reg, callee_save) || arch_register_type_is(reg, ignore))
			pmap_insert(reg_map, ent->key, ent->value);
	}

	be_abi_reg_map_set(reg_map, isa->sp, stack);

	/* Make the Epilogue node and call the arch's epilogue maker. */
	create_barrier(env, bl, &mem, reg_map, 1);
	call->cb->epilogue(env->cb, bl, &mem, reg_map);

	/*
		Maximum size of the in array for Return nodes is
		return args + callee save/ignore registers + memory + stack pointer
	*/
	in_max = pmap_count(reg_map) + n_res + 2;

	in   = obstack_alloc(&env->obst, in_max * sizeof(in[0]));
	regs = obstack_alloc(&env->obst, in_max * sizeof(regs[0]));

	in[0]   = mem;
	in[1]   = be_abi_reg_map_get(reg_map, isa->sp);
	regs[0] = NULL;
	regs[1] = isa->sp;
	n       = 2;

	/* clear SP entry, since it has already been grown. */
	pmap_insert(reg_map, (void *) isa->sp, NULL);
	for(i = 0; i < n_res; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 1, i);

		in[n]     = be_abi_reg_map_get(reg_map, arg->reg);
		regs[n++] = arg->reg;

		/* Clear the map entry to mark the register as processed. */
		be_abi_reg_map_set(reg_map, arg->reg, NULL);
	}

	/* grow the rest of the stuff. */
	pmap_foreach(reg_map, ent) {
		if(ent->value) {
			in[n]     = ent->value;
			regs[n++] = ent->key;
		}
	}

	/* The in array for the new back end return is now ready. */
	if(irn != NULL) {
		dbgi = get_irn_dbg_info(irn);
	} else {
		dbgi = NULL;
	}
	/* we have to pop the shadow parameter in in case of struct returns */
	pop = call->pop;
	ret = be_new_Return(dbgi, env->birg->irg, bl, n_res, pop, n, in);

	/* Set the register classes of the return's parameter accordingly. */
	for(i = 0; i < n; ++i)
		if(regs[i])
			be_node_set_reg_class(ret, i, regs[i]->reg_class);

	/* Free the space of the Epilog's in array and the register <-> proj map. */
	obstack_free(&env->obst, in);
	pmap_destroy(reg_map);

	return ret;
}

typedef struct lower_frame_sels_env_t {
	be_abi_irg_t *env;
	ir_entity    *value_param_list;  /**< the list of all value param entities */
	ir_entity    *value_param_tail;  /**< the tail of the list of all value param entities */
} lower_frame_sels_env_t;

/**
 * Walker: Replaces Sels of frame type and
 * value param type entities by FrameAddress.
 * Links all used entities.
 */
static void lower_frame_sels_walker(ir_node *irn, void *data) {
	lower_frame_sels_env_t *ctx = data;

	if (is_Sel(irn)) {
		ir_graph *irg        = current_ir_graph;
		ir_node  *frame      = get_irg_frame(irg);
		ir_node  *param_base = get_irg_value_param_base(irg);
		ir_node  *ptr        = get_Sel_ptr(irn);

		if (ptr == frame || ptr == param_base) {
			be_abi_irg_t *env = ctx->env;
			ir_entity    *ent = get_Sel_entity(irn);
			ir_node      *bl  = get_nodes_block(irn);
			ir_node      *nw;

			nw = be_new_FrameAddr(env->isa->sp->reg_class, irg, bl, frame, ent);
			exchange(irn, nw);

			/* check, if it's a param sel and if have not seen this entity before */
			if (ptr == param_base &&
					ent != ctx->value_param_tail &&
					get_entity_link(ent) == NULL) {
				set_entity_link(ent, ctx->value_param_list);
				ctx->value_param_list = ent;
				if (ctx->value_param_tail == NULL) ctx->value_param_tail = ent;
			}
		}
	}
}

/**
 * Check if a value parameter is transmitted as a register.
 * This might happen if the address of an parameter is taken which is
 * transmitted in registers.
 *
 * Note that on some architectures this case must be handled specially
 * because the place of the backing store is determined by their ABI.
 *
 * In the default case we move the entity to the frame type and create
 * a backing store into the first block.
 */
static void fix_address_of_parameter_access(be_abi_irg_t *env, ir_entity *value_param_list) {
	be_abi_call_t *call = env->call;
	ir_graph *irg       = env->birg->irg;
	ir_entity *ent, *next_ent, *new_list;
	ir_type *frame_tp;
	DEBUG_ONLY(firm_dbg_module_t *dbg = env->dbg;)

	new_list = NULL;
	for (ent = value_param_list; ent; ent = next_ent) {
		int i = get_struct_member_index(get_entity_owner(ent), ent);
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);

		next_ent = get_entity_link(ent);
		if (arg->in_reg) {
			DBG((dbg, LEVEL_2, "\targ #%d need backing store\n", i));
			set_entity_link(ent, new_list);
			new_list = ent;
		}
	}
	if (new_list) {
		/* ok, change the graph */
		ir_node *start_bl = get_irg_start_block(irg);
		ir_node *first_bl = NULL;
		ir_node *frame, *imem, *nmem, *store, *mem, *args, *args_bl;
		const ir_edge_t *edge;
		optimization_state_t state;
		int offset;

		foreach_block_succ(start_bl, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			if (start_bl != succ) {
				first_bl = succ;
				break;
			}
		}
		assert(first_bl);
		/* we had already removed critical edges, so the following
		   assertion should be always true. */
		assert(get_Block_n_cfgpreds(first_bl) == 1);

		/* now create backing stores */
		frame = get_irg_frame(irg);
		imem = get_irg_initial_mem(irg);

		save_optimization_state(&state);
		set_optimize(0);
		nmem = new_r_Proj(irg, first_bl, get_irg_start(irg), mode_M, pn_Start_M);
		restore_optimization_state(&state);

		/* reroute all edges to the new memory source */
		edges_reroute(imem, nmem, irg);

		store   = NULL;
		mem     = imem;
		args    = get_irg_args(irg);
		args_bl = get_nodes_block(args);
		for (ent = new_list; ent; ent = get_entity_link(ent)) {
			int     i     = get_struct_member_index(get_entity_owner(ent), ent);
			ir_type *tp   = get_entity_type(ent);
			ir_mode *mode = get_type_mode(tp);
			ir_node *addr;

			/* address for the backing store */
			addr = be_new_FrameAddr(env->isa->sp->reg_class, irg, first_bl, frame, ent);

			if (store)
				mem = new_r_Proj(irg, first_bl, store, mode_M, pn_Store_M);

			/* the backing store itself */
			store = new_r_Store(irg, first_bl, mem, addr,
			                    new_r_Proj(irg, args_bl, args, mode, i));
		}
		/* the new memory Proj gets the last Proj from store */
		set_Proj_pred(nmem, store);
		set_Proj_proj(nmem, pn_Store_M);

		/* move all entities to the frame type */
		frame_tp = get_irg_frame_type(irg);
		offset   = get_type_size_bytes(frame_tp);
		for (ent = new_list; ent; ent = get_entity_link(ent)) {
			ir_type *tp = get_entity_type(ent);
			int align = get_type_alignment_bytes(tp);

			offset += align - 1;
			offset &= -align;
			set_entity_owner(ent, frame_tp);
			add_class_member(frame_tp, ent);
			/* must be automatic to set a fixed layout */
			set_entity_allocation(ent, allocation_automatic);
			set_entity_offset(ent, offset);
			offset += get_type_size_bytes(tp);
		}
		set_type_size_bytes(frame_tp, offset);
	}
}

#if 1
/**
 * The start block has no jump, instead it has an initial exec Proj.
 * The backend wants to handle all blocks the same way, so we replace
 * the out cfg edge with a real jump.
 */
static void fix_start_block(ir_node *block, void *env) {
	int      *done = env;
	int      i;
	ir_node  *start_block;
	ir_graph *irg;

	/* we processed the start block, return */
	if (*done)
		return;

	irg         = get_irn_irg(block);
	start_block = get_irg_start_block(irg);

	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		ir_node *pred       = get_Block_cfgpred(block, i);
		ir_node *pred_block = get_nodes_block(pred);

		/* ok, we are in the block, having start as cfg predecessor */
		if (pred_block == start_block) {
			ir_node *jump = new_r_Jmp(irg, pred_block);
			set_Block_cfgpred(block, i, jump);
			*done = 1;
		}
	}
}
#endif

/**
 * Modify the irg itself and the frame type.
 */
static void modify_irg(be_abi_irg_t *env)
{
	be_abi_call_t *call       = env->call;
	const arch_isa_t *isa     = env->birg->main_env->arch_env->isa;
	const arch_register_t *sp = arch_isa_sp(isa);
	ir_graph *irg             = env->birg->irg;
	ir_node *bl               = get_irg_start_block(irg);
	ir_node *end              = get_irg_end_block(irg);
	ir_node *old_mem          = get_irg_initial_mem(irg);
	ir_node *new_mem_proj;
	ir_node *mem;
	ir_type *method_type      = get_entity_type(get_irg_entity(irg));
	pset *dont_save           = pset_new_ptr(8);

	int n_params;
	int i, n;
	unsigned j;

	reg_node_map_t *rm;
	const arch_register_t *fp_reg;
	ir_node *frame_pointer;
	ir_node *reg_params_bl;
	ir_node **args;
	ir_node *arg_tuple;
	ir_node *value_param_base;
	const ir_edge_t *edge;
	ir_type *arg_type, *bet_type, *tp;
	lower_frame_sels_env_t ctx;
	ir_entity **param_map;

	bitset_t *used_proj_nr;
	DEBUG_ONLY(firm_dbg_module_t *dbg = env->dbg;)

	DBG((dbg, LEVEL_1, "introducing abi on %+F\n", irg));

	/* set the links of all frame entities to NULL, we use it
	   to detect if an entity is already linked in the value_param_list */
	tp = get_method_value_param_type(method_type);
	if (tp != NULL) {
		for (i = get_struct_n_members(tp) - 1; i >= 0; --i)
			set_entity_link(get_struct_member(tp, i), NULL);
	}

	/* Convert the Sel nodes in the irg to frame load/store/addr nodes. */
	ctx.env              = env;
	ctx.value_param_list = NULL;
	ctx.value_param_tail = NULL;
	irg_walk_graph(irg, lower_frame_sels_walker, NULL, &ctx);

	/* value_param_base anchor is not needed anymore now */
	value_param_base = get_irg_value_param_base(irg);
	be_kill_node(value_param_base);
	set_irg_value_param_base(irg, new_r_Bad(irg));

	env->frame = obstack_alloc(&env->obst, sizeof(env->frame[0]));
	env->regs  = pmap_create();

	used_proj_nr = bitset_alloca(1024);
	n_params     = get_method_n_params(method_type);
	args         = obstack_alloc(&env->obst, n_params * sizeof(args[0]));
	memset(args, 0, n_params * sizeof(args[0]));

	/* Check if a value parameter is transmitted as a register.
	 * This might happen if the address of an parameter is taken which is
	 * transmitted in registers.
	 *
	 * Note that on some architectures this case must be handled specially
	 * because the place of the backing store is determined by their ABI.
	 *
	 * In the default case we move the entity to the frame type and create
	 * a backing store into the first block.
	 */
	fix_address_of_parameter_access(env, ctx.value_param_list);

	/* Fill the argument vector */
	arg_tuple = get_irg_args(irg);
	foreach_out_edge(arg_tuple, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		if (! is_Anchor(irn)) {
			int nr       = get_Proj_proj(irn);
			args[nr]     = irn;
			DBG((dbg, LEVEL_2, "\treading arg: %d -> %+F\n", nr, irn));
		}
	}

	arg_type = compute_arg_type(env, call, method_type, &param_map);
	bet_type = call->cb->get_between_type(env->cb);
	stack_frame_init(env->frame, arg_type, bet_type, get_irg_frame_type(irg), isa->stack_dir, param_map);

	/* Count the register params and add them to the number of Projs for the RegParams node */
	for(i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		if(arg->in_reg && args[i]) {
			assert(arg->reg != sp && "cannot use stack pointer as parameter register");
			assert(i == get_Proj_proj(args[i]));

			/* For now, associate the register with the old Proj from Start representing that argument. */
			pmap_insert(env->regs, (void *) arg->reg, args[i]);
			bitset_set(used_proj_nr, i);
			DBG((dbg, LEVEL_2, "\targ #%d -> reg %s\n", i, arg->reg->name));
		}
	}

	/* Collect all callee-save registers */
	for(i = 0, n = arch_isa_get_n_reg_class(isa); i < n; ++i) {
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		for(j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = &cls->regs[j];
			if(arch_register_type_is(reg, callee_save) ||
					arch_register_type_is(reg, state)) {
				pmap_insert(env->regs, (void *) reg, NULL);
			}
		}
	}

	pmap_insert(env->regs, (void *) sp, NULL);
	pmap_insert(env->regs, (void *) isa->bp, NULL);
	reg_params_bl   = get_irg_start_block(irg);
	env->reg_params = be_new_RegParams(irg, reg_params_bl, pmap_count(env->regs));
	add_irn_dep(env->reg_params, get_irg_start(irg));

	/*
	 * make proj nodes for the callee save registers.
	 * memorize them, since Return nodes get those as inputs.
	 *
	 * Note, that if a register corresponds to an argument, the regs map contains
	 * the old Proj from start for that argument.
	 */

	rm = reg_map_to_arr(&env->obst, env->regs);
	for(i = 0, n = pmap_count(env->regs); i < n; ++i) {
		arch_register_t *reg = (void *) rm[i].reg;
		ir_mode *mode        = reg->reg_class->mode;
		long nr              = i;
		int pos              = BE_OUT_POS((int) nr);
		int flags            = 0;

		ir_node *proj;

		assert(nr >= 0);
		bitset_set(used_proj_nr, nr);
		proj = new_r_Proj(irg, reg_params_bl, env->reg_params, mode, nr);
		pmap_insert(env->regs, (void *) reg, proj);
		be_set_constr_single_reg(env->reg_params, pos, reg);
		arch_set_irn_register(env->birg->main_env->arch_env, proj, reg);

		/*
		 * If the register is an ignore register,
		 * The Proj for that register shall also be ignored during register allocation.
		 */
		if(arch_register_type_is(reg, ignore))
			flags |= arch_irn_flags_ignore;

		if(reg == sp)
			flags |= arch_irn_flags_modify_sp;

		be_node_set_flags(env->reg_params, pos, flags);

		DBG((dbg, LEVEL_2, "\tregister save proj #%d -> reg %s\n", nr, reg->name));
	}
	obstack_free(&env->obst, rm);

	/* create a new initial memory proj */
	assert(is_Proj(old_mem));
	new_mem_proj = new_r_Proj(irg, get_nodes_block(old_mem),
	                          new_r_Unknown(irg, mode_T), mode_M,
	                          get_Proj_proj(old_mem));
	mem = new_mem_proj;

	/* Generate the Prologue */
	fp_reg  = call->cb->prologue(env->cb, &mem, env->regs);

	/* do the stack allocation BEFORE the barrier, or spill code
	   might be added before it */
	env->init_sp = be_abi_reg_map_get(env->regs, sp);
	env->init_sp = be_new_IncSP(sp, irg, bl, env->init_sp, BE_STACK_FRAME_SIZE_EXPAND);
	be_abi_reg_map_set(env->regs, sp, env->init_sp);

	create_barrier(env, bl, &mem, env->regs, 0);

	env->init_sp = be_abi_reg_map_get(env->regs, sp);
	arch_set_irn_register(env->birg->main_env->arch_env, env->init_sp, sp);

	frame_pointer = be_abi_reg_map_get(env->regs, fp_reg);
	set_irg_frame(irg, frame_pointer);
	pset_insert_ptr(env->ignore_regs, fp_reg);

	/* rewire old mem users to new mem */
	set_Proj_pred(new_mem_proj, get_Proj_pred(old_mem));
	exchange(old_mem, mem);

	set_irg_initial_mem(irg, mem);

	/* Now, introduce stack param nodes for all parameters passed on the stack */
	for(i = 0; i < n_params; ++i) {
		ir_node *arg_proj = args[i];
		ir_node *repl     = NULL;

		if(arg_proj != NULL) {
			be_abi_call_arg_t *arg;
			ir_type *param_type;
			int     nr = get_Proj_proj(arg_proj);
			ir_mode *mode;

			nr         = MIN(nr, n_params);
			arg        = get_call_arg(call, 0, nr);
			param_type = get_method_param_type(method_type, nr);

			if (arg->in_reg) {
				repl = pmap_get(env->regs, (void *) arg->reg);
			} else if(arg->on_stack) {
				ir_node *addr = be_new_FrameAddr(sp->reg_class, irg, reg_params_bl, frame_pointer, arg->stack_ent);

				/* For atomic parameters which are actually used, we create a Load node. */
				if(is_atomic_type(param_type) && get_irn_n_edges(args[i]) > 0) {
					ir_mode *mode      = get_type_mode(param_type);
					ir_mode *load_mode = arg->load_mode;

					ir_node *load = new_r_Load(irg, reg_params_bl, new_NoMem(), addr, load_mode);
					set_irn_pinned(load, op_pin_state_floats);
					repl = new_r_Proj(irg, reg_params_bl, load, load_mode, pn_Load_res);

					if (mode != load_mode) {
						repl = new_r_Conv(irg, reg_params_bl, repl, mode);
					}
				} else {
					/* The stack parameter is not primitive (it is a struct or array),
					 * we thus will create a node representing the parameter's address
					 * on the stack. */
					repl = addr;
				}
			}

			assert(repl != NULL);

			/* Beware: the mode of the register parameters is always the mode of the register class
			   which may be wrong. Add Conv's then. */
			mode = get_irn_mode(args[i]);
			if (mode != get_irn_mode(repl)) {
				repl = new_r_Conv(irg, get_irn_n(repl, -1), repl, mode);
			}
			exchange(args[i], repl);
		}
	}

	/* the arg proj is not needed anymore now and should be only used by the anchor */
	assert(get_irn_n_edges(arg_tuple) == 1);
	be_kill_node(arg_tuple);
	set_irg_args(irg, new_rd_Bad(irg));

	/* All Return nodes hang on the End node, so look for them there. */
	for (i = 0, n = get_Block_n_cfgpreds(end); i < n; ++i) {
		ir_node *irn = get_Block_cfgpred(end, i);

		if (is_Return(irn)) {
			ir_node *ret = create_be_return(env, irn, get_nodes_block(irn), get_Return_mem(irn), get_Return_n_ress(irn));
			exchange(irn, ret);
		}
	}
	/* if we have endless loops here, n might be <= 0. Do NOT create a be_Return then,
	   the code is dead and will never be executed. */

	del_pset(dont_save);
	obstack_free(&env->obst, args);

	/* handle start block here (place a jump in the block) */
	i = 0;
	irg_block_walk_graph(irg, fix_start_block, NULL, &i);
}

/** Fix the state inputs of calls that still hang on unknowns */
static
void fix_call_state_inputs(be_abi_irg_t *env)
{
	const arch_isa_t *isa = env->isa;
	int i, n, n_states;
	arch_register_t **stateregs = NEW_ARR_F(arch_register_t*, 0);

	/* Collect caller save registers */
	n = arch_isa_get_n_reg_class(isa);
	for(i = 0; i < n; ++i) {
		unsigned j;
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		for(j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = arch_register_for_index(cls, j);
			if(arch_register_type_is(reg, state)) {
				ARR_APP1(arch_register_t*, stateregs, (arch_register_t *)reg);
			}
		}
	}

	n = ARR_LEN(env->calls);
	n_states = ARR_LEN(stateregs);
	for(i = 0; i < n; ++i) {
		int s, arity;
		ir_node *call = env->calls[i];

		arity = get_irn_arity(call);

		/* the statereg inputs are the last n inputs of the calls */
		for(s = 0; s < n_states; ++s) {
			int inp = arity - n_states + s;
			const arch_register_t *reg = stateregs[s];
			ir_node *regnode = be_abi_reg_map_get(env->regs, reg);

			set_irn_n(call, inp, regnode);
		}
	}
}

be_abi_irg_t *be_abi_introduce(be_irg_t *birg)
{
	be_abi_irg_t *env  = xmalloc(sizeof(env[0]));
	ir_node *old_frame = get_irg_frame(birg->irg);
	ir_graph *irg      = birg->irg;

	pmap_entry *ent;
	ir_node *dummy;
	optimization_state_t state;
	unsigned *limited_bitset;

	be_omit_fp = birg->main_env->options->omit_fp;

	obstack_init(&env->obst);

	env->isa         = birg->main_env->arch_env->isa;
	env->method_type = get_entity_type(get_irg_entity(irg));
	env->call        = be_abi_call_new(env->isa->sp->reg_class);
	arch_isa_get_call_abi(env->isa, env->method_type, env->call);

	env->ignore_regs  = pset_new_ptr_default();
	env->keep_map     = pmap_create();
	env->dce_survivor = new_survive_dce();
	env->birg         = birg;

	env->sp_req.type    = arch_register_req_type_limited;
	env->sp_req.cls     = arch_register_get_class(env->isa->sp);
	limited_bitset      = rbitset_obstack_alloc(&env->obst, env->sp_req.cls->n_regs);
	rbitset_set(limited_bitset, arch_register_get_index(env->isa->sp));
	env->sp_req.limited = limited_bitset;

	env->sp_cls_req.type  = arch_register_req_type_normal;
	env->sp_cls_req.cls   = arch_register_get_class(env->isa->sp);

	/* Beware: later we replace this node by the real one, ensure it is not CSE'd
	   to another Unknown or the stack pointer gets used */
	save_optimization_state(&state);
	set_optimize(0);
	env->init_sp = dummy  = new_r_Unknown(irg, env->isa->sp->reg_class->mode);
	restore_optimization_state(&state);
	FIRM_DBG_REGISTER(env->dbg, "firm.be.abi");

	env->calls = NEW_ARR_F(ir_node*, 0);

	/* Lower all call nodes in the IRG. */
	process_calls(env);

	/*
		Beware: init backend abi call object after processing calls,
		otherwise some information might be not yet available.
	*/
	env->cb = env->call->cb->init(env->call, birg->main_env->arch_env, irg);

	/* Process the IRG */
	modify_irg(env);

	/* fix call inputs for state registers */
	fix_call_state_inputs(env);

	/* We don't need the keep map anymore. */
	pmap_destroy(env->keep_map);

	/* calls array is not needed anymore */
	DEL_ARR_F(env->calls);

	/* reroute the stack origin of the calls to the true stack origin. */
	exchange(dummy, env->init_sp);
	exchange(old_frame, get_irg_frame(irg));

	/* Make some important node pointers survive the dead node elimination. */
	survive_dce_register_irn(env->dce_survivor, &env->init_sp);
	pmap_foreach(env->regs, ent) {
		survive_dce_register_irn(env->dce_survivor, (ir_node **) &ent->value);
	}

	env->call->cb->done(env->cb);
	env->cb = NULL;
	return env;
}

void be_abi_free(be_abi_irg_t *env)
{
	be_abi_call_free(env->call);
	free_survive_dce(env->dce_survivor);
	del_pset(env->ignore_regs);
	pmap_destroy(env->regs);
	obstack_free(&env->obst, NULL);
	free(env);
}

void be_abi_put_ignore_regs(be_abi_irg_t *abi, const arch_register_class_t *cls, bitset_t *bs)
{
	arch_register_t *reg;

	for(reg = pset_first(abi->ignore_regs); reg; reg = pset_next(abi->ignore_regs))
		if(reg->reg_class == cls)
			bitset_set(bs, reg->index);
}

/* Returns the stack layout from a abi environment. */
const be_stack_layout_t *be_abi_get_stack_layout(const be_abi_irg_t *abi) {
	return abi->frame;
}

/*

  _____ _        ____  _             _
 |  ___(_)_  __ / ___|| |_ __ _  ___| | __
 | |_  | \ \/ / \___ \| __/ _` |/ __| |/ /
 |  _| | |>  <   ___) | || (_| | (__|   <
 |_|   |_/_/\_\ |____/ \__\__,_|\___|_|\_\

*/

typedef ir_node **node_array;

typedef struct fix_stack_walker_env_t {
	node_array sp_nodes;
	const arch_env_t *arch_env;
} fix_stack_walker_env_t;

/**
 * Walker. Collect all stack modifying nodes.
 */
static void collect_stack_nodes_walker(ir_node *node, void *data)
{
	fix_stack_walker_env_t *env = data;

	if (arch_irn_is(env->arch_env, node, modify_sp)) {
		assert(get_irn_mode(node) != mode_M && get_irn_mode(node) != mode_T);
		ARR_APP1(ir_node*, env->sp_nodes, node);
	}
}

void be_abi_fix_stack_nodes(be_abi_irg_t *env)
{
	be_ssa_construction_env_t senv;
	int i, len;
	ir_node **phis;
	be_irg_t *birg = env->birg;
	be_lv_t *lv = be_get_birg_liveness(birg);
	fix_stack_walker_env_t walker_env;
	arch_isa_t *isa;

	walker_env.sp_nodes = NEW_ARR_F(ir_node*, 0);
	walker_env.arch_env = birg->main_env->arch_env;
	isa = walker_env.arch_env->isa;

	irg_walk_graph(birg->irg, collect_stack_nodes_walker, NULL, &walker_env);

	/* nothing to be done if we didn't find any node, in fact we mustn't
	 * continue, as for endless loops incsp might have had no users and is bad
	 * now.
	 */
	len = ARR_LEN(walker_env.sp_nodes);
	if(len == 0) {
		DEL_ARR_F(walker_env.sp_nodes);
		return;
	}

	be_ssa_construction_init(&senv, birg);
	be_ssa_construction_add_copies(&senv, walker_env.sp_nodes,
                                   ARR_LEN(walker_env.sp_nodes));
	be_ssa_construction_fix_users_array(&senv, walker_env.sp_nodes,
	                              ARR_LEN(walker_env.sp_nodes));

	if(lv != NULL) {
		len = ARR_LEN(walker_env.sp_nodes);
		for(i = 0; i < len; ++i) {
			be_liveness_update(lv, walker_env.sp_nodes[i]);
		}
		be_ssa_construction_update_liveness_phis(&senv, lv);
	}

	phis = be_ssa_construction_get_new_phis(&senv);

	/* set register requirements for stack phis */
	len = ARR_LEN(phis);
	for(i = 0; i < len; ++i) {
		ir_node *phi = phis[i];
		be_set_phi_reg_req(walker_env.arch_env, phi, &env->sp_req);
		be_set_phi_flags(walker_env.arch_env, phi, arch_irn_flags_ignore | arch_irn_flags_modify_sp);
		arch_set_irn_register(walker_env.arch_env, phi, env->isa->sp);
	}
	be_ssa_construction_destroy(&senv);

	DEL_ARR_F(walker_env.sp_nodes);
}

static int process_stack_bias(be_abi_irg_t *env, ir_node *bl, int bias)
{
	const arch_env_t *arch_env = env->birg->main_env->arch_env;
	int omit_fp            = env->call->flags.bits.try_omit_fp;
	ir_node *irn;

	sched_foreach(bl, irn) {
		int ofs;

		/*
		   Check, if the node relates to an entity on the stack frame.
		   If so, set the true offset (including the bias) for that
		   node.
		 */
		ir_entity *ent = arch_get_frame_entity(arch_env, irn);
		if(ent) {
			int offset = get_stack_entity_offset(env->frame, ent, bias);
			arch_set_frame_offset(arch_env, irn, offset);
			DBG((env->dbg, LEVEL_2, "%F has offset %d (including bias %d)\n", ent, offset, bias));
		}

		if(omit_fp || be_is_IncSP(irn)) {
			/*
			 * If the node modifies the stack pointer by a constant offset,
			 * record that in the bias.
			 */
			ofs = arch_get_sp_bias(arch_env, irn);

			if(be_is_IncSP(irn)) {
				if(ofs == BE_STACK_FRAME_SIZE_EXPAND) {
					ofs = get_type_size_bytes(get_irg_frame_type(env->birg->irg));
					be_set_IncSP_offset(irn, ofs);
				} else if(ofs == BE_STACK_FRAME_SIZE_SHRINK) {
					ofs = - get_type_size_bytes(get_irg_frame_type(env->birg->irg));
					be_set_IncSP_offset(irn, ofs);
				}
			}

			if(omit_fp)
				bias += ofs;
		}
	}

	return bias;
}

/**
 * A helper struct for the bias walker.
 */
struct bias_walk {
	be_abi_irg_t *env;     /**< The ABI irg environment. */
	int start_block_bias;  /**< The bias at the end of the start block. */
	ir_node *start_block;  /**< The start block of the current graph. */
};

/**
 * Block-Walker: fix all stack offsets
 */
static void stack_bias_walker(ir_node *bl, void *data)
{
	struct bias_walk *bw = data;
	if (bl != bw->start_block) {
		process_stack_bias(bw->env, bl, bw->start_block_bias);
	}
}

void be_abi_fix_stack_bias(be_abi_irg_t *env)
{
	ir_graph *irg  = env->birg->irg;
	struct bias_walk bw;

	stack_frame_compute_initial_offset(env->frame);
	// stack_layout_dump(stdout, env->frame);

	/* Determine the stack bias at the end of the start block. */
	bw.start_block_bias = process_stack_bias(env, get_irg_start_block(irg), 0);

	/* fix the bias is all other blocks */
	bw.env = env;
	bw.start_block = get_irg_start_block(irg);
	irg_block_walk_graph(irg, stack_bias_walker, NULL, &bw);
}

ir_node *be_abi_get_callee_save_irn(be_abi_irg_t *abi, const arch_register_t *reg)
{
	assert(arch_register_type_is(reg, callee_save));
	assert(pmap_contains(abi->regs, (void *) reg));
	return pmap_get(abi->regs, (void *) reg);
}

ir_node *be_abi_get_ignore_irn(be_abi_irg_t *abi, const arch_register_t *reg)
{
	assert(arch_register_type_is(reg, ignore));
	assert(pmap_contains(abi->regs, (void *) reg));
	return pmap_get(abi->regs, (void *) reg);
}

/**
 * Returns non-zero if the ABI has omitted the frame pointer in
 * the current graph.
 */
int be_abi_omit_fp(const be_abi_irg_t *abi) {
	return abi->call->flags.bits.try_omit_fp;
}
