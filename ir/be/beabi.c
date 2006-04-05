/**
 * ABI lowering.
 *
 * @author Sebastian Hack
 * @date 7.3.2005
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "obst.h"
#include "offset.h"

#include "type.h"
#include "irgopt.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irprintf_t.h"
#include "irgopt.h"

#include "be.h"
#include "beabi.h"
#include "bearch.h"
#include "benode_t.h"
#include "belive_t.h"
#include "besched_t.h"

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

typedef struct _be_abi_call_arg_t {
	unsigned is_res   : 1;
	unsigned in_reg   : 1;
	unsigned on_stack : 1;

	int pos;
	const arch_register_t *reg;
	entity *stack_ent;
	unsigned alignment;
	unsigned space_before;
	unsigned space_after;
} be_abi_call_arg_t;

struct _be_abi_call_t {
	be_abi_call_flags_t flags;
	const be_abi_callbacks_t *cb;
	type *between_type;
	set *params;
};

#define N_FRAME_TYPES 3

typedef struct _be_stack_frame_t {
	type *arg_type;
	type *between_type;
	type *frame_type;

	type *order[N_FRAME_TYPES];        /**< arg, between and frame types ordered. */

	int initial_offset;
	int stack_dir;
} be_stack_frame_t;

struct _be_stack_slot_t {
	struct _be_stack_frame_t *frame;
	entity *ent;
};

struct _be_abi_irg_t {
	struct obstack       obst;
	be_stack_frame_t     *frame;        /**< The stack frame model. */
	const be_irg_t       *birg;         /**< The back end IRG. */
	const arch_isa_t     *isa;          /**< The isa. */
	survive_dce_t        *dce_survivor;

	be_abi_call_t        *call;         /**< The ABI call information. */
	type                 *method_type;  /**< The type of the method of the IRG. */

	ir_node              *init_sp;      /**< The node representing the stack pointer
									     at the start of the function. */

	ir_node              *reg_params;   /**< The reg params node. */
	pmap                 *regs;         /**< A map of all callee-save and ignore regs to
											their Projs to the RegParams node. */

	pset                 *stack_phis;   /**< The set of all Phi nodes inserted due to
											stack pointer modifying nodes. */

	int                  start_block_bias;	/**< The stack bias at the end of the start block. */

	void                 *cb;           /**< ABI Callback self pointer. */

	pmap                 *keep_map;     /**< mapping blocks to keep nodes. */
	pset                 *ignore_regs;  /**< Additional registers which shall be ignored. */

	arch_irn_handler_t irn_handler;
	arch_irn_ops_t     irn_ops;
	DEBUG_ONLY(firm_dbg_module_t    *dbg;)          /**< The debugging module. */
};

#define get_abi_from_handler(ptr) firm_container_of(ptr, be_abi_irg_t, irn_handler)
#define get_abi_from_ops(ptr)     firm_container_of(ptr, be_abi_irg_t, irn_ops)

/* Forward, since be need it in be_abi_introduce(). */
static const arch_irn_ops_if_t abi_irn_ops;
static const arch_irn_handler_t abi_irn_handler;

/* Flag: if set, try to omit the frame pointer if called by the backend */
int be_omit_fp = 1;

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
	call->flags        = flags;
	call->cb           = cb;
}

void be_abi_call_param_stack(be_abi_call_t *call, int arg_pos, unsigned alignment, unsigned space_before, unsigned space_after)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 0, arg_pos, 1);
	arg->on_stack     = 1;
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
static be_abi_call_t *be_abi_call_new()
{
	be_abi_call_t *call = xmalloc(sizeof(call[0]));
	call->flags.val  = 0;
	call->params     = new_set(cmp_call_arg, 16);
	call->cb         = NULL;

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

static int get_stack_entity_offset(be_stack_frame_t *frame, entity *ent, int bias)
{
	type *t = get_entity_owner(ent);
	int ofs = get_entity_offset_bytes(ent);

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
static entity *search_ent_with_offset(type *t, int offset)
{
	int i, n;

	for(i = 0, n = get_class_n_members(t); i < n; ++i) {
		entity *ent = get_class_member(t, i);
		if(get_entity_offset_bytes(ent) == offset)
			return ent;
	}

	return NULL;
}

static int stack_frame_compute_initial_offset(be_stack_frame_t *frame)
{
	type   *base = frame->stack_dir < 0 ? frame->between_type : frame->frame_type;
	entity *ent  = search_ent_with_offset(base, 0);
	frame->initial_offset = 0;
	frame->initial_offset = get_stack_entity_offset(frame, ent, 0);
	return frame->initial_offset;
}

static be_stack_frame_t *stack_frame_init(be_stack_frame_t *frame, type *args, type *between, type *locals, int stack_dir)
{
	frame->arg_type       = args;
	frame->between_type   = between;
	frame->frame_type     = locals;
	frame->initial_offset = 0;
	frame->stack_dir      = stack_dir;
	frame->order[1]       = between;

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

static void stack_frame_dump(FILE *file, be_stack_frame_t *frame)
{
	int i, j, n;

	ir_fprintf(file, "initial offset: %d\n", frame->initial_offset);
	for(j = 0; j < N_FRAME_TYPES; ++j) {
		type *t = frame->order[j];

		ir_fprintf(file, "type %d: %Fm size: %d\n", j, t, get_type_size_bytes(t));
		for(i = 0, n = get_class_n_members(t); i < n; ++i) {
			entity *ent = get_class_member(t, i);
			ir_fprintf(file, "\t%F int ofs: %d glob ofs: %d\n", ent, get_entity_offset_bytes(ent), get_stack_entity_offset(frame, ent, 0));
		}
	}
}

/**
 * If irn is a Sel node computes the address of an entity
 * on the frame type return the entity, else NULL.
 */
static INLINE entity *get_sel_ent(ir_node *irn)
{
	if(is_Sel(irn) && get_Sel_ptr(irn) == get_irg_frame(get_irn_irg(irn))) {
		return get_Sel_entity(irn);
	}

	return NULL;
}

/**
 * Walker: Replaces Loads, Stores and Sels of frame type entities
 * by FrameLoad, FrameStore and FrameAdress.
 */
static void lower_frame_sels_walker(ir_node *irn, void *data)
{
	ir_node *nw  = NULL;
	entity *ent = get_sel_ent(irn);

	if(ent != NULL) {
		be_abi_irg_t *env = data;
		ir_node *bl       = get_nodes_block(irn);
		ir_graph *irg     = get_irn_irg(bl);
		ir_node *frame    = get_irg_frame(irg);

		nw = be_new_FrameAddr(env->isa->sp->reg_class, irg, bl, frame, ent);
		exchange(irn, nw);
	}
}

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
 * Transform a call node.
 * @param env The ABI environment for the current irg.
 * @param irn The call node.
 * @param curr_sp The stack pointer node to use.
 * @return The stack pointer after the call.
 */
static ir_node *adjust_call(be_abi_irg_t *env, ir_node *irn, ir_node *curr_sp)
{
	ir_graph *irg             = env->birg->irg;
	const arch_isa_t *isa     = env->birg->main_env->arch_env->isa;
	be_abi_call_t *call       = be_abi_call_new();
	ir_type *mt               = get_Call_type(irn);
	ir_node *call_ptr         = get_Call_ptr(irn);
	int n_params              = get_method_n_params(mt);
	ir_node *curr_mem         = get_Call_mem(irn);
	ir_node *bl               = get_nodes_block(irn);
	pset *results             = pset_new_ptr(8);
	pset *caller_save         = pset_new_ptr(8);
	int stack_size            = 0;
	int stack_dir             = arch_isa_stack_dir(isa);
	const arch_register_t *sp = arch_isa_sp(isa);
	ir_mode *mach_mode        = sp->reg_class->mode;
	struct obstack *obst      = &env->obst;
	ir_node *no_mem           = get_irg_no_mem(irg);
	int no_alloc              = call->flags.bits.frame_is_setup_on_call;

	ir_node *res_proj = NULL;
	int curr_res_proj = pn_Call_max;
	int n_low_args    = 0;
	int n_pos         = 0;

	ir_node *low_call;
	ir_node **in;
	ir_node **res_projs;
	const ir_edge_t *edge;
	int *low_args;
	int *pos;
	int i, n;

	/* Let the isa fill out the abi description for that call node. */
	arch_isa_get_call_abi(isa, mt, call);

	/* Insert code to put the stack arguments on the stack. */
	assert(get_Call_n_params(irn) == n_params);
	for(i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		assert(arg);
		if(arg->on_stack) {
			stack_size += arg->space_before;
			stack_size =  round_up2(stack_size, arg->alignment);
			stack_size += get_type_size_bytes(get_method_param_type(mt, i));
			stack_size += arg->space_after;
			obstack_int_grow(obst, i);
			n_pos++;
		}
	}
	pos = obstack_finish(obst);

	/* Collect all arguments which are passed in registers. */
	for(i = 0, n = get_Call_n_params(irn); i < n; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		if(arg && arg->in_reg) {
			obstack_int_grow(obst, i);
			n_low_args++;
		}
	}
	low_args = obstack_finish(obst);

	/* If there are some parameters which shall be passed on the stack. */
	if(n_pos > 0) {
		int curr_ofs      = 0;
		int do_seq        = call->flags.bits.store_args_sequential && !no_alloc;

		/*
		 * Reverse list of stack parameters if call arguments are from left to right.
		 * We must them reverse again in they are pushed (not stored) and the stack
		 * direction is downwards.
		 */
		if (call->flags.bits.left_to_right ^ (do_seq && stack_dir < 0)) {
			for(i = 0; i < n_pos >> 1; ++i) {
				int other  = n_pos - i - 1;
				int tmp    = pos[i];
				pos[i]     = pos[other];
				pos[other] = tmp;
			}
		}

		/*
		 * If the stack is decreasing and we do not want to store sequentially,
		 * or someone else allocated the call frame
		 * we allocate as much space on the stack all parameters need, by
		 * moving the stack pointer along the stack's direction.
		 */
		if(stack_dir < 0 && !do_seq && !no_alloc) {
			curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, no_mem, stack_size, be_stack_dir_expand);
		}

		assert(mode_is_reference(mach_mode) && "machine mode must be pointer");
		for(i = 0; i < n_pos; ++i) {
			int p                  = pos[i];
			be_abi_call_arg_t *arg = get_call_arg(call, 0, p);
			ir_node *param         = get_Call_param(irn, p);
			ir_node *addr          = curr_sp;
			ir_node *mem           = NULL;
			type *param_type       = get_method_param_type(mt, p);
			int param_size         = get_type_size_bytes(param_type) + arg->space_after;

			curr_ofs += arg->space_before;
			curr_ofs =  round_up2(curr_ofs, arg->alignment);

			/* Make the expression to compute the argument's offset. */
			if(curr_ofs > 0) {
				addr = new_r_Const_long(irg, bl, mode_Is, curr_ofs);
				addr = new_r_Add(irg, bl, curr_sp, addr, mach_mode);
			}

			/* Insert a store for primitive arguments. */
			if(is_atomic_type(param_type)) {
				mem = new_r_Store(irg, bl, curr_mem, addr, param);
				mem = new_r_Proj(irg, bl, mem, mode_M, pn_Store_M);
			}

			/* Make a mem copy for compound arguments. */
			else {
				assert(mode_is_reference(get_irn_mode(param)));
				mem = new_r_CopyB(irg, bl, curr_mem, addr, param, param_type);
				mem = new_r_Proj(irg, bl, mem, mode_M, pn_CopyB_M_regular);
			}

			obstack_ptr_grow(obst, mem);

			curr_ofs += param_size;

			/*
			 * If we wanted to build the arguments sequentially,
			 * the stack pointer for the next must be incremented,
			 * and the memory value propagated.
			 */
			if(do_seq) {
				curr_ofs = 0;
				curr_sp  = be_new_IncSP(sp, irg, bl, curr_sp, curr_mem, param_size, be_stack_dir_expand);
				curr_mem = mem;
			}
		}

		in = (ir_node **) obstack_finish(obst);

		/* We need the sync only, if we didn't build the stores sequentially. */
		if(!do_seq)
			curr_mem = new_r_Sync(irg, bl, n_pos, in);
		obstack_free(obst, in);
	}

	/* Collect caller save registers */
	for(i = 0, n = arch_isa_get_n_reg_class(isa); i < n; ++i) {
		int j;
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		for(j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = arch_register_for_index(cls, j);
			if(arch_register_type_is(reg, caller_save))
				pset_insert_ptr(caller_save, (void *) reg);
		}
	}

	/* search the greatest result proj number */

	/* TODO: what if the result is NOT used? Currently there is
	 * no way to detect this later, especially there is no way to
	 * see this in the proj numbers.
	 * While this is ok for the register allocator, it is bad for
	 * backends which need to change the be_Call further (x87 simulator
	 * for instance. However for this particular case the call_type is
	 * sufficient.).
	 */
	foreach_out_edge(irn, edge) {
		const ir_edge_t *res_edge;
		ir_node *irn = get_edge_src_irn(edge);

		if(is_Proj(irn) && get_Proj_proj(irn) == pn_Call_T_result) {
			res_proj = irn;
			foreach_out_edge(irn, res_edge) {
				int proj;
				be_abi_call_arg_t *arg;
				ir_node *res = get_edge_src_irn(res_edge);

				assert(is_Proj(res));

				proj = get_Proj_proj(res);
				arg = get_call_arg(call, 1, proj);

				/*
					shift the proj number to the right, since we will drop the
					unspeakable Proj_T from the Call. Therefore, all real argument
					Proj numbers must be increased by pn_be_Call_first_res
				*/
				proj += pn_be_Call_first_res;
				set_Proj_proj(res, proj);
				obstack_ptr_grow(obst, res);

				if(proj > curr_res_proj)
					curr_res_proj = proj;
				if(arg->in_reg) {
					pset_remove_ptr(caller_save, arg->reg);
					//pmap_insert(arg_regs, arg->reg, INT_TO_PTR(proj + 1))
				}
			}
		}
	}

	curr_res_proj++;
	obstack_ptr_grow(obst, NULL);
	res_projs = obstack_finish(obst);

	/* make the back end call node and set its register requirements. */
	for(i = 0; i < n_low_args; ++i)
		obstack_ptr_grow(obst, get_Call_param(irn, low_args[i]));

	in = obstack_finish(obst);

	if(env->call->flags.bits.call_has_imm && get_irn_opcode(call_ptr) == iro_SymConst) {
		low_call = be_new_Call(get_irn_dbg_info(irn), irg, bl, curr_mem, curr_sp, curr_sp,
		                       curr_res_proj + pset_count(caller_save), n_low_args, in,
		                       get_Call_type(irn));
		be_Call_set_entity(low_call, get_SymConst_entity(call_ptr));
	}

	else
		low_call = be_new_Call(get_irn_dbg_info(irn), irg, bl, curr_mem, curr_sp, call_ptr,
		                       curr_res_proj + pset_count(caller_save), n_low_args, in,
		                       get_Call_type(irn));

	/*
		TODO:
		Set the register class of the call address to the same as the stack pointer's.
		That' probably buggy for some architectures.
	*/
	be_node_set_reg_class(low_call, be_pos_Call_ptr, sp->reg_class);

	/* Set the register classes and constraints of the Call parameters. */
	for(i = 0; i < n_low_args; ++i) {
		int index = low_args[i];
		be_abi_call_arg_t *arg = get_call_arg(call, 0, index);
		assert(arg->reg != NULL);

		be_set_constr_single_reg(low_call, be_pos_Call_first_arg + index, arg->reg);
	}

	/* Set the register constraints of the results. */
	for(i = 0; res_projs[i]; ++i) {
		ir_node *irn                 = res_projs[i];
		int proj                     = get_Proj_proj(irn);

		/* Correct Proj number since it has been adjusted! (see above) */
		const be_abi_call_arg_t *arg = get_call_arg(call, 1, proj - pn_Call_max);

		assert(arg->in_reg);
		be_set_constr_single_reg(low_call, BE_OUT_POS(proj), arg->reg);
	}
	obstack_free(obst, in);
	exchange(irn, low_call);

	/* redirect the result projs to the lowered call instead of the Proj_T */
	for(i = 0; res_projs[i]; ++i)
		set_Proj_pred(res_projs[i], low_call);

	/* Make additional projs for the caller save registers
	   and the Keep node which keeps them alive. */
	if(pset_count(caller_save) > 0) {
		const arch_register_t *reg;
		ir_node **in, *keep;
		int i, n;

		for(reg = pset_first(caller_save), n = 0; reg; reg = pset_next(caller_save), ++n) {
			ir_node *proj = new_r_Proj(irg, bl, low_call, reg->reg_class->mode, curr_res_proj);

			/* memorize the register in the link field. we need afterwards to set the register class of the keep correctly. */
			be_set_constr_single_reg(low_call, BE_OUT_POS(curr_res_proj), reg);
			set_irn_link(proj, (void *) reg);
			obstack_ptr_grow(obst, proj);
			curr_res_proj++;
		}

		in   = (ir_node **) obstack_finish(obst);
		keep = be_new_Keep(NULL, irg, bl, n, in);
		for(i = 0; i < n; ++i) {
			const arch_register_t *reg = get_irn_link(in[i]);
			be_node_set_reg_class(keep, i, reg->reg_class);
		}
		obstack_free(obst, in);
	}

	/* Clean up the stack. */
	if(stack_size > 0) {
		ir_node *mem_proj = NULL;

		foreach_out_edge(low_call, edge) {
			ir_node *irn = get_edge_src_irn(edge);
			if(is_Proj(irn) && get_Proj_proj(irn) == pn_Call_M) {
				mem_proj = irn;
				break;
			}
		}

		if(!mem_proj)
			mem_proj = new_r_Proj(irg, bl, low_call, mode_M, pn_Call_M);

		 /* Clean up the stack frame if we allocated it */
		if(!no_alloc)
			curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, mem_proj, stack_size, be_stack_dir_shrink);
	}

	be_abi_call_free(call);
	obstack_free(obst, pos);
	del_pset(results);
	del_pset(caller_save);

	return curr_sp;
}

/**
 * Adjust an alloca.
 * The alloca is transformed into a back end alloca node and connected to the stack nodes.
 */
static ir_node *adjust_alloc(be_abi_irg_t *env, ir_node *alloc, ir_node *curr_sp)
{
	if (get_Alloc_where(alloc) == stack_alloc) {
		ir_node *bl        = get_nodes_block(alloc);
		ir_graph *irg      = get_irn_irg(bl);
		ir_node *alloc_mem = NULL;
		ir_node *alloc_res = NULL;

		const ir_edge_t *edge;
		ir_node *new_alloc;

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
		if (alloc_res == NULL) {
			exchange(alloc_mem, get_Alloc_mem(alloc));
			return curr_sp;
		}

		/* The stack pointer will be modified in an unknown manner.
		   We cannot omit it. */
		env->call->flags.bits.try_omit_fp = 0;
		new_alloc = be_new_AddSP(env->isa->sp, irg, bl, curr_sp, get_Alloc_size(alloc));

		exchange(alloc_res, env->isa->stack_dir < 0 ? new_alloc : curr_sp);

		if(alloc_mem != NULL)
			exchange(alloc_mem, new_r_NoMem(irg));

		curr_sp = new_alloc;
	}

	return curr_sp;
}

/**
 * Walker for dependent_on().
 * This function searches a node tgt recursively from a given node
 * but is restricted to the given block.
 * @return 1 if tgt was reachable from curr, 0 if not.
 */
static int check_dependence(ir_node *curr, ir_node *tgt, ir_node *bl, unsigned long visited_nr)
{
	int n, i;

	if(get_irn_visited(curr) >= visited_nr)
		return 0;

	set_irn_visited(curr, visited_nr);
	if(get_nodes_block(curr) != bl)
		return 0;

	if(curr == tgt)
		return 1;

	for(i = 0, n = get_irn_arity(curr); i < n; ++i) {
		if(check_dependence(get_irn_n(curr, i), tgt, bl, visited_nr))
			return 1;
	}

	return 0;
}

/**
 * Check if a node is somehow data dependent on another one.
 * both nodes must be in the same basic block.
 * @param n1 The first node.
 * @param n2 The second node.
 * @return 1, if n1 is data dependent (transitively) on n2, 0 if not.
 */
static int dependent_on(ir_node *n1, ir_node *n2)
{
	ir_node *bl   = get_nodes_block(n1);
	ir_graph *irg = get_irn_irg(bl);
	long vis_nr   = get_irg_visited(irg) + 1;

	assert(bl == get_nodes_block(n2));
	set_irg_visited(irg, vis_nr);
	return check_dependence(n1, n2, bl, vis_nr);
}

static int cmp_call_dependecy(const void *c1, const void *c2)
{
	ir_node *n1 = *(ir_node **) c1;
	ir_node *n2 = *(ir_node **) c2;

	/*
		Classical qsort() comparison function behavior:
		0  if both elements are equal
		1  if second is "smaller" that first
		-1 if first is "smaller" that second
	*/
	return n1 == n2 ? 0 : (dependent_on(n1, n2) ? -1 : 1);
}

static void link_calls_in_block_walker(ir_node *irn, void *data)
{
	if(is_Call(irn)) {
		be_abi_irg_t *env = data;
		ir_node *bl       = get_nodes_block(irn);
		void *save        = get_irn_link(bl);

		env->call->flags.bits.irg_is_leaf = 0;

		set_irn_link(irn, save);
		set_irn_link(bl, irn);
	}
}

/**
 * Process all call nodes inside a basic block.
 * Note that the link field of the block must contain a linked list of all
 * Call nodes inside the block. We first order this list according to data dependency
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
		qsort(nodes, n, sizeof(nodes[0]), cmp_call_dependecy);

		for(i = n - 1; i >= 0; --i) {
			ir_node *irn = nodes[i];

			switch(get_irn_opcode(irn)) {
			case iro_Call:
				curr_sp = adjust_call(env, irn, curr_sp);
				break;
			case iro_Alloc:
				curr_sp = adjust_alloc(env, irn, curr_sp);
				break;
			default:
				break;
			}
		}

		obstack_free(&env->obst, nodes);

		/* Keep the last stack state in the block by tying it to Keep node */
		nodes[0] = curr_sp;
		keep     = be_new_Keep(env->isa->sp->reg_class, get_irn_irg(bl), bl, 1, nodes);
		pmap_insert(env->keep_map, bl, keep);
	}

	set_irn_link(bl, curr_sp);
}

/**
 * Adjust all call nodes in the graph to the ABI conventions.
 */
static void process_calls(be_abi_irg_t *env)
{
	ir_graph *irg = env->birg->irg;

	env->call->flags.bits.irg_is_leaf = 1;
	irg_walk_graph(irg, firm_clear_link, link_calls_in_block_walker, env);
	irg_block_walk_graph(irg, NULL, process_calls_in_block, env);
}

static void collect_return_walker(ir_node *irn, void *data)
{
	if(get_irn_opcode(irn) == iro_Return) {
		struct obstack *obst = data;
		obstack_ptr_grow(obst, irn);
	}
}

#if 0 /*
static ir_node *setup_frame(be_abi_irg_t *env)
{
	const arch_isa_t *isa = env->birg->main_env->arch_env->isa;
	const arch_register_t *sp = isa->sp;
	const arch_register_t *bp = isa->bp;
	be_abi_call_flags_bits_t flags = env->call->flags.bits;
	ir_graph *irg      = env->birg->irg;
	ir_node *bl        = get_irg_start_block(irg);
	ir_node *no_mem    = get_irg_no_mem(irg);
	ir_node *old_frame = get_irg_frame(irg);
	ir_node *stack     = pmap_get(env->regs, (void *) sp);
	ir_node *frame     = pmap_get(env->regs, (void *) bp);

	int stack_nr       = get_Proj_proj(stack);

	if(flags.try_omit_fp) {
		stack = be_new_IncSP(sp, irg, bl, stack, no_mem, BE_STACK_FRAME_SIZE, be_stack_dir_expand);
		frame = stack;
	}

	else {
		frame = be_new_Copy(bp->reg_class, irg, bl, stack);

		be_node_set_flags(frame, -1, arch_irn_flags_dont_spill);
		if(!flags.fp_free) {
			be_set_constr_single_reg(frame, -1, bp);
			be_node_set_flags(frame, -1, arch_irn_flags_ignore);
			arch_set_irn_register(env->birg->main_env->arch_env, frame, bp);
		}

		stack = be_new_IncSP(sp, irg, bl, stack, frame, BE_STACK_FRAME_SIZE, be_stack_dir_expand);
	}

	be_node_set_flags(env->reg_params, -(stack_nr + 1), arch_irn_flags_ignore);
	env->init_sp = stack;
	set_irg_frame(irg, frame);
	edges_reroute(old_frame, frame, irg);

	return frame;
}

static void clearup_frame(be_abi_irg_t *env, ir_node *ret, pmap *reg_map, struct obstack *obst)
{
	const arch_isa_t *isa = env->birg->main_env->arch_env->isa;
	const arch_register_t *sp = isa->sp;
	const arch_register_t *bp = isa->bp;
	ir_graph *irg      = env->birg->irg;
	ir_node *ret_mem   = get_Return_mem(ret);
	ir_node *frame     = get_irg_frame(irg);
	ir_node *bl        = get_nodes_block(ret);
	ir_node *stack     = get_irn_link(bl);

	pmap_entry *ent;

	if(env->call->flags.bits.try_omit_fp) {
		stack = be_new_IncSP(sp, irg, bl, stack, ret_mem, BE_STACK_FRAME_SIZE, be_stack_dir_shrink);
	}

	else {
		stack = be_new_SetSP(sp, irg, bl, stack, frame, ret_mem);
		be_set_constr_single_reg(stack, -1, sp);
		be_node_set_flags(stack, -1, arch_irn_flags_ignore);
	}

	pmap_foreach(env->regs, ent) {
		const arch_register_t *reg = ent->key;
		ir_node *irn               = ent->value;

		if(reg == sp)
			obstack_ptr_grow(&env->obst, stack);
		else if(reg == bp)
			obstack_ptr_grow(&env->obst, frame);
		else if(arch_register_type_is(reg, callee_save) || arch_register_type_is(reg, ignore))
			obstack_ptr_grow(obst, irn);
	}
}
*/
#endif

static ir_type *compute_arg_type(be_abi_irg_t *env, be_abi_call_t *call, ir_type *method_type)
{
	int dir  = env->call->flags.bits.left_to_right ? 1 : -1;
	int inc  = env->birg->main_env->arch_env->isa->stack_dir * dir;
	int n    = get_method_n_params(method_type);
	int curr = inc > 0 ? 0 : n - 1;
	int ofs  = 0;

	char buf[128];
	ir_type *res;
	int i;

	snprintf(buf, sizeof(buf), "%s_arg_type", get_entity_name(get_irg_entity(env->birg->irg)));
	res = new_type_class(new_id_from_str(buf));

	for(i = 0; i < n; ++i, curr += inc) {
		type *param_type       = get_method_param_type(method_type, curr);
		be_abi_call_arg_t *arg = get_call_arg(call, 0, curr);

		if(arg->on_stack) {
			snprintf(buf, sizeof(buf), "param_%d", i);
			arg->stack_ent = new_entity(res, new_id_from_str(buf), param_type);
			ofs += arg->space_before;
			ofs = round_up2(ofs, arg->alignment);
			set_entity_offset_bytes(arg->stack_ent, ofs);
			ofs += arg->space_after;
			ofs += get_type_size_bytes(param_type);
		}
	}

	set_type_size_bytes(res, ofs);
	return res;
}

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

static ir_node *create_barrier(be_abi_irg_t *env, ir_node *bl, ir_node **mem, pmap *regs, int in_req)
{
	ir_graph *irg = env->birg->irg;
	int n;
	int n_regs = pmap_count(regs);
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
		int pos = BE_OUT_POS(n);
		ir_node *proj;
		const arch_register_t *reg = rm[n].reg;

		proj = new_r_Proj(irg, bl, irn, get_irn_mode(rm[n].irn), n);
		be_node_set_reg_class(irn, n, reg->reg_class);
		if(in_req)
			be_set_constr_single_reg(irn, n, reg);
		be_set_constr_single_reg(irn, pos, reg);
		be_node_set_reg_class(irn, pos, reg->reg_class);
		arch_set_irn_register(env->birg->main_env->arch_env, proj, reg);

		/* if the proj projects a ignore register or a node which is set to ignore, propagate this property. */
		if(arch_register_type_is(reg, ignore) || arch_irn_is(env->birg->main_env->arch_env, in[n], ignore))
			be_node_set_flags(irn, pos, arch_irn_flags_ignore);

		pmap_insert(regs, (void *) reg, proj);
	}

	if(mem) {
		*mem = new_r_Proj(irg, bl, irn, mode_M, n);
	}

	obstack_free(&env->obst, rm);
	return irn;
}

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
	ir_node *arg_tuple        = get_irg_args(irg);
	ir_node *no_mem           = get_irg_no_mem(irg);
	ir_node *mem              = get_irg_initial_mem(irg);
	type *method_type         = get_entity_type(get_irg_entity(irg));
	pset *dont_save           = pset_new_ptr(8);
	int n_params              = get_method_n_params(method_type);
	int max_arg               = 0;

	int i, j, n;

	reg_node_map_t *rm;
	const arch_register_t *fp_reg;
	ir_node *frame_pointer;
	ir_node *barrier;
	ir_node *reg_params_bl;
	ir_node **args;
	const ir_edge_t *edge;
	ir_type *arg_type, *bet_type;

	pmap_entry *ent;
	bitset_t *used_proj_nr;
	DEBUG_ONLY(firm_dbg_module_t *dbg = env->dbg;)

	DBG((dbg, LEVEL_1, "introducing abi on %+F\n", irg));

	/* Convert the Sel nodes in the irg to frame load/store/addr nodes. */
	irg_walk_graph(irg, lower_frame_sels_walker, NULL, env);

	env->frame = obstack_alloc(&env->obst, sizeof(env->frame[0]));
	env->regs  = pmap_create();

	/* Find the maximum proj number of the argument tuple proj */
	foreach_out_edge(arg_tuple, edge)  {
		ir_node *irn = get_edge_src_irn(edge);
		int nr       = get_Proj_proj(irn);
		max_arg      = MAX(max_arg, nr);
	}

	used_proj_nr = bitset_alloca(1024);
	max_arg      = MAX(max_arg + 1, n_params);
	args         = obstack_alloc(&env->obst, max_arg * sizeof(args[0]));
	memset(args, 0, max_arg * sizeof(args[0]));

	/* Fill the argument vector */
	foreach_out_edge(arg_tuple, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		int nr       = get_Proj_proj(irn);
		args[nr]     = irn;
		DBG((dbg, LEVEL_2, "\treading arg: %d -> %+F\n", nr, irn));
	}

	arg_type = compute_arg_type(env, call, method_type);
	bet_type = call->cb->get_between_type(env->cb);
	stack_frame_init(env->frame, arg_type, bet_type, get_irg_frame_type(irg), isa->stack_dir);

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
			if(arch_register_type_is(reg, callee_save) || arch_register_type_is(reg, ignore))
				pmap_insert(env->regs, (void *) reg, NULL);
		}
	}

	pmap_insert(env->regs, (void *) sp, NULL);
	pmap_insert(env->regs, (void *) isa->bp, NULL);
	reg_params_bl   = get_irg_start_block(irg);
	env->reg_params = be_new_RegParams(irg, reg_params_bl, pmap_count(env->regs));

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
		ir_node *arg_proj    = rm[i].irn;
		ir_node *proj;
		ir_mode *mode        = arg_proj ? get_irn_mode(arg_proj) : reg->reg_class->mode;
		long nr              = i;
		int pos              = BE_OUT_POS((int) nr);

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
			be_node_set_flags(env->reg_params, pos, arch_irn_flags_ignore);

		DBG((dbg, LEVEL_2, "\tregister save proj #%d -> reg %s\n", nr, reg->name));
	}
	obstack_free(&env->obst, rm);

	/* Generate the Prologue */
	fp_reg  = call->cb->prologue(env->cb, &mem, env->regs);
	barrier = create_barrier(env, bl, &mem, env->regs, 0);

	env->init_sp  = be_abi_reg_map_get(env->regs, sp);
	env->init_sp  = be_new_IncSP(sp, irg, bl, env->init_sp, no_mem, BE_STACK_FRAME_SIZE, be_stack_dir_expand);
	arch_set_irn_register(env->birg->main_env->arch_env, env->init_sp, sp);
	be_abi_reg_map_set(env->regs, sp, env->init_sp);
	frame_pointer = be_abi_reg_map_get(env->regs, fp_reg);
	set_irg_frame(irg, frame_pointer);
	pset_insert_ptr(env->ignore_regs, fp_reg);

	/* Now, introduce stack param nodes for all parameters passed on the stack */
	for(i = 0; i < max_arg; ++i) {
		ir_node *arg_proj = args[i];
		ir_node *repl     = NULL;

		if(arg_proj != NULL) {
			be_abi_call_arg_t *arg;
			ir_type *param_type;
			int nr = get_Proj_proj(arg_proj);

			nr         = MIN(nr, n_params);
			arg        = get_call_arg(call, 0, nr);
			param_type = get_method_param_type(method_type, nr);

			if(arg->in_reg) {
				repl = pmap_get(env->regs, (void *) arg->reg);
			}

			else if(arg->on_stack) {
				/* For atomic parameters which are actually used, we create a StackParam node. */
				if(is_atomic_type(param_type) && get_irn_n_edges(args[i]) > 0) {
					ir_mode *mode                    = get_type_mode(param_type);
					const arch_register_class_t *cls = arch_isa_get_reg_class_for_mode(isa, mode);
					repl = be_new_StackParam(cls, isa->bp->reg_class, irg, reg_params_bl, mode, frame_pointer, arg->stack_ent);
				}

				/* The stack parameter is not primitive (it is a struct or array),
				we thus will create a node representing the parameter's address
				on the stack. */
				else {
					repl = be_new_FrameAddr(sp->reg_class, irg, reg_params_bl, frame_pointer, arg->stack_ent);
				}
			}

			assert(repl != NULL);
			edges_reroute(args[i], repl, irg);
		}
	}

	/* All Return nodes hang on the End node, so look for them there. */
	for(i = 0, n = get_irn_arity(end); i < n; ++i) {
		ir_node *irn = get_irn_n(end, i);

		if(get_irn_opcode(irn) == iro_Return) {
			ir_node *bl    = get_nodes_block(irn);
			int n_res      = get_Return_n_ress(irn);
			pmap *reg_map  = pmap_create();
			ir_node *mem   = get_Return_mem(irn);
			ir_node *keep  = pmap_get(env->keep_map, bl);
			int in_max;
			ir_node *ret;
			int i, n;
			ir_node **in;
			ir_node *stack;
			const arch_register_t **regs;

			/*
				get the valid stack node in this block.
				If we had a call in that block there is a Keep constructed by process_calls()
				which points to the last stack modification in that block. we'll use
				it then. Else we use the stack from the start block and let
				the ssa construction fix the usage.
			*/
			stack = keep ? get_irn_n(keep, 0) : be_abi_reg_map_get(env->regs, sp);
			be_abi_reg_map_set(reg_map, sp, stack);

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

			/* Make the Epilogue node and call the arch's epilogue maker. */
			create_barrier(env, bl, &mem, reg_map, 1);
			call->cb->epilogue(env->cb, bl, &mem, reg_map);

			/*
				Maximum size of the in array for Return nodes is
				return args + callee save/ignore registers + memory + stack pointer
			*/
			in_max = pmap_count(reg_map) + get_Return_n_ress(irn) + 2;

			in   = obstack_alloc(&env->obst, in_max * sizeof(in[0]));
			regs = obstack_alloc(&env->obst, in_max * sizeof(regs[0]));

			in[0]   = mem;
			in[1]   = be_abi_reg_map_get(reg_map, sp);
			regs[0] = NULL;
			regs[1] = sp;
			n       = 2;

			/* clear SP entry, since it has already been grown. */
			pmap_insert(reg_map, (void *) sp, NULL);
			for(i = 0; i < n_res; ++i) {
				ir_node *res           = get_Return_res(irn, i);
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
			ret = be_new_Return(get_irn_dbg_info(irn), irg, bl, n, in);

			/* Set the register classes of the return's parameter accordingly. */
			for(i = 0; i < n; ++i)
				if(regs[i])
					be_node_set_reg_class(ret, i, regs[i]->reg_class);

			/* Free the space of the Epilog's in array and the register <-> proj map. */
			obstack_free(&env->obst, in);
			exchange(irn, ret);
			pmap_destroy(reg_map);
		}
	}

	del_pset(dont_save);
	obstack_free(&env->obst, args);
}

/**
 * Walker: puts all Alloc(stack_alloc) on a obstack
 */
static void collect_alloca_walker(ir_node *irn, void *data)
{
	be_abi_irg_t *env = data;
	if(get_irn_opcode(irn) == iro_Alloc && get_Alloc_where(irn) == stack_alloc)
		obstack_ptr_grow(&env->obst, irn);
}

be_abi_irg_t *be_abi_introduce(be_irg_t *birg)
{
	be_abi_irg_t *env  = xmalloc(sizeof(env[0]));
	ir_node *old_frame = get_irg_frame(birg->irg);
	ir_graph *irg      = birg->irg;

	pmap_entry *ent;
	ir_node *dummy;

	obstack_init(&env->obst);

	env->isa           = birg->main_env->arch_env->isa;
	env->method_type   = get_entity_type(get_irg_entity(irg));
	env->call          = be_abi_call_new();
	arch_isa_get_call_abi(env->isa, env->method_type, env->call);

	env->ignore_regs      = pset_new_ptr_default();
	env->keep_map         = pmap_create();
	env->dce_survivor     = new_survive_dce();
	env->birg             = birg;
	env->stack_phis       = pset_new_ptr(16);
	env->init_sp = dummy  = new_r_Unknown(irg, env->isa->sp->reg_class->mode);
	FIRM_DBG_REGISTER(env->dbg, "firm.be.abi");

	env->cb = env->call->cb->init(env->call, birg->main_env->arch_env, irg);

	memcpy(&env->irn_handler, &abi_irn_handler, sizeof(abi_irn_handler));
	env->irn_ops.impl = &abi_irn_ops;

	/* Lower all call nodes in the IRG. */
	process_calls(env);

	/* Process the IRG */
	modify_irg(env);

	/* We don't need the keep map anymore. */
	pmap_destroy(env->keep_map);

	/* reroute the stack origin of the calls to the true stack origin. */
	edges_reroute(dummy, env->init_sp, irg);
	edges_reroute(old_frame, get_irg_frame(irg), irg);

	/* Make some important node pointers survive the dead node elimination. */
	survive_dce_register_irn(env->dce_survivor, &env->init_sp);
	pmap_foreach(env->regs, ent)
		survive_dce_register_irn(env->dce_survivor, (ir_node **) &ent->value);

	arch_env_push_irn_handler(env->birg->main_env->arch_env, &env->irn_handler);

	env->call->cb->done(env->cb);
	be_liveness(irg);
	return env;
}

void be_abi_free(be_abi_irg_t *env)
{
	free_survive_dce(env->dce_survivor);
	del_pset(env->stack_phis);
	del_pset(env->ignore_regs);
	pmap_destroy(env->regs);
	obstack_free(&env->obst, NULL);
	arch_env_pop_irn_handler(env->birg->main_env->arch_env);
	free(env);
}

void be_abi_put_ignore_regs(be_abi_irg_t *abi, const arch_register_class_t *cls, bitset_t *bs)
{
	arch_register_t *reg;

	for(reg = pset_first(abi->ignore_regs); reg; reg = pset_next(abi->ignore_regs))
		if(reg->reg_class == cls)
			bitset_set(bs, reg->index);
}


/*

  _____ _        ____  _             _
 |  ___(_)_  __ / ___|| |_ __ _  ___| | __
 | |_  | \ \/ / \___ \| __/ _` |/ __| |/ /
 |  _| | |>  <   ___) | || (_| | (__|   <
 |_|   |_/_/\_\ |____/ \__\__,_|\___|_|\_\

*/

/**
 * Walker. Collect all stack modifying nodes.
 */
static void collect_stack_nodes_walker(ir_node *irn, void *data)
{
	pset *s = data;

	if(be_is_AddSP(irn)	|| be_is_IncSP(irn)	|| be_is_SetSP(irn))
		pset_insert_ptr(s, irn);
}

void be_abi_fix_stack_nodes(be_abi_irg_t *env)
{
	dom_front_info_t *df;
	pset *stack_nodes;

	/* We need dominance frontiers for fix up */
	df = be_compute_dominance_frontiers(env->birg->irg);
	stack_nodes = pset_new_ptr(16);
	pset_insert_ptr(stack_nodes, env->init_sp);
	irg_walk_graph(env->birg->irg, collect_stack_nodes_walker, NULL, stack_nodes);
	be_ssa_constr_set_phis(df, stack_nodes, env->stack_phis);
	del_pset(stack_nodes);

	/* Liveness could have changed due to Phi nodes. */
	be_liveness(env->birg->irg);

	/* free these dominance frontiers */
	be_free_dominance_frontiers(df);
}

/**
 * Translates a direction of an IncSP node (either be_stack_dir_shrink, or ...expand)
 * into -1 or 1, respectively.
 * @param irn The node.
 * @return 1, if the direction of the IncSP was along, -1 if against.
 */
static int get_dir(ir_node *irn)
{
	return 1 - 2 * (be_get_IncSP_direction(irn) == be_stack_dir_shrink);
}

static int process_stack_bias(be_abi_irg_t *env, ir_node *bl, int bias)
{
	const arch_env_t *aenv = env->birg->main_env->arch_env;
	int omit_fp            = env->call->flags.bits.try_omit_fp;
	ir_node *irn;

	sched_foreach(bl, irn) {

		/*
			If the node modifies the stack pointer by a constant offset,
			record that in the bias.
		*/
		if(be_is_IncSP(irn)) {
			int ofs = be_get_IncSP_offset(irn);
			int dir = get_dir(irn);

			if(ofs == BE_STACK_FRAME_SIZE) {
				ofs = get_type_size_bytes(get_irg_frame_type(env->birg->irg));
				be_set_IncSP_offset(irn, ofs);
			}

			if(omit_fp)
				bias += dir * ofs;
		}

		/*
			Else check, if the node relates to an entity on the stack frame.
			If so, set the true offset (including the bias) for that
			node.
		*/
		else {
			entity *ent = arch_get_frame_entity(aenv, irn);
			if(ent) {
				int offset = get_stack_entity_offset(env->frame, ent, bias);
				arch_set_frame_offset(aenv, irn, offset);
				DBG((env->dbg, LEVEL_2, "%F has offset %d\n", ent, offset));
			}
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
};

/**
 * Block-Walker: fix all stack offsets
 */
static void stack_bias_walker(ir_node *bl, void *data)
{
	if(bl != get_irg_start_block(get_irn_irg(bl))) {
		struct bias_walk *bw = data;
		process_stack_bias(bw->env, bl, bw->start_block_bias);
	}
}

void be_abi_fix_stack_bias(be_abi_irg_t *env)
{
	ir_graph *irg  = env->birg->irg;
	struct bias_walk bw;

	stack_frame_compute_initial_offset(env->frame);
	// stack_frame_dump(stdout, env->frame);

	/* Determine the stack bias at the end of the start block. */
	bw.start_block_bias = process_stack_bias(env, get_irg_start_block(irg), 0);

	/* fix the bias is all other blocks */
	bw.env = env;
	irg_block_walk_graph(irg, stack_bias_walker, NULL, &bw);
}

ir_node *be_abi_get_callee_save_irn(be_abi_irg_t *abi, const arch_register_t *reg)
{
	assert(arch_register_type_is(reg, callee_save));
	assert(pmap_contains(abi->regs, (void *) reg));
	return pmap_get(abi->regs, (void *) reg);
}

/*
  _____ _____  _   _   _    _                 _ _
 |_   _|  __ \| \ | | | |  | |               | | |
   | | | |__) |  \| | | |__| | __ _ _ __   __| | | ___ _ __
   | | |  _  /| . ` | |  __  |/ _` | '_ \ / _` | |/ _ \ '__|
  _| |_| | \ \| |\  | | |  | | (_| | | | | (_| | |  __/ |
 |_____|_|  \_\_| \_| |_|  |_|\__,_|_| |_|\__,_|_|\___|_|

  for Phi nodes which are created due to stack modifying nodes
  such as IncSP, AddSP and SetSP.

  These Phis are always to be ignored by the reg alloc and are
  fixed on the SP register of the ISA.
*/

static const void *abi_get_irn_ops(const arch_irn_handler_t *handler, const ir_node *irn)
{
	const be_abi_irg_t *abi = get_abi_from_handler(handler);
	const void *res = NULL;

	if(is_Phi(irn) && pset_find_ptr(abi->stack_phis, (void *) irn))
		res = &abi->irn_ops;

	return res;
}

static void be_abi_limited(void *data, bitset_t *bs)
{
	be_abi_irg_t *abi = data;
	bitset_clear_all(bs);
	bitset_set(bs, abi->isa->sp->index);
}

static const arch_register_req_t *abi_get_irn_reg_req(const void *self, arch_register_req_t *req, const ir_node *irn, int pos)
{
	be_abi_irg_t *abi          = get_abi_from_ops(self);
	const arch_register_t *reg = abi->isa->sp;

	memset(req, 0, sizeof(req[0]));

	if(pos == BE_OUT_POS(0)) {
		req->cls         = reg->reg_class;
		req->type        = arch_register_req_type_limited;
		req->limited     = be_abi_limited;
		req->limited_env = abi;
	}

	else if(pos >= 0 && pos < get_irn_arity(irn)) {
		req->cls  = reg->reg_class;
		req->type = arch_register_req_type_normal;
	}

	return req;
}

static void abi_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg)
{
}

static const arch_register_t *abi_get_irn_reg(const void *self, const ir_node *irn)
{
	const be_abi_irg_t *abi = get_abi_from_ops(self);
	return abi->isa->sp;
}

static arch_irn_class_t abi_classify(const void *_self, const ir_node *irn)
{
	return arch_irn_class_normal;
}

static arch_irn_flags_t abi_get_flags(const void *_self, const ir_node *irn)
{
	return arch_irn_flags_ignore;
}

static entity *abi_get_frame_entity(const void *_self, const ir_node *irn)
{
	return NULL;
}

static void abi_set_stack_bias(const void *_self, ir_node *irn, int bias)
{
}

static const arch_irn_ops_if_t abi_irn_ops = {
	abi_get_irn_reg_req,
	abi_set_irn_reg,
	abi_get_irn_reg,
	abi_classify,
	abi_get_flags,
	abi_get_frame_entity,
	abi_set_stack_bias
};

static const arch_irn_handler_t abi_irn_handler = {
	abi_get_irn_ops
};
