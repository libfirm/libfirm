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

#include "type.h"
#include "survive_dce.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irprintf_t.h"

#include "be.h"
#include "beabi.h"
#include "bearch.h"
#include "benode_t.h"
#include "besched_t.h"

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

typedef struct _be_abi_call_arg_t {
	unsigned is_res : 1;
	unsigned in_reg : 1;

	int pos;
	const arch_register_t *reg;
	entity *stack_ent;
} be_abi_call_arg_t;

struct _be_abi_call_t {
	be_abi_call_flags_t flags;
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
	firm_dbg_module_t    *dbg;            /**< The debugging module. */
	be_stack_frame_t     *frame;
	const be_irg_t       *birg;
	const arch_isa_t     *isa;          /**< The isa. */
	survive_dce_t        *dce_survivor;

	be_abi_call_t        *call;
	type                 *method_type;

	ir_node              *init_sp;      /**< The node representing the stack pointer
									     at the start of the function. */

	ir_node              *reg_params;
	pmap                 *regs;
	pset                 *stack_phis;
	int                  start_block_bias;

	arch_irn_handler_t irn_handler;
	arch_irn_ops_t     irn_ops;
};

#define abi_offset_of(type,member) ((char *) &(((type *) 0)->member) - (char *) 0)
#define abi_get_relative(ptr, member) ((void *) ((char *) (ptr) - abi_offset_of(be_abi_irg_t, member)))
#define get_abi_from_handler(ptr) abi_get_relative(ptr, irn_handler)
#define get_abi_from_ops(ptr)     abi_get_relative(ptr, irn_ops)

/* Forward, since be need it in be_abi_introduce(). */
static const arch_irn_ops_if_t abi_irn_ops;
static const arch_irn_handler_t abi_irn_handler;

/*
     _    ____ ___    ____      _ _ _                _
    / \  | __ )_ _|  / ___|__ _| | | |__   __ _  ___| | _____
   / _ \ |  _ \| |  | |   / _` | | | '_ \ / _` |/ __| |/ / __|
  / ___ \| |_) | |  | |__| (_| | | | |_) | (_| | (__|   <\__ \
 /_/   \_\____/___|  \____\__,_|_|_|_.__/ \__,_|\___|_|\_\___/

  These callbacks are used by the backend to set the parameters
  for a specific call type.
*/

static int cmp_call_arg(const void *a, const void *b, size_t n)
{
	const be_abi_call_arg_t *p = a, *q = b;
	return !(p->is_res == q->is_res && p->pos == q->pos);
}

static be_abi_call_arg_t *get_or_set_call_arg(be_abi_call_t *call, int is_res, int pos, int do_insert)
{
	be_abi_call_arg_t arg;
	unsigned hash;

	arg.is_res = is_res;
	arg.pos    = pos;

	hash = is_res * 100 + pos;

	return do_insert
		? set_insert(call->params, &arg, sizeof(arg), hash)
		: set_find(call->params, &arg, sizeof(arg), hash);
}

static INLINE be_abi_call_arg_t *get_call_arg(be_abi_call_t *call, int is_res, int pos)
{
	return get_or_set_call_arg(call, is_res, pos, 0);
}

void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags, ir_type *between_type)
{
	call->flags            = flags;
	call->between_type     = between_type;
}

void be_abi_call_param_stack(be_abi_call_t *call, int arg_pos)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 0, arg_pos, 1);
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

be_abi_call_t *be_abi_call_new(void)
{
	be_abi_call_t *call = malloc(sizeof(call[0]));
	call->flags.val = 0;
	call->params    = new_set(cmp_call_arg, 16);
	return call;
}

void be_abi_call_free(be_abi_call_t *call)
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
 * If irn is a Sel node computing the address of an entity
 * on the frame type return the entity, else NULL.
 */
static INLINE entity *get_sel_ent(ir_node *irn)
{
	if(get_irn_opcode(irn) == iro_Sel
		&& get_Sel_ptr(irn) == get_irg_frame(get_irn_irg(irn))) {

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
	const arch_register_class_t *cls;
	be_abi_irg_t *env = data;
	const arch_isa_t *isa = env->birg->main_env->arch_env->isa;
	ir_graph *irg = get_irn_irg(irn);
	ir_node *frame = get_irg_frame(irg);
	ir_node *nw  = NULL;
	opcode opc   = get_irn_opcode(irn);

	if(opc == iro_Load) {
		ir_node *bl  = get_nodes_block(irn);
		ir_node *sel = get_Load_ptr(irn);
		entity *ent  = get_sel_ent(sel);
		cls = arch_isa_get_reg_class_for_mode(isa, get_Load_mode(irn));
		if(ent != NULL)
			nw = be_new_FrameLoad(isa->sp->reg_class, cls, irg, bl, get_Load_mem(irn), frame, ent);
	}

	else if(opc == iro_Store) {
		ir_node *bl  = get_nodes_block(irn);
		ir_node *val = get_Store_value(irn);
		ir_node *sel = get_Store_ptr(irn);
		entity *ent  = get_sel_ent(sel);
		cls = arch_isa_get_reg_class_for_mode(isa, get_irn_mode(val));
		if(ent != NULL)
			nw = be_new_FrameStore(isa->sp->reg_class, cls,	irg, bl, get_Store_mem(irn), frame, val, ent);
	}

	else {
		entity *ent = get_sel_ent(irn);
		if(ent != NULL) {
			ir_node *bl  = get_nodes_block(irn);
			nw = be_new_FrameAddr(isa->sp->reg_class, irg, bl, frame, ent);
		}
	}

	if(nw != NULL)
		exchange(irn, nw);
}

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
		if(!arg->in_reg) {
			stack_size += get_type_size_bytes(get_method_param_type(mt, i));
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
		int do_seq        = call->flags.bits.store_args_sequential;

		/* Reverse list of stack parameters if call arguments are from left to right */
		if(call->flags.bits.left_to_right) {
			for(i = 0; i < n_pos / 2; ++i) {
				int other  = n_pos - i - 1;
				int tmp    = pos[i];
				pos[i]     = pos[other];
				pos[other] = tmp;
			}
		}

		/*
		 * If the stack is decreasing and we do not want to store sequentially,
		 * we allocate as much space on the stack all parameters need, by
		 * moving the stack pointer along the stack's direction.
		 */
		if(stack_dir < 0 && !do_seq) {
			curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, no_mem, stack_size, be_stack_dir_along);
		}

		assert(mode_is_reference(mach_mode) && "machine mode must be pointer");
		for(i = 0; i < n_pos; ++i) {
			int p            = pos[i];
			ir_node *param   = get_Call_param(irn, p);
			ir_node *addr    = curr_sp;
			ir_node *mem     = NULL;
			type *param_type = get_method_param_type(mt, p);
			int param_size   = get_type_size_bytes(param_type);

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

			/* Make a memcopy for compound arguments. */
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
				curr_sp  = be_new_IncSP(sp, irg, bl, curr_sp, no_mem, param_size, be_stack_dir_along);
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
	foreach_out_edge(irn, edge) {
		const ir_edge_t *res_edge;
		ir_node *irn = get_edge_src_irn(edge);

		if(is_Proj(irn) && get_irn_mode(irn) == mode_T) {
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
					Proj numbers must be increased by pn_Call_max
				*/
		   		proj += pn_Call_max;
				set_Proj_proj(res, proj);
				obstack_ptr_grow(obst, res);

				if(proj > curr_res_proj)
					curr_res_proj = proj;
				if(arg->in_reg)
					pset_remove_ptr(caller_save, arg->reg);
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
	// if(env->call->flags.bits.call_has_imm && get_irn_opcode());
	low_call = be_new_Call(irg, bl, curr_mem, curr_sp, get_Call_ptr(irn), curr_res_proj, n_low_args, in);
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
			ir_node *proj = new_r_Proj(irg, bl, low_call, reg->reg_class->mode, curr_res_proj++);

			/* memorize the register in the link field. we need afterwards to set the register class of the keep correctly. */
			set_irn_link(proj, (void *) reg);
			obstack_ptr_grow(obst, proj);
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

		/* Make a Proj for the stack pointer. */
		curr_sp     = be_new_IncSP(sp, irg, bl, curr_sp, mem_proj, stack_size, be_stack_dir_against);
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
	if(get_Alloc_where(alloc) == stack_alloc) {
		ir_node *new_alloc;

		ir_node *bl   = get_nodes_block(alloc);
		ir_graph *irg = get_irn_irg(bl);

		new_alloc = be_new_Alloca(env->isa->sp, irg, bl, get_Alloc_mem(alloc), curr_sp, get_Alloc_size(alloc));
		exchange(alloc, new_alloc);
		curr_sp = new_r_Proj(irg, bl, new_alloc, mode_P, pn_Alloc_res);
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
		ir_node *bl = get_nodes_block(irn);
		void *save  = get_irn_link(bl);

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
		be_new_Keep(env->isa->sp->reg_class, get_irn_irg(bl), bl, 1, nodes);
	}

	set_irn_link(bl, curr_sp);
}

/**
 * Adjust all call nodes in the graph to the ABI conventions.
 */
static void process_calls(be_abi_irg_t *env)
{
	ir_graph *irg = env->birg->irg;

	irg_walk_graph(irg, firm_clear_link, link_calls_in_block_walker, NULL);
	irg_block_walk_graph(irg, NULL, process_calls_in_block, env);
}

#if 0
/**
 * Walker to implement alloca-style allocations.
 * They are implemented using an add to the stack pointer
 * and a copy instruction.
 */
static void implement_stack_alloc(be_abi_irg_t *env, ir_node *irn)
{
	const arch_isa_t *isa = env->birg->main_env->arch_env->isa;
	ir_node *bl           = get_nodes_block(irn);
	ir_node *res          = env->init_sp;
	ir_node *size;

	assert(get_irn_opcode(irn) == iro_Alloc && get_Alloc_where(irn) == stack_alloc);

	size = get_Alloc_size(irn);
	if(isa->stack_dir > 0)
		res = be_new_Copy(isa->sp->reg_class, env->birg->irg, bl, res);

	res = be_new_AddSP(isa->sp, env->birg->irg, bl, res, size);

	if(isa->stack_dir < 0)
		res = be_new_Copy(isa->sp->reg_class, env->birg->irg, bl, res);

}
#endif

static void collect_return_walker(ir_node *irn, void *data)
{
	if(get_irn_opcode(irn) == iro_Return) {
		struct obstack *obst = data;
		obstack_ptr_grow(obst, irn);
	}
}

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
		stack = be_new_IncSP(sp, irg, bl, stack, no_mem, BE_STACK_FRAME_SIZE, be_stack_dir_along);
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

		stack = be_new_IncSP(sp, irg, bl, stack, frame, BE_STACK_FRAME_SIZE, be_stack_dir_along);
	}

	be_node_set_flags(env->reg_params, -(stack_nr + 1), arch_irn_flags_ignore);
	env->init_sp = stack;
	set_irg_frame(irg, frame);
	edges_reroute(old_frame, frame, irg);

	return frame;
}

static void clearup_frame(be_abi_irg_t *env, ir_node *ret, struct obstack *obst)
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
		stack = be_new_IncSP(sp, irg, bl, stack, ret_mem, BE_STACK_FRAME_SIZE, be_stack_dir_against);
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
			irn = stack;
		else if(reg == bp)
			irn = frame;

		obstack_ptr_grow(obst, irn);
	}
}

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

		if(!arg->in_reg) {
			snprintf(buf, sizeof(buf), "param_%d", i);
			arg->stack_ent = new_entity(res, new_id_from_str(buf), param_type);
			set_entity_offset_bytes(arg->stack_ent, ofs);
			ofs += get_type_size_bytes(param_type);
		}
	}

	set_type_size_bytes(res, ofs);
	return res;
}

/**
 * Modify the irg itself and the frame type.
 */
static void modify_irg(be_abi_irg_t *env)
{
	firm_dbg_module_t *dbg    = env->dbg;
	be_abi_call_t *call       = be_abi_call_new();
	const arch_isa_t *isa     = env->birg->main_env->arch_env->isa;
	const arch_register_t *sp = arch_isa_sp(isa);
	ir_graph *irg             = env->birg->irg;
	ir_node *bl               = get_irg_start_block(irg);
	ir_node *end              = get_irg_end_block(irg);
	ir_node *arg_tuple        = get_irg_args(irg);
	ir_node *no_mem           = get_irg_no_mem(irg);
	type *method_type         = get_entity_type(get_irg_entity(irg));
	int n_params              = get_method_n_params(method_type);
	int max_arg               = 0;
	int reg_params_nr         = 0;
	int arg_offset            = 0;

	int i, j, n;

	ir_node *frame_pointer;
	ir_node *reg_params_bl;
	ir_node **args, **args_repl;
	const ir_edge_t *edge;
	ir_type *arg_type;

	pmap_entry *ent;

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
	max_arg += 1;
	args      = obstack_alloc(&env->obst, max_arg * sizeof(args[0]));
	args_repl = obstack_alloc(&env->obst, max_arg * sizeof(args[0]));
	memset(args, 0, max_arg * sizeof(args[0]));
	memset(args_repl, 0, max_arg * sizeof(args[0]));

	/* Fill the argument vector */
	foreach_out_edge(arg_tuple, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		int nr       = get_Proj_proj(irn);
		args[nr]     = irn;
		DBG((dbg, LEVEL_2, "\treading arg: %d -> %+F\n", nr, irn));
	}

	/* Get the ABI constraints from the ISA */
	arch_isa_get_call_abi(isa, method_type, call);

	arg_type     = compute_arg_type(env, call, method_type);
	stack_frame_init(env->frame, arg_type, call->between_type, get_irg_frame_type(irg), isa->stack_dir);

	/* Count the register params and add them to the number of Projs for the RegParams node */
	for(i = 0; i < n_params; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		if(arg->in_reg) {
			assert(arg->reg != sp && "cannot use stack pointer as parameter register");
			pmap_insert(env->regs, (void *) arg->reg, NULL);
			DBG((dbg, LEVEL_2, "\targ #%d -> reg %s\n", i, arg->reg->name));
		}
	}

	/* Collect all callee-save registers */
	for(i = 0, n = arch_isa_get_n_reg_class(isa); i < n; ++i) {
		const arch_register_class_t *cls = arch_isa_get_reg_class(isa, i);
		for(j = 0; j < cls->n_regs; ++j) {
			const arch_register_t *reg = &cls->regs[j];
			if(arch_register_type_is(reg, callee_save))
				pmap_insert(env->regs, (void *) reg, NULL);
		}
	}

	pmap_insert(env->regs, (void *) sp, NULL);
	pmap_insert(env->regs, (void *) isa->bp, NULL);
	reg_params_nr   = 0;
	reg_params_bl   = get_irg_start_block(irg);
	env->reg_params = be_new_RegParams(irg, reg_params_bl, pmap_count(env->regs));

	/*
	 * make proj nodes for the callee save registers.
	 * memorize them, since Return nodes get those as inputs.
	 */
	for(ent = pmap_first(env->regs); ent; ent = pmap_next(env->regs)) {
		arch_register_t *reg = ent->key;
		int pos = -(reg_params_nr + 1);
		ent->value = new_r_Proj(irg, reg_params_bl, env->reg_params, reg->reg_class->mode, reg_params_nr);
		be_set_constr_single_reg(env->reg_params, pos, reg);
		arch_set_irn_register(env->birg->main_env->arch_env, ent->value, reg);

		/*
		 * If the register is an ignore register,
		 * The Proj for that register shall also be ignored during register allocation.
		 */
		if(arch_register_type_is(reg, ignore))
			be_node_set_flags(env->reg_params, pos, arch_irn_flags_ignore);

		reg_params_nr++;

		DBG((dbg, LEVEL_2, "\tregister save proj #%d -> reg %s\n", reg_params_nr - 1, reg->name));
	}

	/* Insert the code to set up the stack frame */
	frame_pointer = setup_frame(env);

	/* Now, introduce stack param nodes for all parameters passed on the stack */
	for(i = 0; i < max_arg; ++i) {
		ir_node *arg_proj = args[i];
		if(arg_proj != NULL) {
			be_abi_call_arg_t *arg;
			ir_type *param_type;
			int nr = get_Proj_proj(arg_proj);

			nr         = MIN(nr, n_params);
			arg        = get_call_arg(call, 0, nr);
			param_type = get_method_param_type(method_type, nr);

			if(arg->in_reg) {
				args_repl[i] = new_r_Proj(irg, reg_params_bl, env->reg_params, get_irn_mode(arg_proj), reg_params_nr);
				be_set_constr_single_reg(env->reg_params, -(reg_params_nr + 1), arg->reg);
				reg_params_nr++;
			}

			/* when the (stack) parameter is primitive, we insert a StackParam
			node representing the load of that parameter */
			else {

				/* For atomic parameters which are actually used, we create a StackParam node. */
				if(is_atomic_type(param_type) && get_irn_n_edges(args[i]) > 0) {
					ir_mode *mode                    = get_type_mode(param_type);
					const arch_register_class_t *cls = arch_isa_get_reg_class_for_mode(isa, mode);
					args_repl[i] = be_new_StackParam(cls, isa->bp->reg_class, irg, reg_params_bl, mode, frame_pointer, arg->stack_ent);
				}

				/* The stack parameter is not primitive (it is a struct or array),
				we thus will create a node representing the parameter's address
				on the stack. */
				else {
					args_repl[i] = be_new_FrameAddr(sp->reg_class, irg, reg_params_bl, frame_pointer, arg->stack_ent);
				}
			}
		}
	}

	/* reroute the edges from the original argument projs to the RegParam ones. */
	for(i = 0; i < max_arg; ++i) {
		if(args[i] != NULL) {
			assert(args_repl[i] != NULL);
			edges_reroute(args[i], args_repl[i], irg);
		}
	}

	/* All Return nodes hang on the End node, so look for them there. */
	for(i = 0, n = get_irn_arity(end); i < n; ++i) {
		ir_node *irn = get_irn_n(end, i);

		if(get_irn_opcode(irn) == iro_Return) {
			ir_node *bl   = get_nodes_block(irn);
			int n_res     = get_Return_n_ress(irn);
			pmap *reg_map = pmap_create_ex(n_res);
			ir_node *ret;
			int i, n;
			ir_node **in;

			/* collect all arguments of the return */
			obstack_ptr_grow(&env->obst, get_Return_mem(irn));
			for(i = 0; i < n_res; ++i) {
				ir_node *res           = get_Return_res(irn, i);
				be_abi_call_arg_t *arg = get_call_arg(call, 1, i);

				assert(arg->in_reg && "return value must be passed in register");
				pmap_insert(reg_map, res, (void *) arg->reg);
				obstack_ptr_grow(&env->obst, res);
			}

			/* generate the clean up code and add additional parameters to the return. */
			clearup_frame(env, irn, &env->obst);

			/* The in array for the new back end return is now ready. */
			n   = obstack_object_size(&env->obst) / sizeof(in[0]);
			in  = obstack_finish(&env->obst);
			ret = be_new_Return(irg, bl, n, in);

			/* Set the constraints for some arguments of the return. */
			for(i = 0; i < n; i++) {
				const arch_register_t *reg = pmap_get(reg_map, in[i]);
				if(reg != NULL)
					be_set_constr_single_reg(ret, i, reg);
			}
			exchange(irn, ret);
			obstack_free(&env->obst, in);
			pmap_destroy(reg_map);
		}
	}

	obstack_free(&env->obst, args);
	be_abi_call_free(call);
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
	be_abi_irg_t *env = malloc(sizeof(env[0]));

	int i;
	ir_node **nodes;
	pmap_entry *ent;

	env->isa           = birg->main_env->arch_env->isa;
	env->method_type   = get_entity_type(get_irg_entity(birg->irg));
	env->call          = be_abi_call_new();
	arch_isa_get_call_abi(env->isa, env->method_type, env->call);

	env->dce_survivor     = new_survive_dce();
	env->birg             = birg;
	env->dbg              = firm_dbg_register("firm.be.abi");
	env->stack_phis       = pset_new_ptr(16);
	env->init_sp          = new_r_Unknown(birg->irg, env->isa->sp->reg_class->mode);

	obstack_init(&env->obst);

	memcpy(&env->irn_handler, &abi_irn_handler, sizeof(abi_irn_handler));
	env->irn_ops.impl = &abi_irn_ops;

	/* Lower all call nodes in the IRG. */
	process_calls(env);

#if 0
	/* search for stack allocation nodes and record them */
	irg_walk_graph(env->birg->irg, collect_alloca_walker, NULL, env);
	obstack_ptr_grow(&env->obst, NULL);
	nodes = obstack_finish(&env->obst);

	/* If there are stack allocations in the irg, we need a frame pointer */
	if(nodes[0] != NULL)
		env->call->flags.bits.try_omit_fp = 0;
#endif

	modify_irg(env);

	/* Make some important node pointers survive the dead node elimination. */
	survive_dce_register_irn(env->dce_survivor, &env->init_sp);
	for(ent = pmap_first(env->regs); ent; ent = pmap_next(env->regs))
		survive_dce_register_irn(env->dce_survivor, (ir_node **) &ent->value);

	/* Fix the alloca-style allocations */
#if 0
	for(i = 0; nodes[i] != NULL; ++i)
		implement_stack_alloc(env, nodes[i]);
#endif

	arch_env_push_irn_handler(env->birg->main_env->arch_env, &env->irn_handler);

	return env;
}

static void collect_stack_nodes(ir_node *irn, void *data)
{
	pset *s = data;

	switch(be_get_irn_opcode(irn)) {
	case beo_IncSP:
//	case beo_AddSP:
	case beo_SetSP:
		pset_insert_ptr(s, irn);
	default:
		break;
	}
}

void be_abi_fix_stack_nodes(be_abi_irg_t *env)
{
	dom_front_info_t *df;
	pset *stack_ops;

	/* We need dominance frontiers for fix up */
	df = be_compute_dominance_frontiers(env->birg->irg);

	stack_ops = pset_new_ptr_default();
	pset_insert_ptr(stack_ops, env->init_sp);
	irg_walk_graph(env->birg->irg, collect_stack_nodes, NULL, stack_ops);
	be_ssa_constr_set_phis(df, stack_ops, env->stack_phis);
	del_pset(stack_ops);

	/* free these dominance frontiers */
	be_free_dominance_frontiers(df);
}

/**
 * Translates a direction of an IncSP node (either be_stack_dir_against, or ...along)
 * into -1 or 1, respectively.
 * @param irn The node.
 * @return 1, if the direction of the IncSP was along, -1 if against.
 */
static int get_dir(ir_node *irn)
{
	return 1 - 2 * (be_get_IncSP_direction(irn) == be_stack_dir_against);
}

static int process_stack_bias(be_abi_irg_t *env, ir_node *bl, int bias)
{
	const arch_env_t *aenv = env->birg->main_env->arch_env;
	ir_node *irn;
	int start_bias = bias;
	int omit_fp    = env->call->flags.bits.try_omit_fp;

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
	stack_frame_dump(stdout, env->frame);

	/* Determine the stack bias at the and of the start block. */
	bw.start_block_bias = process_stack_bias(env, get_irg_start_block(irg), 0);

	/* fix the bias is all other blocks */
	bw.env = env;
	irg_block_walk_graph(irg, stack_bias_walker, NULL, &bw);
}

void be_abi_free(be_abi_irg_t *env)
{
	free_survive_dce(env->dce_survivor);
	del_pset(env->stack_phis);
	pmap_destroy(env->regs);
	obstack_free(&env->obst, NULL);
	arch_env_pop_irn_handler(env->birg->main_env->arch_env);
	free(env);
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
	return is_Phi(irn) && pset_find_ptr(abi->stack_phis, (void *) irn) != NULL ? &abi->irn_ops : NULL;
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

	req->cls         = reg->reg_class;
	req->type        = arch_register_req_type_limited;
	req->limited     = be_abi_limited;
	req->limited_env = abi;
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
