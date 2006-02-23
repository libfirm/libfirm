/**
 * ABI lowering.
 *
 *
 *
 */

#include "firm_config.h"
#include "obst.h"

#include "type.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "be.h"
#include "beabi.h"
#include "bearch.h"
#include "benode_t.h"

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

typedef struct _be_abi_call_arg_t {
	unsigned is_res : 1;
	unsigned in_reg : 1;

	int pos;
	const arch_register_t *reg;
} be_abi_call_arg_t;

struct _be_abi_call_t {
	be_abi_call_flags_t flags;
	set *params;
};

struct _be_abi_irg_t {
	struct obstack      obst;
	be_irg_t            *birg;
	ir_node             *init_sp;      /**< The node representing the stack pointer
									     at the start of the function. */

	pset                *stack_ops;    /**< Contains all nodes modifying the stack pointer. */
	pset                *ignore_regs;  /**< Contains all registers which shall be ignored
									     during register allocation. */

	unsigned omit_framepointer: 1;     /**< If one, the frame(base-)pointer can be used
									     as an ordinary register. */
};

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

void be_abi_call_set_flags(be_abi_call_t *call, be_abi_call_flags_t flags)
{
	call->flags = flags;
}

void be_abi_call_param_stack(be_abi_call_t *call, int arg_pos)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 0, arg_pos, 1);
}

void be_abi_call_param_reg(be_abi_call_t *call, int arg_pos, const arch_register_t *reg)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 0, arg_pos, 1);
	arg->reg = reg;
}

void be_abi_call_res_reg(be_abi_call_t *call, int arg_pos, const arch_register_t *reg)
{
	be_abi_call_arg_t *arg = get_or_set_call_arg(call, 1, arg_pos, 1);
	arg->reg = reg;
}

be_abi_call_t *be_abi_call_new(void)
{
	be_abi_call_t *call = malloc(sizeof(call[0]));
	call->flags  = BE_ABI_NONE;
	call->params = new_set(cmp_call_arg, 16);
	return call;
}

void be_abi_call_free(be_abi_call_t *call)
{
	del_set(call->params);
	free(call);
}

static INLINE int is_on_stack(be_abi_call_t *call, int pos)
{
	be_abi_call_arg_t *arg = get_call_arg(call, 0, pos);
	return arg && !arg->in_reg;
}

static void adjust_call(be_abi_irg_t *env, ir_node *irn)
{
	ir_graph *irg             = env->birg->irg;
	const arch_isa_t *isa     = env->birg->main_env->arch_env->isa;
	be_abi_call_t *call       = be_abi_call_new();
	ir_type *mt               = get_Call_type(irn);
	int n_params              = get_method_n_params(mt);
	ir_node *curr_sp          = get_irg_frame(irg);
	ir_node *curr_mem         = get_Call_mem(irn);
	ir_node *bl               = get_nodes_block(irn);
	pset *results             = pset_new_ptr(8);
	pset *caller_save         = pset_new_ptr(8);
	int stack_size            = 0;
	int stack_dir             = arch_isa_stack_dir(isa);
	const arch_register_t *sp = arch_isa_sp(isa);
	ir_mode *mach_mode        = sp->reg_class->mode;
	struct obstack *obst      = &env->obst;

	ir_node *res_proj = NULL;
	int curr_res_proj = -1;
	int n_low_args    = 0;
	int n_pos         = 0;

	ir_node *low_call;
	ir_node **in;
	ir_node *sp_proj;
	const ir_edge_t *edge;
	int *low_args;
	int *pos;
	int i, n;

	/* Let the isa fill out the abi description for that call node. */
	arch_isa_get_call_abi(isa, mt, call);

	/* Insert code to put the stack arguments on the stack. */
	for(i = get_irn_arity(irn); i >= 0; --i) {
		if(is_on_stack(call, i)) {
			stack_size += get_type_size_bytes(get_method_param_type(mt, i));
			obstack_int_grow(obst, i);
			n_pos++;
		}
	}
	pos = obstack_finish(obst);

	/* Collect all arguments which are passed in registers. */
	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
		if(arg && arg->in_reg) {
			obstack_int_grow(obst, i);
			n_low_args++;
		}
	}
	low_args = obstack_finish(obst);

	/* If there are some parameters shich shall be passed on the stack. */
	if(n_pos > 0) {
		int curr_ofs      = 0;
		int do_seq        = (call->flags & BE_ABI_USE_PUSH);

		/* Reverse list of stack parameters if call arguments are from left to right */
		if(call->flags & BE_ABI_LEFT_TO_RIGHT) {
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
			curr_sp = be_new_IncSP(sp, irg, bl, curr_sp, stack_size, be_stack_dir_along);
			pset_insert_ptr(env->stack_ops, curr_sp);
		}

		for(i = 0; i < n_pos; ++i) {
			int p            = pos[i];
			ir_node *param   = get_irn_n(irn, p);
			ir_node *addr    = curr_sp;
			ir_node *mem     = NULL;
			type *param_type = get_method_param_type(mt, p);
			int param_size   = get_type_size_bytes(param_type);

			/* Make the expression to compute the argument's offset. */
			if(curr_ofs > 0) {
				addr = new_r_Const_long(irg, bl, mach_mode, curr_ofs);
				addr = new_r_Add(irg, bl, curr_sp, addr, mach_mode);
			}

			/* Insert a store for primitive arguments. */
			if(is_Primitive_type(param_type)) {
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
				curr_sp  = be_new_IncSP(sp, irg, bl, curr_sp, param_size, be_stack_dir_along);
				curr_mem = mem;

				/*
				 * only put the first IncSP to the stack fixup set since the other
				 * ones are correctly connected to other nodes and do not need
				 * to be fixed.
				 */
				if(i == 0)
					pset_insert_ptr(env->stack_ops, curr_sp);
			}
		}

		in = (ir_node **) obstack_finish(obst);

		/* We need the sync only, if we didn't build the stores sequentially. */
		if(!do_seq)
			curr_mem = new_r_Sync(irg, bl, n_pos, in);
		obstack_free(obst, in);
	}

	/* Collect caller save registers */
	for(i = 0; env->birg->main_env->caller_save[i]; ++i)
		pset_insert_ptr(caller_save, env->birg->main_env->caller_save[i]);

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
				if(proj > curr_res_proj)
					curr_res_proj = proj;
				if(arg->in_reg)
					pset_remove_ptr(caller_save, arg->reg);
			}
		}
	}
	curr_res_proj++;

	/* Make additional projs for the caller save registers
	   and the Keep node which keeps them alive. */
	if(pset_count(caller_save) > 0) {
		const arch_register_t *reg;
		ir_node **in;

		if(!res_proj)
			res_proj = new_r_Proj(irg, bl, irn, mode_T, pn_Call_T_result);

		for(reg = pset_first(caller_save); reg; reg = pset_next(caller_save))
			obstack_ptr_grow(obst, new_r_Proj(irg, bl, res_proj, reg->reg_class->mode, curr_res_proj++));

		in = (ir_node **) obstack_finish(obst);
		be_new_Keep(NULL, irg, bl, pset_count(caller_save), in);
		obstack_free(obst, in);
	}

	/* Clean up the stack. */
	if(stack_size > 0) {
		ir_node *last_inc_sp;

		/* Get the result ProjT */
		if(!res_proj)
			res_proj = new_r_Proj(irg, bl, irn, mode_T, pn_Call_T_result);

		/* Make a Proj for the stack pointer. */
		sp_proj     = new_r_Proj(irg, bl, res_proj, sp->reg_class->mode, curr_res_proj++);
		last_inc_sp = be_new_IncSP(sp, irg, bl, sp_proj, stack_size, be_stack_dir_against);
		pset_insert_ptr(env->stack_ops, last_inc_sp);
	}

	/* at last make the backend call node and set its register requirements. */
	for(i = 0; i < n_low_args; ++i)
		obstack_ptr_grow(obst, get_irn_n(irn, low_args[i]));
	in = obstack_finish(obst);
	low_call = be_new_Call(irg, bl, curr_mem, curr_sp, get_Call_ptr(irn), curr_res_proj, n_low_args, in);
	obstack_free(obst, in);

	exchange(irn, low_call);

	be_abi_call_free(call);
	obstack_free(obst, pos);
	del_pset(results);
	del_pset(caller_save);
}

static void adjust_call_walker(ir_node *irn, void *data)
{
	if(get_irn_opcode(irn) == iro_Call)
		adjust_call(data, irn);
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
	pset_insert_ptr(env->stack_ops);

	if(isa->stack_dir < 0)
		res = be_new_Copy(isa->sp->reg_class, env->birg->irg, bl, res);

}

/**
 * Modify the irg itself and the frame type.
 */
static void modify_irg(be_abi_irg_t *env)
{
	be_abi_call_t *call       = be_abi_call_new();
	const arch_isa_t *isa     = env->birg->main_env->arch_env->isa;
	ir_graph *irg             = env->birg->irg;
	ir_node *bl               = get_irg_start_block(irg);
	ir_node *arg_tuple        = get_irg_args(irg);
	type *method_type         = get_entity_type(get_irg_entity(irg));
	int n_params              = get_method_n_params(method_type);
	const arch_register_t *sp = arch_isa_sp(isa);

	int max_arg = 0;
	int reg_params_nr = 0;
	int n_reg_param   = 0;

	int i, j, n;
	ir_node *proj_sp;
	ir_node *frame_pointer;
	ir_node *reg_params, *reg_params_bl;
	ir_node **args, **args_repl, **return_params;
	ir_edge_t *edge;


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
	}

	/* Get the ABI constraints from the ISA */
	arch_isa_get_call_abi(isa, method_type, call);

	{
		const arch_register_t **callee_save = env->birg->main_env->callee_save;
		int n_reg_param = 0;

		/* Count the register params and add them to the number of projs for the RegParams node */
		for(i = 0; i < n_params; ++i) {
			be_abi_call_arg_t *arg = get_call_arg(call, 0, i);
			n_reg_param += arg->in_reg;
		}

		reg_params    = be_new_RegParams(irg, n_reg_param);
		reg_params_bl = get_nodes_block(reg_params);
		reg_params_nr = 0;

		proj_sp       = new_r_Proj(irg, reg_params_bl, reg_params, sp->reg_class->mode, reg_params_nr);
		be_set_constr_single_reg(reg_params, -(reg_params_nr + 1), sp);
		reg_params_nr++;

		/* Make Projs for the callee save registers. */
		for(i = 0; callee_save[i] != NULL; ++i, ++reg_params_nr) {
			const arch_register_t *reg = callee_save[i];
			ir_node *irn = new_r_Proj(irg, reg_params_bl, reg_params, reg->reg_class->mode, reg_params_nr);
			be_set_constr_single_reg(reg_params, -(reg_params_nr + 1), reg);

			/* memorize the callee save proj since they will be arguments to the Return nodes. */
			obstack_ptr_grow(&env->obst, irn);
		}

		obstack_ptr_grow(&env->obst, NULL);
		return_params = obstack_finish(&env->obst);
	}

	/* If we can omit the framepointer, the stack pointer will become the frame pointer */
	frame_pointer = be_new_IncSP(sp, irg, reg_params_bl, proj_sp, 0);
	/* memorize this node for later fixup */
	if(!env->omit_framepointer)
		frame_pointer = be_new_Copy(sp->reg_class, irg, reg_params_bl, proj_sp);
	set_irg_frame(irg, frame_pointer);

	/* reroute the edges from the original argument projs to the RegParam ones. */
	for(i = 0; i < max_arg; ++i) {
		if(args[i]) {
			assert(args_repl != NULL);
			edges_reroute(args[i], args_repl[i], irg);
		}
	}

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
				args_repl[i] = new_r_Proj(irg, reg_params_bl, reg_params, get_irn_mode(arg_proj), reg_params_nr);
				be_set_constr_single_reg(reg_params, -(reg_params_nr + 1), arg->reg);
				reg_params_nr++;
			}

			/* when the (stack) parameter is primitive, we insert a StackParam
			   node representing the load of that parameter */
			else if(is_Primitive_type(param_type)) {
				ir_mode *mode                    = get_type_mode(param_type);
				const arch_register_class_t *cls = arch_isa_get_reg_class_for_mode(env->isa, mode);
				args_repl[i] = be_new_StackParam(irg, cls, frame_pointer, XXX_offset);
			}

			/* The stack parameter is not primitive (it is a struct or array),
			   we thus will create a node representing the parameter's address
			   on the stack. */
			else {
				assert(0 && "struct parameters are not supported");
			}
		}
	}

	obstack_free(&env->obst, args);
	be_abi_call_free(call);
}
#endif

static void collect_alloca_walker(ir_node *irn, void *data)
{
	be_abi_irg_t *env = data;
	if(get_irn_opcode(irn) == iro_Alloc && get_Alloc_where(irn) == stack_alloc)
		obstack_ptr_grow(&env->obst, irn);
}

be_abi_irg_t *be_abi_introduce(be_irg_t *birg)
{
	be_abi_irg_t *env = malloc(sizeof(env[0]));

	ir_node **stack_allocs;

	env->birg        = birg;
	env->stack_ops   = pset_new_ptr(32);
	env->omit_framepointer = 1;
	obstack_init(&env->obst);

	/* search for stack allocation nodes and record them */
	irg_walk_graph(env->birg->irg, collect_alloca_walker, NULL, env);
	obstack_ptr_grow(&env->obst, NULL);
	stack_allocs = obstack_finish(&env->obst);

	/* If there are stack allocations in the irg, we need a framepointer */
	if(stack_allocs[0] != NULL)
		env->omit_framepointer = 0;

#if 0
	modify_irg(&env);

	for(i = 0; stack_allocs[i] != NULL; ++i)
		implement_stack_alloc(&env, stack_allocs[i]);
#endif

	irg_walk_graph(env->birg->irg, NULL, adjust_call_walker, &env);
	return env;
}

void be_abi_fix_stack(be_abi_irg_t *env)
{
	pset *origs = pset_new_ptr_default();

	pset_insert_ptr(origs, env->init_sp);
	be_ssa_constr_sets(env->birg->dom_front, origs, env->stack_ops);
	del_pset(origs);
}


void be_abi_free(be_abi_irg_t *env)
{
	del_pset(env->stack_ops);
	obstack_free(&env->obst, NULL);
	free(env);
}
