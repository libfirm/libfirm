#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "iredges_t.h"

#include "bitset.h"
#include "debug.h"

#include "../bearch.h"                /* the general register allocator interface */
#include "../benode_t.h"
#include "bearch_ia32_t.h"

#include "ia32_new_nodes.h"           /* ia32 nodes interface */
#include "gen_ia32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "ia32_gen_decls.h"           /* interface declaration emitter */
#include "ia32_transform.h"
#include "ia32_emitter.h"
#include "ia32_map_regs.h"

#define DEBUG_MODULE "ir.be.isa.ia32"

/* TODO: ugly */
static set *cur_reg_set = NULL;

#undef is_Start
#define is_Start(irn) (get_irn_opcode(irn) == iro_Start)

/**************************************************
 *                         _ _              _  __
 *                        | | |            (_)/ _|
 *  _ __ ___  __ _    __ _| | | ___   ___   _| |_
 * | '__/ _ \/ _` |  / _` | | |/ _ \ / __| | |  _|
 * | | |  __/ (_| | | (_| | | | (_) | (__  | | |
 * |_|  \___|\__, |  \__,_|_|_|\___/ \___| |_|_|
 *            __/ |
 *           |___/
 **************************************************/

static ir_node *my_skip_proj(const ir_node *n) {
	while (is_Proj(n))
		n = get_Proj_pred(n);
	return (ir_node *)n;
}

static int is_Call_Proj(const ir_node *n) {
	if (is_Proj(n)                               &&
		is_Proj(get_Proj_pred(n))                &&
		get_irn_mode(get_Proj_pred(n)) == mode_T &&
		is_ia32_Call(get_Proj_pred(get_Proj_pred(n))))
	{
		return 1;
	}

	return 0;
}

static int is_Start_Proj(const ir_node *n) {
	if (is_Proj(n)                               &&
		is_Proj(get_Proj_pred(n))                &&
		get_irn_mode(get_Proj_pred(n)) == mode_T &&
		is_Start(get_Proj_pred(get_Proj_pred(n))))
	{
		return 1;
	}

	return 0;
}

static int is_P_frame_base_Proj(const ir_node *n) {
	if (is_Proj(n)                                    &&
		is_Start(n) &&
		get_Proj_proj(n) == pn_Start_P_frame_base)
	{
		return 1;
	}

	return 0;
}

static int is_used_by_Keep(const ir_node *n) {
	return be_is_Keep(get_edge_src_irn(get_irn_out_edge_first(n)));
}

/**
 * Return register requirements for an ia32 node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const arch_register_req_t *ia32_get_irn_reg_req(const void *self, arch_register_req_t *req, const ir_node *irn, int pos) {
	const ia32_register_req_t *irn_req;
	long                       node_pos = pos == -1 ? 0 : pos;
	ir_mode                   *mode     = get_irn_mode(irn);
	firm_dbg_module_t         *mod      = firm_dbg_register(DEBUG_MODULE);
	const ia32_irn_ops_t      *ops      = self;

	if (mode == mode_T || mode == mode_M) {
		DBG((mod, LEVEL_1, "ignoring mode_T, mode_M node %+F\n", irn));
		return NULL;
	}

	DBG((mod, LEVEL_1, "get requirements at pos %d for %+F ... ", pos, irn));


	if (is_Call_Proj(irn) && is_used_by_Keep(irn)) {
		irn_req = ia32_projnum_reg_req_map[get_Proj_proj(irn)];
		memcpy(req, &(irn_req->req), sizeof(*req));
		return req;
	}
	else if (is_Start_Proj(irn)) {
		irn_req = ops->cg->reg_param_req[get_Proj_proj(irn)];
		assert(irn_req && "missing requirement for regparam");
		memcpy(req, &(irn_req->req), sizeof(*req));
		return req;
	}
	else if (is_Proj(irn)) {
		if (pos == -1) {
			node_pos = ia32_translate_proj_pos(irn);
		}
		else {
			node_pos = pos;
		}

		irn = my_skip_proj(irn);

		DBG((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", irn, node_pos));
	}

	if (is_ia32_irn(irn)) {
		if (pos >= 0) {
			irn_req = get_ia32_in_req(irn, pos);
		}
		else {
			irn_req = get_ia32_out_req(irn, node_pos);
		}

		DBG((mod, LEVEL_1, "returning reqs for %+F at pos %d\n", irn, pos));

		memcpy(req, &(irn_req->req), sizeof(*req));

		if (arch_register_req_is(&(irn_req->req), should_be_same) ||
			arch_register_req_is(&(irn_req->req), should_be_different)) {
			assert(irn_req->pos >= 0 && "should be same/different constraint for in -> out NYI");
			req->other = get_irn_n(irn, irn_req->pos);
		}
	}
	else {
		/* treat Phi like Const with default requirements */
		if (is_Phi(irn)) {
			DBG((mod, LEVEL_1, "returning standard reqs for %+F\n", irn));
			if (mode_is_float(mode))
				memcpy(req, &(ia32_default_req_ia32_floating_point.req), sizeof(*req));
			else if (mode_is_int(mode) || mode_is_reference(mode))
				memcpy(req, &(ia32_default_req_ia32_general_purpose.req), sizeof(*req));
			else if (mode == mode_T || mode == mode_M) {
				DBG((mod, LEVEL_1, "ignoring Phi node %+F\n", irn));
				return NULL;
			}
			else
				assert(0 && "unsupported Phi-Mode");
		}
		else if (is_Start(irn)) {
			DBG((mod, LEVEL_1, "returning reqs none for ProjX -> Start (%+F )\n", irn));
			switch (node_pos) {
				case pn_Start_X_initial_exec:
				case pn_Start_P_value_arg_base:
				case pn_Start_P_globals:
				case pn_Start_P_frame_base:
					memcpy(req, &(ia32_default_req_none.req), sizeof(*req));
					break;
				case pn_Start_T_args:
					assert(0 && "ProjT(pn_Start_T_args) should not be asked");
			}
		}
		else if (get_irn_op(irn) == op_Return && pos > 0) {
			DBG((mod, LEVEL_1, "returning reqs EAX for %+F\n", irn));
			memcpy(req, &(ia32_default_req_ia32_general_purpose_eax.req), sizeof(*req));
		}
		else {
			DBG((mod, LEVEL_1, "returning NULL for %+F (not ia32)\n", irn));
			req = NULL;
		}
	}

	return req;
}

static void ia32_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg) {
	int pos = 0;

	if ((is_Call_Proj(irn) && is_used_by_Keep(irn)) ||
		is_P_frame_base_Proj(irn)                   ||
		is_Start_Proj(irn))
	{
		/* don't skip the proj, we want to take the else below */
	}
	else if (is_Proj(irn)) {
		pos = ia32_translate_proj_pos(irn);
		irn = my_skip_proj(irn);
	}

	if (is_ia32_irn(irn)) {
		const arch_register_t **slots;

		slots      = get_ia32_slots(irn);
		slots[pos] = reg;
	}
	else {
		ia32_set_firm_reg(irn, reg, cur_reg_set);
	}
}

static const arch_register_t *ia32_get_irn_reg(const void *self, const ir_node *irn) {
	int pos = 0;
	const arch_register_t *reg = NULL;

	if ((is_Call_Proj(irn) && is_used_by_Keep(irn)) ||
		is_P_frame_base_Proj(irn)                   ||
		is_Start_Proj(irn))
	{
		/* don't skip the proj, we want to take the else below */
	}
	else if (is_Proj(irn)) {
		pos = ia32_translate_proj_pos(irn);
		irn = my_skip_proj(irn);
	}

	if (is_ia32_irn(irn)) {
		const arch_register_t **slots;
		slots = get_ia32_slots(irn);
		reg   = slots[pos];
	}
	else {
		reg = ia32_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t ia32_classify(const void *self, const ir_node *irn) {
	irn = my_skip_proj(irn);
	if (is_cfop(irn))
		return arch_irn_class_branch;
	else if (is_ia32_Call(irn))
		return arch_irn_class_call;
	else if (is_ia32_irn(irn))
		return arch_irn_class_normal;
	else
		return 0;
}

static arch_irn_flags_t ia32_get_flags(const void *self, const ir_node *irn) {
	irn = my_skip_proj(irn);
	if (is_ia32_irn(irn))
		return get_ia32_flags(irn);
	else {
		ir_printf("don't know flags of %+F\n", irn);
		return 0;
	}
}

/* fill register allocator interface */

static const arch_irn_ops_if_t ia32_irn_ops_if = {
	ia32_get_irn_reg_req,
	ia32_set_irn_reg,
	ia32_get_irn_reg,
	ia32_classify,
	ia32_get_flags
};

ia32_irn_ops_t ia32_irn_ops = {
	&ia32_irn_ops_if,
	NULL
};



/**************************************************
 *                _                         _  __
 *               | |                       (_)/ _|
 *   ___ ___   __| | ___  __ _  ___ _ __    _| |_
 *  / __/ _ \ / _` |/ _ \/ _` |/ _ \ '_ \  | |  _|
 * | (_| (_) | (_| |  __/ (_| |  __/ | | | | | |
 *  \___\___/ \__,_|\___|\__, |\___|_| |_| |_|_|
 *                        __/ |
 *                       |___/
 **************************************************/

static void check_for_alloca(ir_node *irn, void *env) {
	int *has_alloca = env;

	if (get_irn_opcode(irn) == iro_Alloc) {
		if (get_Alloc_where(irn) == stack_alloc) {
			*has_alloca = 1;
		}
	}
}

/**
 * Transforms the standard firm graph into
 * an ia32 firm graph
 */
static void ia32_prepare_graph(void *self) {
	ia32_code_gen_t *cg = self;

	if (! is_pseudo_ir_graph(cg->irg)) {
		/* If there is a alloca in the irg, we use %ebp for stack addressing */
		/* instead of %esp, as alloca destroys %esp.                         */

		cg->has_alloca = 0;

		/* check for alloca node */
		irg_walk_blkwise_graph(cg->irg, check_for_alloca, NULL, &(cg->has_alloca));

		if (cg->has_alloca) {
			ia32_general_purpose_regs[REG_EBP].type = arch_register_type_ignore;
		}

		irg_walk_blkwise_graph(cg->irg, NULL, ia32_transform_node, cg);
	}
}



/**
 * Set the register for P_frame_base Proj to %esp.
 */
static void ia32_set_P_frame_base_Proj_reg(ir_node *irn, void *env) {
	ia32_code_gen_t *cg = env;

	if (is_P_frame_base_Proj(irn)) {
		arch_set_irn_register(cg->arch_env, irn, &ia32_general_purpose_regs[REG_ESP]);
	}
}

/**
 * This function is the hook before_sched but more important: it is
 * called after the dead node elimination. The dead node elimination changes
 * the memory location of the nodes, which will change the hash key of
 * the Proj_P_frame_base(Start) and this will fuck up the firm_node -> register
 * hash map. So we need to insert the register for this node after the dead node
 * elimination.
 */
static void ia32_some_stuff_need_to_be_done_after_deadnode_elimination(void *self) {
	ia32_code_gen_t *cg  = self;

	if (! is_pseudo_ir_graph(cg->irg))
		irg_walk_blkwise_graph(cg->irg, NULL, ia32_set_P_frame_base_Proj_reg, cg);
}



/**
 * Dummy functions for hooks we don't need but which must be filled.
 */
static void ia32_before_ra(void *self) {
}



/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ia32_codegen(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph       *irg = cg->irg;
	FILE           *out = cg->out;

	if (cg->emit_decls) {
		ia32_gen_decls(cg->out);
		cg->emit_decls = 0;
	}

//	ia32_finish_irg(irg);
	ia32_gen_routine(out, irg, cg->arch_env);

	cur_reg_set = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);
}

static void *ia32_cg_init(FILE *F, ir_graph *irg, const arch_env_t *arch_env);

static const arch_code_generator_if_t ia32_code_gen_if = {
	ia32_cg_init,
	ia32_prepare_graph,
	ia32_some_stuff_need_to_be_done_after_deadnode_elimination,   /* before scheduling hook */
	ia32_before_ra,      /* before register allocation hook */
	ia32_codegen         /* emit && done */
};

/**
 * Initializes the code generator.
 */
static void *ia32_cg_init(FILE *F, ir_graph *irg, const arch_env_t *arch_env) {
	ia32_isa_t      *isa = (ia32_isa_t *)arch_env->isa;
	ia32_code_gen_t *cg  = malloc(sizeof(*cg));

	cg->impl       = &ia32_code_gen_if;
	cg->irg        = irg;
	cg->reg_set    = new_set(ia32_cmp_irn_reg_assoc, 1024);
	cg->mod        = firm_dbg_register("be.transform.ia32");
	cg->out        = F;
	cg->arch_env   = arch_env;

	isa->num_codegens++;

	if (isa->num_codegens > 1)
		cg->emit_decls = 0;
	else
		cg->emit_decls = 1;

	cur_reg_set = cg->reg_set;

	ia32_irn_ops.cg = cg;

	return (arch_code_generator_t *)cg;
}



/*****************************************************************
 *  ____             _                  _   _____  _____
 * |  _ \           | |                | | |_   _|/ ____|  /\
 * | |_) | __ _  ___| | _____ _ __   __| |   | | | (___   /  \
 * |  _ < / _` |/ __| |/ / _ \ '_ \ / _` |   | |  \___ \ / /\ \
 * | |_) | (_| | (__|   <  __/ | | | (_| |  _| |_ ____) / ____ \
 * |____/ \__,_|\___|_|\_\___|_| |_|\__,_| |_____|_____/_/    \_\
 *
 *****************************************************************/

/**
 * Initializes the backend ISA and opens the output file.
 */
static void *ia32_init(void) {
	static int inited = 0;
	ia32_isa_t *isa   = malloc(sizeof(*isa));

	isa->impl = &ia32_isa_if;

	if(inited)
		return NULL;

	inited = 1;

	isa->num_codegens    = 0;
	isa->reg_projnum_map = new_set(ia32_cmp_reg_projnum_assoc, 1024);

	ia32_register_init(isa);
	ia32_create_opcodes();

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void ia32_done(void *self) {
	free(self);
}



static int ia32_get_n_reg_class(const void *self) {
	return N_CLASSES;
}

static const arch_register_class_t *ia32_get_reg_class(const void *self, int i) {
	assert(i >= 0 && i < N_CLASSES && "Invalid ia32 register class requested.");
	return &ia32_reg_classes[i];
}

static const void *ia32_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn) {
	return &ia32_irn_ops;
}

const arch_irn_handler_t ia32_irn_handler = {
	ia32_get_irn_ops
};

const arch_irn_handler_t *ia32_get_irn_handler(const void *self) {
	return &ia32_irn_handler;
}

long ia32_get_call_projnum_for_reg(const void *self, const arch_register_t *reg) {
	ia32_isa_t *isa = (ia32_isa_t *)self;
	return ia32_get_reg_projnum(reg, isa->reg_projnum_map);
}

int ia32_to_appear_in_schedule(void *block_env, const ir_node *irn) {
	return is_ia32_irn(irn);
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *ia32_get_code_generator_if(void *self) {
	return &ia32_code_gen_if;
}

list_sched_selector_t ia32_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *ia32_get_list_sched_selector(const void *self) {
	memcpy(&ia32_sched_selector, reg_pressure_selector, sizeof(list_sched_selector_t));
	ia32_sched_selector.to_appear_in_schedule = ia32_to_appear_in_schedule;
	return &ia32_sched_selector;
}

#ifdef WITH_LIBCORE
static void ia32_register_options(lc_opt_entry_t *ent)
{
}
#endif /* WITH_LIBCORE */

const arch_isa_if_t ia32_isa_if = {
#ifdef WITH_LIBCORE
	ia32_register_options,
#endif
	ia32_init,
	ia32_done,
	ia32_get_n_reg_class,
	ia32_get_reg_class,
	ia32_get_irn_handler,
	ia32_get_code_generator_if,
	ia32_get_list_sched_selector,
	ia32_get_call_projnum_for_reg
};
