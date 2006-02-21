#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irgmod.h"

#include "bitset.h"
#include "debug.h"

#include "../bearch.h"                /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "bearch_ia32_t.h"

#include "ia32_new_nodes.h"           /* ia32 nodes interface */
#include "gen_ia32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "ia32_gen_decls.h"           /* interface declaration emitter */
#include "ia32_transform.h"
#include "ia32_emitter.h"
#include "ia32_map_regs.h"
#include "ia32_optimize.h"

#define DEBUG_MODULE "firm.be.ia32.isa"

/* TODO: ugly */
static set *cur_reg_set = NULL;

#undef is_Start
#define is_Start(irn) (get_irn_opcode(irn) == iro_Start)

extern ir_node *be_new_NoReg(ir_graph *irg);

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
		is_Start(get_Proj_pred(n)) &&
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
		if (pos >= 0) {
			req = NULL;
		}
		else {
			irn_req = ia32_projnum_reg_req_map[get_Proj_proj(irn)];
			memcpy(req, &(irn_req->req), sizeof(*req));
		}

		return req;
	}
	else if (is_Start_Proj(irn)) {
		irn_req = ops->cg->reg_param_req[get_Proj_proj(irn)];
		assert(irn_req && "missing requirement for regparam");
		memcpy(req, &(irn_req->req), sizeof(*req));
		return req;
		//return NULL;
	}
	else if (is_Proj(irn)) {
		if (pos == -1) {
			node_pos = ia32_translate_proj_pos(irn);
		}
		else {
			node_pos = pos;
		}

		irn = my_skip_proj(irn);

		DB((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", irn, node_pos));
	}

	if (is_ia32_irn(irn)) {
		if (pos >= 0) {
			irn_req = get_ia32_in_req(irn, pos);
		}
		else {
			irn_req = get_ia32_out_req(irn, node_pos);
		}

		DB((mod, LEVEL_1, "returning reqs for %+F at pos %d\n", irn, pos));

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
			DB((mod, LEVEL_1, "returning standard reqs for %+F\n", irn));
			if (mode_is_float(mode))
				memcpy(req, &(ia32_default_req_ia32_fp.req), sizeof(*req));
			else if (mode_is_int(mode) || mode_is_reference(mode))
				memcpy(req, &(ia32_default_req_ia32_gp.req), sizeof(*req));
			else if (mode == mode_T || mode == mode_M) {
				DBG((mod, LEVEL_1, "ignoring Phi node %+F\n", irn));
				return NULL;
			}
			else
				assert(0 && "unsupported Phi-Mode");
		}
		else if (is_Start(irn)) {
			DB((mod, LEVEL_1, "returning reqs none for ProjX -> Start (%+F )\n", irn));
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
			DB((mod, LEVEL_1, "returning reqs EAX for %+F\n", irn));
			memcpy(req, &(ia32_default_req_ia32_gp_eax.req), sizeof(*req));
		}
		else {
			DB((mod, LEVEL_1, "returning NULL for %+F (not ia32)\n", irn));
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
		if (is_Start_Proj(irn))
			return arch_irn_flags_ignore;

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

	if (get_irn_op(irn) == op_Alloc) {
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
			ia32_gp_regs[REG_EBP].type = arch_register_type_ignore;
		}

		irg_walk_blkwise_graph(cg->irg, ia32_place_consts, ia32_transform_node, cg);
	}
}



/**
 * Stack reservation and StackParam lowering.
 */
static void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg) {
#if 0
	firm_dbg_module_t *mod       = cg->mod;
	ir_node           *frame     = get_irg_frame(irg);
	ir_node           *end_block = get_irg_end_block(irg);
	ir_node          **returns, **in, **new_in;
	ir_node           *stack_reserve, *sched_point;
	ir_node           *stack_free, *new_ret, *return_block;
	int                stack_size = 0, i, n_arg;
	arch_register_t   *stack_reg;
	tarval            *stack_size_tv;
	dbg_info          *frame_dbg;

	/* Determine stack register */
	if (cg->has_alloca) {
		stack_reg = &ia32_gp_regs[REG_EBP];
	}
	else {
		stack_reg = &ia32_gp_regs[REG_ESP];
	}

	/* If frame is used, then we need to reserve some stackspace. */
	if (get_irn_n_edges(frame) > 0) {
		/* The initial stack reservation. */
		stack_size    = get_type_size_bytes(get_irg_frame_type(irg));
		frame_dbg     = get_irn_dbg_info(frame);
		stack_reserve = new_rd_ia32_Sub_i(frame_dbg, irg, get_nodes_block(frame), new_NoMem(), mode_Is);
		stack_size_tv = new_tarval_from_long(stack_size, mode_Is);
		set_ia32_Immop_tarval(stack_reserve, stack_size_tv);

		assert(stack_size && "bOrken stack layout");

		/* reroute all edges from frame pointer to corrected frame pointer */
		edges_reroute(frame, stack_reserve, irg);
		set_irn_n(stack_reserve, 0, frame);

		/* schedule frame pointer */
		if (! sched_is_scheduled(frame)) {
			sched_add_after(get_irg_start(irg), frame);
		}

		/* set register */
		arch_set_irn_register(cg->arch_env, frame, stack_reg);
		arch_set_irn_register(cg->arch_env, stack_reserve, stack_reg);

		/* insert into schedule */
		sched_add_after(frame, stack_reserve);

		/* Free stack for each Return node */
		returns = get_Block_cfgpred_arr(end_block);
		for (i = 0; i < get_Block_n_cfgpreds(end_block); i++) {
			assert(get_irn_opcode(returns[i]) == iro_Return && "cfgpred of endblock is not a return");

			return_block = get_nodes_block(returns[i]);

			/* free the stack */
			stack_free = new_rd_ia32_Add_i(frame_dbg, irg, return_block, stack_reserve, mode_Is);
			set_ia32_Immop_tarval(stack_free, stack_size_tv);
			arch_set_irn_register(cg->arch_env, stack_free, stack_reg);

			DBG((mod, LEVEL_1, "examining %+F, %+F created, block %+F", returns[i], stack_free, return_block));

			/* get the old Return arguments */
			n_arg  = get_Return_n_ress(returns[i]);
			in     = get_Return_res_arr(returns[i]);
			new_in = alloca((n_arg + 2) * sizeof(new_in[0]));

			/* copy the old to the new in's */
			memcpy(new_in, in, n_arg * sizeof(in[0]));
			new_in[n_arg++] = stack_free;
			new_in[n_arg++] = get_Return_mem(returns[i]);

			/* create the new return node */
			new_ret = new_rd_ia32_Return(get_irn_dbg_info(returns[i]), irg, return_block, n_arg, new_in);

			/* In case the return node is the only node in the block, */
			/* it is not scheduled, so we need this work-around.      */
			if (! sched_is_scheduled(returns[i])) {
				sched_point = return_block;
			}
			else {
				sched_point = sched_prev(returns[i]);
				sched_remove(returns[i]);
			}

			/* exchange the old return with the new one */
			exchange(returns[i], new_ret);

			DB((mod, LEVEL_1, " ... replaced with %+F\n", new_ret));

			/* remove the old one from schedule and add the new nodes properly */
			sched_add_after(sched_point, new_ret);
			sched_add_after(sched_point, stack_free);
		}
	}
#endif
}



/**
 * Dummy functions for hooks we don't need but which must be filled.
 */
static void ia32_before_sched(void *self) {
	ia32_code_gen_t *cg = self;

	lower_nodes_before_sched(cg->irg, cg->arch_env);
}

static void ia32_before_ra(void *self) {
}


/**
 * Creates a Store for a Spill
 */
static ir_node *ia32_lower_spill(void *self, ir_node *spill) {
	ia32_code_gen_t *cg    = self;
	dbg_info        *dbg   = get_irn_dbg_info(spill);
	ir_node         *block = get_nodes_block(spill);
	ir_node         *ptr   = get_irg_frame(cg->irg);
	ir_node         *val   = be_get_Spill_context(spill);
	ir_node         *mem   = new_rd_NoMem(cg->irg);
	ir_node         *noreg = be_new_NoReg(cg->irg);
	ir_mode         *mode  = get_irn_mode(spill);
	ir_node         *res;
	entity          *ent   = be_get_spill_entity(spill);
	unsigned         offs  = get_entity_offset_bytes(ent);
	char             buf[64];

	DB((cg->mod, LEVEL_1, "lower_spill: got offset %d for %+F\n", offs, ent));

	res = new_rd_ia32_Store(dbg, cg->irg, block, ptr, noreg, val, mem, mode);
	snprintf(buf, sizeof(buf), "%d", offs);
	add_ia32_am_offs(res, buf);

	return res;
}

/**
 * Create a Load for a Spill
 */
static ir_node *ia32_lower_reload(void *self, ir_node *reload) {
	ia32_code_gen_t *cg    = self;
	dbg_info        *dbg   = get_irn_dbg_info(reload);
	ir_node         *block = get_nodes_block(reload);
	ir_node         *ptr   = get_irg_frame(cg->irg);
	ir_mode         *mode  = get_irn_mode(reload);
	ir_node         *pred  = get_irn_n(reload, 0);
	ir_node         *noreg = be_new_NoReg(cg->irg);
	char             buf[64];
	char            *ofs;
	ir_node         *res;

	if (be_is_Spill(pred)) {
		entity   *ent  = be_get_spill_entity(pred);
		unsigned  offs = get_entity_offset_bytes(ent);
		DB((cg->mod, LEVEL_1, "lower_reload: got offset %d for %+F\n", offs, ent));

		snprintf(buf, sizeof(buf), "%d", offs);
	}
	else if (is_ia32_Store(pred)) {
		ofs = get_ia32_am_offs(pred);
		strncpy(buf, ofs, sizeof(buf));
		free(ofs);
	}
	else {
		assert(0 && "unsupported Reload predecessor");
	}

	res = new_rd_ia32_Load(dbg, cg->irg, block, ptr, noreg, pred, mode);
	add_ia32_am_offs(res, buf);

	return res;
}

/**
 * Return the stack register for this irg.
 */
static const arch_register_t *ia32_get_stack_register(void *self) {
	ia32_code_gen_t *cg = self;

	if (cg->has_alloca) {
		return &ia32_gp_regs[REG_EBP];
	}

	return &ia32_gp_regs[REG_ESP];
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

	ia32_finish_irg(irg, cg);
	//dump_ir_block_graph_sched(irg, "-finished");
	ia32_gen_routine(out, irg, cg);

	cur_reg_set = NULL;

	pmap_destroy(cg->tv_ent);
	pmap_destroy(cg->types);

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);
}

static void *ia32_cg_init(FILE *F, ir_graph *irg, const arch_env_t *arch_env);

static const arch_code_generator_if_t ia32_code_gen_if = {
	ia32_cg_init,
	ia32_prepare_graph,
	ia32_before_sched,   /* before scheduling hook */
	ia32_before_ra,      /* before register allocation hook */
	ia32_lower_spill,
	ia32_lower_reload,
	ia32_get_stack_register,
	ia32_codegen         /* emit && done */
};

/**
 * Initializes the code generator.
 */
static void *ia32_cg_init(FILE *F, ir_graph *irg, const arch_env_t *arch_env) {
	ia32_isa_t      *isa = (ia32_isa_t *)arch_env->isa;
	ia32_code_gen_t *cg  = xmalloc(sizeof(*cg));

	cg->impl       = &ia32_code_gen_if;
	cg->irg        = irg;
	cg->reg_set    = new_set(ia32_cmp_irn_reg_assoc, 1024);
	cg->mod        = firm_dbg_register("firm.be.ia32.cg");
	cg->out        = F;
	cg->arch_env   = arch_env;
	cg->types      = pmap_create();
	cg->tv_ent     = pmap_create();

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
	ia32_isa_t *isa   = xmalloc(sizeof(*isa));

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

long ia32_handle_call_proj(const void *self, ir_node *proj, int is_keep) {
	ia32_isa_t *isa = (ia32_isa_t *)self;
	long        pn  = get_Proj_proj(proj);

	if (!is_keep) {
		/* It's not a Keep proj, which means, that it is a result proj. */
		/* Possible result proj numbers are 0 and 1                     */
		/* Set the correct register (depends on the mode) and the       */
		/* corresponding proj number                                    */
		if (mode_is_float(get_irn_mode(proj))) {
			assert(pn == 0 && "only one floating point result supported");

			/* Get the proj number for the floating point result */
			pn = ia32_get_reg_projnum(&ia32_fp_regs[REG_XMM0], isa->reg_projnum_map);
		}
		else {
			/* In case of 64bit return value, the result is */
			/* in EDX:EAX and we have two result projs.     */
			switch (pn) {
				case 0:
					pn = ia32_get_reg_projnum(&ia32_gp_regs[REG_EAX], isa->reg_projnum_map);
					break;
				case 1:
					pn = ia32_get_reg_projnum(&ia32_gp_regs[REG_EDX], isa->reg_projnum_map);
					break;
				default:
					assert(0 && "only two int results supported");
			}
		}

		/* Set the correct proj number */
		set_Proj_proj(proj, pn);
	}
	else {
		/* Set mode to floating point if required */
		if (!strcmp(ia32_reg_classes[CLASS_ia32_fp].name,
					ia32_projnum_reg_req_map[pn]->req.cls->name)) {
			set_irn_mode(proj, mode_F);
		}
	}

	return pn;
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
	memcpy(&ia32_sched_selector, trivial_selector, sizeof(list_sched_selector_t));
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
	ia32_handle_call_proj
};
