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

#include "../beabi.h"                 /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "../be.h"
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

ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi, &ia32_gp_regs[REG_XXX]);
}

ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi, &ia32_fp_regs[REG_XXXX]);
}

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


	if (is_Proj(irn)) {
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

		if (arch_register_req_is(&(irn_req->req), should_be_same)) {
			assert(irn_req->same_pos >= 0 && "should be same constraint for in -> out NYI");
			req->other_same = get_irn_n(irn, irn_req->same_pos);
		}

		if (arch_register_req_is(&(irn_req->req), should_be_different)) {
			assert(irn_req->different_pos >= 0 && "should be different constraint for in -> out NYI");
			req->other_different = get_irn_n(irn, irn_req->different_pos);
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
		else {
			DB((mod, LEVEL_1, "returning NULL for %+F (not ia32)\n", irn));
			req = NULL;
		}
	}

	return req;
}

static void ia32_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg) {
	int pos = 0;

	if (is_Proj(irn)) {
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

	if (is_Proj(irn)) {
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
		return 0;
	}
}

static entity *ia32_get_frame_entity(const void *self, const ir_node *irn)
{
	/* TODO: Implement */
	return NULL;
}

static void ia32_set_stack_bias(const void *self, ir_node *irn, int bias) {
	if (get_ia32_use_frame(irn)) {
		/* TODO: correct offset */
	}
}

/* fill register allocator interface */

static const arch_irn_ops_if_t ia32_irn_ops_if = {
	ia32_get_irn_reg_req,
	ia32_set_irn_reg,
	ia32_get_irn_reg,
	ia32_classify,
	ia32_get_flags,
	ia32_get_frame_entity,
	ia32_set_stack_bias
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

/**
 * Transforms the standard firm graph into
 * an ia32 firm graph
 */
static void ia32_prepare_graph(void *self) {
	ia32_code_gen_t *cg = self;

	irg_walk_blkwise_graph(cg->irg, ia32_place_consts, ia32_transform_node, cg);
	dump_ir_block_graph_sched(cg->irg, "-transformed");
	edges_deactivate(cg->irg);
	edges_activate(cg->irg);
	irg_walk_blkwise_graph(cg->irg, NULL, ia32_optimize_am, cg);
	dump_ir_block_graph_sched(cg->irg, "-am");
}



/**
 * Stack reservation and StackParam lowering.
 */
static void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg) {

}



/**
 * Dummy functions for hooks we don't need but which must be filled.
 */
static void ia32_before_sched(void *self) {
}

static void ia32_before_ra(void *self) {
}


/**
 * Creates a Store for a Spill
 */
static ir_node *ia32_lower_spill(void *self, ir_node *spill) {
	ia32_code_gen_t *cg    = self;
	ir_graph        *irg   = cg->irg;
	dbg_info        *dbg   = get_irn_dbg_info(spill);
	ir_node         *block = get_nodes_block(spill);
	ir_node         *ptr   = get_irg_frame(irg);
	ir_node         *val   = be_get_Spill_context(spill);
	ir_node         *mem   = new_rd_NoMem(irg);
	ir_mode         *mode  = get_irn_mode(spill);
	entity          *ent   = be_get_spill_entity(spill);
	unsigned         offs  = get_entity_offset_bytes(ent);
	ir_node         *noreg, *res;
	char             buf[64];

	DB((cg->mod, LEVEL_1, "lower_spill: got offset %d for %+F\n", offs, ent));

	if (mode_is_float(mode)) {
		noreg = ia32_new_NoReg_fp(cg);
		res   = new_rd_ia32_fStore(dbg, irg, block, ptr, noreg, val, mem, mode);
	}
	else {
		noreg = ia32_new_NoReg_gp(cg);
		res   = new_rd_ia32_Store(dbg, irg, block, ptr, noreg, val, mem, mode);
	}

	snprintf(buf, sizeof(buf), "%d", offs);
	add_ia32_am_offs(res, buf);

	return res;
}

/**
 * Create a Load for a Spill
 */
static ir_node *ia32_lower_reload(void *self, ir_node *reload) {
	ia32_code_gen_t *cg    = self;
	ir_graph        *irg   = cg->irg;
	dbg_info        *dbg   = get_irn_dbg_info(reload);
	ir_node         *block = get_nodes_block(reload);
	ir_node         *ptr   = get_irg_frame(irg);
	ir_mode         *mode  = get_irn_mode(reload);
	ir_node         *pred  = get_irn_n(reload, 0);
	char             buf[64];
	char            *ofs;
	ir_node         *noreg, *res;

	/* Get the offset to Load from. It can either be a Spill or a Store. */
	if (be_is_Spill(pred)) {
		entity   *ent  = be_get_spill_entity(pred);
		unsigned  offs = get_entity_offset_bytes(ent);
		DB((cg->mod, LEVEL_1, "lower_reload: got offset %d for %+F\n", offs, ent));

		snprintf(buf, sizeof(buf), "%d", offs);
	}
	else if (is_ia32_Store(pred) || is_ia32_fStore(pred)) {
		ofs = get_ia32_am_offs(pred);
		strncpy(buf, ofs, sizeof(buf));
		free(ofs);
	}
	else {
		assert(0 && "unsupported Reload predecessor");
	}

	/* Create the Load */
	if (mode_is_float(mode)) {
		noreg = ia32_new_NoReg_fp(cg);
		res   = new_rd_ia32_fLoad(dbg, irg, block, ptr, noreg, pred, mode_T);
	}
	else {
		noreg = ia32_new_NoReg_gp(cg);
		res   = new_rd_ia32_Load(dbg, irg, block, ptr, noreg, pred, mode_T);
	}

	/* Set offset */
	add_ia32_am_offs(res, buf);

	/* Return the result Proj */
	return new_rd_Proj(dbg, irg, block, res, mode, 0);
}

/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ia32_codegen(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;
	FILE            *out = cg->out;

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

static void *ia32_cg_init(FILE *F, const be_irg_t *birg);

static const arch_code_generator_if_t ia32_code_gen_if = {
	ia32_cg_init,
	ia32_prepare_graph,
	ia32_before_sched,   /* before scheduling hook */
	ia32_before_ra,      /* before register allocation hook */
	ia32_lower_spill,
	ia32_lower_reload,
	ia32_codegen         /* emit && done */
};

/**
 * Initializes the code generator.
 */
static void *ia32_cg_init(FILE *F, const be_irg_t *birg) {
	ia32_isa_t      *isa = (ia32_isa_t *)birg->main_env->arch_env->isa;
	ia32_code_gen_t *cg  = xcalloc(1, sizeof(*cg));

	cg->impl     = &ia32_code_gen_if;
	cg->irg      = birg->irg;
	cg->reg_set  = new_set(ia32_cmp_irn_reg_assoc, 1024);
	cg->mod      = firm_dbg_register("firm.be.ia32.cg");
	cg->out      = F;
	cg->arch_env = birg->main_env->arch_env;
	cg->types    = pmap_create();
	cg->tv_ent   = pmap_create();
	cg->birg     = birg;

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

static ia32_isa_t ia32_isa_template = {
	&ia32_isa_if,
	&ia32_gp_regs[REG_ESP],
	&ia32_gp_regs[REG_EBP],
	-1,
	0
};

/**
 * Initializes the backend ISA.
 */
static void *ia32_init(void) {
	static int inited = 0;
	ia32_isa_t *isa;

	if(inited)
		return NULL;

	isa = xcalloc(1, sizeof(*isa));
	memcpy(isa, &ia32_isa_template, sizeof(*isa));

	ia32_register_init(isa);
	ia32_create_opcodes();

	inited = 1;

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

/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *ia32_get_reg_class_for_mode(const void *self, const ir_mode *mode) {
	if (mode_is_float(mode))
		return &ia32_reg_classes[CLASS_ia32_fp];
	else
		return &ia32_reg_classes[CLASS_ia32_gp];
}

/**
 * Produces the type which sits between the stack args and the locals on the stack.
 * it will contain the return address and space to store the old base pointer.
 * @return The Firm type modelling the ABI between type.
 */
static ir_type *get_between_type(void)
{
	static ir_type *between_type = NULL;
	static entity *old_bp_ent    = NULL;

	if(!between_type) {
		entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);
		ir_type *old_bp_type   = new_type_primitive(new_id_from_str("bp"), mode_P);

		between_type           = new_type_class(new_id_from_str("ia32_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("old_bp"), ret_addr_type);

		set_entity_offset_bytes(old_bp_ent, 0);
		set_entity_offset_bytes(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
	}

	return between_type;
}

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
void ia32_get_call_abi(const void *self, ir_type *method_type, be_abi_call_t *abi) {
	ir_type  *between_type;
	ir_type  *tp;
	ir_mode  *mode;
	unsigned  cc        = get_method_calling_convention(method_type);
	int       n         = get_method_n_params(method_type);
	int       biggest_n = -1;
	int       stack_idx = 0;
	int       i, ignore;
	ir_mode **modes;
	const arch_register_t *reg;

	/* get the between type and the frame pointer save entity */
	between_type = get_between_type();

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, BE_ABI_NONE, between_type);

	/* collect the mode for each type */
	modes = alloca(n * sizeof(modes[0]));

	for (i = 0; i < n; i++) {
		tp       = get_method_param_type(method_type, i);
		modes[i] = get_type_mode(tp);
	}

	/* set register parameters  */
	if (cc & cc_reg_param) {
		/* determine the number of parameters passed via registers */
		biggest_n = ia32_get_n_regparam_class(n, modes, &ignore, &ignore);

		/* loop over all parameters and set the register requirements */
		for (i = 0; i <= biggest_n; i++) {
			reg = ia32_get_RegParam_reg(n, modes, i, cc);
			assert(reg && "kaputt");
			be_abi_call_param_reg(abi, i, reg);
		}

		stack_idx = i;
	}


	/* set stack parameters */
	for (i = stack_idx; i < n; i++) {
		be_abi_call_param_stack(abi, i);
	}


	/* set return registers */
	n = get_method_n_ress(method_type);

	assert(n <= 2 && "more than two results not supported");

	/* In case of 64bit returns, we will have two 32bit values */
	if (n == 2) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		assert(!mode_is_float(mode) && "two FP results not supported");

		tp   = get_method_res_type(method_type, 1);
		mode = get_type_mode(tp);

		assert(!mode_is_float(mode) && "two FP results not supported");

		be_abi_call_res_reg(abi, 0, &ia32_gp_regs[REG_EAX]);
		be_abi_call_res_reg(abi, 1, &ia32_gp_regs[REG_EDX]);
	}
	else if (n == 1) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		be_abi_call_res_reg(abi, 0, &ia32_fp_regs[mode_is_float(mode) ? REG_XMM0 : REG_EAX]);
	}
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
	ia32_get_reg_class_for_mode,
	ia32_get_call_abi,
	ia32_get_irn_handler,
	ia32_get_code_generator_if,
	ia32_get_list_sched_selector
};
