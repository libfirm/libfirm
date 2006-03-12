/* The main TEMPLATE backend driver file. */
/* $Id$ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"

#include "bitset.h"
#include "debug.h"

#include "../bearch.h"                /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "../be.h"
#include "../beabi.h"

#include "bearch_TEMPLATE_t.h"

#include "TEMPLATE_new_nodes.h"           /* TEMPLATE nodes interface */
#include "gen_TEMPLATE_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "TEMPLATE_gen_decls.h"           /* interface declaration emitter */
#include "TEMPLATE_transform.h"
#include "TEMPLATE_emitter.h"
#include "TEMPLATE_map_regs.h"

#define DEBUG_MODULE "firm.be.TEMPLATE.isa"

/* TODO: ugly, but we need it to get access to the registers assigned to Phi nodes */
static set *cur_reg_set = NULL;

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
 * Return register requirements for a TEMPLATE node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const arch_register_req_t *TEMPLATE_get_irn_reg_req(const void *self, arch_register_req_t *req, const ir_node *irn, int pos) {
	const TEMPLATE_register_req_t *irn_req;
	long               node_pos = pos == -1 ? 0 : pos;
	ir_mode           *mode     = get_irn_mode(irn);
	firm_dbg_module_t *mod      = firm_dbg_register(DEBUG_MODULE);

	if (mode == mode_T || mode == mode_M) {
		DBG((mod, LEVEL_1, "ignoring mode_T, mode_M node %+F\n", irn));
		return NULL;
	}

	DBG((mod, LEVEL_1, "get requirements at pos %d for %+F ... ", pos, irn));

	if (is_Proj(irn)) {
		/* in case of a proj, we need to get the correct OUT slot */
		/* of the node corresponding to the proj number */
		if (pos == -1) {
			node_pos = TEMPLATE_translate_proj_pos(irn);
		}
		else {
			node_pos = pos;
		}

		irn = my_skip_proj(irn);

		DB((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", irn, node_pos));
	}

	/* get requirements for our own nodes */
	if (is_TEMPLATE_irn(irn)) {
		if (pos >= 0) {
			irn_req = get_TEMPLATE_in_req(irn, pos);
		}
		else {
			irn_req = get_TEMPLATE_out_req(irn, node_pos);
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
	/* get requirements for FIRM nodes */
	else {
		/* treat Phi like Const with default requirements */
		if (is_Phi(irn)) {
			DB((mod, LEVEL_1, "returning standard reqs for %+F\n", irn));

			if (mode_is_float(mode)) {
				memcpy(req, &(TEMPLATE_default_req_TEMPLATE_floating_point.req), sizeof(*req));
			}
			else if (mode_is_int(mode) || mode_is_reference(mode)) {
				memcpy(req, &(TEMPLATE_default_req_TEMPLATE_general_purpose.req), sizeof(*req));
			}
			else if (mode == mode_T || mode == mode_M) {
				DBG((mod, LEVEL_1, "ignoring Phi node %+F\n", irn));
				return NULL;
			}
			else {
				assert(0 && "unsupported Phi-Mode");
			}
		}
		else {
			DB((mod, LEVEL_1, "returning NULL for %+F (node not supported)\n", irn));
			req = NULL;
		}
	}

	return req;
}

static void TEMPLATE_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg) {
	int pos = 0;

	if (is_Proj(irn)) {
		pos = TEMPLATE_translate_proj_pos(irn);
		irn = my_skip_proj(irn);
	}

	if (is_TEMPLATE_irn(irn)) {
		const arch_register_t **slots;

		slots      = get_TEMPLATE_slots(irn);
		slots[pos] = reg;
	}
	else {
		/* here we set the registers for the Phi nodes */
		TEMPLATE_set_firm_reg(irn, reg, cur_reg_set);
	}
}

static const arch_register_t *TEMPLATE_get_irn_reg(const void *self, const ir_node *irn) {
	int pos = 0;
	const arch_register_t *reg = NULL;

	if (is_Proj(irn)) {
		pos = TEMPLATE_translate_proj_pos(irn);
		irn = my_skip_proj(irn);
	}

	if (is_TEMPLATE_irn(irn)) {
		const arch_register_t **slots;
		slots = get_TEMPLATE_slots(irn);
		reg   = slots[pos];
	}
	else {
		reg = TEMPLATE_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t TEMPLATE_classify(const void *self, const ir_node *irn) {
	irn = my_skip_proj(irn);

	if (is_cfop(irn)) {
		return arch_irn_class_branch;
	}
	else if (is_TEMPLATE_irn(irn)) {
		return arch_irn_class_normal;
	}

	return 0;
}

static arch_irn_flags_t TEMPLATE_get_flags(const void *self, const ir_node *irn) {
	irn = my_skip_proj(irn);

	if (is_TEMPLATE_irn(irn)) {
		return get_TEMPLATE_flags(irn);
	}
	else if (is_Unknown(irn)) {
		return arch_irn_flags_ignore;
	}

	return 0;
}

static entity *TEMPLATE_get_frame_entity(const void *self, const ir_node *irn) {
	/* TODO: return the entity assigned to the frame */
	return NULL;
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void TEMPLATE_set_stack_bias(const void *self, ir_node *irn, int bias) {
	/* TODO: correct offset if irn accesses the stack */
}

/* fill register allocator interface */

static const arch_irn_ops_if_t TEMPLATE_irn_ops_if = {
	TEMPLATE_get_irn_reg_req,
	TEMPLATE_set_irn_reg,
	TEMPLATE_get_irn_reg,
	TEMPLATE_classify,
	TEMPLATE_get_flags,
	TEMPLATE_get_frame_entity,
	TEMPLATE_set_stack_bias
};

TEMPLATE_irn_ops_t TEMPLATE_irn_ops = {
	&TEMPLATE_irn_ops_if,
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
 * a TEMLPATE firm graph
 */
static void TEMPLATE_prepare_graph(void *self) {
	TEMPLATE_code_gen_t *cg = self;

	irg_walk_blkwise_graph(cg->irg, NULL, TEMPLATE_transform_node, cg);
}



/**
 * Called immediatly before emit phase.
 */
static void TEMPLATE_finish_irg(ir_graph *irg, TEMPLATE_code_gen_t *cg) {
	/* TODO: - fix offsets for nodes accessing stack
			 - ...
	*/
}


/**
 * These are some hooks which must be filled but are probably not needed.
 */
static void TEMPLATE_before_sched(void *self) {
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void TEMPLATE_before_ra(void *self) {
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void TEMPLATE_after_ra(void *self) {
	/* Some stuff you need to do immediatly after register allocation */
}



/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void TEMPLATE_emit_and_done(void *self) {
	TEMPLATE_code_gen_t *cg = self;
	ir_graph           *irg = cg->irg;
	FILE               *out = cg->out;

	if (cg->emit_decls) {
		TEMPLATE_gen_decls(cg->out);
		cg->emit_decls = 0;
	}

	TEMPLATE_finish_irg(irg, cg);
	dump_ir_block_graph_sched(irg, "-TEMPLATE-finished");
	TEMPLATE_gen_routine(out, irg, cg);

	cur_reg_set = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);
}

static void *TEMPLATE_cg_init(FILE *F, const be_irg_t *birg);

static const arch_code_generator_if_t TEMPLATE_code_gen_if = {
	TEMPLATE_cg_init,
	TEMPLATE_prepare_graph,
	TEMPLATE_before_sched,   /* before scheduling hook */
	TEMPLATE_before_ra,      /* before register allocation hook */
	TEMPLATE_after_ra,       /* after register allocation hook */
	TEMPLATE_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *TEMPLATE_cg_init(FILE *F, const be_irg_t *birg) {
	TEMPLATE_isa_t      *isa = (TEMPLATE_isa_t *)birg->main_env->arch_env->isa;
	TEMPLATE_code_gen_t *cg  = xmalloc(sizeof(*cg));

	cg->impl     = &TEMPLATE_code_gen_if;
	cg->irg      = birg->irg;
	cg->reg_set  = new_set(TEMPLATE_cmp_irn_reg_assoc, 1024);
	cg->mod      = firm_dbg_register("firm.be.TEMPLATE.cg");
	cg->out      = F;
	cg->arch_env = birg->main_env->arch_env;
	cg->birg     = birg;

	isa->num_codegens++;

	if (isa->num_codegens > 1)
		cg->emit_decls = 0;
	else
		cg->emit_decls = 1;

	cur_reg_set = cg->reg_set;

	TEMPLATE_irn_ops.cg = cg;

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

static TEMPLATE_isa_t TEMPLATE_isa_template = {
	&TEMPLATE_isa_if,
	&TEMPLATE_general_purpose_regs[REG_R6],
	&TEMPLATE_general_purpose_regs[REG_R7],
	-1,
	0
};

/**
 * Initializes the backend ISA and opens the output file.
 */
static void *TEMPLATE_init(void) {
	static int inited = 0;
	TEMPLATE_isa_t *isa;

	if(inited)
		return NULL;

	isa = xcalloc(1, sizeof(*isa));
	memcpy(isa, &TEMPLATE_isa_template, sizeof(*isa));

	TEMPLATE_register_init(isa);
	TEMPLATE_create_opcodes();

	inited = 1;

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void TEMPLATE_done(void *self) {
	free(self);
}



static int TEMPLATE_get_n_reg_class(const void *self) {
	return N_CLASSES;
}

static const arch_register_class_t *TEMPLATE_get_reg_class(const void *self, int i) {
	assert(i >= 0 && i < N_CLASSES && "Invalid TEMPLATE register class requested.");
	return &TEMPLATE_reg_classes[i];
}



/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *TEMPLATE_get_reg_class_for_mode(const void *self, const ir_mode *mode) {
	if (mode_is_float(mode))
		return &TEMPLATE_reg_classes[CLASS_TEMPLATE_floating_point];
	else
		return &TEMPLATE_reg_classes[CLASS_TEMPLATE_general_purpose];
}



/**
 * Produces the type which sits between the stack args and the locals on the stack.
 * it will contain the return address and space to store the old base pointer.
 * @return The Firm type modelling the ABI between type.
 */
static ir_type *get_between_type(void) {
	static ir_type *between_type = NULL;
	static entity *old_bp_ent    = NULL;

	if(!between_type) {
		entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);
		ir_type *old_bp_type   = new_type_primitive(new_id_from_str("bp"), mode_P);

		between_type           = new_type_class(new_id_from_str("TEMPLATE_between_type"));
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
void TEMPLATE_get_call_abi(const void *self, ir_type *method_type, be_abi_call_t *abi) {
	ir_type  *between_type;
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);
	const arch_register_t *reg;
	be_abi_call_flags_t call_flags;

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;
	call_flags.bits.store_args_sequential = 1;
	call_flags.bits.try_omit_fp           = 1;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* get the between type and the frame pointer save entity */
	between_type = get_between_type();

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, between_type);

	for (i = 0; i < n; i++) {
		/* TODO: implement register parameter: */
		/* reg = get reg for param i;          */
		/* be_abi_call_param_reg(abi, i, reg); */

		/* default: all parameters on stack */
		be_abi_call_param_stack(abi, i);
	}

	/* TODO: set correct return register */
	/* default: return value is in R0 resp. F0 */
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		be_abi_call_res_reg(abi, 0,
			mode_is_float(mode) ? &TEMPLATE_floating_point_regs[REG_F0] : &TEMPLATE_general_purpose_regs[REG_R0]);
	}
}

static const void *TEMPLATE_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn) {
	return &TEMPLATE_irn_ops;
}

const arch_irn_handler_t TEMPLATE_irn_handler = {
	TEMPLATE_get_irn_ops
};

const arch_irn_handler_t *TEMPLATE_get_irn_handler(const void *self) {
	return &TEMPLATE_irn_handler;
}

int TEMPLATE_to_appear_in_schedule(void *block_env, const ir_node *irn) {
	return is_TEMPLATE_irn(irn);
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *TEMPLATE_get_code_generator_if(void *self) {
	return &TEMPLATE_code_gen_if;
}

list_sched_selector_t TEMPLATE_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *TEMPLATE_get_list_sched_selector(const void *self) {
	memcpy(&TEMPLATE_sched_selector, trivial_selector, sizeof(list_sched_selector_t));
	TEMPLATE_sched_selector.to_appear_in_schedule = TEMPLATE_to_appear_in_schedule;
	return &TEMPLATE_sched_selector;
}

#ifdef WITH_LIBCORE
static void TEMPLATE_register_options(lc_opt_entry_t *ent)
{
}
#endif /* WITH_LIBCORE */

const arch_isa_if_t TEMPLATE_isa_if = {
#ifdef WITH_LIBCORE
	TEMPLATE_register_options,
#endif
	TEMPLATE_init,
	TEMPLATE_done,
	TEMPLATE_get_n_reg_class,
	TEMPLATE_get_reg_class,
	TEMPLATE_get_reg_class_for_mode,
	TEMPLATE_get_call_abi,
	TEMPLATE_get_irn_handler,
	TEMPLATE_get_code_generator_if,
	TEMPLATE_get_list_sched_selector,
};
