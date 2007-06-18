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
 * @brief    The main TEMPLATE backend driver file.
 * @version  $Id$
 */
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

#include "../bearch_t.h"
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "be.h"
#include "../beabi.h"
#include "../bemodule.h"
#include "../begnuas.h"

#include "bearch_TEMPLATE_t.h"

#include "TEMPLATE_new_nodes.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "TEMPLATE_transform.h"
#include "TEMPLATE_emitter.h"
#include "TEMPLATE_map_regs.h"

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

/**
 * Return register requirements for a TEMPLATE node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const
arch_register_req_t *TEMPLATE_get_irn_reg_req(const void *self,
                                              const ir_node *node, int pos) {
	long               node_pos = pos == -1 ? 0 : pos;
	ir_mode           *mode     = get_irn_mode(node);
	(void) self;

	if (mode == mode_T || mode == mode_M) {
		return arch_no_register_req;
	}

	if (is_Proj(node)) {
		/* in case of a proj, we need to get the correct OUT slot */
		/* of the node corresponding to the proj number */
		if (pos == -1) {
			node_pos = TEMPLATE_translate_proj_pos(node);
		} else {
			node_pos = pos;
		}

		node = skip_Proj_const(node);
	}

	/* get requirements for our own nodes */
	if (is_TEMPLATE_irn(node)) {
		const arch_register_req_t *req;
		if (pos >= 0) {
			req = get_TEMPLATE_in_req(node, pos);
		} else {
			req = get_TEMPLATE_out_req(node, node_pos);
		}

		assert(req != NULL);

		return req;
	}

	/* unknowns should be transformed already */
	assert(!is_Unknown(node));

	return arch_no_register_req;
}

static void TEMPLATE_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg)
{
	int pos = 0;
	(void) self;

	if (is_Proj(irn)) {
		pos = TEMPLATE_translate_proj_pos(irn);
		irn = skip_Proj(irn);
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

static
const arch_register_t *TEMPLATE_get_irn_reg(const void *self,
                                            const ir_node *irn)
{
	int pos = 0;
	const arch_register_t *reg = NULL;
	(void) self;

	if (is_Proj(irn)) {
		pos = TEMPLATE_translate_proj_pos(irn);
		irn = skip_Proj_const(irn);
	}

	if (is_TEMPLATE_irn(irn)) {
		const arch_register_t * const *slots;
		slots = get_TEMPLATE_slots_const(irn);
		reg   = slots[pos];
	}
	else {
		reg = TEMPLATE_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t TEMPLATE_classify(const void *self, const ir_node *irn) {
	irn = skip_Proj_const(irn);
	(void) self;

	if (is_cfop(irn)) {
		return arch_irn_class_branch;
	}
	else if (is_TEMPLATE_irn(irn)) {
		return arch_irn_class_normal;
	}

	return 0;
}

static arch_irn_flags_t TEMPLATE_get_flags(const void *self, const ir_node *irn) {
	irn = skip_Proj_const(irn);
	(void) self;

	if (is_TEMPLATE_irn(irn)) {
		return get_TEMPLATE_flags(irn);
	}
	else if (is_Unknown(irn)) {
		return arch_irn_flags_ignore;
	}

	return 0;
}

static ir_entity *TEMPLATE_get_frame_entity(const void *self, const ir_node *node) {
	(void) self;
	(void) node;
	/* TODO: return the ir_entity assigned to the frame */
	return NULL;
}

static void TEMPLATE_set_frame_entity(const void *self, ir_node *node, ir_entity *ent) {
	(void) self;
	(void) node;
	(void) ent;
	/* TODO: set the ir_entity assigned to the frame */
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void TEMPLATE_set_frame_offset(const void *self, ir_node *irn, int offset) {
	(void) self;
	(void) irn;
	(void) offset;
	/* TODO: correct offset if irn accesses the stack */
}

static int TEMPLATE_get_sp_bias(const void *self, const ir_node *irn) {
	(void) self;
	(void) irn;
	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_if_t TEMPLATE_irn_ops_if = {
	TEMPLATE_get_irn_reg_req,
	TEMPLATE_set_irn_reg,
	TEMPLATE_get_irn_reg,
	TEMPLATE_classify,
	TEMPLATE_get_flags,
	TEMPLATE_get_frame_entity,
	TEMPLATE_set_frame_entity,
	TEMPLATE_set_frame_offset,
	TEMPLATE_get_sp_bias,
	NULL,    /* get_inverse             */
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
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
static void TEMPLATE_finish_irg(void *self) {
	TEMPLATE_code_gen_t *cg = self;
	ir_graph            *irg = cg->irg;

	dump_ir_block_graph_sched(irg, "-TEMPLATE-finished");
}


/**
 * These are some hooks which must be filled but are probably not needed.
 */
static void TEMPLATE_before_sched(void *self) {
	(void) self;
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void TEMPLATE_before_ra(void *self) {
	(void) self;
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void TEMPLATE_after_ra(void *self) {
	(void) self;
	/* Some stuff you need to do immediatly after register allocation */
}



/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void TEMPLATE_emit_and_done(void *self) {
	TEMPLATE_code_gen_t *cg = self;
	ir_graph           *irg = cg->irg;

	TEMPLATE_gen_routine(cg, irg);

	cur_reg_set = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(cg);
}

static void *TEMPLATE_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t TEMPLATE_code_gen_if = {
	TEMPLATE_cg_init,
	NULL,                    /* before abi introduce hook */
	TEMPLATE_prepare_graph,
	NULL,                    /* spill hook */
	TEMPLATE_before_sched,   /* before scheduling hook */
	TEMPLATE_before_ra,      /* before register allocation hook */
	TEMPLATE_after_ra,       /* after register allocation hook */
	TEMPLATE_finish_irg,
	TEMPLATE_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *TEMPLATE_cg_init(be_irg_t *birg) {
	const arch_env_t    *arch_env = be_get_birg_arch_env(birg);
	TEMPLATE_isa_t      *isa      = (TEMPLATE_isa_t *) arch_env->isa;
	TEMPLATE_code_gen_t *cg       = xmalloc(sizeof(*cg));

	cg->impl     = &TEMPLATE_code_gen_if;
	cg->irg      = be_get_birg_irg(birg);
	cg->reg_set  = new_set(TEMPLATE_cmp_irn_reg_assoc, 1024);
	cg->arch_env = arch_env;
	cg->isa      = isa;
	cg->birg     = birg;
	FIRM_DBG_REGISTER(cg->mod, "firm.be.TEMPLATE.cg");

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
	{
		&TEMPLATE_isa_if,             /* isa interface implementation */
		&TEMPLATE_general_purpose_regs[REG_SP],  /* stack pointer register */
		&TEMPLATE_general_purpose_regs[REG_BP],  /* base pointer register */
		-1,                          /* stack direction */
		NULL,                        /* main environment */
		7,                           /* costs for a spill instruction */
		5,                           /* costs for a reload instruction */
	},
	{ NULL, },                       /* emitter environment */
};

/**
 * Initializes the backend ISA and opens the output file.
 */
static void *TEMPLATE_init(FILE *outfile) {
	static int run_once = 0;
	TEMPLATE_isa_t *isa;

	if(run_once)
		return NULL;
	run_once = 1;

	isa = xcalloc(1, sizeof(*isa));
	memcpy(isa, &TEMPLATE_isa_template, sizeof(*isa));

	be_emit_init_env(&isa->emit, outfile);

	TEMPLATE_register_init();
	TEMPLATE_create_opcodes();

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void TEMPLATE_done(void *self) {
	TEMPLATE_isa_t *isa = self;

	/* emit now all global declarations */
	be_gas_emit_decls(&isa->emit, isa->arch_isa.main_env, 0);

	be_emit_destroy_env(&isa->emit);
	free(self);
}



static int TEMPLATE_get_n_reg_class(const void *self)
{
	(void) self;
	return N_CLASSES;
}

static const arch_register_class_t *TEMPLATE_get_reg_class(const void *self,
                                                           int i)
{
	(void) self;
	assert(i >= 0 && i < N_CLASSES && "Invalid TEMPLATE register class requested.");
	return &TEMPLATE_reg_classes[i];
}



/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *TEMPLATE_get_reg_class_for_mode(const void *self,
		const ir_mode *mode)
{
	(void) self;
	if (mode_is_float(mode))
		return &TEMPLATE_reg_classes[CLASS_TEMPLATE_floating_point];
	else
		return &TEMPLATE_reg_classes[CLASS_TEMPLATE_general_purpose];
}



typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_env_t *arch_env;
	const arch_isa_t *isa;
	ir_graph *irg;
} TEMPLATE_abi_env_t;

static void *TEMPLATE_abi_init(const be_abi_call_t *call, const arch_env_t *arch_env, ir_graph *irg)
{
	TEMPLATE_abi_env_t *env = xmalloc(sizeof(env[0]));
	be_abi_call_flags_t fl = be_abi_call_get_flags(call);
	env->flags    = fl.bits;
	env->irg      = irg;
	env->arch_env = arch_env;
	env->isa      = arch_env->isa;
	return env;
}

/**
 * Get the between type for that call.
 * @param self The callback object.
 * @return The between type of for that call.
 */
static ir_type *TEMPLATE_get_between_type(void *self)
{
	static ir_type *between_type = NULL;
	static ir_entity *old_bp_ent = NULL;
	(void) self;

	if(!between_type) {
		ir_entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);
		ir_type *old_bp_type   = new_type_primitive(new_id_from_str("bp"), mode_P);

		between_type           = new_type_class(new_id_from_str("TEMPLATE_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("old_bp"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
	}

	return between_type;
}

static void TEMPLATE_abi_dont_save_regs(void *self, pset *s)
{
	TEMPLATE_abi_env_t *env = self;
	if (env->flags.try_omit_fp) {
		/* insert the BP register into the ignore set */
		pset_insert_ptr(s, env->isa->bp);
	}
}

/**
 * Build the prolog, return the BASE POINTER register
 */
static const arch_register_t *TEMPLATE_abi_prologue(void *self, ir_node **mem,
                                                    pmap *reg_map)
{
	TEMPLATE_abi_env_t *env = self;
	(void) reg_map;
	(void) mem;

	if(env->flags.try_omit_fp)
		return env->isa->sp;
	return env->isa->bp;
}

/* Build the epilog */
static void TEMPLATE_abi_epilogue(void *self, ir_node *bl, ir_node **mem,
                                  pmap *reg_map)
{
	(void) self;
	(void) bl;
	(void) mem;
	(void) reg_map;
}

static const be_abi_callbacks_t TEMPLATE_abi_callbacks = {
	TEMPLATE_abi_init,
	free,
	TEMPLATE_get_between_type,
	TEMPLATE_abi_dont_save_regs,
	TEMPLATE_abi_prologue,
	TEMPLATE_abi_epilogue,
};

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
void TEMPLATE_get_call_abi(const void *self, ir_type *method_type,
                           be_abi_call_t *abi)
{
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);
	be_abi_call_flags_t call_flags;
	(void) self;

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;
	call_flags.bits.store_args_sequential = 1;
	call_flags.bits.try_omit_fp           = 1;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &TEMPLATE_abi_callbacks);

	for (i = 0; i < n; i++) {
		/* TODO: implement register parameter: */
		/* reg = get reg for param i;          */
		/* be_abi_call_param_reg(abi, i, reg); */

		/* default: all parameters on stack */
		be_abi_call_param_stack(abi, i, 4, 0, 0);
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

static const void *TEMPLATE_get_irn_ops(const arch_irn_handler_t *self,
                                        const ir_node *irn)
{
	(void) self;
	(void) irn;
	return &TEMPLATE_irn_ops;
}

const arch_irn_handler_t TEMPLATE_irn_handler = {
	TEMPLATE_get_irn_ops
};

const arch_irn_handler_t *TEMPLATE_get_irn_handler(const void *self)
{
	(void) self;
	return &TEMPLATE_irn_handler;
}

int TEMPLATE_to_appear_in_schedule(void *block_env, const ir_node *irn)
{
	(void) block_env;

	if(!is_TEMPLATE_irn(irn))
		return -1;

	return 1;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *TEMPLATE_get_code_generator_if(
		void *self)
{
	(void) self;
	return &TEMPLATE_code_gen_if;
}

list_sched_selector_t TEMPLATE_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *TEMPLATE_get_list_sched_selector(
		const void *self, list_sched_selector_t *selector)
{
	(void) self;
	memcpy(&TEMPLATE_sched_selector, trivial_selector, sizeof(list_sched_selector_t));
	TEMPLATE_sched_selector.to_appear_in_schedule = TEMPLATE_to_appear_in_schedule;
	return &TEMPLATE_sched_selector;
}

static const ilp_sched_selector_t *TEMPLATE_get_ilp_sched_selector(const void *self) {
	return NULL;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int TEMPLATE_get_reg_class_alignment(const void *self, const arch_register_class_t *cls) {
	ir_mode *mode = arch_register_class_mode(cls);
	return get_mode_size_bytes(mode);
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *TEMPLATE_get_backend_params(void) {
	static arch_dep_params_t ad = {
		1,  /* allow subs */
		0,  /* Muls are fast enough on Firm */
		31, /* shift would be ok */
		0,  /* no Mulhs */
		0,  /* no Mulhu */
		0,  /* no Mulh */
	};
	static backend_params p = {
		0,     /* no dword lowering */
		0,     /* no inline assembly */
		0,     /* no different calling conventions */
		NULL,  /* no additional opcodes */
		NULL,  /* will be set later */
		NULL,  /* no creator function */
		NULL,  /* context for create_intrinsic_fkt */
		NULL,  /* parameter for if conversion */
	};

	p.dep_param = &ad;
	return &p;
}

static const be_execution_unit_t ***TEMPLATE_get_allowed_execution_units(const void *self, const ir_node *irn) {
	/* TODO */
	assert(0);
	return NULL;
}

static const be_machine_t *TEMPLATE_get_machine(const void *self) {
	/* TODO */
	assert(0);
	return NULL;
}

static ir_graph **TEMPLATE_get_backend_irg_list(const void *self, ir_graph ***irgs) {
	return NULL;
}


const arch_isa_if_t TEMPLATE_isa_if = {
	TEMPLATE_init,
	TEMPLATE_done,
	TEMPLATE_get_n_reg_class,
	TEMPLATE_get_reg_class,
	TEMPLATE_get_reg_class_for_mode,
	TEMPLATE_get_call_abi,
	TEMPLATE_get_irn_handler,
	TEMPLATE_get_code_generator_if,
	TEMPLATE_get_list_sched_selector,
	TEMPLATE_get_ilp_sched_selector,
	TEMPLATE_get_reg_class_alignment,
    TEMPLATE_get_backend_params,
	TEMPLATE_get_allowed_execution_units,
	TEMPLATE_get_machine,
	TEMPLATE_get_backend_irg_list
};

void be_init_arch_TEMPLATE(void)
{
	be_register_isa_if("TEMPLATE", &TEMPLATE_isa_if);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_TEMPLATE);
