/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main TEMPLATE backend driver file.
 */
#include "bearch_TEMPLATE_t.h"

#include "irgwalk.h"
#include "irprog.h"
#include "ircons.h"
#include "irdump.h"
#include "irgmod.h"
#include "lower_calls.h"
#include "lower_builtins.h"
#include "debug.h"
#include "be_t.h"
#include "bearch.h"
#include "benode.h"
#include "belower.h"
#include "besched.h"
#include "beabi.h"
#include "bemodule.h"
#include "begnuas.h"
#include "belistsched.h"
#include "bestack.h"
#include "bespillutil.h"

#include "TEMPLATE_new_nodes.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "TEMPLATE_transform.h"
#include "TEMPLATE_emitter.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_entity *TEMPLATE_get_frame_entity(const ir_node *node)
{
	(void) node;
	/* TODO: return the ir_entity assigned to the frame */
	return NULL;
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void TEMPLATE_set_frame_offset(ir_node *irn, int offset)
{
	(void) irn;
	(void) offset;
	/* TODO: correct offset if irn accesses the stack */
}

static int TEMPLATE_get_sp_bias(const ir_node *irn)
{
	(void) irn;
	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_t TEMPLATE_irn_ops = {
	TEMPLATE_get_frame_entity,
	TEMPLATE_set_frame_offset,
	TEMPLATE_get_sp_bias,
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};



/**
 * Transforms the standard firm graph into
 * a TEMLPATE firm graph
 */
static void TEMPLATE_prepare_graph(ir_graph *irg)
{
	/* transform nodes into assembler instructions */
	be_timer_push(T_CODEGEN);
	TEMPLATE_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");
}



/**
 * Last touchups and emitting of the generated code of a function.
 */
static void TEMPLATE_emit(ir_graph *irg)
{
	/* fix stack entity offsets */
	be_abi_fix_stack_nodes(irg);
	be_abi_fix_stack_bias(irg);
	/* emit code */
	TEMPLATE_emit_function(irg);
}


static void TEMPLATE_before_ra(ir_graph *irg)
{
	(void) irg;
	/* Some stuff you need to do after scheduling but before register allocation */
}


extern const arch_isa_if_t TEMPLATE_isa_if;
static TEMPLATE_isa_t TEMPLATE_isa_template = {
	{
		&TEMPLATE_isa_if,            /* isa interface implementation */
		N_TEMPLATE_REGISTERS,
		TEMPLATE_registers,
		N_TEMPLATE_CLASSES,
		TEMPLATE_reg_classes,
		&TEMPLATE_registers[REG_SP], /* stack pointer register */
		&TEMPLATE_registers[REG_BP], /* base pointer register */
		2,                           /* power of two stack alignment for calls, 2^2 == 4 */
		7,                           /* costs for a spill instruction */
		5,                           /* costs for a reload instruction */
	},
};

static void TEMPLATE_init(void)
{
	TEMPLATE_register_init();
	TEMPLATE_create_opcodes(&TEMPLATE_irn_ops);
}

static void TEMPLATE_finish(void)
{
	TEMPLATE_free_opcodes();
}

static arch_env_t *TEMPLATE_begin_codegeneration(void)
{
	TEMPLATE_isa_t *isa = XMALLOC(TEMPLATE_isa_t);
	*isa = TEMPLATE_isa_template;

	return &isa->base;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void TEMPLATE_end_codegeneration(void *self)
{
	free(self);
}

/**
 * Get the between type for that call.
 * @param self The callback object.
 * @return The between type of for that call.
 */
static ir_type *TEMPLATE_get_between_type(ir_graph *irg)
{
	static ir_type *between_type = NULL;
	static ir_entity *old_bp_ent = NULL;
	(void) irg;

	if (!between_type) {
		ir_entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(mode_P);
		ir_type *old_bp_type   = new_type_primitive(mode_P);

		between_type           = new_type_class(new_id_from_str("TEMPLATE_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("old_bp"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
	}

	return between_type;
}

static const be_abi_callbacks_t TEMPLATE_abi_callbacks = {
	TEMPLATE_get_between_type,
};

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void TEMPLATE_get_call_abi(ir_type *method_type, be_abi_call_t *abi)
{
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);

	/* set abi flags for calls */
	be_abi_call_flags_t call_flags = be_abi_call_get_flags(abi);
	call_flags.call_has_imm = true;
	be_abi_call_set_flags(abi, call_flags, &TEMPLATE_abi_callbacks);

	for (i = 0; i < n; i++) {
		/* TODO: implement register parameter: */
		/* reg = get reg for param i;          */
		/* be_abi_call_param_reg(abi, i, reg, ABI_CONTEXT_BOTH); */

		/* default: all parameters on stack */
		tp   = get_method_param_type(method_type, i);
		mode = get_type_mode(tp);
		be_abi_call_param_stack(abi, i, mode, 4, 0, 0, ABI_CONTEXT_BOTH);
	}

	/* TODO: set correct return register */
	/* default: return value is in R0 resp. F0 */
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		be_abi_call_res_reg(abi, 0,
			mode_is_float(mode) ? &TEMPLATE_registers[REG_F0] : &TEMPLATE_registers[REG_R0], ABI_CONTEXT_BOTH);
	}
}

static void TEMPLATE_lower_for_target(void)
{
	lower_builtins(0, NULL);
	be_after_irp_transform("lower-builtins");

	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN);
	be_after_irp_transform("lower-calls");
}

static int TEMPLATE_is_mux_allowed(ir_node *sel, ir_node *mux_false,
                                   ir_node *mux_true)
{
	(void) sel;
	(void) mux_false;
	(void) mux_true;
	return false;
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *TEMPLATE_get_backend_params(void)
{
	static backend_params p = {
		0,     /* 0: little-endian, 1: big-endian */
		1,     /* modulo shift efficient */
		0,     /* non-modulo shift efficient */
		0,     /* PIC code supported */
		NULL,  /* architecture dependent settings, will be set later */
		TEMPLATE_is_mux_allowed,  /* parameter for if conversion */
		32,    /* machine size - a 32bit CPU */
		NULL,  /* float arithmetic mode */
		NULL,  /* long long type */
		NULL,  /* unsigned long long type */
		NULL,  /* long double type */
		0,     /* no trampoline support: size 0 */
		0,     /* no trampoline support: align 0 */
		NULL,  /* no trampoline support: no trampoline builder */
		4      /* alignment of stack parameter: typically 4 (32bit) or 8 (64bit) */
	};
	return &p;
}

static int TEMPLATE_is_valid_clobber(const char *clobber)
{
	(void) clobber;
	return 0;
}

/**
 * Check if the given register is callee or caller save.
 */
static int TEMPLATE_register_saved_by(const arch_register_t *reg, int callee)
{
	if (callee) {
		/* check for callee saved */
		if (reg->reg_class == &TEMPLATE_reg_classes[CLASS_TEMPLATE_gp]) {
			switch (reg->index) {
			case REG_GP_R7:
			case REG_GP_R8:
			case REG_GP_R9:
			case REG_GP_R10:
			case REG_GP_R11:
			case REG_GP_R12:
			case REG_GP_R13:
				return 1;
			default:
				return 0;
			}
		}
	} else {
		/* check for caller saved */
		if (reg->reg_class == &TEMPLATE_reg_classes[CLASS_TEMPLATE_gp]) {
			switch (reg->index) {
			case REG_GP_R0:
			case REG_GP_R1:
			case REG_GP_R2:
			case REG_GP_R3:
			case REG_GP_R4:
			case REG_GP_R5:
			case REG_GP_R6:
				return 1;
			default:
				return 0;
			}
		} else if (reg->reg_class == &TEMPLATE_reg_classes[CLASS_TEMPLATE_fp]) {
			/* all FP registers are caller save */
			return 1;
		}
	}
	return 0;
}

const arch_isa_if_t TEMPLATE_isa_if = {
	TEMPLATE_init,
	TEMPLATE_finish,
    TEMPLATE_get_backend_params,
	TEMPLATE_lower_for_target,
	TEMPLATE_is_valid_clobber,

	TEMPLATE_begin_codegeneration,
	TEMPLATE_end_codegeneration,
	TEMPLATE_get_call_abi,
	NULL, /* mark remat */
	be_new_spill,
	be_new_reload,
	TEMPLATE_register_saved_by,

	NULL, /* handle intrinsics */
	TEMPLATE_prepare_graph,
	TEMPLATE_before_ra,
	TEMPLATE_emit,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_TEMPLATE)
void be_init_arch_TEMPLATE(void)
{
	be_register_isa_if("TEMPLATE", &TEMPLATE_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.TEMPLATE.cg");
	TEMPLATE_init_transform();
}
