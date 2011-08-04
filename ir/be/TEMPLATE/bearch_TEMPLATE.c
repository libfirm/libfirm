/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#include "config.h"

#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"

#include "bitset.h"
#include "debug.h"

#include "be.h"
#include "../bearch.h"
#include "../benode.h"
#include "../belower.h"
#include "../besched.h"
#include "../beabi.h"
#include "../bemodule.h"
#include "../begnuas.h"
#include "../belistsched.h"

#include "bearch_TEMPLATE_t.h"

#include "TEMPLATE_new_nodes.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "TEMPLATE_transform.h"
#include "TEMPLATE_emitter.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static arch_irn_class_t TEMPLATE_classify(const ir_node *irn)
{
	(void) irn;
	return arch_irn_class_none;
}

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
	TEMPLATE_classify,
	TEMPLATE_get_frame_entity,
	TEMPLATE_set_frame_offset,
	TEMPLATE_get_sp_bias,
	NULL,    /* get_inverse             */
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
	TEMPLATE_transform_graph(irg);
}



/**
 * Called immediatly before emit phase.
 */
static void TEMPLATE_finish_irg(ir_graph *irg)
{
	(void) irg;
}


static void TEMPLATE_before_ra(ir_graph *irg)
{
	(void) irg;
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void TEMPLATE_after_ra(ir_graph *irg)
{
	(void) irg;
	/* Some stuff you need to do immediatly after register allocation */
}

static void TEMPLATE_init_graph(ir_graph *irg)
{
	(void) irg;
}



extern const arch_isa_if_t TEMPLATE_isa_if;
static TEMPLATE_isa_t TEMPLATE_isa_template = {
	{
		&TEMPLATE_isa_if,             /* isa interface implementation */
		N_TEMPLATE_REGISTERS,
		TEMPLATE_registers,
		N_TEMPLATE_CLASSES,
		TEMPLATE_reg_classes,
		&TEMPLATE_registers[REG_SP],  /* stack pointer register */
		&TEMPLATE_registers[REG_BP],  /* base pointer register */
		&TEMPLATE_reg_classes[CLASS_TEMPLATE_gp],  /* link pointer register class */
		2,                           /* power of two stack alignment for calls, 2^2 == 4 */
		NULL,                        /* main environment */
		7,                           /* costs for a spill instruction */
		5,                           /* costs for a reload instruction */
		false,                       /* no custom abi handling */
	},
};

/**
 * Initializes the backend ISA
 */
static arch_env_t *TEMPLATE_init(FILE *outfile)
{
	TEMPLATE_isa_t *isa = XMALLOC(TEMPLATE_isa_t);
	*isa = TEMPLATE_isa_template;

	be_emit_init(outfile);

	TEMPLATE_register_init();
	TEMPLATE_create_opcodes(&TEMPLATE_irn_ops);

	return &isa->base;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void TEMPLATE_done(void *self)
{
	TEMPLATE_isa_t *isa = (TEMPLATE_isa_t*)self;

	/* emit now all global declarations */
	be_gas_emit_decls(isa->base.main_env);

	be_emit_exit();
	free(self);
}

/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
static const arch_register_class_t *TEMPLATE_get_reg_class_for_mode(const ir_mode *mode)
{
	if (mode_is_float(mode))
		return &TEMPLATE_reg_classes[CLASS_TEMPLATE_fp];
	else
		return &TEMPLATE_reg_classes[CLASS_TEMPLATE_gp];
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
static void TEMPLATE_get_call_abi(const void *self, ir_type *method_type,
                                  be_abi_call_t *abi)
{
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);
	be_abi_call_flags_t call_flags;
	(void) self;

	/* set abi flags for calls */
	call_flags.bits.store_args_sequential = 1;
	call_flags.bits.try_omit_fp           = 1;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* set stack parameter passing style */
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

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int TEMPLATE_get_reg_class_alignment(const arch_register_class_t *cls)
{
	ir_mode *mode = arch_register_class_mode(cls);
	return get_mode_size_bytes(mode);
}

static void TEMPLATE_lower_for_target(void)
{
	lower_params_t params = {
		4,                                     /* def_ptr_alignment */
		LF_COMPOUND_RETURN | LF_RETURN_HIDDEN, /* flags */
		ADD_HIDDEN_ALWAYS_IN_FRONT,            /* hidden_params */
		NULL,                                  /* find pointer type */
		NULL,                                  /* ret_compound_in_regs */
	};

	/* lower compound param handling */
	lower_calls_with_compounds(&params);
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
		0,     /* no inline assembly */
		0,     /* no support for Rotl nodes */
		0,     /* 0: little-endian, 1: big-endian */
		NULL,  /* architecture dependent settings, will be set later */
		TEMPLATE_is_mux_allowed,  /* parameter for if conversion */
		32,    /* machine size - a 32bit CPU */
		NULL,  /* float arithmetic mode */
		0,     /* size of long double */
		0,     /* no trampoline support: size 0 */
		0,     /* no trampoline support: align 0 */
		NULL,  /* no trampoline support: no trampoline builder */
		4      /* alignment of stack parameter: typically 4 (32bit) or 8 (64bit) */
	};
	return &p;
}

static ir_graph **TEMPLATE_get_backend_irg_list(const void *self,
                                                ir_graph ***irgs)
{
	(void) self;
	(void) irgs;
	return NULL;
}

static asm_constraint_flags_t TEMPLATE_parse_asm_constraint(const char **c)
{
	(void) c;
	return ASM_CONSTRAINT_FLAG_INVALID;
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
	TEMPLATE_lower_for_target,
	TEMPLATE_done,
	NULL,                /* handle intrinsics */
	TEMPLATE_get_reg_class_for_mode,
	TEMPLATE_get_call_abi,
	TEMPLATE_get_reg_class_alignment,
    TEMPLATE_get_backend_params,
	TEMPLATE_get_backend_irg_list,
	NULL,                    /* mark remat */
	TEMPLATE_parse_asm_constraint,
	TEMPLATE_is_valid_clobber,

	TEMPLATE_init_graph,
	NULL,   /* get_pic_base */
	NULL,   /* before_abi */
	TEMPLATE_prepare_graph,
	TEMPLATE_before_ra,
	TEMPLATE_after_ra,
	TEMPLATE_finish_irg,
	TEMPLATE_emit_routine,
	TEMPLATE_register_saved_by,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_TEMPLATE)
void be_init_arch_TEMPLATE(void)
{
	be_register_isa_if("TEMPLATE", &TEMPLATE_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.TEMPLATE.cg");
	TEMPLATE_init_transform();
}
