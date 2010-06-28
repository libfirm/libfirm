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
 * @brief    The main amd64 backend driver file.
 * @version  $Id: bearch_amd64.c 26909 2010-01-05 15:56:54Z matze $
 */
#include "config.h"

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "irdump.h"

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
#include "../beflags.h"

#include "bearch_amd64_t.h"

#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "amd64_transform.h"
#include "amd64_emitter.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static arch_irn_class_t amd64_classify(const ir_node *irn)
{
	(void) irn;
	return 0;
}

static ir_entity *amd64_get_frame_entity(const ir_node *node)
{
	if (is_amd64_FrameAddr(node)) {
		const amd64_SymConst_attr_t *attr = get_irn_generic_attr_const(node);
		return attr->entity;
	}

	(void) node;
	/* TODO: return the ir_entity assigned to the frame */
	return NULL;
}

static void amd64_set_frame_entity(ir_node *node, ir_entity *ent)
{
	(void) node;
	(void) ent;
	/* TODO: set the ir_entity assigned to the frame */
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void amd64_set_frame_offset(ir_node *irn, int offset)
{
	if (is_amd64_FrameAddr(irn)) {
		amd64_SymConst_attr_t *attr = get_irn_generic_attr(irn);
		attr->fp_offset += offset;
	}
}

static int amd64_get_sp_bias(const ir_node *irn)
{
	(void) irn;
	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_t amd64_irn_ops = {
	get_amd64_in_req,
	amd64_classify,
	amd64_get_frame_entity,
	amd64_set_frame_entity,
	amd64_set_frame_offset,
	amd64_get_sp_bias,
	NULL,    /* get_inverse             */
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};



/**
 * Transforms the standard firm graph into
 * a amd64 firm graph
 */
static void amd64_prepare_graph(void *self)
{
	amd64_code_gen_t *cg = self;

	amd64_transform_graph (cg);

	if (cg->dump)
		dump_ir_graph(cg->irg, "transformed");
}



/**
 * Called immediatly before emit phase.
 */
static void amd64_finish_irg(void *self)
{
	amd64_code_gen_t *cg = self;
	ir_graph         *irg = cg->irg;

	dump_ir_graph(irg, "amd64-finished");
}

static void amd64_before_ra(void *self)
{
	amd64_code_gen_t *cg = self;

	be_sched_fix_flags(cg->birg, &amd64_reg_classes[CLASS_amd64_flags], 0);
}

static void amd64_after_ra(void *self)
{
	(void) self;
	/* Some stuff you need to do immediatly after register allocation */
}



/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void amd64_emit_and_done(void *self)
{
	amd64_code_gen_t *cg  = self;
	ir_graph         *irg = cg->irg;

	amd64_gen_routine(cg, irg);

	/* de-allocate code generator */
	free(cg);
}

static void *amd64_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t amd64_code_gen_if = {
	amd64_cg_init,
	NULL,                    /* get_pic_base hook */
	NULL,                    /* before abi introduce hook */
	amd64_prepare_graph,
	NULL,                    /* spill hook */
	amd64_before_ra,      /* before register allocation hook */
	amd64_after_ra,       /* after register allocation hook */
	amd64_finish_irg,
	amd64_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *amd64_cg_init(be_irg_t *birg)
{
	const arch_env_t    *arch_env = be_get_birg_arch_env(birg);
	amd64_isa_t      *isa      = (amd64_isa_t *) arch_env;
	amd64_code_gen_t *cg       = XMALLOC(amd64_code_gen_t);

	cg->impl     = &amd64_code_gen_if;
	cg->irg      = be_get_birg_irg(birg);
	cg->isa      = isa;
	cg->birg     = birg;
	cg->dump     = (birg->main_env->options->dump_flags & DUMP_BE) ? 1 : 0;

	return (arch_code_generator_t *)cg;
}


typedef ir_node *(*create_const_node_func) (dbg_info *dbg, ir_node *block);

/**
 * Used to create per-graph unique pseudo nodes.
 */
static inline ir_node *create_const(amd64_code_gen_t *cg, ir_node **place,
                                    create_const_node_func func,
                                    const arch_register_t* reg)
{
	ir_node *block, *res;

	if (*place != NULL)
		return *place;

	block = get_irg_start_block(cg->irg);
	res = func(NULL, block);
	arch_set_irn_register(res, reg);
	*place = res;

	return res;
}

const arch_isa_if_t amd64_isa_if;
static amd64_isa_t amd64_isa_template = {
	{
		&amd64_isa_if,             /* isa interface implementation */
		&amd64_gp_regs[REG_RSP],  /* stack pointer register */
		&amd64_gp_regs[REG_RBP],  /* base pointer register */
		&amd64_reg_classes[CLASS_amd64_gp],  /* link pointer register class */
		-1,                          /* stack direction */
		2,                           /* power of two stack alignment for calls, 2^2 == 4 */
		NULL,                        /* main environment */
		7,                           /* costs for a spill instruction */
		5,                           /* costs for a reload instruction */
	},
};

/**
 * Initializes the backend ISA
 */
static arch_env_t *amd64_init(FILE *outfile)
{
	static int run_once = 0;
	amd64_isa_t *isa;

	if(run_once)
		return NULL;
	run_once = 1;

	isa = XMALLOC(amd64_isa_t);
	memcpy(isa, &amd64_isa_template, sizeof(*isa));

	be_emit_init(outfile);

	amd64_register_init();
	amd64_create_opcodes(&amd64_irn_ops);

	return &isa->arch_env;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void amd64_done(void *self)
{
	amd64_isa_t *isa = self;

	/* emit now all global declarations */
	be_gas_emit_decls(isa->arch_env.main_env);

	be_emit_exit();
	free(self);
}


static unsigned amd64_get_n_reg_class(void)
{
	return N_CLASSES;
}

static const arch_register_class_t *amd64_get_reg_class(unsigned i)
{
	assert(i < N_CLASSES);
	return &amd64_reg_classes[i];
}



/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
static const arch_register_class_t *amd64_get_reg_class_for_mode(const ir_mode *mode)
{
	assert(!mode_is_float(mode));
	return &amd64_reg_classes[CLASS_amd64_gp];
}



typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_env_t *arch_env;
	ir_graph *irg;
} amd64_abi_env_t;

static void *amd64_abi_init(const be_abi_call_t *call, const arch_env_t *arch_env, ir_graph *irg)
{
	amd64_abi_env_t *env = XMALLOC(amd64_abi_env_t);
	be_abi_call_flags_t fl = be_abi_call_get_flags(call);
	env->flags    = fl.bits;
	env->irg      = irg;
	env->arch_env = arch_env;
	return env;
}

/**
 * Get the between type for that call.
 * @param self The callback object.
 * @return The between type of for that call.
 */
static ir_type *amd64_get_between_type(void *self)
{
	static ir_type *between_type = NULL;
	static ir_entity *old_bp_ent = NULL;
	(void) self;

	if(!between_type) {
		ir_entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(mode_P);
		ir_type *old_bp_type   = new_type_primitive(mode_P);

		between_type           = new_type_class(new_id_from_str("amd64_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("old_bp"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
	}

	return between_type;
}

/**
 * Build the prolog, return the BASE POINTER register
 */
static const arch_register_t *amd64_abi_prologue(void *self, ir_node **mem,
                                                    pmap *reg_map, int *stack_bias)
{
	amd64_abi_env_t  *env  = self;
	const arch_env_t *aenv = env->arch_env;
	(void) mem;
	(void) stack_bias;
	(void) aenv;
	(void) reg_map;

	if (!env->flags.try_omit_fp) {
		/* FIXME: maybe later here should be some code to generate
		 * the usual abi prologue */
		return env->arch_env->bp;
	}

	return env->arch_env->sp;
}

/* Build the epilog */
static void amd64_abi_epilogue(void *self, ir_node *bl, ir_node **mem,
                               pmap *reg_map)
{
	amd64_abi_env_t  *env  = self;
	const arch_env_t *aenv = env->arch_env;
	ir_node          *curr_sp  = be_abi_reg_map_get(reg_map, aenv->sp);
	ir_node          *curr_bp  = be_abi_reg_map_get(reg_map, aenv->bp);
	(void) bl;
	(void) mem;

	if (env->flags.try_omit_fp) {
		curr_sp = be_new_IncSP(aenv->sp, bl, curr_sp, BE_STACK_FRAME_SIZE_SHRINK, 0);
	}

	be_abi_reg_map_set(reg_map, aenv->sp, curr_sp);
	be_abi_reg_map_set(reg_map, aenv->bp, curr_bp);
}

static const be_abi_callbacks_t amd64_abi_callbacks = {
	amd64_abi_init,
	free,
	amd64_get_between_type,
	amd64_abi_prologue,
	amd64_abi_epilogue,
};

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void amd64_get_call_abi(const void *self, ir_type *method_type,
                           be_abi_call_t *abi)
{
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);
	be_abi_call_flags_t call_flags;
	int no_reg = 0;

	(void) self;

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;
	call_flags.bits.store_args_sequential = 1;
	call_flags.bits.try_omit_fp           = 1;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &amd64_abi_callbacks);

	for (i = 0; i < n; i++) {
		tp   = get_method_param_type(method_type, i);
		mode = get_type_mode(tp);
		//d// printf ("MODE %p %p XX %d\n", mode, mode_Iu, i);

		if (!no_reg && (i == 0 || i == 1) && mode == mode_Iu) {
			//d// printf("TEST%d\n", i);
			be_abi_call_param_reg(abi, i,
			                      i == 0 ? &amd64_gp_regs[REG_RDI]
			                             : &amd64_gp_regs[REG_RSI],
			                      ABI_CONTEXT_BOTH);
		/* default: all parameters on stack */
		} else {
			no_reg = 1;
			be_abi_call_param_stack(abi, i, mode, 4, 0, 0, ABI_CONTEXT_BOTH);
		}
	}

	/* TODO: set correct return register */
	/* default: return value is in R0 resp. F0 */
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		/* FIXME: No floating point yet */
		/* be_abi_call_res_reg(abi, 0,
			mode_is_float(mode) ? &amd64_fp_regs[REG_F0] : &amd64_gp_regs[REG_R0], ABI_CONTEXT_BOTH) */;

		be_abi_call_res_reg(abi, 0,
			&amd64_gp_regs[REG_RAX], ABI_CONTEXT_BOTH);
	}
}

static int amd64_to_appear_in_schedule(void *block_env, const ir_node *irn)
{
	(void) block_env;

	if(!is_amd64_irn(irn))
		return -1;

	return 1;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *amd64_get_code_generator_if(
		void *self)
{
	(void) self;
	return &amd64_code_gen_if;
}

list_sched_selector_t amd64_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *amd64_get_list_sched_selector(
		const void *self, list_sched_selector_t *selector)
{
	(void) self;
	(void) selector;

	amd64_sched_selector = trivial_selector;
	amd64_sched_selector.to_appear_in_schedule = amd64_to_appear_in_schedule;
	return &amd64_sched_selector;
}

static const ilp_sched_selector_t *amd64_get_ilp_sched_selector(
		const void *self)
{
	(void) self;
	return NULL;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int amd64_get_reg_class_alignment(const arch_register_class_t *cls)
{
	ir_mode *mode = arch_register_class_mode(cls);
	return get_mode_size_bytes(mode);
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *amd64_get_backend_params(void) {
	static backend_params p = {
		0,     /* no dword lowering */
		0,     /* no inline assembly */
		NULL,  /* will be set later */
		NULL,  /* no creator function */
		NULL,  /* context for create_intrinsic_fkt */
		NULL,  /* parameter for if conversion */
		NULL,  /* float arithmetic mode */
		0,     /* no trampoline support: size 0 */
		0,     /* no trampoline support: align 0 */
		NULL,  /* no trampoline support: no trampoline builder */
		4      /* alignment of stack parameter: typically 4 (32bit) or 8 (64bit) */
	};
	return &p;
}

static const be_execution_unit_t ***amd64_get_allowed_execution_units(
		const ir_node *irn)
{
	(void) irn;
	/* TODO */
	assert(0);
	return NULL;
}

static const be_machine_t *amd64_get_machine(const void *self)
{
	(void) self;
	/* TODO */
	assert(0);
	return NULL;
}

static ir_graph **amd64_get_backend_irg_list(const void *self,
                                                ir_graph ***irgs)
{
	(void) self;
	(void) irgs;
	return NULL;
}

static asm_constraint_flags_t amd64_parse_asm_constraint(const char **c)
{
	(void) c;
	return ASM_CONSTRAINT_FLAG_INVALID;
}

static int amd64_is_valid_clobber(const char *clobber)
{
	(void) clobber;
	return 0;
}

const arch_isa_if_t amd64_isa_if = {
	amd64_init,
	amd64_done,
	NULL,                /* handle intrinsics */
	amd64_get_n_reg_class,
	amd64_get_reg_class,
	amd64_get_reg_class_for_mode,
	amd64_get_call_abi,
	amd64_get_code_generator_if,
	amd64_get_list_sched_selector,
	amd64_get_ilp_sched_selector,
	amd64_get_reg_class_alignment,
    amd64_get_backend_params,
	amd64_get_allowed_execution_units,
	amd64_get_machine,
	amd64_get_backend_irg_list,
	NULL,                    /* mark remat */
	amd64_parse_asm_constraint,
	amd64_is_valid_clobber
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_amd64);
void be_init_arch_amd64(void)
{
	be_register_isa_if("amd64", &amd64_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.cg");
	amd64_init_transform();
}
