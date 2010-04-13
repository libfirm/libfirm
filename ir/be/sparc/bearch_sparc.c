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
 * @brief    The main sparc backend driver file.
 * @version  $Id$
 */

#include "config.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "iroptimize.h"
#include "lowering.h"
#include "error.h"

#include "bitset.h"
#include "debug.h"
#include "array_t.h"
#include "irtools.h"

#include "../bearch.h"
#include "../benode.h"
#include "../belower.h"
#include "../besched.h"
#include "be.h"
#include "../beabi.h"
#include "../bemachine.h"
#include "../beilpsched.h"
#include "../bemodule.h"
#include "../beirg.h"
#include "../bespillslots.h"
#include "../begnuas.h"
#include "../belistsched.h"
#include "../beflags.h"

#include "bearch_sparc_t.h"
#include "bearch_sparc.h"

#include "sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"
#include "sparc_transform.h"
#include "sparc_emitter.h"
#include "sparc_map_regs.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static arch_irn_class_t sparc_classify(const ir_node *irn)
{
	(void) irn;
	return 0;
}

static ir_entity *sparc_get_frame_entity(const ir_node *irn)
{
	const sparc_attr_t *attr = get_sparc_attr_const(irn);

	if (is_sparc_FrameAddr(irn)) {
		const sparc_symconst_attr_t *attr = get_irn_generic_attr_const(irn);
		return attr->entity;
	}

	if (attr->is_load_store) {
		const sparc_load_store_attr_t *load_store_attr = get_sparc_load_store_attr_const(irn);
		if (load_store_attr->is_frame_entity) {
			return load_store_attr->entity;
		}
	}

	return NULL;
}

static void sparc_set_frame_entity(ir_node *node, ir_entity *ent)
{
	(void) node;
	(void) ent;
	panic("sparc_set_frame_entity() called. This should not happen.");
	/* TODO: set the ir_entity assigned to the frame */
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void sparc_set_frame_offset(ir_node *irn, int offset)
{
	if (is_sparc_FrameAddr(irn)) {
		sparc_symconst_attr_t *attr = get_irn_generic_attr(irn);
		attr->fp_offset += offset;
	} else {
		sparc_load_store_attr_t *attr = get_sparc_load_store_attr(irn);
		assert(attr->base.is_load_store);
		attr->offset += offset;
	}
}

static int sparc_get_sp_bias(const ir_node *irn)
{
	(void) irn;
	return SPARC_MIN_STACKSIZE;
}

/* fill register allocator interface */

static const arch_irn_ops_t sparc_irn_ops = {
	get_sparc_in_req,
	sparc_classify,
	sparc_get_frame_entity,
	sparc_set_frame_entity,
	sparc_set_frame_offset,
	sparc_get_sp_bias,
	NULL,    /* get_inverse             */
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};



/**
 * Transforms the standard firm graph into
 * a SPARC firm graph
 */
static void sparc_prepare_graph(void *self)
{
	sparc_code_gen_t *cg = self;

	/* transform FIRM into SPARC asm nodes */
	sparc_transform_graph(cg);

	if (cg->dump)
		be_dump(cg->irg, "-transformed", dump_ir_block_graph_sched);
}



static ir_node *sparc_flags_remat(ir_node *node, ir_node *after)
{
	ir_node *block;
	ir_node *copy;

	if (is_Block(after)) {
		block = after;
	} else {
		block = get_nodes_block(after);
	}
	copy = exact_copy(node);
	set_nodes_block(copy, block);
	sched_add_after(after, copy);
	return copy;
}

static void sparc_before_ra(void *self)
{
	sparc_code_gen_t *cg = self;
	/* fixup flags register */
	be_sched_fix_flags(cg->birg, &sparc_reg_classes[CLASS_sparc_flags], &sparc_flags_remat);
}

/**
 * transform reload node => load
 */
static void transform_Reload(ir_node *node)
{
	ir_graph  *irg    = get_irn_irg(node);
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irg_frame(irg);
	ir_node   *mem    = get_irn_n(node, be_pos_Reload_mem);
	ir_mode   *mode   = get_irn_mode(node);
	ir_entity *entity = be_get_frame_entity(node);
	const arch_register_t *reg;
	ir_node   *proj;
	ir_node   *load;

	ir_node  *sched_point = sched_prev(node);

	load = new_bd_sparc_Load(dbgi, block, ptr, mem, mode, entity, false, 0, true);
	sched_add_after(sched_point, load);
	sched_remove(node);

	proj = new_rd_Proj(dbgi, load, mode, pn_sparc_Load_res);

	reg = arch_get_irn_register(node);
	arch_set_irn_register(proj, reg);

	exchange(node, proj);
}

/**
 * transform spill node => store
 */
static void transform_Spill(ir_node *node)
{
	ir_graph  *irg    = get_irn_irg(node);
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irg_frame(irg);
	ir_node   *mem    = new_NoMem();
	ir_node   *val    = get_irn_n(node, be_pos_Spill_val);
	ir_mode   *mode   = get_irn_mode(val);
	ir_entity *entity = be_get_frame_entity(node);
	ir_node   *sched_point;
	ir_node   *store;

	sched_point = sched_prev(node);
	store = new_bd_sparc_Store(dbgi, block, ptr, val, mem, mode, entity, false, 0, true);
	sched_remove(node);
	sched_add_after(sched_point, store);

	exchange(node, store);
}

/**
 * walker to transform be_Spill and be_Reload nodes
 */
static void sparc_after_ra_walker(ir_node *block, void *data)
{
	ir_node *node, *prev;
	(void) data;

	for (node = sched_last(block); !sched_is_begin(node); node = prev) {
		prev = sched_prev(node);

		if (be_is_Reload(node)) {
			transform_Reload(node);
		} else if (be_is_Spill(node)) {
			transform_Spill(node);
		}
	}
}


static void sparc_after_ra(void *self)
{
	sparc_code_gen_t *cg = self;
	be_coalesce_spillslots(cg->birg);

	irg_block_walk_graph(cg->irg, NULL, sparc_after_ra_walker, NULL);
}



/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void sparc_emit_and_done(void *self)
{
	sparc_code_gen_t *cg = self;
	ir_graph           *irg = cg->irg;

	sparc_gen_routine(cg, irg);

	/* de-allocate code generator */
	free(cg);
}

static void *sparc_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t sparc_code_gen_if = {
	sparc_cg_init,
	NULL,                    /* get_pic_base hook */
	NULL,                    /* before abi introduce hook */
	sparc_prepare_graph,
	NULL,                    /* spill hook */
	sparc_before_ra,      /* before register allocation hook */
	sparc_after_ra,       /* after register allocation hook */
	NULL,
	sparc_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *sparc_cg_init(be_irg_t *birg)
{
	static ir_type *int_tp = NULL;
	sparc_isa_t      *isa = (sparc_isa_t *)birg->main_env->arch_env;
	sparc_code_gen_t *cg;

	if (! int_tp) {
		/* create an integer type with machine size */
		int_tp = new_type_primitive(mode_Is);
	}

	cg 				 = XMALLOC(sparc_code_gen_t);
	cg->impl				= &sparc_code_gen_if;
	cg->irg				= birg->irg;
	//cg->reg_set				= new_set(arm_cmp_irn_reg_assoc, 1024);
	cg->isa				= isa;
	cg->birg				= birg;
	//cg->int_tp				= int_tp;
	//cg->have_fp_insn	= 0;
	//cg->unknown_gp		= NULL;
	//cg->unknown_fpa		= NULL;
	cg->dump				= (birg->main_env->options->dump_flags & DUMP_BE) ? 1 : 0;

	/* enter the current code generator */
	isa->cg = cg;

	return (arch_code_generator_t *)cg;
}



const arch_isa_if_t sparc_isa_if;
static sparc_isa_t sparc_isa_template = {
	{
		&sparc_isa_if,             /* isa interface implementation */
		&sparc_gp_regs[REG_SP],  /* stack pointer register */
		&sparc_gp_regs[REG_FP],  /* base pointer register */
		&sparc_reg_classes[CLASS_sparc_gp],  /* link pointer register class */
		-1,                          /* stack direction */
		1,                           /* power of two stack alignment for calls, 2^2 == 4 */
		NULL,                        /* main environment */
		7,                           /* costs for a spill instruction */
		5,                           /* costs for a reload instruction */
	},
	NULL						/* current code generator */
};

/**
 * Initializes the backend ISA
 */
static arch_env_t *sparc_init(FILE *outfile)
{
	static int run_once = 0;
	sparc_isa_t *isa;

	if (run_once)
		return NULL;
	run_once = 1;

	isa = XMALLOC(sparc_isa_t);
	memcpy(isa, &sparc_isa_template, sizeof(*isa));

	be_emit_init(outfile);

	sparc_register_init();
	sparc_create_opcodes(&sparc_irn_ops);

	return &isa->arch_env;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void sparc_done(void *self)
{
	sparc_isa_t *isa = self;

	/* emit now all global declarations */
	be_gas_emit_decls(isa->arch_env.main_env);

	be_emit_exit();
	free(self);
}


static unsigned sparc_get_n_reg_class(void)
{
	return N_CLASSES;
}

static const arch_register_class_t *sparc_get_reg_class(unsigned i)
{
	assert(i < N_CLASSES);
	return &sparc_reg_classes[i];
}



/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
static const arch_register_class_t *sparc_get_reg_class_for_mode(const ir_mode *mode)
{
	if (mode_is_float(mode))
		return &sparc_reg_classes[CLASS_sparc_fp];
	else
		return &sparc_reg_classes[CLASS_sparc_gp];
}



typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_env_t *arch_env;
	ir_graph *irg;
} sparc_abi_env_t;

static void *sparc_abi_init(const be_abi_call_t *call, const arch_env_t *arch_env, ir_graph *irg)
{
	sparc_abi_env_t *env = XMALLOC(sparc_abi_env_t);
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
static ir_type *sparc_get_between_type(void *self)
{
	static ir_type *between_type = NULL;
	(void) self;

	if (between_type == NULL) {
		between_type = new_type_class(new_id_from_str("sparc_between_type"));
		set_type_size_bytes(between_type, 0);
	}

	return between_type;
}


/**
 * Build the prolog, return the BASE POINTER register
 */
static const arch_register_t *sparc_abi_prologue(void *self, ir_node **mem,
                                                    pmap *reg_map, int *stack_bias)
{
	sparc_abi_env_t *env = self;
	ir_node *block = get_irg_start_block(env->irg);
	const arch_register_t *fp = &sparc_gp_regs[REG_FP];
	const arch_register_t *sp = &sparc_gp_regs[REG_SP];

	// sp
	ir_node *sp_proj = be_abi_reg_map_get(reg_map, sp);


	//ir_type *frame_type = get_irg_frame_type(env->irg);
	//frame_alloc_area(frame_type, reserved_stack_size, 1, 1);

	(void) reg_map;
	(void) mem;
	(void) stack_bias;

	// alloc min required stack space
	// TODO: the min stacksize depends on wether this is a leaf procedure or not
	ir_node *save = new_bd_sparc_Save(NULL, block, sp_proj, *mem, SPARC_MIN_STACKSIZE);

	*stack_bias -= SPARC_MIN_STACKSIZE;
	sp_proj = new_r_Proj(block, save, sp->reg_class->mode, pn_sparc_Save_stack);
	*mem    = new_r_Proj(block, save, mode_M, pn_sparc_Save_mem);

	arch_set_irn_register(sp_proj, sp);
	be_abi_reg_map_set(reg_map, sp, sp_proj);

	// we always have a framepointer
	return fp;
}

/* Build the epilog */
static void sparc_abi_epilogue(void *self, ir_node *bl, ir_node **mem,
                                  pmap *reg_map)
{
	(void) self;
	(void) bl;
	(void) mem;
	(void) reg_map;
}

static const be_abi_callbacks_t sparc_abi_callbacks = {
	sparc_abi_init,
	free,
	sparc_get_between_type,
	sparc_abi_prologue,
	sparc_abi_epilogue,
};

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void sparc_get_call_abi(const void *self, ir_type *method_type,
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
	/* */
	call_flags.bits.try_omit_fp           = 0;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &sparc_abi_callbacks);

	for (i = 0; i < n; i++) {
		/* reg = get reg for param i;          */
		/* be_abi_call_param_reg(abi, i, reg); */

		/* pass outgoing params 0-5 via registers, remaining via stack */
		/* on sparc we need to set the ABI context since register names of parameters change to i0-i5 if we are the callee */
		if (i < 6) {
			be_abi_call_param_reg(abi, i, sparc_get_RegParamOut_reg(i), ABI_CONTEXT_CALLER);
			be_abi_call_param_reg(abi, i, sparc_get_RegParamIn_reg(i), ABI_CONTEXT_CALLEE);
		} else {
			tp   = get_method_param_type(method_type, i);
			mode = get_type_mode(tp);
			be_abi_call_param_stack(abi, i, mode, 4, 0, 0, ABI_CONTEXT_BOTH); /*< stack args have no special context >*/
		}
	}

	/* set return value register: return value is in i0 resp. f0 */
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		be_abi_call_res_reg(abi, 0,
			mode_is_float(mode) ? &sparc_fp_regs[REG_F0] : &sparc_gp_regs[REG_I0], ABI_CONTEXT_CALLEE); /*< return has no special context >*/

		be_abi_call_res_reg(abi, 0,
					mode_is_float(mode) ? &sparc_fp_regs[REG_F0] : &sparc_gp_regs[REG_O0], ABI_CONTEXT_CALLER); /*< return has no special context >*/
	}
}

static int sparc_to_appear_in_schedule(void *block_env, const ir_node *irn)
{
	(void) block_env;

	if (!is_sparc_irn(irn))
		return -1;

	return 1;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *sparc_get_code_generator_if(
		void *self)
{
	(void) self;
	return &sparc_code_gen_if;
}

list_sched_selector_t sparc_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *sparc_get_list_sched_selector(
		const void *self, list_sched_selector_t *selector)
{
	(void) self;
	(void) selector;

	sparc_sched_selector = trivial_selector;
	sparc_sched_selector.to_appear_in_schedule = sparc_to_appear_in_schedule;
	return &sparc_sched_selector;
}

static const ilp_sched_selector_t *sparc_get_ilp_sched_selector(
		const void *self)
{
	(void) self;
	return NULL;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int sparc_get_reg_class_alignment(const arch_register_class_t *cls)
{
	ir_mode *mode = arch_register_class_mode(cls);
	return get_mode_size_bytes(mode);
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *sparc_get_backend_params(void)
{
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

static const be_execution_unit_t ***sparc_get_allowed_execution_units(
		const ir_node *irn)
{
	(void) irn;
	/* TODO */
	panic("sparc_get_allowed_execution_units not implemented yet");
}

static const be_machine_t *sparc_get_machine(const void *self)
{
	(void) self;
	/* TODO */
	panic("sparc_get_machine not implemented yet");
}

static ir_graph **sparc_get_backend_irg_list(const void *self,
                                                ir_graph ***irgs)
{
	(void) self;
	(void) irgs;
	return NULL;
}

static asm_constraint_flags_t sparc_parse_asm_constraint(const char **c)
{
	(void) c;
	return ASM_CONSTRAINT_FLAG_INVALID;
}

static int sparc_is_valid_clobber(const char *clobber)
{
	(void) clobber;
	return 0;
}

const arch_isa_if_t sparc_isa_if = {
	sparc_init,
	sparc_done,
	NULL,                /* handle intrinsics */
	sparc_get_n_reg_class,
	sparc_get_reg_class,
	sparc_get_reg_class_for_mode,
	sparc_get_call_abi,
	sparc_get_code_generator_if,
	sparc_get_list_sched_selector,
	sparc_get_ilp_sched_selector,
	sparc_get_reg_class_alignment,
    sparc_get_backend_params,
	sparc_get_allowed_execution_units,
	sparc_get_machine,
	sparc_get_backend_irg_list,
	NULL,                    /* mark remat */
	sparc_parse_asm_constraint,
	sparc_is_valid_clobber
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_sparc);
void be_init_arch_sparc(void)
{
	be_register_isa_if("sparc", &sparc_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.cg");
	sparc_init_transform();
	sparc_init_emitter();
}
