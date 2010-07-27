/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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

#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "iroptimize.h"
#include "irtools.h"
#include "irdump.h"
#include "lowering.h"

#include "bitset.h"
#include "debug.h"
#include "array_t.h"
#include "error.h"

#include "../bearch.h"
#include "../benode.h"
#include "../belower.h"
#include "../besched.h"
#include "be.h"
#include "../bemachine.h"
#include "../beilpsched.h"
#include "../bemodule.h"
#include "../beirg.h"
#include "../bespillslots.h"
#include "../begnuas.h"
#include "../belistsched.h"
#include "../beflags.h"

#include "bearch_sparc_t.h"

#include "sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"
#include "sparc_transform.h"
#include "sparc_emitter.h"

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

static int sparc_get_sp_bias(const ir_node *node)
{
	if (is_sparc_Save(node)) {
		const sparc_save_attr_t *attr = get_sparc_save_attr_const(node);
		/* Note we do not retport the change of the SPARC_MIN_STACKSIZE
		 * size, since we have additional magic in the emitter which
		 * calculates that! */
		assert(attr->initial_stacksize >= SPARC_MIN_STACKSIZE);
		return attr->initial_stacksize - SPARC_MIN_STACKSIZE;
	}
	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_t sparc_irn_ops = {
	get_sparc_in_req,
	sparc_classify,
	sparc_get_frame_entity,
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
		dump_ir_graph(cg->irg, "transformed");
}

static void sparc_before_ra(void *self)
{
	sparc_code_gen_t *cg = self;
	/* fixup flags register */
	be_sched_fix_flags(cg->irg, &sparc_reg_classes[CLASS_sparc_flags_class],
	                   NULL, NULL);
}

/**
 * transform reload node => load
 */
static void transform_Reload(ir_node *node)
{
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irn_n(node, be_pos_Spill_frame);
	ir_node   *mem    = get_irn_n(node, be_pos_Reload_mem);
	ir_mode   *mode   = get_irn_mode(node);
	ir_entity *entity = be_get_frame_entity(node);
	const arch_register_t *reg;
	ir_node   *proj;
	ir_node   *load;

	ir_node  *sched_point = sched_prev(node);

	load = new_bd_sparc_Ld(dbgi, block, ptr, mem, mode, entity, false, 0, true);
	sched_add_after(sched_point, load);
	sched_remove(node);

	proj = new_rd_Proj(dbgi, load, mode, pn_sparc_Ld_res);

	reg = arch_get_irn_register(node);
	arch_set_irn_register(proj, reg);

	exchange(node, proj);
}

/**
 * transform spill node => store
 */
static void transform_Spill(ir_node *node)
{
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irn_n(node, be_pos_Spill_frame);
	ir_node   *mem    = new_NoMem();
	ir_node   *val    = get_irn_n(node, be_pos_Spill_val);
	ir_mode   *mode   = get_irn_mode(val);
	ir_entity *entity = be_get_frame_entity(node);
	ir_node   *sched_point;
	ir_node   *store;

	sched_point = sched_prev(node);
	store = new_bd_sparc_St(dbgi, block, ptr, val, mem, mode, entity, false, 0, true);
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

static void sparc_collect_frame_entity_nodes(ir_node *node, void *data)
{
	be_fec_env_t  *env = data;
	const ir_mode *mode;
	int            align;
	ir_entity     *entity;
	const sparc_load_store_attr_t *attr;

	if (be_is_Reload(node) && be_get_frame_entity(node) == NULL) {
		mode  = get_irn_mode(node);
		align = get_mode_size_bytes(mode);
		be_node_needs_frame_entity(env, node, mode, align);
		return;
	}

	if (!is_sparc_Ld(node) && !is_sparc_Ldf(node))
		return;

	attr   = get_sparc_load_store_attr_const(node);
	entity = attr->entity;
	mode   = attr->load_store_mode;
	align  = get_mode_size_bytes(mode);
	if (entity != NULL)
		return;
	if (!attr->is_frame_entity)
		return;
	be_node_needs_frame_entity(env, node, mode, align);
}

static void sparc_set_frame_entity(ir_node *node, ir_entity *entity)
{
	if (is_be_node(node)) {
		be_node_set_frame_entity(node, entity);
	} else {
		/* we only say be_node_needs_frame_entity on nodes with load_store
		 * attributes, so this should be fine */
		sparc_load_store_attr_t *attr = get_sparc_load_store_attr(node);
		attr->entity = entity;
	}
}


static void sparc_after_ra(void *self)
{
	sparc_code_gen_t *cg      = self;
	ir_graph         *irg     = cg->irg;
	be_fec_env_t     *fec_env = be_new_frame_entity_coalescer(irg);

	irg_walk_graph(irg, NULL, sparc_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, sparc_set_frame_entity);
	be_free_frame_entity_coalescer(fec_env);

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

static void *sparc_cg_init(ir_graph *irg);

static const arch_code_generator_if_t sparc_code_gen_if = {
	sparc_cg_init,
	NULL,                 /* get_pic_base hook */
	NULL,                 /* before abi introduce hook */
	sparc_prepare_graph,
	NULL,                 /* spill hook */
	sparc_before_ra,      /* before register allocation hook */
	sparc_after_ra,       /* after register allocation hook */
	NULL,
	sparc_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *sparc_cg_init(ir_graph *irg)
{
	sparc_isa_t      *isa = (sparc_isa_t *) be_get_irg_arch_env(irg);
	sparc_code_gen_t *cg  = XMALLOCZ(sparc_code_gen_t);

	cg->impl      = &sparc_code_gen_if;
	cg->irg       = irg;
	cg->isa       = isa;
	cg->dump      = (be_get_irg_options(irg)->dump_flags & DUMP_BE) != 0;
	cg->constants = pmap_create();

	/* enter the current code generator */
	isa->cg = cg;

	return (arch_code_generator_t*) cg;
}

const arch_isa_if_t sparc_isa_if;
static sparc_isa_t sparc_isa_template = {
	{
		&sparc_isa_if,                      /* isa interface implementation */
		&sparc_gp_regs[REG_SP],             /* stack pointer register */
		&sparc_gp_regs[REG_FRAME_POINTER],  /* base pointer register */
		&sparc_reg_classes[CLASS_sparc_gp], /* link pointer register class */
		-1,                                 /* stack direction */
		3,                                  /* power of two stack alignment
		                                       for calls */
		NULL,                               /* main environment */
		7,                                  /* costs for a spill instruction */
		5,                                  /* costs for a reload instruction */
		true,                               /* custom abi handling */
	},
	NULL						/* current code generator */
};


static void sparc_handle_intrinsics(void)
{
	ir_type *tp, *int_tp, *uint_tp;
	i_record records[8];
	int n_records = 0;

	runtime_rt rt_iMod, rt_uMod;

#define ID(x) new_id_from_chars(x, sizeof(x)-1)

	int_tp  = new_type_primitive(mode_Is);
	uint_tp = new_type_primitive(mode_Iu);


	/* SPARC has no signed mod instruction ... */
	{
		i_instr_record *map_Mod = &records[n_records++].i_instr;

		tp = new_type_method(2, 1);
		set_method_param_type(tp, 0, int_tp);
		set_method_param_type(tp, 1, int_tp);
		set_method_res_type(tp, 0, int_tp);

		rt_iMod.ent             = new_entity(get_glob_type(), ID(".rem"), tp);
		set_entity_ld_ident(rt_iMod.ent, ID(".rem"));
		rt_iMod.mode            = mode_T;
		rt_iMod.res_mode        = mode_Is;
		rt_iMod.mem_proj_nr     = pn_Mod_M;
		rt_iMod.regular_proj_nr = pn_Mod_X_regular;
		rt_iMod.exc_proj_nr     = pn_Mod_X_except;
		rt_iMod.exc_mem_proj_nr = pn_Mod_M;
		rt_iMod.res_proj_nr     = pn_Mod_res;

		set_entity_visibility(rt_iMod.ent, ir_visibility_external);

		map_Mod->kind     = INTRINSIC_INSTR;
		map_Mod->op       = op_Mod;
		map_Mod->i_mapper = (i_mapper_func)i_mapper_RuntimeCall;
		map_Mod->ctx      = &rt_iMod;
	}
	/* ... nor an unsigned mod. */
	{
		i_instr_record *map_Mod = &records[n_records++].i_instr;

		tp = new_type_method(2, 1);
		set_method_param_type(tp, 0, uint_tp);
		set_method_param_type(tp, 1, uint_tp);
		set_method_res_type(tp, 0, uint_tp);

		rt_uMod.ent             = new_entity(get_glob_type(), ID(".urem"), tp);
		set_entity_ld_ident(rt_uMod.ent, ID(".urem"));
		rt_uMod.mode            = mode_T;
		rt_uMod.res_mode        = mode_Iu;
		rt_uMod.mem_proj_nr     = pn_Mod_M;
		rt_uMod.regular_proj_nr = pn_Mod_X_regular;
		rt_uMod.exc_proj_nr     = pn_Mod_X_except;
		rt_uMod.exc_mem_proj_nr = pn_Mod_M;
		rt_uMod.res_proj_nr     = pn_Mod_res;

		set_entity_visibility(rt_uMod.ent, ir_visibility_external);

		map_Mod->kind     = INTRINSIC_INSTR;
		map_Mod->op       = op_Mod;
		map_Mod->i_mapper = (i_mapper_func)i_mapper_RuntimeCall;
		map_Mod->ctx      = &rt_uMod;
	}

	if (n_records > 0)
		lower_intrinsics(records, n_records, /*part_block_used=*/0);
}


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
	sparc_handle_intrinsics();

	return &isa->base;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void sparc_done(void *self)
{
	sparc_isa_t *isa = self;

	/* emit now all global declarations */
	be_gas_emit_decls(isa->base.main_env);

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
	NULL,
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
