/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main amd64 backend driver file.
 */
#include "irgwalk.h"
#include "irprog.h"
#include "ircons.h"
#include "irgmod.h"
#include "irdump.h"
#include "lower_calls.h"
#include "debug.h"
#include "error.h"
#include "be_t.h"
#include "bearch.h"
#include "beirg.h"
#include "benode.h"
#include "belower.h"
#include "besched.h"
#include "beabi.h"
#include "bemodule.h"
#include "begnuas.h"
#include "belistsched.h"
#include "beflags.h"
#include "bespillslots.h"
#include "bespillutil.h"
#include "bestack.h"

#include "bearch_amd64_t.h"

#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "amd64_transform.h"
#include "amd64_emitter.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_entity *amd64_get_frame_entity(const ir_node *node)
{
	if (is_amd64_FrameAddr(node)) {
		const amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr_const(node);
		return attr->entity;

	} else if (is_amd64_Store(node)) {
		const amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr_const(node);
		return attr->entity;

	} else if (is_amd64_LoadS(node) || is_amd64_LoadZ(node)) {
		const amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr_const(node);
		return attr->entity;
	}

	(void) node;
	/* TODO: return the ir_entity assigned to the frame */
	return NULL;
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void amd64_set_frame_offset(ir_node *irn, int offset)
{
	if (is_amd64_FrameAddr(irn)) {
		amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr(irn);
		attr->fp_offset += offset;

	} else if (is_amd64_Store(irn)) {
		amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr(irn);
		attr->fp_offset += offset;

	} else if (is_amd64_LoadS(irn) || is_amd64_LoadZ(irn)) {
		amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr(irn);
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
	amd64_get_frame_entity,
	amd64_set_frame_offset,
	amd64_get_sp_bias,
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};

static void amd64_before_ra(ir_graph *irg)
{
	be_sched_fix_flags(irg, &amd64_reg_classes[CLASS_amd64_flags], NULL, NULL);
}

static void transform_Reload(ir_node *node)
{
	ir_graph  *irg    = get_irn_irg(node);
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irg_frame(irg);
	ir_node   *mem    = get_irn_n(node, n_be_Reload_mem);
	ir_mode   *mode   = get_irn_mode(node);
	ir_entity *entity = be_get_frame_entity(node);

	ir_node *load = new_bd_amd64_LoadZ(dbgi, block, ptr, mem, INSN_MODE_64, entity);
	sched_replace(node, load);

	ir_node *proj = new_rd_Proj(dbgi, load, mode, pn_amd64_LoadZ_res);

	const arch_register_t *reg = arch_get_irn_register(node);
	arch_set_irn_register(proj, reg);

	exchange(node, proj);
}

static void transform_Spill(ir_node *node)
{
	ir_graph  *irg    = get_irn_irg(node);
	ir_node   *block  = get_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *ptr    = get_irg_frame(irg);
	ir_node   *mem    = get_irg_no_mem(irg);
	ir_node   *val    = get_irn_n(node, n_be_Spill_val);
	ir_entity *entity = be_get_frame_entity(node);

	ir_node *store = new_bd_amd64_Store(dbgi, block, ptr, val, mem, INSN_MODE_64, entity);
	sched_replace(node, store);

	exchange(node, store);
}

static void amd64_after_ra_walker(ir_node *block, void *data)
{
	(void) data;

	sched_foreach_reverse_safe(block, node) {
		if (be_is_Reload(node)) {
			transform_Reload(node);
		} else if (be_is_Spill(node)) {
			transform_Spill(node);
		}
	}
}

static void amd64_set_frame_entity(ir_node *node, ir_entity *entity)
{
	assert(be_is_Reload(node) || be_is_Spill(node));
	be_node_set_frame_entity(node, entity);
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void amd64_collect_frame_entity_nodes(ir_node *node, void *data)
{
	if (be_is_Reload(node) && be_get_frame_entity(node) == NULL) {
		be_fec_env_t  *env   = (be_fec_env_t*)data;
		const ir_mode *mode  = get_irn_mode(node);
		int            align = get_mode_size_bytes(mode);
		be_node_needs_frame_entity(env, node, mode, align);
	}
}

/**
 * Called immediatly before emit phase.
 */
static void amd64_finish_graph(ir_graph *irg)
{
	be_stack_layout_t *stack_layout = be_get_irg_stack_layout(irg);
	bool               at_begin     = stack_layout->sp_relative ? true : false;
	be_fec_env_t      *fec_env      = be_new_frame_entity_coalescer(irg);

	/* create and coalesce frame entities */
	irg_walk_graph(irg, NULL, amd64_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, amd64_set_frame_entity, at_begin);
	be_free_frame_entity_coalescer(fec_env);

	irg_block_walk_graph(irg, NULL, amd64_after_ra_walker, NULL);

	/* fix stack entity offsets */
	be_abi_fix_stack_nodes(irg);
	be_abi_fix_stack_bias(irg);

	/* Fix 2-address code constraints. */
	amd64_finish_irg(irg);
}

extern const arch_isa_if_t amd64_isa_if;
static amd64_isa_t amd64_isa_template = {
	{
		&amd64_isa_if,             /* isa interface implementation */
		N_AMD64_REGISTERS,
		amd64_registers,
		N_AMD64_CLASSES,
		amd64_reg_classes,
		&amd64_registers[REG_RSP], /* stack pointer register */
		&amd64_registers[REG_RBP], /* base pointer register */
		3,                         /* power of two stack alignment for calls, 2^2 == 4 */
		7,                         /* costs for a spill instruction */
		5,                         /* costs for a reload instruction */
	},
};

static void amd64_init(void)
{
	amd64_register_init();
	amd64_create_opcodes(&amd64_irn_ops);
}

static void amd64_finish(void)
{
	amd64_free_opcodes();
}

static arch_env_t *amd64_begin_codegeneration(void)
{
	amd64_isa_t *isa = XMALLOC(amd64_isa_t);
	*isa = amd64_isa_template;

	return &isa->base;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void amd64_end_codegeneration(void *self)
{
	free(self);
}

/**
 * prepare graph and perform code selection.
 */
static void amd64_prepare_graph(ir_graph *irg)
{
	be_abi_introduce(irg);
	if (be_options.dump_flags & DUMP_BE) {
		dump_ir_graph(irg, "abi");
	}

	be_timer_push(T_CODEGEN);
	amd64_transform_graph(irg);
	be_timer_pop(T_CODEGEN);

	if (be_options.dump_flags & DUMP_BE)
		dump_ir_graph(irg, "code-selection");
}

/**
 * Get the between type for that call.
 * @param self The callback object.
 * @return The between type of for that call.
 */
static ir_type *amd64_get_between_type(ir_graph *irg)
{
	static ir_type *between_type = NULL;
	static ir_entity *old_bp_ent = NULL;
	(void) irg;

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

static const be_abi_callbacks_t amd64_abi_callbacks = {
	amd64_get_between_type,
};

static const arch_register_t *gpreg_param_reg_std[] = {
	&amd64_registers[REG_RDI],
	&amd64_registers[REG_RSI],
	&amd64_registers[REG_RDX],
	&amd64_registers[REG_RCX],
	&amd64_registers[REG_R8],
	&amd64_registers[REG_R9],
};

static const arch_register_t *amd64_get_RegParam_reg(int n)
{
	assert(n < 6 && n >=0 && "register param > 6 requested");
	return gpreg_param_reg_std[n];
}

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void amd64_get_call_abi(ir_type *method_type, be_abi_call_t *abi)
{
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);
	int no_reg = 0;

	/* set abi flags for calls */
	be_abi_call_flags_t call_flags = be_abi_call_get_flags(abi);
	call_flags.call_has_imm = true;
	be_abi_call_set_flags(abi, call_flags, &amd64_abi_callbacks);

	for (i = 0; i < n; i++) {
		tp   = get_method_param_type(method_type, i);
		mode = get_type_mode(tp);
		//d// printf ("MODE %p %p XX %d\n", mode, mode_Iu, i);

		if (!no_reg && i < 6 && mode_is_data (mode)) {
			//d// printf("TEST%d\n", i);
			be_abi_call_param_reg(abi, i, amd64_get_RegParam_reg (i),
			                      ABI_CONTEXT_BOTH);
		/* default: all parameters on stack */
		} else {
			no_reg = 1;
			be_abi_call_param_stack(abi, i, mode, 8, 0, 0, ABI_CONTEXT_BOTH);
		}
	}

	/* TODO: set correct return register */
	/* default: return value is in R0 resp. F0 */
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		if (mode_is_float(mode))
			panic("float not supported yet");

		be_abi_call_res_reg(abi, 0,
			&amd64_registers[REG_RAX], ABI_CONTEXT_BOTH);
	}
}

static void amd64_lower_for_target(void)
{
	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN);

	size_t n_irgs = get_irp_n_irgs();
	for (size_t i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);
		lower_switch(irg, 4, 256, mode_Iu);
	}

	for (size_t i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);
		/* Turn all small CopyBs into loads/stores, and turn all bigger
		 * CopyBs into memcpy calls, because we cannot handle CopyB nodes
		 * during code generation yet.
		 * TODO:  Adapt this once custom CopyB handling is implemented. */
		lower_CopyB(irg, 64, 65, true);
	}
}

static int amd64_is_mux_allowed(ir_node *sel, ir_node *mux_false,
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
static const backend_params *amd64_get_backend_params(void) {
	static backend_params p = {
		0,     /* little endian */
		1,     /* modulo shift is efficient */
		0,     /* non-modulo shift is not efficient */
		0,     /* PIC code not supported */
		NULL,  /* will be set later */
		amd64_is_mux_allowed,  /* parameter for if conversion */
		64,    /* machine size */
		NULL,  /* float arithmetic mode */
		NULL,  /* long long type */
		NULL,  /* unsigned long long type */
		NULL,  /* long double type (not supported yet) */
		0,     /* no trampoline support: size 0 */
		0,     /* no trampoline support: align 0 */
		NULL,  /* no trampoline support: no trampoline builder */
		8      /* alignment of stack parameter: typically 4 (32bit) or 8 (64bit) */
	};
	return &p;
}

static int amd64_is_valid_clobber(const char *clobber)
{
	(void) clobber;
	return 0;
}

static int amd64_register_saved_by(const arch_register_t *reg, int callee)
{
	switch (reg->global_index) {
	case REG_RBX:
	case REG_RBP:
	case REG_R12:
	case REG_R13:
	case REG_R14:
	case REG_R15:
		return callee;

	case REG_RAX:
	case REG_RCX:
	case REG_RDX:
	case REG_RSI:
	case REG_RDI:
	case REG_R8:
	case REG_R9:
	case REG_R10:
	case REG_R11:
		return !callee;

	default:
		return 0;
	}
}

const arch_isa_if_t amd64_isa_if = {
	amd64_init,
	amd64_finish,
    amd64_get_backend_params,
	amd64_lower_for_target,
	amd64_is_valid_clobber,

	amd64_begin_codegeneration,
	amd64_end_codegeneration,
	amd64_get_call_abi,
	NULL,              /* mark remat */
	be_new_spill,
	be_new_reload,
	amd64_register_saved_by,

	NULL,              /* handle intrinsics */
	amd64_prepare_graph,
	amd64_before_ra,
	amd64_finish_graph,
	amd64_gen_routine,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_amd64)
void be_init_arch_amd64(void)
{
	be_register_isa_if("amd64", &amd64_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.cg");

	amd64_init_finish();
	amd64_init_transform();
}
