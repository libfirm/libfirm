/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main amd64 backend driver file.
 */
#include "beabi.h"
#include "beabihelper.h"
#include "bearch.h"
#include "beflags.h"
#include "begnuas.h"
#include "beirg.h"
#include "belistsched.h"
#include "belower.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespillslots.h"
#include "bespillutil.h"
#include "bestack.h"
#include "be_t.h"
#include "debug.h"
#include "error.h"
#include "ircons.h"
#include "irdump.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irprog_t.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "util.h"

#include "bearch_amd64_t.h"

#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "amd64_transform.h"
#include "amd64_emitter.h"
#include "amd64_cconv.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_entity *amd64_get_frame_entity(const ir_node *node)
{
	if (!is_amd64_irn(node))
		return NULL;
	if (!amd64_has_addr_attr(node))
		return NULL;
	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	ir_entity *entity = attr->addr.immediate.entity;
	if (entity == NULL)
		return NULL;
	ir_type *owner = get_entity_owner(entity);
	if (is_frame_type(owner))
		return entity;
	ir_graph *irg = get_irn_irg(node);
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
	if (owner == layout->arg_type)
		return entity;
	return NULL;
}

static int get_insn_mode_bytes(amd64_insn_mode_t insn_mode)
{
	switch (insn_mode) {
	case INSN_MODE_8:  return 1;
	case INSN_MODE_16: return 2;
	case INSN_MODE_32: return 4;
	case INSN_MODE_64: return 8;
	}
	panic("bad insn mode");
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void amd64_set_frame_offset(ir_node *node, int offset)
{
	if (!is_amd64_irn(node))
		return;
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.offset += offset;
	if (is_amd64_PopAM(node)) {
		ir_graph          *irg    = get_irn_irg(node);
		be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
		if (layout->sp_relative)
			attr->addr.immediate.offset -= get_insn_mode_bytes(attr->insn_mode);
	}
}

static int amd64_get_sp_bias(const ir_node *node)
{
	if (is_amd64_Start(node)) {
		ir_graph *irg        = get_irn_irg(node);
		ir_type  *frame_type = get_irg_frame_type(irg);
		return get_type_size_bytes(frame_type);
	} else if (is_amd64_Return(node)) {
		ir_graph *irg        = get_irn_irg(node);
		ir_type  *frame_type = get_irg_frame_type(irg);
		return -(int)get_type_size_bytes(frame_type);
	} else if (is_amd64_PushAM(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		return get_insn_mode_bytes(attr->insn_mode);
	} else if (is_amd64_PopAM(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		return -get_insn_mode_bytes(attr->insn_mode);
	}
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

	be_add_missing_keeps(irg);
}

static const arch_register_req_t amd64_requirement_gp = {
	arch_register_req_type_normal,
	&amd64_reg_classes[CLASS_amd64_gp],
	NULL,
	0,
	0,
	1
};

static const unsigned amd64_limited_gp_rsp [] = { (1 << REG_GP_RSP) };
static const arch_register_req_t amd64_single_reg_req_gp_rsp = {
	arch_register_req_type_limited,
	&amd64_reg_classes[CLASS_amd64_gp],
	amd64_limited_gp_rsp,
	0,
	0,
	1
};

static const arch_register_req_t *am_pushpop_base_reqs[] = {
	&amd64_single_reg_req_gp_rsp,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static ir_node *create_push(ir_node *node, ir_node *schedpoint, ir_node *sp,
                            ir_node *mem, ir_entity *ent)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *frame = get_irg_frame(irg);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input       = 0;
	addr.index_input      = NO_INPUT;
	addr.immediate.entity = ent;
	ir_node *in[] = { sp, frame, mem };
	ir_node *push = new_bd_amd64_PushAM(dbgi, block, ARRAY_SIZE(in), in,
	                                    INSN_MODE_64, addr);
	arch_set_irn_register_reqs_in(push, am_pushpop_base_reqs);
	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ir_node *node, ir_node *schedpoint, ir_node *sp, ir_entity *ent)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *frame = get_irg_frame(irg);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input  = 0;
	addr.index_input = NO_INPUT;
	addr.immediate.entity = ent;
	ir_node *in[] = { sp, frame, get_irg_no_mem(irg) };

	ir_node *pop = new_bd_amd64_PopAM(dbgi, block, ARRAY_SIZE(in), in,
	                                  INSN_MODE_64, addr);
	arch_set_irn_register_reqs_in(pop, am_pushpop_base_reqs);
	sched_add_before(schedpoint, pop);

	return pop;
}

static ir_node* create_spproj(ir_node *pred, int pos)
{
	const arch_register_t *spreg = &amd64_registers[REG_RSP];
	ir_mode               *spmode = spreg->reg_class->mode;
	ir_node               *sp     = new_r_Proj(pred, spmode, pos);
	arch_set_irn_register(sp, spreg);
	return sp;
}

/**
 * Transform MemPerm, currently we do this the ugly way and produce
 * push/pop into/from memory cascades. This is possible without using
 * any registers.
 */
static void transform_MemPerm(ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *sp    = be_get_initial_reg_value(irg, &amd64_registers[REG_RSP]);
	int       arity = be_get_MemPerm_entity_arity(node);
	ir_node **pops  = ALLOCAN(ir_node*, arity);
	ir_node  *in[1];
	ir_node  *keep;
	int       i;

	/* create Pushs */
	for (i = 0; i < arity; ++i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(inent);
		unsigned entsize = get_type_size_bytes(enttype);
		unsigned entsize2 = get_type_size_bytes(get_entity_type(outent));
		ir_node *mem = get_irn_n(node, i + 1);
		ir_node *push;

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;
		/* spillslot should be 64bit size */
		assert(entsize == 8);

		push = create_push(node, node, sp, mem, inent);
		sp = create_spproj(push, pn_amd64_PushAM_stack);
		set_irn_n(node, i, new_r_Bad(irg, mode_X));
	}

	/* create pops */
	for (i = arity; i-- > 0; ) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(outent);
		unsigned entsize = get_type_size_bytes(enttype);
		unsigned entsize2 = get_type_size_bytes(get_entity_type(inent));

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;
		assert(entsize == 8);

		ir_node *pop = create_pop(node, node, sp, outent);
		sp = create_spproj(pop, pn_amd64_PopAM_stack);
		pops[i] = pop;
	}

	in[0] = sp;
	keep  = be_new_Keep(block, 1, in);
	sched_replace(node, keep);

	/* exchange memprojs */
	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		int p = get_Proj_proj(proj);

		assert(p < arity);

		set_Proj_pred(proj, pops[p]);
		set_Proj_proj(proj, pn_amd64_PopAM_M);
	}

	/* remove memperm */
	kill_node(node);
}

static void amd64_after_ra_walker(ir_node *block, void *data)
{
	(void) data;

	sched_foreach_reverse_safe(block, node) {
		if (be_is_MemPerm(node)) {
			transform_MemPerm(node);
		}
	}
}

static void amd64_set_frame_entity(ir_node *node, ir_entity *entity)
{
	assert(is_amd64_Store(node) || is_amd64_Mov(node)
	    || is_amd64_Movs(node));
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.entity = entity;
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void amd64_collect_frame_entity_nodes(ir_node *node, void *data)
{
	/* we are only interested to report Load nodes */
	if (!is_amd64_Mov(node) && !is_amd64_Movs(node))
		return;

	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	if (attr->needs_frame_ent) {
		be_fec_env_t  *env   = (be_fec_env_t*)data;
		const ir_mode *mode  = mode_Lu; /* TODO: improve */
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

	/* emit code */
	amd64_emit_function(irg);
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
	amd64_cconv_init();
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
	be_timer_push(T_CODEGEN);
	amd64_transform_graph(irg);
	be_timer_pop(T_CODEGEN);

	be_dump(DUMP_BE, irg, "code-selection");
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
	be_after_irp_transform("lower-calls");

	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, mode_Iu);
		be_after_transform(irg, "lower-switch");
	}

	foreach_irp_irg(i, irg) {
		/* Turn all small CopyBs into loads/stores, and turn all bigger
		 * CopyBs into memcpy calls, because we cannot handle CopyB nodes
		 * during code generation yet.
		 * TODO:  Adapt this once custom CopyB handling is implemented. */
		lower_CopyB(irg, 64, 65, true);
		be_after_transform(irg, "lower-copyb");
	}

	lower_builtins(0, NULL);
	be_after_irp_transform("lower-builtins");
}

static int amd64_is_mux_allowed(ir_node *sel, ir_node *mux_false,
                                ir_node *mux_true)
{
	/* optimizable by middleend */
	if (ir_is_optimizable_mux(sel, mux_false, mux_true))
		return true;
	return false;
}

static const ir_settings_arch_dep_t amd64_arch_dep = {
	1,     /* also use subs */
	4,     /* maximum shifts */
	63,    /* maximum shift amount */
	NULL,  /* evaluate the instruction sequence */

	1,  /* allow Mulhs */
	1,  /* allow Mulus */
	32, /* Mulh allowed up to 32 bit */
};
/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *amd64_get_backend_params(void) {
	static backend_params p = {
		false,     /* little endian */
		false,     /* PIC code not supported */
		true,      /* unaligned memory */
		32,    /* modulo shift */
		&amd64_arch_dep,
		amd64_is_mux_allowed,  /* parameter for if conversion */
		64,    /* machine size */
		NULL,  /* float arithmetic mode */
		NULL,  /* long long type */
		NULL,  /* unsigned long long type */
		NULL,  /* long double type (not supported yet) */
		0,     /* no trampoline support: size 0 */
		0,     /* no trampoline support: align 0 */
		NULL,  /* no trampoline support: no trampoline builder */
		8,     /* alignment of stack parameter: typically 4 (32bit) or 8 (64bit) */
		ir_overflow_indefinite
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
	amd64_new_spill,
	amd64_new_reload,
	amd64_register_saved_by,

	NULL,              /* handle intrinsics */
	amd64_prepare_graph,
	amd64_before_ra,
	amd64_finish_graph,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_amd64)
void be_init_arch_amd64(void)
{
	be_register_isa_if("amd64", &amd64_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.cg");

	amd64_init_finish();
	amd64_init_transform();
}
