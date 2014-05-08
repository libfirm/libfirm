/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main amd64 backend driver file.
 */
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
#include "lower_mode_b.h"
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

static void amd64_set_frame_entity(ir_node *node, ir_entity *entity,
                                   const ir_type *type)
{
	(void)type;
	assert(is_amd64_Store(node) || is_amd64_Mov(node)
	    || is_amd64_Movs(node));
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.entity = entity;
}

static bool is_frame_load(const ir_node *node)
{
	return is_amd64_Mov(node) || is_amd64_Movs(node);
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void amd64_collect_frame_entity_nodes(ir_node *node, void *data)
{
	/* we are only interested to report Load nodes */
	if (!is_frame_load(node))
		return;

	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	if (attr->needs_frame_ent) {
		be_fec_env_t  *env   = (be_fec_env_t*)data;
		const ir_mode *mode  = mode_Lu; /* TODO: improve */
		const ir_type *type  = get_type_for_mode(mode);
		be_load_needs_frame_entity(env, node, type);
	}
}

static void introduce_epilogue(ir_node *ret)
{
	const arch_register_t *sp         = &amd64_registers[REG_RSP];
	ir_graph              *irg        = get_irn_irg(ret);
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *first_sp   = get_irn_n(ret, n_be_Return_sp);
	ir_node               *curr_sp    = first_sp;

	if(!layout->sp_relative) {
		assert(false);
	} else {
		if (frame_size > 0) {
			ir_node *incsp = be_new_IncSP(sp, block, curr_sp,
			                              - (int) frame_size, 0);
			sched_add_before(ret, incsp);
			curr_sp = incsp;
		}
	}
	set_irn_n(ret, n_be_Return_sp, curr_sp);
}

static void introduce_prologue_epilogue(ir_graph *irg)
{
	const arch_register_t *sp         = &amd64_registers[REG_RSP];
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *initial_sp = be_get_initial_reg_value(irg, sp);

	if (!layout->sp_relative) {
		assert(false);
	} else {
		if (frame_size > 0) {
			ir_node *const incsp = be_new_IncSP(sp, block, initial_sp,
			                                    frame_size, 0);
			sched_add_after(start, incsp);
		}
	}

	/* introduce epilogue for every return node */
	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(be_is_Return(ret) || is_amd64_Return(ret));
		introduce_epilogue(ret);
	}
}

/**
 * Called immediatly before emit phase.
 */
static void amd64_finish_graph(ir_graph *irg)
{
	be_stack_layout_t *stack_layout = be_get_irg_stack_layout(irg);
	bool               at_begin     = stack_layout->sp_relative;
	be_fec_env_t      *fec_env      = be_new_frame_entity_coalescer(irg);

	/* create and coalesce frame entities */
	irg_walk_graph(irg, NULL, amd64_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, amd64_set_frame_entity, at_begin);
	be_free_frame_entity_coalescer(fec_env);

	irg_block_walk_graph(irg, NULL, amd64_after_ra_walker, NULL);

	/* fix stack entity offsets */
	be_abi_fix_stack_nodes(irg);
	be_abi_fix_stack_bias(irg);

	introduce_prologue_epilogue(irg);

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
		/* lower for mode_b stuff */
		ir_lower_mode_b(irg, mode_Lu);
		be_after_transform(irg, "lower-modeb");
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

const arch_isa_if_t amd64_isa_if = {
	amd64_init,
	amd64_finish,
    amd64_get_backend_params,
	amd64_lower_for_target,
	amd64_is_valid_clobber,

	amd64_begin_codegeneration,
	amd64_end_codegeneration,
	NULL,
	NULL,              /* mark remat */
	amd64_new_spill,
	amd64_new_reload,
	NULL,

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
