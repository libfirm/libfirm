/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_bearch_t.h"

#include "be2addr.h"
#include "be_t.h"
#include "beirg.h"
#include "bemodule.h"
#include "bera.h"
#include "besched.h"
#include "bespillslots.h"
#include "bestack.h"
#include "betranshlp.h"
#include "gen_mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"
#include "irarch.h"
#include "iredges.h"
#include "irgwalk.h"
#include "irprog_t.h"
#include "isas.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lowering.h"
#include "mips_emitter.h"
#include "mips_lower64.h"
#include "mips_transform.h"
#include "target_t.h"
#include "util.h"

static ir_settings_arch_dep_t const mips_arch_dep = {
	.replace_muls         = true,
	.replace_divs         = true,
	.replace_mods         = true,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.max_bits_for_mulh    = MIPS_MACHINE_SIZE,
};

static void mips_init_asm_constraints(void)
{
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP,     "Rm");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER,  "cdrvy");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_ANY,       "g");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE, "IJKLMNOPin");
}

static int mips_ifconv(ir_node const *const sel, ir_node const *const mux_false,
                       ir_node const *mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return false;
}

static void mips_init(void)
{
	mips_init_asm_constraints();
	mips_create_opcodes();
	mips_register_init();

	ir_target.experimental
		= "the MIPS backend is highly experimental and unfinished";
	ir_target.allow_ifconv       = mips_ifconv;
	ir_target.float_int_overflow = ir_overflow_indefinite;
}

static void mips_finish(void)
{
	mips_free_opcodes();
}

static void mips_select_instructions(ir_graph *const irg)
{
	be_timer_push(T_CODEGEN);
	mips_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");

	place_code(irg);
	be_dump(DUMP_BE, irg, "place");
}

static ir_node *mips_new_spill(ir_node *const value, ir_node *const after)
{
	ir_mode *const mode = get_irn_mode(value);
	if (be_mode_needs_gp_reg(mode)) {
		ir_node  *const block = get_block(after);
		ir_graph *const irg   = get_irn_irg(after);
		ir_node  *const nomem = get_irg_no_mem(irg);
		ir_node  *const frame = get_irg_frame(irg);
		ir_node  *const store = new_bd_mips_sw(NULL, block, nomem, frame, value, NULL, 0);
		sched_add_after(after, store);
		return store;
	}
	TODO(value);
}

static ir_node *mips_new_reload(ir_node *const value, ir_node *const spill, ir_node *const before)
{
	ir_mode *const mode = get_irn_mode(value);
	if (be_mode_needs_gp_reg(mode)) {
		ir_node  *const block = get_block(before);
		ir_graph *const irg   = get_irn_irg(before);
		ir_node  *const frame = get_irg_frame(irg);
		ir_node  *const load  = new_bd_mips_lw(NULL, block, spill, frame, NULL, 0);
		sched_add_before(before, load);
		return be_new_Proj(load, pn_mips_lw_res);
	}
	TODO(value);
}

static regalloc_if_t const mips_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = mips_new_spill,
	.new_reload  = mips_new_reload,
};

static void mips_collect_frame_entity_nodes(ir_node *const node, void *const data)
{
	be_fec_env_t *const env = (be_fec_env_t*)data;

	if (is_mips_lw(node)) {
		ir_node  *const base  = get_irn_n(node, n_mips_lw_base);
		ir_graph *const irg   = get_irn_irg(node);
		ir_node  *const frame = get_irg_frame(irg);
		if (base == frame) {
			mips_immediate_attr_t const *const attr = get_mips_immediate_attr_const(node);
			if (!attr->ent) {
				unsigned const size     = MIPS_MACHINE_SIZE / 8; // TODO
				unsigned const po2align = log2_floor(size);
				be_load_needs_frame_entity(env, node, size, po2align);
			}
		}
	}
}

static void mips_set_frame_entity(ir_node *const node, ir_entity *const entity, unsigned const size, unsigned const po2align)
{
	(void)size, (void)po2align;

	mips_immediate_attr_t *const imm = get_mips_immediate_attr(node);
	imm->ent = entity;
}

static void mips_assign_spill_slots(ir_graph *const irg)
{
	be_fec_env_t *const fec_env = be_new_frame_entity_coalescer(irg);
	irg_walk_graph(irg, NULL, mips_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, mips_set_frame_entity, true);
	be_free_frame_entity_coalescer(fec_env);
}

static ir_node *mips_new_IncSP(ir_node *const block, ir_node *const sp, int const offset, unsigned const align)
{
	return be_new_IncSP(block, sp, offset, align);
}

static void mips_introduce_prologue(ir_graph *const irg, unsigned const size)
{
	ir_node *const start    = get_irg_start(irg);
	ir_node *const block    = get_nodes_block(start);
	ir_node *const start_sp = be_get_Start_proj(irg, &mips_registers[REG_SP]);
	ir_node *const inc_sp   = mips_new_IncSP(block, start_sp, size, 0);
	sched_add_after(start, inc_sp);
	edges_reroute_except(start_sp, inc_sp, inc_sp);
}

static void mips_introduce_epilogue(ir_node *const ret, unsigned const size)
{
	ir_node *const block  = get_nodes_block(ret);
	ir_node *const ret_sp = get_irn_n(ret, n_mips_ret_stack);
	ir_node *const inc_sp = mips_new_IncSP(block, ret_sp, -(int)size, 0);
	sched_add_before(ret, inc_sp);
	set_irn_n(ret, n_mips_ret_stack, inc_sp);
}

static void mips_introduce_prologue_epilogue(ir_graph *const irg)
{
	ir_type *const frame = get_irg_frame_type(irg);
	unsigned const size  = get_type_size(frame);
	if (size == 0)
		return;

	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_mips_ret(ret));
		mips_introduce_epilogue(ret, size);
	}

	mips_introduce_prologue(irg, size);
}

static void mips_sp_sim(ir_node *const node, stack_pointer_state_t *const state)
{
	if (is_mips_irn(node)) {
		switch ((mips_opcodes)get_mips_irn_opcode(node)) {
		case iro_mips_addiu:
		case iro_mips_lb:
		case iro_mips_lbu:
		case iro_mips_lh:
		case iro_mips_lhu:
		case iro_mips_lw:
		case iro_mips_sb:
		case iro_mips_sh:
		case iro_mips_sw: {
			mips_immediate_attr_t *const imm = get_mips_immediate_attr(node);
			ir_entity             *const ent = imm->ent;
			if (ent && is_frame_type(get_entity_owner(ent))) {
				imm->ent  = NULL;
				imm->val += state->offset + get_entity_offset(ent);
			}
			break;
		}

		default:
			break;
		}
	}
}

static void mips_generate_code(FILE *const output, char const *const cup_name)
{
	be_begin(output, cup_name);

	unsigned *const sp_is_non_ssa = rbitset_alloca(N_MIPS_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_SP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		be_irg_t *const birg = be_birg_from_irg(irg);
		birg->non_ssa_regs = sp_is_non_ssa;

		mips_select_instructions(irg);
		be_step_schedule(irg);
		be_step_regalloc(irg, &mips_regalloc_if);

		mips_assign_spill_slots(irg);

		ir_type *const frame = get_irg_frame_type(irg);
		be_sort_frame_entities(frame, true);
		be_layout_frame_type(frame, 0, 0);

		mips_introduce_prologue_epilogue(irg);
		be_fix_stack_nodes(irg, &mips_registers[REG_SP]);
		birg->non_ssa_regs = NULL;
		be_sim_stack_pointer(irg, 0, 3, &mips_sp_sim);

		be_handle_2addr(irg, NULL);

		mips_emit_function(irg);
		be_step_last(irg);
	}

	be_finish();
}

static void mips_lower_for_target(void)
{
	ir_arch_lower(&mips_arch_dep);
	be_after_irp_transform("lower-arch-dep");

	lower_calls_with_compounds(LF_RETURN_HIDDEN,
	                           dont_lower_aggregates, NULL,
	                           lower_aggregates_as_pointers, NULL,
	                           reset_stateless_abi);
	be_after_irp_transform("lower-calls");

	foreach_irp_irg(i, irg) {
		lower_CopyB(irg, 16, 17, false);
		be_after_transform(irg, "lower-copyb");
	}

	static ir_builtin_kind const supported[] = {
		ir_bk_saturating_increment,
	};
	lower_builtins(ARRAY_SIZE(supported), supported, NULL);

	ir_mode *const mode_gp = mips_reg_classes[CLASS_mips_gp].mode;
	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, mode_gp);
		be_after_transform(irg, "lower-switch");
	}

	mips_lower64();
	be_after_irp_transform("lower-64");
}

static unsigned mips_get_op_estimated_cost(ir_node const *const node)
{
	(void)node; // TODO
	return 1;
}

arch_isa_if_t const mips_isa_if = {
	.name                  = "mips",
	.pointer_size          = 4,
	.modulo_shift          = 32,
	.big_endian            = true,
	.po2_biggest_alignment = 3,
	.pic_supported         = false,
	.n_registers           = N_MIPS_REGISTERS,
	.registers             = mips_registers,
	.n_register_classes    = N_MIPS_CLASSES,
	.register_classes      = mips_reg_classes,
	.init                  = mips_init,
	.finish                = mips_finish,
	.generate_code         = mips_generate_code,
	.lower_for_target      = mips_lower_for_target,
	.register_prefix       = '$',
	.get_op_estimated_cost = mips_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_mips)
void be_init_arch_mips(void)
{
}
