/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#include "riscv_bearch_t.h"

#include "be2addr.h"
#include "be_t.h"
#include "beirg.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "bespillslots.h"
#include "bestack.h"
#include "betranshlp.h"
#include "beutil.h"
#include "gen_riscv_new_nodes.h"
#include "gen_riscv_regalloc_if.h"
#include "irarch.h"
#include "ircons.h"
#include "iredges.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irprog_t.h"
#include "irtools.h"
#include "lc_opts_enum.h"
#include "lower_alloc.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_mode_b.h"
#include "lower_softfloat.h"
#include "lowering.h"
#include "platform_t.h"
#include "riscv_abi.h"
#include "riscv_emitter.h"
#include "riscv_lower64.h"
#include "riscv_transform.h"
#include "target_t.h"
#include "util.h"

typedef enum {
	ilp32d,
	ilp32,
} riscv_abi_t;
static const lc_opt_enum_int_items_t abi_items[] = {
		{ "ilp32d", ilp32d },
		{ "ilp32",  ilp32  },
		{ NULL,     0      },
};

static int abi;
static lc_opt_enum_int_var_t abi_var = {
		&abi, abi_items
};

typedef enum {
	rv32imafd,
	rv32ima,
} riscv_isa_t;
static const lc_opt_enum_int_items_t isa_items[] = {
		{ "rv32g",     rv32imafd },
		{ "rv32imafd", rv32imafd },
		{ "rv32ima",   rv32ima   },
		{ NULL,        0         },
};

static int isa;
static lc_opt_enum_int_var_t isa_var = {
		&isa, isa_items
};

static const lc_opt_table_entry_t riscv_options[] = {
		LC_OPT_ENT_ENUM_INT("arch", "Generate code for given RISC-V ISA", &isa_var),
		LC_OPT_ENT_ENUM_INT("abi",  "Specify integer and floating-point calling convention",  &abi_var),
		LC_OPT_LAST
};

static bool use_softfloat;

static ir_settings_arch_dep_t const riscv_arch_dep = {
	.replace_muls         = true,
	.replace_divs         = true,
	.replace_mods         = true,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.max_bits_for_mulh    = RISCV_MACHINE_SIZE,
};

/**
 * Splits a 32 bit immediate value into a 20 bit hi and a 12 bit lo part, which can be realised by lui and addi
 * instructions.
 */
riscv_hi_lo_imm calc_hi_lo(int32_t val) {
	int32_t hi = ((uint32_t) val >> 12) + ((uint32_t) val >> 11 & 1);
	if (hi >= 1048576) { //2^20
		hi = 0;
	}
	int32_t const lo = (uint32_t) val - ((uint32_t) hi << 12);
	return (riscv_hi_lo_imm) {hi, lo};
}

static void riscv_init_asm_constraints(void)
{
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP,     "m");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER,  "r");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_ANY,       "g");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE, "IJKin");
}

static int riscv_ifconv(ir_node const *const sel, ir_node const *const mux_false, ir_node const *mux_true)
{
	(void)sel;
	(void)mux_false;
	(void)mux_true;
	return false;
}

static void riscv_init(void)
{
	riscv_init_asm_constraints();
	riscv_create_opcodes();
	riscv_register_init();

	ir_target.experimental = "the RISC-V backend is highly experimental and unfinished";

	ir_target.allow_ifconv       = riscv_ifconv;
	ir_target.float_int_overflow = ir_overflow_min_max;
	ir_platform_set_va_list_type_pointer();

	use_softfloat = ((riscv_isa_t)isa == rv32ima);
	if (use_softfloat && (riscv_abi_t)abi == ilp32d) {
		panic("requested ABI requires -march to subsume the 'D' extension");
	} else if (!use_softfloat && (riscv_abi_t)abi == ilp32) {
		panic("Use of hard-float instructions with soft-float ABI not supported yet");
	}
}

static void riscv_finish(void)
{
	riscv_free_opcodes();
}

static void riscv_select_instructions(ir_graph *const irg)
{
	be_timer_push(T_CODEGEN);
	riscv_transform_graph(irg);
	be_timer_pop(T_CODEGEN);
	be_dump(DUMP_BE, irg, "code-selection");

	place_code(irg);
	be_dump(DUMP_BE, irg, "place");
}

static ir_node *riscv_new_spill(ir_node *const value, ir_node *const after)
{
	ir_mode *const mode = get_irn_mode(value);
	if (be_mode_needs_gp_reg(mode)) {
		ir_node  *const block = get_block(after);
		ir_graph *const irg   = get_irn_irg(after);
		ir_node  *const nomem = get_irg_no_mem(irg);
		ir_node  *const frame = get_irg_frame(irg);
		ir_node  *const store = new_bd_riscv_sw(NULL, block, nomem, frame, value, NULL, 0);
		sched_add_after(after, store);
		return store;
	}
	TODO(value);
}

static ir_node *riscv_new_reload(ir_node *const value, ir_node *const spill, ir_node *const before)
{
	ir_mode *const mode = get_irn_mode(value);
	if (be_mode_needs_gp_reg(mode)) {
		ir_node  *const block = get_block(before);
		ir_graph *const irg   = get_irn_irg(before);
		ir_node  *const frame = get_irg_frame(irg);
		ir_node  *const load  = new_bd_riscv_lw(NULL, block, spill, frame, NULL, 0);
		sched_add_before(before, load);
		return be_new_Proj(load, pn_riscv_lw_res);
	}
	TODO(value);
}

static regalloc_if_t const riscv_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = riscv_new_spill,
	.new_reload  = riscv_new_reload,
};

static void riscv_collect_frame_entity_nodes(ir_node *const node, void *const data)
{
	be_fec_env_t *const env = (be_fec_env_t*)data;

	if (is_riscv_lw(node)) {
		ir_node  *const base  = get_irn_n(node, n_riscv_lw_base);
		ir_graph *const irg   = get_irn_irg(node);
		ir_node  *const frame = get_irg_frame(irg);
		if (base == frame) {
			riscv_immediate_attr_t const *const attr = get_riscv_immediate_attr_const(node);
			if (!attr->ent) {
				unsigned const size     = RISCV_REGISTER_SIZE;
				unsigned const po2align = log2_floor(size);
				be_load_needs_frame_entity(env, node, size, po2align);
			}
		}
	}
}

static void riscv_set_frame_entity(ir_node *const node, ir_entity *const entity, unsigned const size, unsigned const po2align)
{
	(void)size, (void)po2align;

	riscv_immediate_attr_t *const imm = get_riscv_immediate_attr(node);
	imm->ent = entity;
}

static void riscv_assign_spill_slots(ir_graph *const irg, bool omit_fp)
{
	be_fec_env_t *const fec_env = be_new_frame_entity_coalescer(irg);
	irg_walk_graph(irg, NULL, riscv_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, riscv_set_frame_entity, omit_fp);
	be_free_frame_entity_coalescer(fec_env);
}

static ir_node *riscv_new_IncSP(ir_node *const block, ir_node *const sp, int const offset, unsigned const align)
{
	return be_new_IncSP(block, sp, offset, align);
}

static void riscv_introduce_prologue(ir_graph *const irg, unsigned size, bool omit_fp, bool is_variadic)
{
	ir_node *const start    = get_irg_start(irg);
	ir_node *const block    = get_nodes_block(start);
	ir_node *const start_sp = be_get_Start_proj(irg, &riscv_registers[REG_SP]);
	ir_node *const start_fp = be_get_Start_proj(irg, &riscv_registers[REG_S0]);

	if (!omit_fp) {
		ir_node *const mem        = get_irg_initial_mem(irg);

		/* increase stack frame */
		ir_node *const inc_sp   = riscv_new_IncSP(block, start_sp, size, 0);
		edges_reroute_except(start_sp, inc_sp, inc_sp);
		sched_add_after(start, inc_sp);

		unsigned aligned = round_up2(size, 1u << RISCV_PO2_STACK_ALIGNMENT);

		int fp_save_offset = is_variadic ? -(RISCV_N_PARAM_REGS + 1) * RISCV_REGISTER_SIZE : -RISCV_REGISTER_SIZE;
		if (!is_simm12(aligned)) {
			/* if desired size is not as 12 bit immediate encodeable, first compute the large offset in t0 */
			riscv_hi_lo_imm imm = calc_hi_lo(aligned);
			ir_node      *res;
			if (imm.hi != 0) {
				res = new_bd_riscv_lui(NULL, block, NULL, imm.hi);
				arch_set_irn_register(res, &riscv_registers[REG_T0]);
				sched_add_after(inc_sp, res);
			} else {
				res = get_Start_zero(irg);
			}
			if (imm.lo != 0) {
				ir_node *const add_after = imm.hi != 0 ? res : inc_sp;
				res = new_bd_riscv_addi(NULL, block, res, NULL, imm.lo);
				arch_set_irn_register(res, &riscv_registers[REG_T0]);
				sched_add_after(add_after, res);
			}
			ir_node *const add = new_bd_riscv_add(NULL, block, inc_sp, res);
			arch_set_irn_register(add, &riscv_registers[REG_T0]);
			sched_add_after(res, add);

			/* save fp to stack */
			ir_node *const store_fp = new_bd_riscv_sw(NULL, block, mem, add, start_fp, NULL, fp_save_offset);
			sched_add_after(add, store_fp);
			edges_reroute_except(mem, store_fp, store_fp);

			/* set current fp */
			ir_node *const curr_fp = be_new_Copy(block, add);
			arch_set_irn_register(curr_fp, &riscv_registers[REG_S0]);
			sched_add_after(store_fp, curr_fp);
			edges_reroute_except(start_fp, curr_fp, store_fp);
		} else {
			/* save fp to stack */
			ir_node *const store_fp = new_bd_riscv_sw(NULL, block, mem, start_sp, start_fp, NULL, aligned + fp_save_offset);
			sched_add_after(inc_sp, store_fp);
			edges_reroute_except(mem, store_fp, store_fp);

			/* set current fp */
			ir_node *const curr_fp = new_bd_riscv_addi(NULL, block, inc_sp, NULL, aligned);
			sched_add_after(store_fp, curr_fp);
			edges_reroute_except(start_fp, curr_fp, store_fp);
			arch_set_irn_register(curr_fp, &riscv_registers[REG_S0]);
		}

		be_keep_if_unused(inc_sp);
	} else {
		ir_node *const inc_sp   = riscv_new_IncSP(block, start_sp, size, 0);
		sched_add_after(start, inc_sp);
		edges_reroute_except(start_sp, inc_sp, inc_sp);
	}
}

static void riscv_introduce_epilogue(ir_node *const ret, unsigned const size, bool omit_fp, bool is_variadic)
{
	ir_node *const block  = get_nodes_block(ret);
	if (!omit_fp) {
		int const n_fp = be_get_input_pos_for_req(ret, &riscv_single_reg_req_gp_s0);
		ir_node  *curr_fp  = get_irn_n(ret, n_fp);
		ir_node  *curr_mem = get_irn_n(ret, n_riscv_ret_mem);

		/* restore old fp */
		int fp_save_offset = is_variadic ? -(RISCV_N_PARAM_REGS + 1) * RISCV_REGISTER_SIZE : -RISCV_REGISTER_SIZE;
		ir_node *const load_old_fp = new_bd_riscv_lw(NULL, block, curr_mem, curr_fp, NULL, fp_save_offset);
		curr_mem = be_new_Proj(load_old_fp, pn_riscv_lw_M);
		ir_node *const old_fp  = be_new_Proj_reg(load_old_fp, pn_riscv_lw_res, &riscv_registers[REG_T0]);
		sched_add_before(ret, load_old_fp);

		/* Copy fp to sp */
		ir_node *const curr_sp = be_new_Copy(block, curr_fp);
		arch_set_irn_register(curr_sp, &riscv_registers[REG_SP]);
		sched_add_before(ret, curr_sp);

		/* set fp to old fp */
		ir_node *const restored_fp = be_new_Copy(block, old_fp);
		arch_set_irn_register(restored_fp, &riscv_registers[REG_S0]);
		sched_add_before(ret, restored_fp);

		set_irn_n(ret, n_fp,            restored_fp);
		set_irn_n(ret, n_riscv_ret_mem, curr_mem);
		set_irn_n(ret, n_riscv_ret_stack, curr_sp);
	} else {
		ir_node *const ret_sp = get_irn_n(ret, n_riscv_ret_stack);
		ir_node *const inc_sp = riscv_new_IncSP(block, ret_sp, -(int)size, 0);
		sched_add_before(ret, inc_sp);
		set_irn_n(ret, n_riscv_ret_stack, inc_sp);
	}

}

static void riscv_introduce_prologue_epilogue(ir_graph *const irg, bool omit_fp)
{
	ir_type *const frame = get_irg_frame_type(irg);
	ir_entity *const fun_ent  = get_irg_entity(irg);
	ir_type         *fun_type = get_entity_type(fun_ent);
	unsigned size  = get_type_size(frame);
	bool is_variadic = is_method_variadic(fun_type);
	if (size == 0 && omit_fp && !is_variadic)
		return;

	if (!omit_fp) {
		// additional slot for saved frame pointer
		size += RISCV_REGISTER_SIZE;
	}

	if (is_variadic) {
		size += 32;
	}

	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_riscv_ret(ret));
		riscv_introduce_epilogue(ret, size, omit_fp, is_variadic);
	}

	riscv_introduce_prologue(irg, size, omit_fp, is_variadic);
}

static void riscv_sp_sim(ir_node *const node, stack_pointer_state_t *const state)
{
	if (be_is_MemPerm(node)) {
		ir_graph *irg = get_irn_irg(node);
		if (riscv_get_irg_data(irg)->omit_fp) {
			be_set_MemPerm_offset(node, state->offset);
		}
		return;
	}
	if (is_riscv_irn(node)) {
		switch ((riscv_opcodes)get_riscv_irn_opcode(node)) {
		case iro_riscv_addi:
		case iro_riscv_FrameAddr:
		case iro_riscv_lb:
		case iro_riscv_lbu:
		case iro_riscv_lh:
		case iro_riscv_lhu:
		case iro_riscv_lw:
		case iro_riscv_sb:
		case iro_riscv_sh:
		case iro_riscv_sw: {
			riscv_immediate_attr_t *const imm = get_riscv_immediate_attr(node);
			ir_entity              *const ent = imm->ent;
			if (ent && is_frame_type(get_entity_owner(ent))) {
				imm->ent  = NULL;
				ir_graph *const irg = get_irn_irg(node);
				if (riscv_get_irg_data(irg)->omit_fp) {
					imm->val += state->offset;
				}
				imm->val += get_entity_offset(ent);
			}
			break;
		}
		case iro_riscv_SubSP:
		case iro_riscv_SubSPimm:
			state->align_padding = 0;
		default:
			break;
		}
	}
}

static void riscv_generate_code(FILE *const output, char const *const cup_name)
{
	be_begin(output, cup_name);

	unsigned *const sp_is_non_ssa = rbitset_alloca(N_RISCV_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_SP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		be_irg_t *const birg = be_birg_from_irg(irg);

		struct obstack *obst = be_get_be_obst(irg);
		birg->isa_link = OALLOCZ(obst, riscv_irg_data_t);
		birg->non_ssa_regs = sp_is_non_ssa;

		riscv_select_instructions(irg);
		be_step_schedule(irg);
		be_step_regalloc(irg, &riscv_regalloc_if);

		riscv_irg_data_t const *const irg_data = riscv_get_irg_data(irg);
		bool const omit_fp = irg_data->omit_fp;

		riscv_assign_spill_slots(irg, omit_fp);

		ir_type *const frame = get_irg_frame_type(irg);
		be_sort_frame_entities(frame, omit_fp);
		ir_entity *const fun_ent  = get_irg_entity(irg);
		ir_type         *fun_type = get_entity_type(fun_ent);
		// variadic functions copy the register arguments to the vararg save area on the stack
		int begin = is_method_variadic(fun_type) ? -(RISCV_REGISTER_SIZE * RISCV_N_PARAM_REGS) : 0;
		// slot for saved frame pointer
		begin -= omit_fp ? 0 : RISCV_REGISTER_SIZE;
		be_layout_frame_type(frame, begin, 0);

		riscv_introduce_prologue_epilogue(irg, omit_fp);
		be_fix_stack_nodes(irg, &riscv_registers[REG_SP]);
		birg->non_ssa_regs = NULL;
		be_sim_stack_pointer(irg, 0, RISCV_PO2_STACK_ALIGNMENT, &riscv_sp_sim);

		riscv_finish_graph(irg);
		be_handle_2addr(irg, NULL);

		riscv_emit_function(irg);
		be_step_last(irg);
	}

	be_finish();
}

static void riscv32_lower_va_arg(ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_graph *irg   = get_irn_irg(node);

	ir_type       *aptype   = get_method_res_type(get_Builtin_type(node), 0);
	ir_node       *ap       = get_irn_n(node, 1);
	ir_node *const node_mem = get_Builtin_mem(node);

	ir_mode *const offset_mode = get_reference_offset_mode(mode_P);
	unsigned round_up;

	ir_mode *apmode = get_type_mode(aptype);
	ir_node *res;
	ir_node *new_mem;
	ir_type *const original_aptype = aptype;

	if (!apmode) {
		// compounds are generally passed by reference
		assert(is_compound_type(aptype));
		apmode = mode_P;
		aptype = get_type_for_mode(apmode);
	}

	// align the va_arg pointer according to the argument alignment
	round_up = round_up2(get_type_alignment(aptype), RISCV_PARAM_STACK_ALIGN);
	if (round_up > RISCV_PARAM_STACK_ALIGN) {
		// ap = (ap + (round_up - 1)) & -(round_up)
		ir_node *const align_min_1 = new_rd_Const_long(dbgi, irg, offset_mode, (int) round_up - 1);
		ir_node *const align_add = new_rd_Add(dbgi, block, ap, align_min_1);
		ir_node *const align_neg = new_rd_Const_long(dbgi, irg, offset_mode, -round_up);
		ir_node *const align_add_conv = new_rd_Conv(dbgi, block, align_add, offset_mode);
		ir_node *const align_and = new_rd_And(dbgi, block, align_add_conv, align_neg);
		ir_node *const align_and_conv = new_rd_Conv(dbgi, block, align_and, mode_P);
		ap = align_and_conv;
	}

	if (is_compound_type(original_aptype) && get_type_size(original_aptype) <= 2 * RISCV_REGISTER_SIZE) {
		// structs with size <= 2*XLEN are passed by value, not by reference
		res = ap;
		new_mem = node_mem;
		aptype = original_aptype;
	} else {
		ir_node *const load = new_rd_Load(dbgi, block, node_mem, ap, apmode, aptype, cons_none);
		res = new_r_Proj(load, apmode, pn_Load_res);
		new_mem = new_r_Proj(load, mode_M, pn_Load_M);
	}

	round_up = round_up2(get_type_size(aptype), RISCV_PARAM_STACK_ALIGN);
	ir_node *const offset      = new_r_Const_long(irg, offset_mode, round_up);
	ir_node *const new_ap      = new_rd_Add(dbgi, block, ap, offset);

	ir_node *const in[] = { new_mem, res, new_ap };
	turn_into_tuple(node, ARRAY_SIZE(in), in);
}

static void riscv_lower_for_target(void)
{
	ir_arch_lower(&riscv_arch_dep);
	be_after_irp_transform("lower-arch-dep");

	lower_calls_with_compounds(LF_RETURN_HIDDEN,
	                           riscv_lower_parameter, NULL,
	                           riscv_lower_result, NULL,
	                           reset_stateless_abi);
	be_after_irp_transform("lower-calls");

	foreach_irp_irg(i, irg) {
		lower_CopyB(irg, 16, 17, false);
		be_after_transform(irg, "lower-copyb");
	}

	if (use_softfloat) {
		lower_floating_point();
		be_after_irp_transform("lower-fp");
	}

	static ir_builtin_kind const supported[] = {
		ir_bk_saturating_increment,
		ir_bk_va_start
	};
	lower_builtins(ARRAY_SIZE(supported), supported, riscv32_lower_va_arg);
	foreach_irp_irg(i, irg) {
		be_after_transform(irg, "lower-builtins");
	}

	ir_mode *const mode_gp = riscv_reg_classes[CLASS_riscv_gp].mode;
	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, mode_gp);
		be_after_transform(irg, "lower-switch");
	}

	riscv_lower64();
	be_after_irp_transform("lower-64");

	foreach_irp_irg(i, irg) {
		ir_lower_mode_b(irg, mode_gp);
		be_after_transform(irg, "lower-modeb");
		lower_alloc(irg, RISCV_PO2_STACK_ALIGNMENT);
		be_after_transform(irg, "lower-alloc");
	}
}

static unsigned riscv_get_op_estimated_cost(ir_node const *const node)
{
	(void)node; // TODO
	return 1;
}

arch_isa_if_t const riscv32_isa_if = {
	.name                  = "riscv32",
	.pointer_size          = 4,
	.modulo_shift          = 32,
	.big_endian            = false,
	.po2_biggest_alignment = 4,
	.pic_supported         = false,
	.n_registers           = N_RISCV_REGISTERS,
	.registers             = riscv_registers,
	.n_register_classes    = N_RISCV_CLASSES,
	.register_classes      = riscv_reg_classes,
	.init                  = riscv_init,
	.finish                = riscv_finish,
	.generate_code         = riscv_generate_code,
	.lower_for_target      = riscv_lower_for_target,
	.get_op_estimated_cost = riscv_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_riscv32)
void be_init_arch_riscv32(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *riscv_grp = lc_opt_get_grp(be_grp, "riscv32");
	lc_opt_add_table(riscv_grp, riscv_options);
}
