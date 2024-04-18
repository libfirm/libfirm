/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Handles fpu rounding modes
 * @author  Matthias Braun
 *
 * The problem we deal with here is that the x86 ABI says the user can control
 * the fpu rounding mode, which means that when we do some operations like float
 * to int conversion which are specified as truncation in the C standard we have
 * to spill, change and restore the fpu rounding mode between spills.
 */
#include "array.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "bessaconstr.h"
#include "bestate.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_architecture.h"
#include "ia32_bearch_t.h"
#include "ia32_new_nodes.h"
#include "ia32_transform.h"
#include "ircons.h"
#include "irgwalk.h"
#include "tv.h"

static ir_entity *fpcw_round    = NULL;
static ir_entity *fpcw_truncate = NULL;

static ir_entity *create_ent(ir_entity **const dst, int value, const char *name)
{
	if (!*dst) {
		ir_mode   *const mode = mode_Hu;
		ir_type   *const type = get_type_for_mode(mode);
		ir_type   *const glob = get_glob_type();
		ident     *const id   = new_id_from_str(name);
		ir_entity *const ent
			= new_global_entity(glob, id, type, ir_visibility_local,
			                    IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

		ir_tarval        *const cnst = new_tarval_from_long(value, mode);
		ir_initializer_t *const init = create_initializer_tarval(cnst);
		set_entity_initializer(ent, init);
		*dst = ent;
	}
	return *dst;
}

static ir_node *create_fnstcw(ir_node *const block, ir_node *const frame, ir_node *const noreg, ir_node *const nomem, ir_node *const state)
{
	ir_node *const fnstcw = new_bd_ia32_FnstCW(NULL, block, frame, noreg, nomem,
	                                           state);
	ia32_attr_t *const attr = get_ia32_attr(fnstcw);
	attr->addr.variant = X86_ADDR_BASE;
	set_ia32_op_type(fnstcw, ia32_AddrModeD);
	set_ia32_frame_use(fnstcw, IA32_FRAME_USE_32BIT);
	return fnstcw;
}

static ir_node *create_fpu_mode_spill(void *const env, ir_node *const state, bool const force, ir_node *const after)
{
	(void)env;

	if (!force && is_ia32_ChangeCW(state))
		return NULL;

	ir_node       *spill;
	ir_node *const block = get_nodes_block(state);
	/* Don't spill the fpcw in unsafe mode. */
	if (ia32_cg_config.use_unsafe_floatconv) {
		spill = new_bd_ia32_FnstCWNOP(NULL, block, state);
	} else {
		ir_graph *const irg   = get_irn_irg(state);
		ir_node  *const noreg = ia32_new_NoReg_gp(irg);
		ir_node  *const nomem = get_irg_no_mem(irg);
		ir_node  *const frame = get_irg_frame(irg);
		spill = create_fnstcw(block, frame, noreg, nomem, state);
	}
	sched_add_after(skip_Proj(after), spill);
	return spill;
}

static ir_node *create_fpu_mode_reload(void *const env, ir_node *const state, ir_node *const spill, ir_node *const before, ir_node *const last_state)
{
	(void)env;
	(void)state;

	ir_node        *reload;
	ir_node  *const block = get_nodes_block(before);
	ir_graph *const irg   = get_irn_irg(block);
	ir_node  *const noreg = ia32_new_NoReg_gp(irg);
	ir_node  *const nomem = get_irg_no_mem(irg);
	if (ia32_cg_config.use_unsafe_floatconv) {
		reload = new_bd_ia32_FldCW(NULL, block, noreg, noreg, nomem);
		ir_entity *const rounding_mode = spill ?
			create_ent(&fpcw_round,    0xC7F, "_fpcw_round") :
			create_ent(&fpcw_truncate, 0x37F, "_fpcw_truncate");
		ia32_attr_t *const attr = get_ia32_attr(reload);
		attr->addr.immediate.entity = rounding_mode;
		attr->addr.immediate.kind   = X86_IMM_ADDR;
		attr->addr.variant          = X86_ADDR_JUST_IMM;
	} else {
		ir_node       *mem;
		ir_node *const frame = get_irg_frame(irg);
		if (spill) {
			mem = spill;
		} else {
			assert(last_state);
			ir_node *const cwstore = create_fnstcw(block, frame, noreg, nomem, last_state);
			sched_add_before(before, cwstore);

			ir_node *const load = new_bd_ia32_Load(NULL, block, frame, noreg, cwstore, X86_SIZE_16, false);
			ia32_attr_t *const load_attr = get_ia32_attr(load);
			load_attr->addr.variant = X86_ADDR_BASE;
			set_ia32_op_type(load, ia32_AddrModeS);
			set_ia32_frame_use(load, IA32_FRAME_USE_32BIT);
			sched_add_before(before, load);

			ir_node *const load_res = be_new_Proj(load, pn_ia32_Load_res);

			/* TODO: Make the actual mode configurable in ChangeCW. */
			ir_node *const or_const = ia32_create_Immediate(irg, 0xC00);
			ir_node *const orn      = new_bd_ia32_Or(NULL, block, noreg, noreg,
			                                         nomem, load_res, or_const,
			                                         X86_SIZE_32);
			sched_add_before(before, orn);

			ir_node *const store = new_bd_ia32_Store(NULL, block, frame, noreg,
			                                         nomem, orn, X86_SIZE_32);
			ia32_attr_t *const store_attr = get_ia32_attr(store);
			/* Use ia32_mode_gp, as movl has a shorter opcode than movw. */
			store_attr->addr.variant = X86_ADDR_BASE;
			set_ia32_op_type(store, ia32_AddrModeD);
			set_ia32_frame_use(store, IA32_FRAME_USE_32BIT);
			sched_add_before(before, store);
			mem = be_new_Proj(store, pn_ia32_Store_M);
		}

		reload = new_bd_ia32_FldCW(NULL, block, frame, noreg, mem);
		ia32_attr_t *const attr = get_ia32_attr(reload);
		attr->addr.variant = X86_ADDR_BASE;
		set_ia32_frame_use(reload, IA32_FRAME_USE_32BIT);
	}

	get_ia32_attr(reload)->size = X86_SIZE_16;
	set_ia32_op_type(reload, ia32_AddrModeS);
	arch_set_irn_register(reload, &ia32_registers[REG_FPCW]);
	sched_add_before(before, reload);
	return reload;
}

typedef struct collect_fpu_mode_nodes_env_t {
	ir_node **state_nodes;
} collect_fpu_mode_nodes_env_t;

static void collect_fpu_mode_nodes_walker(ir_node *node, void *data)
{
	if (is_Proj(node) || is_ia32_ChangeCW(node))
		return;

	collect_fpu_mode_nodes_env_t *env = (collect_fpu_mode_nodes_env_t*)data;
	be_foreach_out(node, o) {
		const arch_register_t *reg = arch_get_irn_register_out(node, o);
		if (reg != &ia32_registers[REG_FPCW])
			continue;
		ir_node *value = node;
		if (get_irn_mode(value) == mode_T)
			value = be_get_or_make_Proj_for_pn(node, o);
		ARR_APP1(ir_node*, env->state_nodes, value);
	}
}

static void rewire_fpu_mode_nodes(ir_graph *irg)
{
	/* do ssa construction for the fpu modes */
	collect_fpu_mode_nodes_env_t env;
	env.state_nodes = NEW_ARR_F(ir_node*, 0);
	irg_walk_graph(irg, collect_fpu_mode_nodes_walker, NULL, &env);

	/* nothing needs to be done, in fact we must not continue as for endless
	 * loops noone is using the initial_value and it will point to a bad node
	 * now */
	if (ARR_LEN(env.state_nodes) == 0) {
		DEL_ARR_F(env.state_nodes);
		return;
	}

	be_ssa_construction_env_t senv;
	be_ssa_construction_init(&senv, irg);
	be_ssa_construction_add_copies(&senv, env.state_nodes,
	                               ARR_LEN(env.state_nodes));
	arch_register_t const *const reg           = &ia32_registers[REG_FPCW];
	ir_node               *const initial_value = be_get_Start_proj(irg, reg);
	be_ssa_construction_fix_users(&senv, initial_value);

	/* set registers for the phis */
	ir_node **phis = be_ssa_construction_get_new_phis(&senv);
	for (size_t i = 0, len = ARR_LEN(phis); i < len; ++i) {
		ir_node *phi = phis[i];
		arch_set_irn_register(phi, reg);
	}
	be_ssa_construction_destroy(&senv);
	DEL_ARR_F(env.state_nodes);

	be_invalidate_live_sets(irg);
}

void ia32_setup_fpu_mode(ir_graph *irg)
{
	/* do ssa construction for the fpu modes */
	rewire_fpu_mode_nodes(irg);

	/* ensure correct fpu mode for operations */
	be_assure_state(irg, &ia32_registers[REG_FPCW],
	                NULL, create_fpu_mode_spill, create_fpu_mode_reload);
}
