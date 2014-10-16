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
#include "bearch_ia32_t.h"
#include "ia32_fpu.h"
#include "ia32_new_nodes.h"
#include "ia32_architecture.h"
#include "gen_ia32_regalloc_if.h"

#include "ircons.h"
#include "irgwalk.h"
#include "tv.h"
#include "array.h"

#include "bearch.h"
#include "besched.h"
#include "benode.h"
#include "bestate.h"
#include "beutil.h"
#include "bessaconstr.h"

static ir_entity *fpcw_round    = NULL;
static ir_entity *fpcw_truncate = NULL;

static ir_entity *create_ent(int value, const char *name)
{
	ir_mode   *mode = mode_Hu;
	ir_type   *type = new_type_primitive(mode);
	set_type_alignment_bytes(type, 4);
	ir_type   *glob = get_glob_type();
	ir_entity *ent = new_entity(glob, new_id_from_str(name), type);
	set_entity_ld_ident(ent, get_entity_ident(ent));
	set_entity_visibility(ent, ir_visibility_local);
	add_entity_linkage(ent, IR_LINKAGE_CONSTANT);

	ir_graph *cnst_irg = get_const_code_irg();
	ir_node  *cnst     = new_r_Const_long(cnst_irg, mode, value);
	set_atomic_ent_value(ent, cnst);

	return ent;
}

static void create_fpcw_entities(void)
{
	fpcw_round    = create_ent(0xc7f, "_fpcw_round");
	fpcw_truncate = create_ent(0x37f, "_fpcw_truncate");
}

static ir_node *create_fpu_mode_spill(void *env, ir_node *state, bool force,
                                      ir_node *after)
{
	(void)env;

	/* we don't spill the fpcw in unsafe mode */
	if (ia32_cg_config.use_unsafe_floatconv) {
		ir_node *block = get_nodes_block(state);
		if (force || !is_ia32_ChangeCW(state)) {
			ir_node *spill = new_bd_ia32_FnstCWNOP(NULL, block, state);
			sched_add_after(after, spill);
			return spill;
		}
		return NULL;
	}

	if (force || !is_ia32_ChangeCW(state)) {
		ir_graph *irg   = get_irn_irg(state);
		ir_node  *block = get_nodes_block(state);
		ir_node  *noreg = ia32_new_NoReg_gp(irg);
		ir_node  *nomem = get_irg_no_mem(irg);
		ir_node  *frame = get_irg_frame(irg);
		ir_node  *spill
			= new_bd_ia32_FnstCW(NULL, block, frame, noreg, nomem, state);
		set_ia32_op_type(spill, ia32_AddrModeD);
		/* use ia32_mode_gp, as movl has a shorter opcode than movw */
		set_ia32_ls_mode(spill, ia32_mode_gp);
		set_ia32_use_frame(spill);

		sched_add_after(skip_Proj(after), spill);
		return spill;
	}
	return NULL;
}

static void set_32bit_stackent(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.need_32bit_stackent = true;
}

static ir_node *create_fldcw_ent(ir_node *block, ir_entity *entity)
{
	ir_graph *irg    = get_irn_irg(block);
	ir_node  *nomem  = get_irg_no_mem(irg);
	ir_node  *noreg  = ia32_new_NoReg_gp(irg);
	ir_node  *reload = new_bd_ia32_FldCW(NULL, block, noreg, noreg, nomem);
	set_ia32_op_type(reload, ia32_AddrModeS);
	set_ia32_ls_mode(reload, ia32_reg_classes[CLASS_ia32_fp_cw].mode);
	set_ia32_am_ent(reload, entity);
	set_ia32_use_frame(reload);
	arch_set_irn_register(reload, &ia32_registers[REG_FPCW]);
	set_32bit_stackent(reload);

	return reload;
}

static ir_node *create_fpu_mode_reload(void *env, ir_node *state,
                                       ir_node *spill, ir_node *before,
                                       ir_node *last_state)
{
	(void)env;
	ir_graph *irg    = get_irn_irg(state);
	ir_node  *block  = get_nodes_block(before);
	ir_node  *frame  = get_irg_frame(irg);
	ir_node  *noreg  = ia32_new_NoReg_gp(irg);
	ir_node  *reload = NULL;

	if (ia32_cg_config.use_unsafe_floatconv) {
		if (fpcw_round == NULL)
			create_fpcw_entities();
		if (spill != NULL) {
			reload = create_fldcw_ent(block, fpcw_round);
		} else {
			reload = create_fldcw_ent(block, fpcw_truncate);
		}
		sched_add_before(before, reload);
		return reload;
	}

	if (spill != NULL) {
		reload = new_bd_ia32_FldCW(NULL, block, frame, noreg, spill);
		set_ia32_op_type(reload, ia32_AddrModeS);
		set_ia32_ls_mode(reload, ia32_reg_classes[CLASS_ia32_fp_cw].mode);
		set_ia32_use_frame(reload);
		set_32bit_stackent(reload);
		arch_set_irn_register(reload, &ia32_registers[REG_FPCW]);

		sched_add_before(before, reload);
	} else {
		ir_mode *lsmode = mode_Hu;
		ir_node *nomem  = get_irg_no_mem(irg);
		ir_node *cwstore, *load, *load_res, *orn, *store, *fldcw;
		ir_node *store_proj;
		ir_node *or_const;

		assert(last_state != NULL);
		cwstore = new_bd_ia32_FnstCW(NULL, block, frame, noreg, nomem,
		                             last_state);
		set_ia32_op_type(cwstore, ia32_AddrModeD);
		set_ia32_ls_mode(cwstore, lsmode);
		set_ia32_use_frame(cwstore);
		set_32bit_stackent(cwstore);
		sched_add_before(before, cwstore);

		load = new_bd_ia32_Load(NULL, block, frame, noreg, cwstore);
		set_ia32_op_type(load, ia32_AddrModeS);
		set_ia32_ls_mode(load, lsmode);
		set_ia32_use_frame(load);
		set_32bit_stackent(load);
		sched_add_before(before, load);

		load_res = new_r_Proj(load, ia32_mode_gp, pn_ia32_Load_res);

		/* TODO: make the actual mode configurable in ChangeCW... */
		or_const = new_bd_ia32_Immediate(NULL, get_irg_start_block(irg),
		                                 NULL, 0, 3072);
		arch_set_irn_register(or_const, &ia32_registers[REG_GP_NOREG]);
		orn = new_bd_ia32_Or(NULL, block, noreg, noreg, nomem, load_res,
		                    or_const);
		sched_add_before(before, orn);

		store = new_bd_ia32_Store(NULL, block, frame, noreg, nomem, orn);
		set_ia32_op_type(store, ia32_AddrModeD);
		/* use ia32_mode_gp, as movl has a shorter opcode than movw */
		set_ia32_ls_mode(store, ia32_mode_gp);
		set_ia32_use_frame(store);
		set_32bit_stackent(store);
		store_proj = new_r_Proj(store, mode_M, pn_ia32_Store_M);
		sched_add_before(before, store);

		fldcw = new_bd_ia32_FldCW(NULL, block, frame, noreg, store_proj);
		set_ia32_op_type(fldcw, ia32_AddrModeS);
		set_ia32_ls_mode(fldcw, lsmode);
		set_ia32_use_frame(fldcw);
		set_32bit_stackent(fldcw);
		arch_set_irn_register(fldcw, &ia32_registers[REG_FPCW]);
		sched_add_before(before, fldcw);

		reload = fldcw;
	}

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
	for (unsigned o = 0, n = arch_get_irn_n_outs(node); o < n; ++o) {
		const arch_register_t *reg = arch_get_irn_register_out(node, o);
		if (reg != &ia32_registers[REG_FPCW])
			continue;
		ir_node *value = node;
		if (get_irn_mode(value) == mode_T) {
			value = be_get_Proj_for_pn(node, o);
			if (value == NULL)
				value = new_r_Proj(node, ia32_mode_fpcw, o);
		}
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

	const arch_register_t *reg = &ia32_registers[REG_FPCW];
	ir_node *initial_value = be_get_initial_reg_value(irg, reg);
	be_ssa_construction_env_t senv;
	be_ssa_construction_init(&senv, irg);
	be_ssa_construction_add_copies(&senv, env.state_nodes,
	                               ARR_LEN(env.state_nodes));
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
