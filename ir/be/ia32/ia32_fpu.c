/**
 * @file
 * @brief   Handles fpu rounding modes
 * @author  Matthias Braun
 * @version $Id$
 *
 * The problem we deal with here is that the x86 ABI says the user can control
 * the fpu rounding mode, which means that when we do some operations like float
 * to int conversion which are specified as truncation in the C standard we have
 * to spill, change and restore the fpu rounding mode between spills.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ia32_fpu.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"

#include "ircons.h"
#include "irgwalk.h"
#include "tv.h"
#include "array.h"

#include "../beirgmod.h"
#include "../bearch.h"
#include "../besched.h"
#include "../beabi.h"
#include "../benode_t.h"
#include "../bestate.h"
#include "../beutil.h"
#include "../bessaconstr.h"

static ir_node *create_fpu_mode_spill(void *env, ir_node *state, int force,
                                      ir_node *after)
{
	ia32_code_gen_t *cg = env;
	ir_node *spill = NULL;

	if(force == 1 || !is_ia32_ChangeCW(state)) {
		ir_graph *irg = get_irn_irg(state);
		ir_node *block = get_nodes_block(state);
		ir_node *noreg = ia32_new_NoReg_gp(cg);
		ir_node *nomem = new_NoMem();
		ir_node *frame = get_irg_frame(irg);

		spill = new_rd_ia32_FnstCW(NULL, irg, block, frame, noreg, state,
		                           nomem);
		set_ia32_am_support(spill, ia32_am_Dest);
		set_ia32_op_type(spill, ia32_AddrModeD);
		set_ia32_am_flavour(spill, ia32_B);
		set_ia32_ls_mode(spill, ia32_reg_classes[CLASS_ia32_fp_cw].mode);
		set_ia32_use_frame(spill);

		sched_add_after(after, spill);
	}

	return spill;
}

static ir_node *create_fpu_mode_reload(void *env, ir_node *state,
                                       ir_node *spill, ir_node *before,
                                       ir_node *last_state)
{
	ia32_code_gen_t *cg = env;
	ir_graph *irg = get_irn_irg(state);
	ir_node *block = get_nodes_block(before);
	ir_node *frame = get_irg_frame(irg);
	ir_node *noreg = ia32_new_NoReg_gp(cg);
	ir_node *reload = NULL;

	if(spill != NULL) {
		reload = new_rd_ia32_FldCW(NULL, irg, block, frame, noreg, spill);
		set_ia32_am_support(reload, ia32_am_Source);
		set_ia32_op_type(reload, ia32_AddrModeS);
		set_ia32_am_flavour(reload, ia32_B);
		set_ia32_ls_mode(reload, ia32_reg_classes[CLASS_ia32_fp_cw].mode);
		set_ia32_use_frame(reload);
		arch_set_irn_register(cg->arch_env, reload, &ia32_fp_cw_regs[REG_FPCW]);

		sched_add_before(before, reload);
	} else {
		ir_mode *lsmode = ia32_reg_classes[CLASS_ia32_fp_cw].mode;
		ir_node *nomem = new_NoMem();
		ir_node *cwstore, *load, *load_res, *or, *store, *fldcw;

		assert(last_state != NULL);
		cwstore = new_rd_ia32_FnstCW(NULL, irg, block, frame, noreg, last_state,
		                             nomem);
		set_ia32_am_support(cwstore, ia32_am_Dest);
		set_ia32_op_type(cwstore, ia32_AddrModeD);
		set_ia32_am_flavour(cwstore, ia32_B);
		set_ia32_ls_mode(cwstore, lsmode);
		set_ia32_use_frame(cwstore);
		sched_add_before(before, cwstore);

		load = new_rd_ia32_Load(NULL, irg, block, frame, noreg, cwstore);
		set_ia32_am_support(load, ia32_am_Source);
		set_ia32_op_type(load, ia32_AddrModeS);
		set_ia32_am_flavour(load, ia32_B);
		set_ia32_ls_mode(load, lsmode);
		set_ia32_use_frame(load);
		sched_add_before(before, load);

		load_res = new_r_Proj(irg, block, load, mode_Iu, pn_ia32_Load_res);
		sched_add_before(before, load_res);

		/* TODO: make the actual mode configurable in ChangeCW... */
		or = new_rd_ia32_Or(NULL, irg, block, noreg, noreg, load_res, noreg,
		                    nomem);
		set_ia32_Immop_tarval(or, new_tarval_from_long(3072, mode_Iu));
		sched_add_before(before, or);

		store = new_rd_ia32_Store(NULL, irg, block, frame, noreg, or, nomem);
		set_ia32_am_support(store, ia32_am_Dest);
		set_ia32_op_type(store, ia32_AddrModeD);
		set_ia32_am_flavour(store, ia32_B);
		set_ia32_ls_mode(store, lsmode);
		set_ia32_use_frame(store);
		sched_add_before(before, store);

		fldcw = new_rd_ia32_FldCW(NULL, irg, block, frame, noreg, store);
		set_ia32_am_support(fldcw, ia32_am_Source);
		set_ia32_op_type(fldcw, ia32_AddrModeS);
		set_ia32_am_flavour(fldcw, ia32_B);
		set_ia32_ls_mode(fldcw, lsmode);
		set_ia32_use_frame(fldcw);
		arch_set_irn_register(cg->arch_env, fldcw, &ia32_fp_cw_regs[REG_FPCW]);
		sched_add_before(before, fldcw);

		reload = fldcw;
	}

	return reload;
}

typedef struct collect_fpu_mode_nodes_env_t {
	const arch_env_t *arch_env;
	ir_node         **state_nodes;
} collect_fpu_mode_nodes_env_t;

static
void collect_fpu_mode_nodes_walker(ir_node *node, void *data)
{
	collect_fpu_mode_nodes_env_t *env = data;

	const arch_register_t *reg = arch_get_irn_register(env->arch_env, node);
	if(reg == &ia32_fp_cw_regs[REG_FPCW] && !is_ia32_ChangeCW(node)) {
		ARR_APP1(ir_node*, env->state_nodes, node);
	}
}

static
void rewire_fpu_mode_nodes(be_irg_t *birg)
{
	collect_fpu_mode_nodes_env_t env;
	be_ssa_construction_env_t senv;
	const arch_register_t *reg = &ia32_fp_cw_regs[REG_FPCW];
	ir_graph *irg = be_get_birg_irg(birg);
	ir_node *initial_value;
	ir_node **phis;
	be_lv_t *lv = be_get_birg_liveness(birg);
	int i, len;

	/* do ssa construction for the fpu modes */
	env.arch_env = birg->main_env->arch_env;
	env.state_nodes = NEW_ARR_F(ir_node*, 0);
	irg_walk_graph(irg, collect_fpu_mode_nodes_walker, NULL, &env);

	initial_value = be_abi_get_ignore_irn(birg->abi, reg);

	/* nothing needs to be done, in fact we must not continue as for endless
	 * loops noone is using the initial_value and it will point to a bad node
	 * now
	 */
	if(ARR_LEN(env.state_nodes) == 0) {
		DEL_ARR_F(env.state_nodes);
		return;
	}

	be_ssa_construction_init(&senv, birg);
	be_ssa_construction_add_copies(&senv, env.state_nodes,
	                               ARR_LEN(env.state_nodes));
	be_ssa_construction_fix_users(&senv, initial_value);

	if(lv != NULL) {
		be_ssa_construction_update_liveness_phis(&senv, lv);
		be_liveness_update(lv, initial_value);
		len = ARR_LEN(env.state_nodes);
		for(i = 0; i < len; ++i) {
			be_liveness_update(lv, env.state_nodes[i]);
		}
	}

	/* set registers for the phis */
	phis = be_ssa_construction_get_new_phis(&senv);
	len = ARR_LEN(phis);
	for(i = 0; i < len; ++i) {
		ir_node *phi = phis[i];
		be_set_phi_flags(env.arch_env, phi, arch_irn_flags_ignore);
		arch_set_irn_register(env.arch_env, phi, reg);
	}
	be_ssa_construction_destroy(&senv);
	DEL_ARR_F(env.state_nodes);
}

void ia32_setup_fpu_mode(ia32_code_gen_t *cg)
{
	/* do ssa construction for the fpu modes */
	rewire_fpu_mode_nodes(cg->birg);

	/* ensure correct fpu mode for operations */
	be_assure_state(cg->birg, &ia32_fp_cw_regs[REG_FPCW],
	                cg, create_fpu_mode_spill, create_fpu_mode_reload);
}
