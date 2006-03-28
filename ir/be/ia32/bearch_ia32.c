/**
 * This is the main ia32 firm backend driver.
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"

#include "bitset.h"
#include "debug.h"

#include "../beabi.h"                 /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "../be.h"
#include "bearch_ia32_t.h"

#include "ia32_new_nodes.h"           /* ia32 nodes interface */
#include "gen_ia32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "ia32_gen_decls.h"           /* interface declaration emitter */
#include "ia32_transform.h"
#include "ia32_emitter.h"
#include "ia32_map_regs.h"
#include "ia32_optimize.h"
#include "ia32_x87.h"

#define DEBUG_MODULE "firm.be.ia32.isa"

/* TODO: ugly */
static set *cur_reg_set = NULL;

#undef is_Start
#define is_Start(irn) (get_irn_opcode(irn) == iro_Start)

ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi, &ia32_gp_regs[REG_GP_NOREG]);
}

ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi,
		USE_SSE2(cg) ? &ia32_xmm_regs[REG_XMM_NOREG] : &ia32_vfp_regs[REG_VFP_NOREG]);
}

/**************************************************
 *                         _ _              _  __
 *                        | | |            (_)/ _|
 *  _ __ ___  __ _    __ _| | | ___   ___   _| |_
 * | '__/ _ \/ _` |  / _` | | |/ _ \ / __| | |  _|
 * | | |  __/ (_| | | (_| | | | (_) | (__  | | |
 * |_|  \___|\__, |  \__,_|_|_|\___/ \___| |_|_|
 *            __/ |
 *           |___/
 **************************************************/

static ir_node *my_skip_proj(const ir_node *n) {
	while (is_Proj(n))
		n = get_Proj_pred(n);
	return (ir_node *)n;
}


/**
 * Return register requirements for an ia32 node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const arch_register_req_t *ia32_get_irn_reg_req(const void *self, arch_register_req_t *req, const ir_node *irn, int pos) {
	const ia32_irn_ops_t      *ops = self;
	const ia32_register_req_t *irn_req;
	long                       node_pos = pos == -1 ? 0 : pos;
	ir_mode                   *mode     = is_Block(irn) ? NULL : get_irn_mode(irn);
	FIRM_DBG_REGISTER(firm_dbg_module_t *mod, DEBUG_MODULE);

	if (is_Block(irn) || mode == mode_M || mode == mode_X) {
		DBG((mod, LEVEL_1, "ignoring Block, mode_M, mode_X node %+F\n", irn));
		return NULL;
	}

	if (mode == mode_T && pos < 0) {
		DBG((mod, LEVEL_1, "ignoring request OUT requirements for node %+F\n", irn));
		return NULL;
	}

	DBG((mod, LEVEL_1, "get requirements at pos %d for %+F ... ", pos, irn));

	if (is_Proj(irn)) {
		if (pos == -1) {
			node_pos = ia32_translate_proj_pos(irn);
		}
		else {
			node_pos = pos;
		}

		irn = my_skip_proj(irn);

		DB((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", irn, node_pos));
	}

	if (is_ia32_irn(irn)) {
		if (pos >= 0) {
			irn_req = get_ia32_in_req(irn, pos);
		}
		else {
			irn_req = get_ia32_out_req(irn, node_pos);
		}

		DB((mod, LEVEL_1, "returning reqs for %+F at pos %d\n", irn, pos));

		memcpy(req, &(irn_req->req), sizeof(*req));

		if (arch_register_req_is(&(irn_req->req), should_be_same)) {
			assert(irn_req->same_pos >= 0 && "should be same constraint for in -> out NYI");
			req->other_same = get_irn_n(irn, irn_req->same_pos);
		}

		if (arch_register_req_is(&(irn_req->req), should_be_different)) {
			assert(irn_req->different_pos >= 0 && "should be different constraint for in -> out NYI");
			req->other_different = get_irn_n(irn, irn_req->different_pos);
		}
	}
	else {
		/* treat Phi like Const with default requirements */
		if (is_Phi(irn)) {
			DB((mod, LEVEL_1, "returning standard reqs for %+F\n", irn));
			if (mode_is_float(mode)) {
				if (USE_SSE2(ops->cg))
					memcpy(req, &(ia32_default_req_ia32_xmm.req), sizeof(*req));
				else
					memcpy(req, &(ia32_default_req_ia32_vfp.req), sizeof(*req));
			}
			else if (mode_is_int(mode) || mode_is_reference(mode))
				memcpy(req, &(ia32_default_req_ia32_gp.req), sizeof(*req));
			else if (mode == mode_T || mode == mode_M) {
				DBG((mod, LEVEL_1, "ignoring Phi node %+F\n", irn));
				return NULL;
			}
			else
				assert(0 && "unsupported Phi-Mode");
		}
		else {
			DB((mod, LEVEL_1, "returning NULL for %+F (not ia32)\n", irn));
			req = NULL;
		}
	}

	return req;
}

static void ia32_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg) {
	int                   pos = 0;
	const ia32_irn_ops_t *ops = self;

	if (get_irn_mode(irn) == mode_X) {
		return;
	}

	DBG((ops->cg->mod, LEVEL_1, "ia32 assigned register %s to node %+F\n", reg->name, irn));

	if (is_Proj(irn)) {
		pos = ia32_translate_proj_pos(irn);
		irn = my_skip_proj(irn);
	}

	if (is_ia32_irn(irn)) {
		const arch_register_t **slots;

		slots      = get_ia32_slots(irn);
		slots[pos] = reg;
	}
	else {
		ia32_set_firm_reg(irn, reg, cur_reg_set);
	}
}

static const arch_register_t *ia32_get_irn_reg(const void *self, const ir_node *irn) {
	int pos = 0;
	const arch_register_t *reg = NULL;

	if (is_Proj(irn)) {

		if (get_irn_mode(irn) == mode_X) {
			return NULL;
		}

		pos = ia32_translate_proj_pos(irn);
		irn = my_skip_proj(irn);
	}

	if (is_ia32_irn(irn)) {
		const arch_register_t **slots;
		slots = get_ia32_slots(irn);
		reg   = slots[pos];
	}
	else {
		reg = ia32_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t ia32_classify(const void *self, const ir_node *irn) {
	irn = my_skip_proj(irn);
	if (is_cfop(irn))
		return arch_irn_class_branch;
	else if (is_ia32_irn(irn))
		return arch_irn_class_normal;
	else
		return 0;
}

static arch_irn_flags_t ia32_get_flags(const void *self, const ir_node *irn) {
	irn = my_skip_proj(irn);
	if (is_ia32_irn(irn))
		return get_ia32_flags(irn);
	else {
		return 0;
	}
}

static entity *ia32_get_frame_entity(const void *self, const ir_node *irn) {
	return is_ia32_irn(irn) ? get_ia32_frame_ent(irn) : NULL;
}

static void ia32_set_stack_bias(const void *self, ir_node *irn, int bias) {
	char buf[64];
	const ia32_irn_ops_t *ops = self;

	if (is_ia32_use_frame(irn) && bias != 0) {
		ia32_am_flavour_t am_flav = get_ia32_am_flavour(irn);

		DBG((ops->cg->mod, LEVEL_1, "stack biased %+F with %d\n", irn, bias));
		snprintf(buf, sizeof(buf), "%d", bias);
		add_ia32_am_offs(irn, buf);
		am_flav |= ia32_O;
		set_ia32_am_flavour(irn, am_flav);
	}
}

typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_isa_t *isa;
	ir_graph *irg;
} ia32_abi_env_t;

static void *ia32_abi_init(const be_abi_call_t *call, const arch_env_t *aenv, ir_graph *irg)
{
	ia32_abi_env_t *env    = xmalloc(sizeof(env[0]));
	be_abi_call_flags_t fl = be_abi_call_get_flags(call);
	env->flags = fl.bits;
	env->irg   = irg;
	env->isa   = aenv->isa;
	return env;
}

static void ia32_abi_dont_save_regs(void *self, pset *s)
{
	ia32_abi_env_t *env = self;
	if(env->flags.try_omit_fp)
		pset_insert_ptr(s, env->isa->bp);
}

static const arch_register_t *ia32_abi_prologue(void *self, ir_node **mem, pmap *reg_map)
{
	ia32_abi_env_t *env              = self;
	const arch_register_t *frame_reg = env->isa->sp;

	if(!env->flags.try_omit_fp) {
		int reg_size         = get_mode_size_bytes(env->isa->bp->reg_class->mode);
		ir_node *bl          = get_irg_start_block(env->irg);
		ir_node *curr_sp     = be_abi_reg_map_get(reg_map, env->isa->sp);
		ir_node *curr_bp     = be_abi_reg_map_get(reg_map, env->isa->bp);
		ir_node *curr_no_reg = be_abi_reg_map_get(reg_map, &ia32_gp_regs[REG_GP_NOREG]);
		ir_node *store_bp;

		curr_sp  = be_new_IncSP(env->isa->sp, env->irg, bl, curr_sp, *mem, reg_size, be_stack_dir_expand);
		store_bp = new_rd_ia32_Store(NULL, env->irg, bl, curr_sp, curr_no_reg, curr_bp, *mem, mode_T);
		set_ia32_am_support(store_bp, ia32_am_Dest);
		set_ia32_am_flavour(store_bp, ia32_B);
		set_ia32_op_type(store_bp, ia32_AddrModeD);
		*mem     = new_r_Proj(env->irg, bl, store_bp, mode_M, 0);
		curr_bp  = be_new_Copy(env->isa->bp->reg_class, env->irg, bl, curr_sp);
		be_set_constr_single_reg(curr_bp, BE_OUT_POS(0), env->isa->bp);
		be_node_set_flags(curr_bp, BE_OUT_POS(0), arch_irn_flags_ignore);

		be_abi_reg_map_set(reg_map, env->isa->sp, curr_sp);
		be_abi_reg_map_set(reg_map, env->isa->bp, curr_bp);
	}

	return frame_reg;
}

static void ia32_abi_epilogue(void *self, ir_node *bl, ir_node **mem, pmap *reg_map)
{
	ia32_abi_env_t *env = self;
	ir_node *curr_sp     = be_abi_reg_map_get(reg_map, env->isa->sp);
	ir_node *curr_bp     = be_abi_reg_map_get(reg_map, env->isa->bp);
	ir_node *curr_no_reg = be_abi_reg_map_get(reg_map, &ia32_gp_regs[REG_GP_NOREG]);

	if(env->flags.try_omit_fp) {
		curr_sp = be_new_IncSP(env->isa->sp, env->irg, bl, curr_sp, *mem, BE_STACK_FRAME_SIZE, be_stack_dir_shrink);
	}

	else {
		ir_node *load_bp;
		ir_mode *mode_bp = env->isa->bp->reg_class->mode;

		curr_sp = be_new_SetSP(env->isa->sp, env->irg, bl, curr_sp, curr_bp, *mem);
		load_bp = new_rd_ia32_Load(NULL, env->irg, bl, curr_sp, curr_no_reg, *mem, mode_T);
		set_ia32_am_support(load_bp, ia32_am_Source);
		set_ia32_am_flavour(load_bp, ia32_B);
		set_ia32_op_type(load_bp, ia32_AddrModeS);
		set_ia32_ls_mode(load_bp, mode_bp);
		curr_bp = new_r_Proj(env->irg, bl, load_bp, mode_bp, 0);
		*mem    = new_r_Proj(env->irg, bl, load_bp, mode_M, 1);
	}

	be_abi_reg_map_set(reg_map, env->isa->sp, curr_sp);
	be_abi_reg_map_set(reg_map, env->isa->bp, curr_bp);
}

/**
 * Produces the type which sits between the stack args and the locals on the stack.
 * it will contain the return address and space to store the old base pointer.
 * @return The Firm type modeling the ABI between type.
 */
static ir_type *ia32_abi_get_between_type(void *self)
{
	static ir_type *omit_fp_between_type = NULL;
	static ir_type *between_type         = NULL;

	ia32_abi_env_t *env = self;

	if(!between_type) {
		entity *old_bp_ent;
		entity *ret_addr_ent;
		entity *omit_fp_ret_addr_ent;

		ir_type *old_bp_type   = new_type_primitive(new_id_from_str("bp"), mode_P);
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);

		between_type           = new_type_class(new_id_from_str("ia32_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("ret_addr"), ret_addr_type);

		set_entity_offset_bytes(old_bp_ent, 0);
		set_entity_offset_bytes(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));

		omit_fp_between_type   = new_type_class(new_id_from_str("ia32_between_type_omit_fp"));
		omit_fp_ret_addr_ent   = new_entity(omit_fp_between_type, new_id_from_str("ret_addr"), ret_addr_type);

		set_entity_offset_bytes(omit_fp_ret_addr_ent, 0);
		set_type_size_bytes(omit_fp_between_type, get_type_size_bytes(ret_addr_type));
	}

	return env->flags.try_omit_fp ? omit_fp_between_type : between_type;
}

static const be_abi_callbacks_t ia32_abi_callbacks = {
	ia32_abi_init,
	free,
	ia32_abi_get_between_type,
	ia32_abi_dont_save_regs,
	ia32_abi_prologue,
	ia32_abi_epilogue,
};

/* fill register allocator interface */

static const arch_irn_ops_if_t ia32_irn_ops_if = {
	ia32_get_irn_reg_req,
	ia32_set_irn_reg,
	ia32_get_irn_reg,
	ia32_classify,
	ia32_get_flags,
	ia32_get_frame_entity,
	ia32_set_stack_bias
};

ia32_irn_ops_t ia32_irn_ops = {
	&ia32_irn_ops_if,
	NULL
};



/**************************************************
 *                _                         _  __
 *               | |                       (_)/ _|
 *   ___ ___   __| | ___  __ _  ___ _ __    _| |_
 *  / __/ _ \ / _` |/ _ \/ _` |/ _ \ '_ \  | |  _|
 * | (_| (_) | (_| |  __/ (_| |  __/ | | | | | |
 *  \___\___/ \__,_|\___|\__, |\___|_| |_| |_|_|
 *                        __/ |
 *                       |___/
 **************************************************/

/**
 * Transforms the standard firm graph into
 * an ia32 firm graph
 */
static void ia32_prepare_graph(void *self) {
	ia32_code_gen_t *cg = self;
	firm_dbg_module_t *old_mod = cg->mod;

	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.transform");
	irg_walk_blkwise_graph(cg->irg, ia32_place_consts_set_modes, ia32_transform_node, cg);
	be_dump(cg->irg, "-transformed", dump_ir_block_graph_sched);

	cg->mod = old_mod;

	if (cg->opt.doam) {
		edges_deactivate(cg->irg);
		//dead_node_elimination(cg->irg);
		edges_activate(cg->irg);

		irg_walk_blkwise_graph(cg->irg, NULL, ia32_optimize_am, cg);
		be_dump(cg->irg, "-am", dump_ir_block_graph_sched);
	}
}


/**
 * Insert copies for all ia32 nodes where the should_be_same requirement
 * is not fulfilled.
 * Transform Sub into Neg -- Add if IN2 == OUT
 */
static void ia32_finish_irg_walker(ir_node *irn, void *env) {
	ia32_code_gen_t            *cg = env;
	const ia32_register_req_t **reqs;
	const arch_register_t      *out_reg, *in_reg;
	int                         n_res, i;
	ir_node                    *copy, *in_node, *block;
	ia32_op_type_t              op_tp;

	if (! is_ia32_irn(irn))
		return;

	/* AM Dest nodes don't produce any values  */
	op_tp = get_ia32_op_type(irn);
	if (op_tp == ia32_AddrModeD)
		return;

	reqs  = get_ia32_out_req_all(irn);
	n_res = get_ia32_n_res(irn);
	block = get_nodes_block(irn);

	/* check all OUT requirements, if there is a should_be_same */
	if (op_tp == ia32_Normal) {
		for (i = 0; i < n_res; i++) {
			if (arch_register_req_is(&(reqs[i]->req), should_be_same)) {
				/* get in and out register */
				out_reg = get_ia32_out_reg(irn, i);
				in_node = get_irn_n(irn, reqs[i]->same_pos);
				in_reg  = arch_get_irn_register(cg->arch_env, in_node);

				/* don't copy ignore nodes */
				if (arch_irn_is(cg->arch_env, in_node, ignore))
					continue;

				/* check if in and out register are equal */
				if (arch_register_get_index(out_reg) != arch_register_get_index(in_reg)) {
					DBG((cg->mod, LEVEL_1, "inserting copy for %+F in_pos %d\n", irn, reqs[i]->same_pos));

					/* create copy from in register */
					copy = be_new_Copy(arch_register_get_class(in_reg), cg->irg, block, in_node);

					/* destination is the out register */
					arch_set_irn_register(cg->arch_env, copy, out_reg);

					/* insert copy before the node into the schedule */
					sched_add_before(irn, copy);

					/* set copy as in */
					set_irn_n(irn, reqs[i]->same_pos, copy);
				}
			}
		}
	}

	/* If we have a CondJmp with immediate, we need to    */
	/* check if it's the right operand, otherwise we have */
	/* to change it, as CMP doesn't support immediate as  */
	/* left operands.                                     */
	if (is_ia32_CondJmp(irn) && (is_ia32_ImmConst(irn) || is_ia32_ImmSymConst(irn)) && op_tp == ia32_AddrModeS) {
		long pnc = get_negated_pnc(get_ia32_pncode(irn), get_ia32_res_mode(irn));
		set_ia32_op_type(irn, ia32_AddrModeD);
		set_ia32_pncode(irn, pnc);
	}

	/* check if there is a sub which need to be transformed */
	ia32_transform_sub_to_neg_add(irn, cg);

	/* transform a LEA into an Add if possible */
	ia32_transform_lea_to_add(irn, cg);

	/* check for peephole optimization */
	ia32_peephole_optimization(irn, cg);
}

/**
 * Add Copy nodes for not fulfilled should_be_equal constraints
 */
static void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg) {
	irg_walk_blkwise_graph(irg, NULL, ia32_finish_irg_walker, cg);
}



/**
 * Dummy functions for hooks we don't need but which must be filled.
 */
static void ia32_before_sched(void *self) {
}

/**
 * Called before the register allocator.
 * Calculate a block schedule here. We need it for the x87
 * simulator and the emitter.
 */
static void ia32_before_ra(void *self) {
	ia32_code_gen_t *cg = self;

	cg->blk_sched = sched_create_block_schedule(cg->irg);
}


/**
 * Transforms a be node into a Load.
 */
static void transform_to_Load(ia32_transform_env_t *env) {
	ir_node *irn         = env->irn;
	entity  *ent         = be_get_frame_entity(irn);
	ir_mode *mode        = env->mode;
	ir_node *noreg       = ia32_new_NoReg_gp(env->cg);
	ir_node *nomem       = new_rd_NoMem(env->irg);
	ir_node *sched_point = NULL;
	ir_node *ptr         = get_irn_n(irn, 0);
	ir_node *mem         = be_is_Reload(irn) ? get_irn_n(irn, 1) : nomem;
	ir_node *new_op, *proj;
	const arch_register_t *reg;

	if (sched_is_scheduled(irn)) {
		sched_point = sched_prev(irn);
	}

	if (mode_is_float(mode)) {
		new_op = new_rd_ia32_fLoad(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem, mode_T);
	}

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);
	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, pn_Load_res);

	if (sched_point) {
		sched_add_after(sched_point, new_op);
		sched_add_after(new_op, proj);

		sched_remove(irn);
	}

	/* copy the register from the old node to the new Load */
	reg = arch_get_irn_register(env->cg->arch_env, irn);
	arch_set_irn_register(env->cg->arch_env, new_op, reg);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env));

	exchange(irn, proj);
}

/**
 * Transforms a be node into a Store.
 */
static void transform_to_Store(ia32_transform_env_t *env) {
	ir_node *irn   = env->irn;
	entity  *ent   = be_get_frame_entity(irn);
	ir_mode *mode  = env->mode;
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *nomem = new_rd_NoMem(env->irg);
	ir_node *ptr   = get_irn_n(irn, 0);
	ir_node *val   = get_irn_n(irn, 1);
	ir_node *new_op, *proj;
	ir_node *sched_point = NULL;

	if (sched_is_scheduled(irn)) {
		sched_point = sched_prev(irn);
	}

	if (mode_is_float(mode)) {
		new_op = new_rd_ia32_fStore(env->dbg, env->irg, env->block, ptr, noreg, val, nomem, mode_T);
	}
	else if (get_mode_size_bits(mode) == 8) {
		new_op = new_rd_ia32_Store8Bit(env->dbg, env->irg, env->block, ptr, noreg, val, nomem, mode_T);
	}
	else {
		new_op = new_rd_ia32_Store(env->dbg, env->irg, env->block, ptr, noreg, val, nomem, mode_T);
	}

	set_ia32_am_support(new_op, ia32_am_Dest);
	set_ia32_op_type(new_op, ia32_AddrModeD);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);
	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, 0);

	if (sched_point) {
		sched_add_after(sched_point, new_op);
		sched_add_after(new_op, proj);

		sched_remove(irn);
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env));

	exchange(irn, proj);
}

/**
 * Calls the transform functions for StackParam, Spill and Reload.
 */
static void ia32_after_ra_walker(ir_node *node, void *env) {
	ia32_code_gen_t *cg = env;
	ia32_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block = get_nodes_block(node);
	tenv.dbg   = get_irn_dbg_info(node);
	tenv.irg   = current_ir_graph;
	tenv.irn   = node;
	tenv.mod   = cg->mod;
	tenv.mode  = get_irn_mode(node);
	tenv.cg    = cg;

	/* be_is_StackParam(node) || */
	if (be_is_Reload(node)) {
		transform_to_Load(&tenv);
	}
	else if (be_is_Spill(node)) {
		/* we always spill the whole register  */
		tenv.mode = mode_is_float(get_irn_mode(be_get_Spill_context(node))) ? mode_D : mode_Is;
		transform_to_Store(&tenv);
	}
}

/**
 * We transform StackParam, Spill and Reload here. This needs to be done before
 * stack biasing otherwise we would miss the corrected offset for these nodes.
 */
static void ia32_after_ra(void *self) {
	ia32_code_gen_t *cg = self;
	irg_walk_blkwise_graph(cg->irg, NULL, ia32_after_ra_walker, self);

	/* if we do x87 code generation, rewrite all the virtual instructions and registers */
	if (cg->used_x87) {
		x87_simulate_graph(cg->arch_env, cg->irg, cg->blk_sched);
		be_dump(cg->irg, "-x87", dump_ir_extblock_graph_sched);
	}
}


/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ia32_codegen(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;
	FILE            *out = cg->out;

	if (cg->emit_decls) {
		ia32_gen_decls(cg->out);
		cg->emit_decls = 0;
	}

	ia32_finish_irg(irg, cg);
	be_dump(irg, "-finished", dump_ir_block_graph_sched);
	ia32_gen_routine(out, irg, cg);

	cur_reg_set = NULL;

	pmap_destroy(cg->tv_ent);
	pmap_destroy(cg->types);

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);
}

static void *ia32_cg_init(FILE *F, const be_irg_t *birg);

static const arch_code_generator_if_t ia32_code_gen_if = {
	ia32_cg_init,
	NULL,                /* before abi introduce hook */
	ia32_prepare_graph,
	ia32_before_sched,   /* before scheduling hook */
	ia32_before_ra,      /* before register allocation hook */
	ia32_after_ra,       /* after register allocation hook */
	ia32_codegen         /* emit && done */
};

/**
 * Initializes the code generator.
 */
static void *ia32_cg_init(FILE *F, const be_irg_t *birg) {
	ia32_isa_t      *isa = (ia32_isa_t *)birg->main_env->arch_env->isa;
	ia32_code_gen_t *cg  = xcalloc(1, sizeof(*cg));

	cg->impl      = &ia32_code_gen_if;
	cg->irg       = birg->irg;
	cg->reg_set   = new_set(ia32_cmp_irn_reg_assoc, 1024);
	cg->out       = F;
	cg->arch_env  = birg->main_env->arch_env;
	cg->types     = pmap_create();
	cg->tv_ent    = pmap_create();
	cg->birg      = birg;
	cg->blk_sched = NULL;
	cg->fp_kind   = isa->fp_kind;
	cg->used_x87  = 0;

	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.cg");

	/* set optimizations */
	cg->opt.incdec    = 0;
	cg->opt.doam      = 1;
	cg->opt.placecnst = 1;
	cg->opt.immops    = 1;
	cg->opt.extbb     = 1;

#ifndef NDEBUG
	if (isa->name_obst_size) {
		//printf("freed %d bytes from name obst\n", isa->name_obst_size);
		isa->name_obst_size = 0;
		obstack_free(isa->name_obst, NULL);
		obstack_init(isa->name_obst);
	}
#endif /* NDEBUG */

	isa->num_codegens++;

	if (isa->num_codegens > 1)
		cg->emit_decls = 0;
	else
		cg->emit_decls = 1;

	cur_reg_set = cg->reg_set;

	ia32_irn_ops.cg = cg;

	return (arch_code_generator_t *)cg;
}



/*****************************************************************
 *  ____             _                  _   _____  _____
 * |  _ \           | |                | | |_   _|/ ____|  /\
 * | |_) | __ _  ___| | _____ _ __   __| |   | | | (___   /  \
 * |  _ < / _` |/ __| |/ / _ \ '_ \ / _` |   | |  \___ \ / /\ \
 * | |_) | (_| | (__|   <  __/ | | | (_| |  _| |_ ____) / ____ \
 * |____/ \__,_|\___|_|\_\___|_| |_|\__,_| |_____|_____/_/    \_\
 *
 *****************************************************************/

static ia32_isa_t ia32_isa_template = {
	&ia32_isa_if,            /* isa interface implementation */
	&ia32_gp_regs[REG_ESP],  /* stack pointer register */
	&ia32_gp_regs[REG_EBP],  /* base pointer register */
	-1,                      /* stack direction */
	0,                       /* number of code generator objects so far */
	NULL,                    /* 16bit register names */
	NULL,                    /* 8bit register names */
	fp_sse2,                 /* use SSE2 unit for fp operations */
#ifndef NDEBUG
	NULL,                    /* name obstack */
	0                        /* name obst size */
#endif
};

/**
 * Initializes the backend ISA.
 */
static void *ia32_init(void) {
	static int inited = 0;
	ia32_isa_t *isa;

	if(inited)
		return NULL;

	isa = xcalloc(1, sizeof(*isa));
	memcpy(isa, &ia32_isa_template, sizeof(*isa));

	ia32_register_init(isa);
	ia32_create_opcodes();
	ia32_register_copy_attr_func();

	isa->regs_16bit = pmap_create();
	isa->regs_8bit  = pmap_create();
//	isa->fp_kind    = fp_x87;

	ia32_build_16bit_reg_map(isa->regs_16bit);
	ia32_build_8bit_reg_map(isa->regs_8bit);

#ifndef NDEBUG
	isa->name_obst = xcalloc(1, sizeof(*(isa->name_obst)));
	obstack_init(isa->name_obst);
	isa->name_obst_size = 0;
#endif /* NDEBUG */

	inited = 1;

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void ia32_done(void *self) {
	ia32_isa_t *isa = self;

	pmap_destroy(isa->regs_16bit);
	pmap_destroy(isa->regs_8bit);

#ifndef NDEBUG
	//printf("name obst size = %d bytes\n", isa->name_obst_size);
	obstack_free(isa->name_obst, NULL);
#endif /* NDEBUG */

	free(self);
}


/**
 * Return the number of register classes for this architecture.
 * We report always these:
 *  - the general purpose registers
 *  - the floating point register set (depending on the unit used for FP)
 *  - MMX/SE registers (currently not supported)
 */
static int ia32_get_n_reg_class(const void *self) {
	return 2;
}

/**
 * Return the register class for index i.
 */
static const arch_register_class_t *ia32_get_reg_class(const void *self, int i) {
	const ia32_isa_t *isa = self;
	assert(i >= 0 && i < 2 && "Invalid ia32 register class requested.");
	if (i == 0)
		return &ia32_reg_classes[CLASS_ia32_gp];
	return USE_SSE2(isa) ? &ia32_reg_classes[CLASS_ia32_xmm] : &ia32_reg_classes[CLASS_ia32_vfp];
}

/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *ia32_get_reg_class_for_mode(const void *self, const ir_mode *mode) {
	const ia32_isa_t *isa = self;
	if (mode_is_float(mode)) {
		return USE_SSE2(isa) ? &ia32_reg_classes[CLASS_ia32_xmm] : &ia32_reg_classes[CLASS_ia32_vfp];
	}
	else
		return &ia32_reg_classes[CLASS_ia32_gp];
}

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void ia32_get_call_abi(const void *self, ir_type *method_type, be_abi_call_t *abi) {
	const ia32_isa_t *isa = self;
	ir_type  *tp;
	ir_mode  *mode;
	unsigned  cc        = get_method_calling_convention(method_type);
	int       n         = get_method_n_params(method_type);
	int       biggest_n = -1;
	int       stack_idx = 0;
	int       i, ignore_1, ignore_2;
	ir_mode **modes;
	const arch_register_t *reg;
	be_abi_call_flags_t call_flags;

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;
	call_flags.bits.store_args_sequential = 0;
	call_flags.bits.try_omit_fp           = 1;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &ia32_abi_callbacks);

	/* collect the mode for each type */
	modes = alloca(n * sizeof(modes[0]));

	for (i = 0; i < n; i++) {
		tp       = get_method_param_type(method_type, i);
		modes[i] = get_type_mode(tp);
	}

	/* set register parameters  */
	if (cc & cc_reg_param) {
		/* determine the number of parameters passed via registers */
		biggest_n = ia32_get_n_regparam_class(n, modes, &ignore_1, &ignore_2);

		/* loop over all parameters and set the register requirements */
		for (i = 0; i <= biggest_n; i++) {
			reg = ia32_get_RegParam_reg(n, modes, i, cc);
			assert(reg && "kaputt");
			be_abi_call_param_reg(abi, i, reg);
		}

		stack_idx = i;
	}


	/* set stack parameters */
	for (i = stack_idx; i < n; i++) {
		be_abi_call_param_stack(abi, i, 1, 0, 0);
	}


	/* set return registers */
	n = get_method_n_ress(method_type);

	assert(n <= 2 && "more than two results not supported");

	/* In case of 64bit returns, we will have two 32bit values */
	if (n == 2) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		assert(!mode_is_float(mode) && "two FP results not supported");

		tp   = get_method_res_type(method_type, 1);
		mode = get_type_mode(tp);

		assert(!mode_is_float(mode) && "two FP results not supported");

		be_abi_call_res_reg(abi, 0, &ia32_gp_regs[REG_EAX]);
		be_abi_call_res_reg(abi, 1, &ia32_gp_regs[REG_EDX]);
	}
	else if (n == 1) {
		const arch_register_t *reg;

		tp   = get_method_res_type(method_type, 0);
		assert(is_atomic_type(tp));
		mode = get_type_mode(tp);

		reg = mode_is_float(mode) ?
			(USE_SSE2(isa) ? &ia32_xmm_regs[REG_XMM0] : &ia32_vfp_regs[REG_VF0]) :
			&ia32_gp_regs[REG_EAX];

		be_abi_call_res_reg(abi, 0, reg);
	}
}


static const void *ia32_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn) {
	return &ia32_irn_ops;
}

const arch_irn_handler_t ia32_irn_handler = {
	ia32_get_irn_ops
};

const arch_irn_handler_t *ia32_get_irn_handler(const void *self) {
	return &ia32_irn_handler;
}

int ia32_to_appear_in_schedule(void *block_env, const ir_node *irn) {
	return is_ia32_irn(irn);
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *ia32_get_code_generator_if(void *self) {
	return &ia32_code_gen_if;
}

list_sched_selector_t ia32_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *ia32_get_list_sched_selector(const void *self) {
	memcpy(&ia32_sched_selector, reg_pressure_selector, sizeof(list_sched_selector_t));
	ia32_sched_selector.to_appear_in_schedule = ia32_to_appear_in_schedule;
	return &ia32_sched_selector;
}

#ifdef WITH_LIBCORE
static void ia32_register_options(lc_opt_entry_t *ent)
{
}
#endif /* WITH_LIBCORE */

const arch_isa_if_t ia32_isa_if = {
#ifdef WITH_LIBCORE
	ia32_register_options,
#endif
	ia32_init,
	ia32_done,
	ia32_get_n_reg_class,
	ia32_get_reg_class,
	ia32_get_reg_class_for_mode,
	ia32_get_call_abi,
	ia32_get_irn_handler,
	ia32_get_code_generator_if,
	ia32_get_list_sched_selector
};
