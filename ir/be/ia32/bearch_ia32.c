/**
 * This is the main ia32 firm backend driver.
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#endif /* WITH_LIBCORE */

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
#include "ia32_dbg_stat.h"

#define DEBUG_MODULE "firm.be.ia32.isa"

/* TODO: ugly */
static set *cur_reg_set = NULL;

#undef is_Start
#define is_Start(irn) (get_irn_opcode(irn) == iro_Start)

/* Creates the unique per irg GP NoReg node. */
ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi, &ia32_gp_regs[REG_GP_NOREG]);
}

/* Creates the unique per irg FP NoReg node. */
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
		/* treat Unknowns like Const with default requirements */
		if (is_Unknown(irn)) {
			DB((mod, LEVEL_1, "returning UKNWN reqs for %+F\n", irn));
			if (mode_is_float(mode)) {
				if (USE_SSE2(ops->cg))
					memcpy(req, &(ia32_default_req_ia32_xmm_xmm_UKNWN), sizeof(*req));
				else
					memcpy(req, &(ia32_default_req_ia32_vfp_vfp_UKNWN), sizeof(*req));
			}
			else if (mode_is_int(mode) || mode_is_reference(mode))
				memcpy(req, &(ia32_default_req_ia32_gp_gp_UKNWN), sizeof(*req));
			else if (mode == mode_T || mode == mode_M) {
				DBG((mod, LEVEL_1, "ignoring Unknown node %+F\n", irn));
				return NULL;
			}
			else
				assert(0 && "unsupported Unknown-Mode");
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
	else if (is_ia32_Cnst(irn))
		return arch_irn_class_const;
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
		if (is_Unknown(irn))
			return arch_irn_flags_ignore;
		return 0;
	}
}

static entity *ia32_get_frame_entity(const void *self, const ir_node *irn) {
	return is_ia32_irn(irn) ? get_ia32_frame_ent(irn) : NULL;
}

static void ia32_set_stack_bias(const void *self, ir_node *irn, int bias) {
	char buf[64];
	const ia32_irn_ops_t *ops = self;

	if (get_ia32_frame_ent(irn)) {
		ia32_am_flavour_t am_flav = get_ia32_am_flavour(irn);

		DBG((ops->cg->mod, LEVEL_1, "stack biased %+F with %d\n", irn, bias));
		snprintf(buf, sizeof(buf), "%d", bias);

		if (get_ia32_op_type(irn) == ia32_Normal) {
			set_ia32_cnst(irn, buf);
		}
		else {
			add_ia32_am_offs(irn, buf);
			am_flav |= ia32_O;
			set_ia32_am_flavour(irn, am_flav);
		}
	}
}

typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_isa_t *isa;
	const arch_env_t *aenv;
	ir_graph *irg;
} ia32_abi_env_t;

static void *ia32_abi_init(const be_abi_call_t *call, const arch_env_t *aenv, ir_graph *irg)
{
	ia32_abi_env_t *env    = xmalloc(sizeof(env[0]));
	be_abi_call_flags_t fl = be_abi_call_get_flags(call);
	env->flags = fl.bits;
	env->irg   = irg;
	env->aenv  = aenv;
	env->isa   = aenv->isa;
	return env;
}

static void ia32_abi_dont_save_regs(void *self, pset *s)
{
	ia32_abi_env_t *env = self;
	if(env->flags.try_omit_fp)
		pset_insert_ptr(s, env->isa->bp);
}

/**
 * Generate the routine prologue.
 * @param self    The callback object.
 * @param mem     A pointer to the mem node. Update this if you define new memory.
 * @param reg_map A map mapping all callee_save/ignore/parameter registers to their defining nodes.
 * @return        The register which shall be used as a stack frame base.
 *
 * All nodes which define registers in @p reg_map must keep @p reg_map current.
 */
static const arch_register_t *ia32_abi_prologue(void *self, ir_node **mem, pmap *reg_map)
{
	ia32_abi_env_t *env              = self;

	if (!env->flags.try_omit_fp) {
		int reg_size         = get_mode_size_bytes(env->isa->bp->reg_class->mode);
		ir_node *bl          = get_irg_start_block(env->irg);
		ir_node *curr_sp     = be_abi_reg_map_get(reg_map, env->isa->sp);
		ir_node *curr_bp     = be_abi_reg_map_get(reg_map, env->isa->bp);
		ir_node *push;

		/* push ebp */
		push    = new_rd_ia32_Push(NULL, env->irg, bl, curr_sp, curr_bp, *mem);
		curr_sp = new_r_Proj(env->irg, bl, push, get_irn_mode(curr_sp), pn_ia32_Push_stack);
		*mem    = new_r_Proj(env->irg, bl, push, mode_M, pn_ia32_Push_M);

		/* the push must have SP out register */
		arch_set_irn_register(env->aenv, curr_sp, env->isa->sp);
		set_ia32_flags(push, arch_irn_flags_ignore);

		/* move esp to ebp */
		curr_bp  = be_new_Copy(env->isa->bp->reg_class, env->irg, bl, curr_sp);
		be_set_constr_single_reg(curr_bp, BE_OUT_POS(0), env->isa->bp);
		arch_set_irn_register(env->aenv, curr_bp, env->isa->bp);
		be_node_set_flags(curr_bp, BE_OUT_POS(0), arch_irn_flags_ignore);

		/* beware: the copy must be done before any other sp use */
		curr_sp = be_new_CopyKeep_single(env->isa->sp->reg_class, env->irg, bl, curr_sp, curr_bp, get_irn_mode(curr_sp));
		be_set_constr_single_reg(curr_sp, BE_OUT_POS(0), env->isa->sp);
		arch_set_irn_register(env->aenv, curr_sp, env->isa->sp);
		be_node_set_flags(curr_sp, BE_OUT_POS(0), arch_irn_flags_ignore);

		be_abi_reg_map_set(reg_map, env->isa->sp, curr_sp);
		be_abi_reg_map_set(reg_map, env->isa->bp, curr_bp);

		return env->isa->bp;
	}

	return env->isa->sp;
}

/**
 * Generate the routine epilogue.
 * @param self    The callback object.
 * @param mem     A pointer to the mem node. Update this if you define new memory.
 * @param reg_map A map mapping all callee_save/ignore/parameter registers to their defining nodes.
 * @return        The register which shall be used as a stack frame base.
 *
 * All nodes which define registers in @p reg_map must keep @p reg_map current.
 */
static void ia32_abi_epilogue(void *self, ir_node *bl, ir_node **mem, pmap *reg_map)
{
	ia32_abi_env_t *env  = self;
	ir_node *curr_sp     = be_abi_reg_map_get(reg_map, env->isa->sp);
	ir_node *curr_bp     = be_abi_reg_map_get(reg_map, env->isa->bp);

	if (env->flags.try_omit_fp) {
		/* simply remove the stack frame here */
		curr_sp = be_new_IncSP(env->isa->sp, env->irg, bl, curr_sp, *mem, BE_STACK_FRAME_SIZE, be_stack_dir_shrink);
	}
	else {
		const ia32_isa_t *isa = (ia32_isa_t *)env->isa;
		ir_mode *mode_bp = env->isa->bp->reg_class->mode;
		int reg_size     = get_mode_size_bytes(env->isa->bp->reg_class->mode);

		/* AMD processors prefer leave at the end of a routine */
    if (ARCH_AMD(isa->opt_arch)) {
			ir_node *leave;

			/* leave */
			leave = new_rd_ia32_Leave(NULL, env->irg, bl, curr_sp, *mem);
			set_ia32_flags(leave, arch_irn_flags_ignore);
			curr_bp = new_r_Proj(current_ir_graph, bl, leave, mode_bp, pn_ia32_Leave_frame);
			curr_sp = new_r_Proj(current_ir_graph, bl, leave, get_irn_mode(curr_sp), pn_ia32_Leave_stack);
			*mem    = new_r_Proj(current_ir_graph, bl, leave, mode_M, pn_ia32_Leave_M);
		}
		else {
			ir_node *pop;

			/* copy ebp to esp */
			curr_sp = be_new_SetSP(env->isa->sp, env->irg, bl, curr_sp, curr_bp, *mem);

			/* pop ebp */
			pop = new_rd_ia32_Pop(NULL, env->irg, bl, curr_sp, *mem);
			set_ia32_flags(pop, arch_irn_flags_ignore);
			curr_bp = new_r_Proj(current_ir_graph, bl, pop, mode_bp, pn_ia32_Pop_res);
			curr_sp = new_r_Proj(current_ir_graph, bl, pop, get_irn_mode(curr_sp), pn_ia32_Pop_stack);
			*mem    = new_r_Proj(current_ir_graph, bl, pop, mode_M, pn_ia32_Pop_M);
		}
		arch_set_irn_register(env->aenv, curr_sp, env->isa->sp);
		arch_set_irn_register(env->aenv, curr_bp, env->isa->bp);
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
	DEBUG_ONLY(firm_dbg_module_t *old_mod = cg->mod;)

	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.transform");
	ia32_register_transformers();
	irg_walk_blkwise_graph(cg->irg, ia32_place_consts_set_modes, ia32_transform_node, cg);
	be_dump(cg->irg, "-transformed", dump_ir_block_graph_sched);

	if (cg->opt & IA32_OPT_DOAM) {
		edges_deactivate(cg->irg);
		//dead_node_elimination(cg->irg);
		edges_activate(cg->irg);

		FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.am");

		irg_walk_blkwise_graph(cg->irg, NULL, ia32_optimize_am, cg);
		be_dump(cg->irg, "-am", dump_ir_block_graph_sched);
	}

	DEBUG_ONLY(cg->mod = old_mod;)
}


/**
 * Insert copies for all ia32 nodes where the should_be_same requirement
 * is not fulfilled.
 * Transform Sub into Neg -- Add if IN2 == OUT
 */
static void ia32_finish_node(ir_node *irn, void *env) {
	ia32_code_gen_t            *cg = env;
	const ia32_register_req_t **reqs;
	const arch_register_t      *out_reg, *in_reg, *in2_reg;
	int                         n_res, i;
	ir_node                    *copy, *in_node, *block, *in2_node;
	ia32_op_type_t              op_tp;

	if (is_ia32_irn(irn)) {
		/* AM Dest nodes don't produce any values  */
		op_tp = get_ia32_op_type(irn);
		if (op_tp == ia32_AddrModeD)
			goto end;

		reqs  = get_ia32_out_req_all(irn);
		n_res = get_ia32_n_res(irn);
		block = get_nodes_block(irn);

		/* check all OUT requirements, if there is a should_be_same */
		if ((op_tp == ia32_Normal || op_tp == ia32_AddrModeS) &&
			! is_ia32_Lea(irn) && ! is_ia32_Conv_I2I(irn) && ! is_ia32_Conv_I2I8Bit(irn))
		{
			for (i = 0; i < n_res; i++) {
				if (arch_register_req_is(&(reqs[i]->req), should_be_same)) {
					/* get in and out register */
					out_reg  = get_ia32_out_reg(irn, i);
					in_node  = get_irn_n(irn, reqs[i]->same_pos);
					in_reg   = arch_get_irn_register(cg->arch_env, in_node);

					/* don't copy ignore nodes */
					if (arch_irn_is(cg->arch_env, in_node, ignore) && is_Proj(in_node))
						continue;

					/* check if in and out register are equal */
					if (! REGS_ARE_EQUAL(out_reg, in_reg)) {
						/* in case of a commutative op: just exchange the in's */
						/* beware: the current op could be everything, so test for ia32 */
						/*         commutativity first before getting the second in     */
						if (is_ia32_commutative(irn)) {
							in2_node = get_irn_n(irn, reqs[i]->same_pos ^ 1);
							in2_reg  = arch_get_irn_register(cg->arch_env, in2_node);

							if (REGS_ARE_EQUAL(out_reg, in2_reg)) {
								set_irn_n(irn, reqs[i]->same_pos, in2_node);
								set_irn_n(irn, reqs[i]->same_pos ^ 1, in_node);
							}
							else
								goto insert_copy;
						}
						else {
insert_copy:
							DBG((cg->mod, LEVEL_1, "inserting copy for %+F in_pos %d\n", irn, reqs[i]->same_pos));
							/* create copy from in register */
							copy = be_new_Copy(arch_register_get_class(in_reg), cg->irg, block, in_node);

							DBG_OPT_2ADDRCPY(copy);

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
	}
end:

	/* check for peephole optimization */
	ia32_peephole_optimization(irn, cg);
}

static void ia32_finish_irg_walker(ir_node *block, void *env) {
	ir_node *irn, *next;

	for (irn = sched_first(block); !sched_is_end(irn); irn = next) {
		next = sched_next(irn);
		ia32_finish_node(irn, env);
	}
}

/**
 * Add Copy nodes for not fulfilled should_be_equal constraints
 */
static void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg) {
	irg_block_walk_graph(irg, NULL, ia32_finish_irg_walker, cg);
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
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xLoad(env->dbg, env->irg, env->block, ptr, noreg, mem);
		else
			new_op = new_rd_ia32_vfld(env->dbg, env->irg, env->block, ptr, noreg, mem);
	}
	else {
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem);
	}

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);
	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	DBG_OPT_RELOAD2LD(irn, new_op);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, pn_Load_res);

	if (sched_point) {
		sched_add_after(sched_point, new_op);
		sched_add_after(new_op, proj);

		sched_remove(irn);
	}

	/* copy the register from the old node to the new Load */
	reg = arch_get_irn_register(env->cg->arch_env, irn);
	arch_set_irn_register(env->cg->arch_env, new_op, reg);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, new_op));

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
		if (USE_SSE2(env->cg))
			new_op = new_rd_ia32_xStore(env->dbg, env->irg, env->block, ptr, noreg, val, nomem);
		else
			new_op = new_rd_ia32_vfst(env->dbg, env->irg, env->block, ptr, noreg, val, nomem);
	}
	else if (get_mode_size_bits(mode) == 8) {
		new_op = new_rd_ia32_Store8Bit(env->dbg, env->irg, env->block, ptr, noreg, val, nomem);
	}
	else {
		new_op = new_rd_ia32_Store(env->dbg, env->irg, env->block, ptr, noreg, val, nomem);
	}

	set_ia32_am_support(new_op, ia32_am_Dest);
	set_ia32_op_type(new_op, ia32_AddrModeD);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);
	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	DBG_OPT_SPILL2ST(irn, new_op);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode_M, pn_ia32_Store_M);

	if (sched_point) {
		sched_add_after(sched_point, new_op);
		sched_add_after(new_op, proj);

		sched_remove(irn);
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, new_op));

	exchange(irn, proj);
}

/**
 * Fix the mode of Spill/Reload
 */
static ir_mode *fix_spill_mode(ia32_code_gen_t *cg, ir_mode *mode)
{
	if (mode_is_float(mode)) {
		if (USE_SSE2(cg))
			mode = mode_D;
		else
			mode = mode_E;
	}
	else
		mode = mode_Is;
	return mode;
}

/**
 * Block-Walker: Calls the transform functions Spill and Reload.
 */
static void ia32_after_ra_walker(ir_node *block, void *env) {
	ir_node *node, *prev;
	ia32_code_gen_t *cg = env;
	ia32_transform_env_t tenv;

	tenv.block = block;
	tenv.irg   = current_ir_graph;
	tenv.cg    = cg;
	DEBUG_ONLY(tenv.mod = cg->mod;)

	/* beware: the schedule is changed here */
	for (node = sched_last(block); !sched_is_begin(node); node = prev) {
		prev = sched_prev(node);
		if (be_is_Reload(node)) {
			/* we always reload the whole register  */
			tenv.dbg  = get_irn_dbg_info(node);
			tenv.irn  = node;
			tenv.mode = fix_spill_mode(cg, get_irn_mode(node));
			transform_to_Load(&tenv);
		}
		else if (be_is_Spill(node)) {
			/* we always spill the whole register  */
			tenv.dbg  = get_irn_dbg_info(node);
			tenv.irn  = node;
			tenv.mode = fix_spill_mode(cg, get_irn_mode(be_get_Spill_context(node)));
			transform_to_Store(&tenv);
		}
	}
}

/**
 * We transform Spill and Reload here. This needs to be done before
 * stack biasing otherwise we would miss the corrected offset for these nodes.
 *
 * If x87 instruction should be emitted, run the x87 simulator and patch
 * the virtual instructions. This must obviously be done after register allocation.
 */
static void ia32_after_ra(void *self) {
	ia32_code_gen_t *cg = self;
	irg_block_walk_graph(cg->irg, NULL, ia32_after_ra_walker, self);

	/* if we do x87 code generation, rewrite all the virtual instructions and registers */
	if (cg->used_fp == fp_x87) {
		x87_simulate_graph(cg->arch_env, cg->irg, cg->blk_sched);
	}
}


/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ia32_codegen(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;

	ia32_finish_irg(irg, cg);
	be_dump(irg, "-finished", dump_ir_block_graph_sched);
	ia32_gen_routine(cg->isa->out, irg, cg);

	cur_reg_set = NULL;

	/* remove it from the isa */
	cg->isa->cg = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);

}

static void *ia32_cg_init(const be_irg_t *birg);

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
 * Initializes a IA32 code generator.
 */
static void *ia32_cg_init(const be_irg_t *birg) {
	ia32_isa_t      *isa = (ia32_isa_t *)birg->main_env->arch_env->isa;
	ia32_code_gen_t *cg  = xcalloc(1, sizeof(*cg));

	cg->impl      = &ia32_code_gen_if;
	cg->irg       = birg->irg;
	cg->reg_set   = new_set(ia32_cmp_irn_reg_assoc, 1024);
	cg->arch_env  = birg->main_env->arch_env;
	cg->isa       = isa;
	cg->birg      = birg;
	cg->blk_sched = NULL;
	cg->fp_to_gp  = NULL;
	cg->gp_to_fp  = NULL;
	cg->fp_kind   = isa->fp_kind;
	cg->used_fp   = fp_none;

	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.cg");

	/* copy optimizations from isa for easier access */
	cg->opt = isa->opt;

	/* enter it */
	isa->cg = cg;

#ifndef NDEBUG
	if (isa->name_obst_size) {
		//printf("freed %d bytes from name obst\n", isa->name_obst_size);
		isa->name_obst_size = 0;
		obstack_free(isa->name_obst, NULL);
		obstack_init(isa->name_obst);
	}
#endif /* NDEBUG */

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

/**
 * The template that generates a new ISA object.
 * Note that this template can be changed by command line
 * arguments.
 */
static ia32_isa_t ia32_isa_template = {
  {
	  &ia32_isa_if,            /* isa interface implementation */
	  &ia32_gp_regs[REG_ESP],  /* stack pointer register */
	  &ia32_gp_regs[REG_EBP],  /* base pointer register */
	  -1,                      /* stack direction */
  },
	NULL,                    /* 16bit register names */
	NULL,                    /* 8bit register names */
	NULL,                    /* types */
	NULL,                    /* tv_ents */
	(0 |
	IA32_OPT_INCDEC    |     /* optimize add 1, sub 1 into inc/dec               default: on  */
	IA32_OPT_DOAM      |     /* optimize address mode                            default: on  */
	IA32_OPT_PLACECNST |     /* place constants immediately before instructions, default: on  */
	IA32_OPT_IMMOPS    |     /* operations can use immediates,                   default: on  */
	IA32_OPT_EXTBB),         /* use extended basic block scheduling,             default: on  */
	arch_pentium_4,          /* instruction architecture */
	arch_pentium_4,          /* optimize for architecture */
	fp_sse2,                 /* use sse2 unit */
	NULL,                    /* current code generator */
#ifndef NDEBUG
	NULL,                    /* name obstack */
	0                        /* name obst size */
#endif
};

/**
 * Initializes the backend ISA.
 */
static void *ia32_init(FILE *file_handle) {
	static int inited = 0;
	ia32_isa_t *isa;

	if (inited)
		return NULL;

	isa = xmalloc(sizeof(*isa));
	memcpy(isa, &ia32_isa_template, sizeof(*isa));

	ia32_register_init(isa);
	ia32_create_opcodes();
	ia32_register_copy_attr_func();

	if ((ARCH_INTEL(isa->arch) && isa->arch < arch_pentium_4) ||
	    (ARCH_AMD(isa->arch) && isa->arch < arch_athlon))
		/* no SSE2 for these cpu's */
		isa->fp_kind = fp_x87;

	if (ARCH_INTEL(isa->opt_arch) && isa->opt_arch >= arch_pentium_4) {
		/* Pentium 4 don't like inc and dec instructions */
		isa->opt &= ~IA32_OPT_INCDEC;
	}

	isa->regs_16bit = pmap_create();
	isa->regs_8bit  = pmap_create();
	isa->types      = pmap_create();
	isa->tv_ent     = pmap_create();
	isa->out        = file_handle;

	ia32_build_16bit_reg_map(isa->regs_16bit);
	ia32_build_8bit_reg_map(isa->regs_8bit);

	/* patch register names of x87 registers */
	if (USE_x87(isa)) {
	  ia32_st_regs[0].name = "st";
	  ia32_st_regs[1].name = "st(1)";
	  ia32_st_regs[2].name = "st(2)";
	  ia32_st_regs[3].name = "st(3)";
	  ia32_st_regs[4].name = "st(4)";
	  ia32_st_regs[5].name = "st(5)";
	  ia32_st_regs[6].name = "st(6)";
	  ia32_st_regs[7].name = "st(7)";
	}

#ifndef NDEBUG
	isa->name_obst = xmalloc(sizeof(*isa->name_obst));
	obstack_init(isa->name_obst);
	isa->name_obst_size = 0;
#endif /* NDEBUG */

	fprintf(isa->out, "\t.intel_syntax\n");

	inited = 1;

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void ia32_done(void *self) {
	ia32_isa_t *isa = self;

	/* emit now all global declarations */
	ia32_gen_decls(isa->out);

	pmap_destroy(isa->regs_16bit);
	pmap_destroy(isa->regs_8bit);
	pmap_destroy(isa->tv_ent);
	pmap_destroy(isa->types);

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
 *  - MMX/SSE registers (currently not supported)
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
	be_abi_call_flags_t call_flags = be_abi_call_get_flags(abi);

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;  /* always last arg first on stack */
	call_flags.bits.store_args_sequential = 0;  /* use stores instead of push */
	/* call_flags.bits.try_omit_fp                 not changed: can handle both settings */
	call_flags.bits.fp_free               = 0;  /* the frame pointer is fixed in IA32 */
	call_flags.bits.call_has_imm          = 1;  /* IA32 calls can have immediate address */

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
//	memcpy(&ia32_sched_selector, reg_pressure_selector, sizeof(list_sched_selector_t));
	memcpy(&ia32_sched_selector, trivial_selector, sizeof(list_sched_selector_t));
	ia32_sched_selector.to_appear_in_schedule = ia32_to_appear_in_schedule;
	return &ia32_sched_selector;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int ia32_get_reg_class_alignment(const void *self, const arch_register_class_t *cls) {
	ir_mode *mode = arch_register_class_mode(cls);
	int bytes     = get_mode_size_bytes(mode);

	if (mode_is_float(mode) && bytes > 8)
		return 16;
	return bytes;
}

#ifdef WITH_LIBCORE

/* instruction set architectures. */
static const lc_opt_enum_int_items_t arch_items[] = {
	{ "386",        arch_i386, },
	{ "486",        arch_i486, },
	{ "pentium",    arch_pentium, },
	{ "586",        arch_pentium, },
	{ "pentiumpro", arch_pentium_pro, },
	{ "686",        arch_pentium_pro, },
	{ "pentiummmx", arch_pentium_mmx, },
	{ "pentium2",   arch_pentium_2, },
	{ "p2",         arch_pentium_2, },
	{ "pentium3",   arch_pentium_3, },
	{ "p3",         arch_pentium_3, },
	{ "pentium4",   arch_pentium_4, },
	{ "p4",         arch_pentium_4, },
	{ "pentiumm",   arch_pentium_m, },
	{ "pm",         arch_pentium_m, },
	{ "core",       arch_core, },
	{ "k6",         arch_k6, },
	{ "athlon",     arch_athlon, },
	{ "athlon64",   arch_athlon_64, },
	{ "opteron",    arch_opteron, },
	{ NULL,         0 }
};

static lc_opt_enum_int_var_t arch_var = {
	&ia32_isa_template.arch, arch_items
};

static lc_opt_enum_int_var_t opt_arch_var = {
	&ia32_isa_template.opt_arch, arch_items
};

static const lc_opt_enum_int_items_t fp_unit_items[] = {
	{ "x87" ,    fp_x87 },
	{ "sse2",    fp_sse2 },
	{ NULL,      0 }
};

static lc_opt_enum_int_var_t fp_unit_var = {
	&ia32_isa_template.fp_kind, fp_unit_items
};

static const lc_opt_table_entry_t ia32_options[] = {
	LC_OPT_ENT_ENUM_INT("arch",   "select the instruction architecture", &arch_var),
	LC_OPT_ENT_ENUM_INT("opt",    "optimize for instruction architecture", &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpunit", "select the floating point unit", &fp_unit_var),
	LC_OPT_ENT_NEGBIT("noaddrmode", "do not use address mode", &ia32_isa_template.opt, IA32_OPT_DOAM),
	LC_OPT_ENT_NEGBIT("noplacecnst", "do not place constants", &ia32_isa_template.opt, IA32_OPT_PLACECNST),
	LC_OPT_ENT_NEGBIT("noimmop", "no operations with immediates", &ia32_isa_template.opt, IA32_OPT_IMMOPS),
	LC_OPT_ENT_NEGBIT("noextbb", "do not use extended basic block scheduling", &ia32_isa_template.opt, IA32_OPT_EXTBB),
	{ NULL }
};

/**
 * Register command line options for the ia32 backend.
 *
 * Options so far:
 *
 * ia32-arch=arch    create instruction for arch
 * ia32-opt=arch     optimize for run on arch
 * ia32-fpunit=unit  select floating point unit (x87 or SSE2)
 * ia32-incdec       optimize for inc/dec
 * ia32-noaddrmode   do not use address mode
 * ia32-noplacecnst  do not place constants,
 * ia32-noimmop      no operations with immediates
 * ia32-noextbb      do not use extended basic block scheduling
 */
static void ia32_register_options(lc_opt_entry_t *ent)
{
	lc_opt_entry_t *be_grp_ia32 = lc_opt_get_grp(ent, "ia32");
	lc_opt_add_table(be_grp_ia32, ia32_options);
}
#endif /* WITH_LIBCORE */

const arch_isa_if_t ia32_isa_if = {
	ia32_init,
	ia32_done,
	ia32_get_n_reg_class,
	ia32_get_reg_class,
	ia32_get_reg_class_for_mode,
	ia32_get_call_abi,
	ia32_get_irn_handler,
	ia32_get_code_generator_if,
	ia32_get_list_sched_selector,
	ia32_get_reg_class_alignment,
#ifdef WITH_LIBCORE
	ia32_register_options
#endif
};
