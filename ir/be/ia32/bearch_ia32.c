/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       This is the main ia32 firm backend driver.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#include <math.h>

#include "pseudo_irg.h"
#include "irarch.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irbitset.h"
#include "irgopt.h"
#include "pdeq.h"
#include "pset.h"
#include "debug.h"
#include "error.h"
#include "xmalloc.h"
#include "irtools.h"

#include "../beabi.h"
#include "../beirg_t.h"
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "be.h"
#include "../be_t.h"
#include "../beirgmod.h"
#include "../be_dbgout.h"
#include "../beblocksched.h"
#include "../bemachine.h"
#include "../beilpsched.h"
#include "../bespillslots.h"
#include "../bemodule.h"
#include "../begnuas.h"
#include "../bestate.h"

#include "bearch_ia32_t.h"

#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"
#include "gen_ia32_machine.h"
#include "ia32_transform.h"
#include "ia32_emitter.h"
#include "ia32_map_regs.h"
#include "ia32_optimize.h"
#include "ia32_x87.h"
#include "ia32_dbg_stat.h"
#include "ia32_finish.h"
#include "ia32_util.h"
#include "ia32_fpu.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* TODO: ugly */
static set *cur_reg_set = NULL;

ir_mode         *mode_fpcw       = NULL;
ia32_code_gen_t *ia32_current_cg = NULL;

typedef ir_node *(*create_const_node_func) (dbg_info *dbg, ir_graph *irg, ir_node *block);

static INLINE ir_node *create_const(ia32_code_gen_t *cg, ir_node **place,
                                    create_const_node_func func,
                                    const arch_register_t* reg)
{
	ir_node *block, *res;

	if(*place != NULL)
		return *place;

	block = get_irg_start_block(cg->irg);
	res = func(NULL, cg->irg, block);
	arch_set_irn_register(cg->arch_env, res, reg);
	*place = res;

	add_irn_dep(get_irg_end(cg->irg), res);
	/* add_irn_dep(get_irg_start(cg->irg), res); */

	return res;
}

/* Creates the unique per irg GP NoReg node. */
ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->noreg_gp, new_rd_ia32_NoReg_GP,
	                    &ia32_gp_regs[REG_GP_NOREG]);
}

ir_node *ia32_new_NoReg_vfp(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->noreg_vfp, new_rd_ia32_NoReg_VFP,
	                    &ia32_vfp_regs[REG_VFP_NOREG]);
}

ir_node *ia32_new_NoReg_xmm(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->noreg_xmm, new_rd_ia32_NoReg_XMM,
	                    &ia32_xmm_regs[REG_XMM_NOREG]);
}

/* Creates the unique per irg FP NoReg node. */
ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg) {
	return USE_SSE2(cg) ? ia32_new_NoReg_xmm(cg) : ia32_new_NoReg_vfp(cg);
}

ir_node *ia32_new_Unknown_gp(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->unknown_gp, new_rd_ia32_Unknown_GP,
	                    &ia32_gp_regs[REG_GP_UKNWN]);
}

ir_node *ia32_new_Unknown_vfp(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->unknown_vfp, new_rd_ia32_Unknown_VFP,
	                    &ia32_vfp_regs[REG_VFP_UKNWN]);
}

ir_node *ia32_new_Unknown_xmm(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->unknown_xmm, new_rd_ia32_Unknown_XMM,
	                    &ia32_xmm_regs[REG_XMM_UKNWN]);
}

ir_node *ia32_new_Fpu_truncate(ia32_code_gen_t *cg) {
	return create_const(cg, &cg->fpu_trunc_mode, new_rd_ia32_ChangeCW,
                        &ia32_fp_cw_regs[REG_FPCW]);
}


/**
 * Returns gp_noreg or fp_noreg, depending in input requirements.
 */
ir_node *ia32_get_admissible_noreg(ia32_code_gen_t *cg, ir_node *irn, int pos) {
	const arch_register_req_t *req;

	req = arch_get_register_req(cg->arch_env, irn, pos);
	assert(req != NULL && "Missing register requirements");
	if (req->cls == &ia32_reg_classes[CLASS_ia32_gp])
		return ia32_new_NoReg_gp(cg);

	return ia32_new_NoReg_fp(cg);
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

/**
 * Return register requirements for an ia32 node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const arch_register_req_t *ia32_get_irn_reg_req(const void *self,
                                                       const ir_node *node,
													   int pos)
{
	long node_pos = pos == -1 ? 0 : pos;
	ir_mode *mode     = is_Block(node) ? NULL : get_irn_mode(node);
	(void) self;

	if (is_Block(node) || mode == mode_X) {
		return arch_no_register_req;
	}

	if (mode == mode_T && pos < 0) {
		return arch_no_register_req;
	}

	if (is_Proj(node)) {
		if(mode == mode_M)
			return arch_no_register_req;

		if(pos >= 0) {
			return arch_no_register_req;
		}

		node_pos = (pos == -1) ? get_Proj_proj(node) : pos;
		node     = skip_Proj_const(node);
	}

	if (is_ia32_irn(node)) {
		const arch_register_req_t *req;
		if(pos >= 0)
			req = get_ia32_in_req(node, pos);
		else
			req = get_ia32_out_req(node, node_pos);

		assert(req != NULL);

		return req;
	}

	/* unknowns should be transformed already */
	assert(!is_Unknown(node));

	return arch_no_register_req;
}

static void ia32_set_irn_reg(const void *self, ir_node *irn,
                             const arch_register_t *reg)
{
	int                   pos = 0;
	(void) self;

	if (get_irn_mode(irn) == mode_X) {
		return;
	}

	if (is_Proj(irn)) {
		pos = get_Proj_proj(irn);
		irn = skip_Proj(irn);
	}

	if (is_ia32_irn(irn)) {
		const arch_register_t **slots;

		slots      = get_ia32_slots(irn);
		slots[pos] = reg;
	} else {
		ia32_set_firm_reg(irn, reg, cur_reg_set);
	}
}

static const arch_register_t *ia32_get_irn_reg(const void *self,
                                               const ir_node *irn)
{
	int pos = 0;
	const arch_register_t *reg = NULL;
	(void) self;

	if (is_Proj(irn)) {

		if (get_irn_mode(irn) == mode_X) {
			return NULL;
		}

		pos = get_Proj_proj(irn);
		irn = skip_Proj_const(irn);
	}

	if (is_ia32_irn(irn)) {
		const arch_register_t **slots;
		slots = get_ia32_slots(irn);
		reg   = slots[pos];
	} else {
		reg = ia32_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t ia32_classify(const void *self, const ir_node *irn) {
	arch_irn_class_t classification = arch_irn_class_normal;
	(void) self;

	irn = skip_Proj_const(irn);

	if (is_cfop(irn))
		classification |= arch_irn_class_branch;

	if (! is_ia32_irn(irn))
		return classification & ~arch_irn_class_normal;

	if (is_ia32_Ld(irn))
		classification |= arch_irn_class_load;

	if (is_ia32_St(irn))
		classification |= arch_irn_class_store;

	if (is_ia32_need_stackent(irn))
		classification |= arch_irn_class_reload;

	return classification;
}

static arch_irn_flags_t ia32_get_flags(const void *self, const ir_node *irn) {
	arch_irn_flags_t flags = arch_irn_flags_none;
	(void) self;

	if (is_Unknown(irn))
		return arch_irn_flags_ignore;

	if(is_Proj(irn) && mode_is_datab(get_irn_mode(irn))) {
		ir_node *pred = get_Proj_pred(irn);

		if(is_ia32_irn(pred)) {
			flags = get_ia32_out_flags(pred, get_Proj_proj(irn));
		}

		irn = pred;
	}

	if (is_ia32_irn(irn)) {
		flags |= get_ia32_flags(irn);
	}

	return flags;
}

/**
 * The IA32 ABI callback object.
 */
typedef struct {
	be_abi_call_flags_bits_t flags;  /**< The call flags. */
	const arch_isa_t *isa;           /**< The ISA handle. */
	const arch_env_t *aenv;          /**< The architecture environment. */
	ir_graph *irg;                   /**< The associated graph. */
} ia32_abi_env_t;

static ir_entity *ia32_get_frame_entity(const void *self, const ir_node *irn) {
	(void) self;
	return is_ia32_irn(irn) ? get_ia32_frame_ent(irn) : NULL;
}

static void ia32_set_frame_entity(const void *self, ir_node *irn, ir_entity *ent) {
	(void) self;
	set_ia32_frame_ent(irn, ent);
}

static void ia32_set_frame_offset(const void *self, ir_node *irn, int bias) {
	const ia32_irn_ops_t *ops = self;

	if (get_ia32_frame_ent(irn)) {
		if (is_ia32_Pop(irn)) {
			int omit_fp = be_abi_omit_fp(ops->cg->birg->abi);
			if (omit_fp) {
				/* Pop nodes modify the stack pointer before calculating the destination
				 * address, so fix this here
				 */
				bias -= 4;
			}
		}

		add_ia32_am_offs_int(irn, bias);
	}
}

static int ia32_get_sp_bias(const void *self, const ir_node *node)
{
	(void) self;

	if (is_ia32_Push(node))
		return 4;

	if (is_ia32_Pop(node))
		return -4;

	return 0;
}

/**
 * Put all registers which are saved by the prologue/epilogue in a set.
 *
 * @param self  The callback object.
 * @param s     The result set.
 */
static void ia32_abi_dont_save_regs(void *self, pset *s)
{
	ia32_abi_env_t *env = self;
	if(env->flags.try_omit_fp)
		pset_insert_ptr(s, env->isa->bp);
}

/**
 * Generate the routine prologue.
 *
 * @param self    The callback object.
 * @param mem     A pointer to the mem node. Update this if you define new memory.
 * @param reg_map A map mapping all callee_save/ignore/parameter registers to their defining nodes.
 *
 * @return        The register which shall be used as a stack frame base.
 *
 * All nodes which define registers in @p reg_map must keep @p reg_map current.
 */
static const arch_register_t *ia32_abi_prologue(void *self, ir_node **mem, pmap *reg_map)
{
	ia32_abi_env_t *env = self;
	const ia32_isa_t *isa     = (ia32_isa_t *)env->isa;
	ia32_code_gen_t *cg = isa->cg;

	if (! env->flags.try_omit_fp) {
		ir_node *bl      = get_irg_start_block(env->irg);
		ir_node *curr_sp = be_abi_reg_map_get(reg_map, env->isa->sp);
		ir_node *curr_bp = be_abi_reg_map_get(reg_map, env->isa->bp);
		ir_node *noreg = ia32_new_NoReg_gp(cg);
		ir_node *push;

		/* ALL nodes representing bp must be set to ignore. */
		be_node_set_flags(get_Proj_pred(curr_bp), BE_OUT_POS(get_Proj_proj(curr_bp)), arch_irn_flags_ignore);

		/* push ebp */
		push    = new_rd_ia32_Push(NULL, env->irg, bl, noreg, noreg, curr_bp, curr_sp, *mem);
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
 * @param bl      The block for the epilog
 * @param mem     A pointer to the mem node. Update this if you define new memory.
 * @param reg_map A map mapping all callee_save/ignore/parameter registers to their defining nodes.
 * @return        The register which shall be used as a stack frame base.
 *
 * All nodes which define registers in @p reg_map must keep @p reg_map current.
 */
static void ia32_abi_epilogue(void *self, ir_node *bl, ir_node **mem, pmap *reg_map)
{
	ia32_abi_env_t *env     = self;
	ir_node        *curr_sp = be_abi_reg_map_get(reg_map, env->isa->sp);
	ir_node        *curr_bp = be_abi_reg_map_get(reg_map, env->isa->bp);

	if (env->flags.try_omit_fp) {
		/* simply remove the stack frame here */
		curr_sp = be_new_IncSP(env->isa->sp, env->irg, bl, curr_sp, BE_STACK_FRAME_SIZE_SHRINK);
		add_irn_dep(curr_sp, *mem);
	} else {
		const ia32_isa_t *isa     = (ia32_isa_t *)env->isa;
		ia32_code_gen_t *cg = isa->cg;
		ir_mode          *mode_bp = env->isa->bp->reg_class->mode;

		/* gcc always emits a leave at the end of a routine */
		if (1 || ARCH_AMD(isa->opt_arch)) {
			ir_node *leave;

			/* leave */
			leave   = new_rd_ia32_Leave(NULL, env->irg, bl, curr_sp, curr_bp);
			set_ia32_flags(leave, arch_irn_flags_ignore);
			curr_bp = new_r_Proj(current_ir_graph, bl, leave, mode_bp, pn_ia32_Leave_frame);
			curr_sp = new_r_Proj(current_ir_graph, bl, leave, get_irn_mode(curr_sp), pn_ia32_Leave_stack);
		} else {
			ir_node *noreg = ia32_new_NoReg_gp(cg);
			ir_node *pop;

			/* copy ebp to esp */
			curr_sp = be_new_SetSP(env->isa->sp, env->irg, bl, curr_sp, curr_bp, *mem);

			/* pop ebp */
			pop     = new_rd_ia32_Pop(NULL, env->irg, bl, noreg, noreg, curr_sp, *mem);
			set_ia32_flags(pop, arch_irn_flags_ignore);
			curr_bp = new_r_Proj(current_ir_graph, bl, pop, mode_bp, pn_ia32_Pop_res);
			curr_sp = new_r_Proj(current_ir_graph, bl, pop, get_irn_mode(curr_sp), pn_ia32_Pop_stack);

			*mem = new_r_Proj(current_ir_graph, bl, pop, mode_M, pn_ia32_Pop_M);
		}
		arch_set_irn_register(env->aenv, curr_sp, env->isa->sp);
		arch_set_irn_register(env->aenv, curr_bp, env->isa->bp);
	}

	be_abi_reg_map_set(reg_map, env->isa->sp, curr_sp);
	be_abi_reg_map_set(reg_map, env->isa->bp, curr_bp);
}

/**
 * Initialize the callback object.
 * @param call The call object.
 * @param aenv The architecture environment.
 * @param irg  The graph with the method.
 * @return     Some pointer. This pointer is passed to all other callback functions as self object.
 */
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

/**
 * Destroy the callback object.
 * @param self The callback object.
 */
static void ia32_abi_done(void *self) {
	free(self);
}

/**
 * Produces the type which sits between the stack args and the locals on the stack.
 * it will contain the return address and space to store the old base pointer.
 * @return The Firm type modeling the ABI between type.
 */
static ir_type *ia32_abi_get_between_type(void *self)
{
#define IDENT(s) new_id_from_chars(s, sizeof(s)-1)
	static ir_type *omit_fp_between_type = NULL;
	static ir_type *between_type         = NULL;

	ia32_abi_env_t *env = self;

	if (! between_type) {
		ir_entity *old_bp_ent;
		ir_entity *ret_addr_ent;
		ir_entity *omit_fp_ret_addr_ent;

		ir_type *old_bp_type   = new_type_primitive(IDENT("bp"), mode_Iu);
		ir_type *ret_addr_type = new_type_primitive(IDENT("return_addr"), mode_Iu);

		between_type           = new_type_struct(IDENT("ia32_between_type"));
		old_bp_ent             = new_entity(between_type, IDENT("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, IDENT("ret_addr"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
		set_type_state(between_type, layout_fixed);

		omit_fp_between_type = new_type_struct(IDENT("ia32_between_type_omit_fp"));
		omit_fp_ret_addr_ent = new_entity(omit_fp_between_type, IDENT("ret_addr"), ret_addr_type);

		set_entity_offset(omit_fp_ret_addr_ent, 0);
		set_type_size_bytes(omit_fp_between_type, get_type_size_bytes(ret_addr_type));
		set_type_state(omit_fp_between_type, layout_fixed);
	}

	return env->flags.try_omit_fp ? omit_fp_between_type : between_type;
#undef IDENT
}

/**
 * Get the estimated cycle count for @p irn.
 *
 * @param self The this pointer.
 * @param irn  The node.
 *
 * @return     The estimated cycle count for this operation
 */
static int ia32_get_op_estimated_cost(const void *self, const ir_node *irn)
{
	int cost;
	ia32_op_type_t op_tp;
	const ia32_irn_ops_t *ops = self;

	if (is_Proj(irn))
		return 0;
	if (!is_ia32_irn(irn))
		return 0;

	assert(is_ia32_irn(irn));

	cost  = get_ia32_latency(irn);
	op_tp = get_ia32_op_type(irn);

	if (is_ia32_CopyB(irn)) {
		cost = 250;
		if (ARCH_INTEL(ops->cg->arch))
			cost += 150;
	}
	else if (is_ia32_CopyB_i(irn)) {
		int size = get_ia32_pncode(irn);
		cost     = 20 + (int)ceil((4/3) * size);
		if (ARCH_INTEL(ops->cg->arch))
			cost += 150;
	}
	/* in case of address mode operations add additional cycles */
	else if (op_tp == ia32_AddrModeD || op_tp == ia32_AddrModeS) {
		/*
			In case of stack access and access to fixed addresses add 5 cycles
			(we assume they are in cache), other memory operations cost 20
			cycles.
		*/
		if(is_ia32_use_frame(irn) ||
				(is_ia32_NoReg_GP(get_irn_n(irn, 0)) &&
		         is_ia32_NoReg_GP(get_irn_n(irn, 1)))) {
			cost += 5;
		} else {
			cost += 20;
		}
	}

	return cost;
}

/**
 * Returns the inverse operation if @p irn, recalculating the argument at position @p i.
 *
 * @param irn       The original operation
 * @param i         Index of the argument we want the inverse operation to yield
 * @param inverse   struct to be filled with the resulting inverse op
 * @param obstack   The obstack to use for allocation of the returned nodes array
 * @return          The inverse operation or NULL if operation invertible
 */
static arch_inverse_t *ia32_get_inverse(const void *self, const ir_node *irn, int i, arch_inverse_t *inverse, struct obstack *obst) {
	ir_graph *irg;
	ir_mode  *mode;
	ir_mode  *irn_mode;
	ir_node  *block, *noreg, *nomem;
	dbg_info *dbg;
	(void) self;

	/* we cannot invert non-ia32 irns */
	if (! is_ia32_irn(irn))
		return NULL;

	/* operand must always be a real operand (not base, index or mem) */
	if (i != 2 && i != 3)
		return NULL;

	/* we don't invert address mode operations */
	if (get_ia32_op_type(irn) != ia32_Normal)
		return NULL;

	/* TODO: adjust for new immediates... */
	ir_fprintf(stderr, "TODO: fix get_inverse for new immediates (%+F)\n",
	           irn);
	return NULL;

	irg      = get_irn_irg(irn);
	block    = get_nodes_block(irn);
	mode     = get_irn_mode(irn);
	irn_mode = get_irn_mode(irn);
	noreg    = get_irn_n(irn, 0);
	nomem    = new_r_NoMem(irg);
	dbg      = get_irn_dbg_info(irn);

	/* initialize structure */
	inverse->nodes = obstack_alloc(obst, 2 * sizeof(inverse->nodes[0]));
	inverse->costs = 0;
	inverse->n     = 1;

	switch (get_ia32_irn_opcode(irn)) {
		case iro_ia32_Add:
#if 0
			if (get_ia32_immop_type(irn) == ia32_ImmConst) {
				/* we have an add with a const here */
				/* invers == add with negated const */
				inverse->nodes[0] = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				inverse->costs   += 1;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
				set_ia32_Immop_tarval(inverse->nodes[0], tarval_neg(get_ia32_Immop_tarval(irn)));
				set_ia32_commutative(inverse->nodes[0]);
			}
			else if (get_ia32_immop_type(irn) == ia32_ImmSymConst) {
				/* we have an add with a symconst here */
				/* invers == sub with const */
				inverse->nodes[0] = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				inverse->costs   += 2;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
			}
			else {
				/* normal add: inverse == sub */
				inverse->nodes[0] = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, (ir_node*) irn, get_irn_n(irn, i ^ 1), nomem);
				inverse->costs   += 2;
			}
#endif
			break;
		case iro_ia32_Sub:
#if 0
			if (get_ia32_immop_type(irn) != ia32_ImmNone) {
				/* we have a sub with a const/symconst here */
				/* invers == add with this const */
				inverse->nodes[0] = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				inverse->costs   += (get_ia32_immop_type(irn) == ia32_ImmSymConst) ? 5 : 1;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
			}
			else {
				/* normal sub */
				if (i == 2) {
					inverse->nodes[0] = new_rd_ia32_Add(dbg, irg, block, noreg, noreg, (ir_node*) irn, get_irn_n(irn, 3), nomem);
				}
				else {
					inverse->nodes[0] = new_rd_ia32_Sub(dbg, irg, block, noreg, noreg, get_irn_n(irn, 2), (ir_node*) irn, nomem);
				}
				inverse->costs += 1;
			}
#endif
			break;
		case iro_ia32_Xor:
#if 0
			if (get_ia32_immop_type(irn) != ia32_ImmNone) {
				/* xor with const: inverse = xor */
				inverse->nodes[0] = new_rd_ia32_Xor(dbg, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				inverse->costs   += (get_ia32_immop_type(irn) == ia32_ImmSymConst) ? 5 : 1;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
			}
			else {
				/* normal xor */
				inverse->nodes[0] = new_rd_ia32_Xor(dbg, irg, block, noreg, noreg, (ir_node *) irn, get_irn_n(irn, i), nomem);
				inverse->costs   += 1;
			}
#endif
			break;
		case iro_ia32_Not: {
			inverse->nodes[0] = new_rd_ia32_Not(dbg, irg, block, (ir_node*) irn);
			inverse->costs   += 1;
			break;
		}
		case iro_ia32_Neg: {
			inverse->nodes[0] = new_rd_ia32_Neg(dbg, irg, block, (ir_node*) irn);
			inverse->costs   += 1;
			break;
		}
		default:
			/* inverse operation not supported */
			return NULL;
	}

	return inverse;
}

static ir_mode *get_spill_mode_mode(const ir_mode *mode)
{
	if(mode_is_float(mode))
		return mode_D;

	return mode_Iu;
}

/**
 * Get the mode that should be used for spilling value node
 */
static ir_mode *get_spill_mode(const ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	return get_spill_mode_mode(mode);
}

/**
 * Checks whether an addressmode reload for a node with mode mode is compatible
 * with a spillslot of mode spill_mode
 */
static int ia32_is_spillmode_compatible(const ir_mode *mode, const ir_mode *spillmode)
{
	if(mode_is_float(mode)) {
		return mode == spillmode;
	} else {
		return 1;
	}
}

/**
 * Check if irn can load it's operand at position i from memory (source addressmode).
 * @param self   Pointer to irn ops itself
 * @param irn    The irn to be checked
 * @param i      The operands position
 * @return Non-Zero if operand can be loaded
 */
static int ia32_possible_memory_operand(const void *self, const ir_node *irn, unsigned int i) {
	ir_node *op = get_irn_n(irn, i);
	const ir_mode *mode = get_irn_mode(op);
	const ir_mode *spillmode = get_spill_mode(op);
	(void) self;

	if (! is_ia32_irn(irn)                            ||  /* must be an ia32 irn */
		get_irn_arity(irn) != 5                       ||  /* must be a binary operation */
		get_ia32_op_type(irn) != ia32_Normal          ||  /* must not already be a addressmode irn */
		! (get_ia32_am_support(irn) & ia32_am_Source) ||  /* must be capable of source addressmode */
		! ia32_is_spillmode_compatible(mode, spillmode) ||
		(i != 2 && i != 3)                            ||  /* a "real" operand position must be requested */
		is_ia32_use_frame(irn))                           /* must not already use frame */
		return 0;

	if(i == 2) {
		const arch_register_req_t *req;
		if(!is_ia32_commutative(irn))
			return 0;
		/* we can't swap left/right for limited registers
		 * (As this (currently) breaks constraint handling copies)
		 */
		req = get_ia32_in_req(irn, 2);
		if(req->type & arch_register_req_type_limited) {
			return 0;
		}
	}

	return 1;
}

static void ia32_perform_memory_operand(const void *self, ir_node *irn,
                                        ir_node *spill, unsigned int i)
{
	const ia32_irn_ops_t *ops = self;
	ia32_code_gen_t      *cg  = ops->cg;

	assert(ia32_possible_memory_operand(self, irn, i) && "Cannot perform memory operand change");

	if (i == 2) {
		ia32_swap_left_right(irn);
	}

	set_ia32_op_type(irn, ia32_AddrModeS);
	set_ia32_ls_mode(irn, get_irn_mode(get_irn_n(irn, i)));
	set_ia32_use_frame(irn);
	set_ia32_need_stackent(irn);

	set_irn_n(irn, 0, get_irg_frame(get_irn_irg(irn)));
	set_irn_n(irn, 3, ia32_get_admissible_noreg(cg, irn, 3));
	set_irn_n(irn, 4, spill);

	/* immediates are only allowed on the right side */
	if(i == 2 && is_ia32_Immediate(get_irn_n(irn, 2))) {
		ia32_swap_left_right(irn);
	}
}

static const be_abi_callbacks_t ia32_abi_callbacks = {
	ia32_abi_init,
	ia32_abi_done,
	ia32_abi_get_between_type,
	ia32_abi_dont_save_regs,
	ia32_abi_prologue,
	ia32_abi_epilogue
};

/* fill register allocator interface */

static const arch_irn_ops_if_t ia32_irn_ops_if = {
	ia32_get_irn_reg_req,
	ia32_set_irn_reg,
	ia32_get_irn_reg,
	ia32_classify,
	ia32_get_flags,
	ia32_get_frame_entity,
	ia32_set_frame_entity,
	ia32_set_frame_offset,
	ia32_get_sp_bias,
	ia32_get_inverse,
	ia32_get_op_estimated_cost,
	ia32_possible_memory_operand,
	ia32_perform_memory_operand,
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

	ir_lower_mode_b(cg->irg, mode_Iu, 0);
	/* do local optimisations */
	optimize_graph_df(cg->irg);
	if(cg->dump)
		be_dump(cg->irg, "-lower_modeb", dump_ir_block_graph_sched);

	/* transform nodes into assembler instructions */
	ia32_transform_graph(cg);

	/* do local optimisations (mainly CSE) */
	optimize_graph_df(cg->irg);

	if (cg->dump)
		be_dump(cg->irg, "-transformed", dump_ir_block_graph_sched);

	/* optimize address mode */
	ia32_optimize_graph(cg);

	if (cg->dump)
		be_dump(cg->irg, "-am", dump_ir_block_graph_sched);

	/* do code placement, to optimize the position of constants */
	place_code(cg->irg);

	if (cg->dump)
		be_dump(cg->irg, "-place", dump_ir_block_graph_sched);
}

/**
 * Dummy functions for hooks we don't need but which must be filled.
 */
static void ia32_before_sched(void *self) {
	(void) self;
}

/**
 * Called before the register allocator.
 * Calculate a block schedule here. We need it for the x87
 * simulator and the emitter.
 */
static void ia32_before_ra(void *self) {
	ia32_code_gen_t *cg              = self;

	/* setup fpu rounding modes */
	ia32_setup_fpu_mode(cg);
}


/**
 * Transforms a be_Reload into a ia32 Load.
 */
static void transform_to_Load(ia32_code_gen_t *cg, ir_node *node) {
	ir_graph *irg        = get_irn_irg(node);
	dbg_info *dbg        = get_irn_dbg_info(node);
	ir_node *block       = get_nodes_block(node);
	ir_entity *ent       = be_get_frame_entity(node);
	ir_mode *mode        = get_irn_mode(node);
	ir_mode *spillmode   = get_spill_mode(node);
	ir_node *noreg       = ia32_new_NoReg_gp(cg);
	ir_node *sched_point = NULL;
	ir_node *ptr         = get_irg_frame(irg);
	ir_node *mem         = get_irn_n(node, be_pos_Reload_mem);
	ir_node *new_op, *proj;
	const arch_register_t *reg;

	if (sched_is_scheduled(node)) {
		sched_point = sched_prev(node);
	}

	if (mode_is_float(spillmode)) {
		if (USE_SSE2(cg))
			new_op = new_rd_ia32_xLoad(dbg, irg, block, ptr, noreg, mem, spillmode);
		else
			new_op = new_rd_ia32_vfld(dbg, irg, block, ptr, noreg, mem, spillmode);
	}
	else if (get_mode_size_bits(spillmode) == 128) {
		// Reload 128 bit sse registers
		new_op = new_rd_ia32_xxLoad(dbg, irg, block, ptr, noreg, mem);
	}
	else
		new_op = new_rd_ia32_Load(dbg, irg, block, ptr, noreg, mem);

	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_ls_mode(new_op, spillmode);
	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	DBG_OPT_RELOAD2LD(node, new_op);

	proj = new_rd_Proj(dbg, irg, block, new_op, mode, pn_ia32_Load_res);

	if (sched_point) {
		sched_add_after(sched_point, new_op);
		sched_remove(node);
	}

	/* copy the register from the old node to the new Load */
	reg = arch_get_irn_register(cg->arch_env, node);
	arch_set_irn_register(cg->arch_env, new_op, reg);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(cg, node));

	exchange(node, proj);
}

/**
 * Transforms a be_Spill node into a ia32 Store.
 */
static void transform_to_Store(ia32_code_gen_t *cg, ir_node *node) {
	ir_graph *irg  = get_irn_irg(node);
	dbg_info *dbg  = get_irn_dbg_info(node);
	ir_node *block = get_nodes_block(node);
	ir_entity *ent = be_get_frame_entity(node);
	const ir_node *spillval = get_irn_n(node, be_pos_Spill_val);
	ir_mode *mode  = get_spill_mode(spillval);
	ir_node *noreg = ia32_new_NoReg_gp(cg);
	ir_node *nomem = new_rd_NoMem(irg);
	ir_node *ptr   = get_irg_frame(irg);
	ir_node *val   = get_irn_n(node, be_pos_Spill_val);
	ir_node *store;
	ir_node *sched_point = NULL;

	if (sched_is_scheduled(node)) {
		sched_point = sched_prev(node);
	}

	/* No need to spill unknown values... */
	if(is_ia32_Unknown_GP(val) ||
		is_ia32_Unknown_VFP(val) ||
		is_ia32_Unknown_XMM(val)) {
		store = nomem;
		if(sched_point)
			sched_remove(node);

		exchange(node, store);
		return;
	}

	if (mode_is_float(mode)) {
		if (USE_SSE2(cg))
			store = new_rd_ia32_xStore(dbg, irg, block, ptr, noreg, val, nomem);
		else
			store = new_rd_ia32_vfst(dbg, irg, block, ptr, noreg, val, nomem, mode);
	} else if (get_mode_size_bits(mode) == 128) {
		// Spill 128 bit SSE registers
		store = new_rd_ia32_xxStore(dbg, irg, block, ptr, noreg, val, nomem);
	} else if (get_mode_size_bits(mode) == 8) {
		store = new_rd_ia32_Store8Bit(dbg, irg, block, ptr, noreg, val, nomem);
	} else {
		store = new_rd_ia32_Store(dbg, irg, block, ptr, noreg, val, nomem);
	}

	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, mode);
	set_ia32_frame_ent(store, ent);
	set_ia32_use_frame(store);
	SET_IA32_ORIG_NODE(store, ia32_get_old_node_name(cg, node));
	DBG_OPT_SPILL2ST(node, store);

	if (sched_point) {
		sched_add_after(sched_point, store);
		sched_remove(node);
	}

	exchange(node, store);
}

static ir_node *create_push(ia32_code_gen_t *cg, ir_node *node, ir_node *schedpoint, ir_node *sp, ir_node *mem, ir_entity *ent) {
	ir_graph *irg = get_irn_irg(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_node *block = get_nodes_block(node);
	ir_node *noreg = ia32_new_NoReg_gp(cg);
	ir_node *frame = get_irg_frame(irg);

	ir_node *push = new_rd_ia32_Push(dbg, irg, block, frame, noreg, noreg, sp, mem);

	set_ia32_frame_ent(push, ent);
	set_ia32_use_frame(push);
	set_ia32_op_type(push, ia32_AddrModeS);
	set_ia32_ls_mode(push, mode_Is);

	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ia32_code_gen_t *cg, ir_node *node, ir_node *schedpoint, ir_node *sp, ir_entity *ent) {
	ir_graph *irg = get_irn_irg(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_node *block = get_nodes_block(node);
	ir_node *noreg = ia32_new_NoReg_gp(cg);
	ir_node *frame = get_irg_frame(irg);

	ir_node *pop = new_rd_ia32_Pop(dbg, irg, block, frame, noreg, sp, new_NoMem());

	set_ia32_frame_ent(pop, ent);
	set_ia32_use_frame(pop);
	set_ia32_op_type(pop, ia32_AddrModeD);
	set_ia32_ls_mode(pop, mode_Is);

	sched_add_before(schedpoint, pop);

	return pop;
}

static ir_node* create_spproj(ia32_code_gen_t *cg, ir_node *node, ir_node *pred, int pos) {
	ir_graph *irg = get_irn_irg(node);
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_node *block = get_nodes_block(node);
	ir_mode *spmode = mode_Iu;
	const arch_register_t *spreg = &ia32_gp_regs[REG_ESP];
	ir_node *sp;

	sp = new_rd_Proj(dbg, irg, block, pred, spmode, pos);
	arch_set_irn_register(cg->arch_env, sp, spreg);

	return sp;
}

/**
 * Transform memperm, currently we do this the ugly way and produce
 * push/pop into/from memory cascades. This is possible without using
 * any registers.
 */
static void transform_MemPerm(ia32_code_gen_t *cg, ir_node *node) {
	ir_graph *irg = get_irn_irg(node);
	ir_node *block = get_nodes_block(node);
	ir_node *in[1];
	ir_node *keep;
	int i, arity;
	ir_node *sp = be_abi_get_ignore_irn(cg->birg->abi, &ia32_gp_regs[REG_ESP]);
	const ir_edge_t *edge;
	const ir_edge_t *next;
	ir_node **pops;

	arity = be_get_MemPerm_entity_arity(node);
	pops = alloca(arity * sizeof(pops[0]));

	// create pushs
	for(i = 0; i < arity; ++i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(inent);
		int entbits = get_type_size_bits(enttype);
		int entbits2 = get_type_size_bits(get_entity_type(outent));
		ir_node *mem = get_irn_n(node, i + 1);
		ir_node *push;

		/* work around cases where entities have different sizes */
		if(entbits2 < entbits)
			entbits = entbits2;
		assert( (entbits == 32 || entbits == 64) && "spillslot on x86 should be 32 or 64 bit");

		push = create_push(cg, node, node, sp, mem, inent);
		sp = create_spproj(cg, node, push, pn_ia32_Push_stack);
		if(entbits == 64) {
			// add another push after the first one
			push = create_push(cg, node, node, sp, mem, inent);
			add_ia32_am_offs_int(push, 4);
			sp = create_spproj(cg, node, push, pn_ia32_Push_stack);
		}

		set_irn_n(node, i, new_Bad());
	}

	// create pops
	for(i = arity - 1; i >= 0; --i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(outent);
		int entbits = get_type_size_bits(enttype);
		int entbits2 = get_type_size_bits(get_entity_type(inent));
		ir_node *pop;

		/* work around cases where entities have different sizes */
		if(entbits2 < entbits)
			entbits = entbits2;
		assert( (entbits == 32 || entbits == 64) && "spillslot on x86 should be 32 or 64 bit");

		pop = create_pop(cg, node, node, sp, outent);
		sp = create_spproj(cg, node, pop, pn_ia32_Pop_stack);
		if(entbits == 64) {
			add_ia32_am_offs_int(pop, 4);

			// add another pop after the first one
			pop = create_pop(cg, node, node, sp, outent);
			sp = create_spproj(cg, node, pop, pn_ia32_Pop_stack);
		}

		pops[i] = pop;
	}

	in[0] = sp;
	keep = be_new_Keep(&ia32_reg_classes[CLASS_ia32_gp], irg, block, 1, in);
	sched_add_before(node, keep);

	// exchange memprojs
	foreach_out_edge_safe(node, edge, next) {
		ir_node *proj = get_edge_src_irn(edge);
		int p = get_Proj_proj(proj);

		assert(p < arity);

		set_Proj_pred(proj, pops[p]);
		set_Proj_proj(proj, pn_ia32_Pop_M);
	}

	// remove memperm
	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		set_irn_n(node, i, new_Bad());
	}
	sched_remove(node);
}

/**
 * Block-Walker: Calls the transform functions Spill and Reload.
 */
static void ia32_after_ra_walker(ir_node *block, void *env) {
	ir_node *node, *prev;
	ia32_code_gen_t *cg = env;

	/* beware: the schedule is changed here */
	for (node = sched_last(block); !sched_is_begin(node); node = prev) {
		prev = sched_prev(node);

		if (be_is_Reload(node)) {
			transform_to_Load(cg, node);
		} else if (be_is_Spill(node)) {
			transform_to_Store(cg, node);
		} else if(be_is_MemPerm(node)) {
			transform_MemPerm(cg, node);
		}
	}
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void ia32_collect_frame_entity_nodes(ir_node *node, void *data)
{
	be_fec_env_t *env = data;

	if (be_is_Reload(node) && be_get_frame_entity(node) == NULL) {
		const ir_mode *mode = get_spill_mode_mode(get_irn_mode(node));
		int align = get_mode_size_bytes(mode);
		be_node_needs_frame_entity(env, node, mode, align);
	} else if(is_ia32_irn(node) && get_ia32_frame_ent(node) == NULL
	          && is_ia32_use_frame(node)) {
		if (is_ia32_need_stackent(node) || is_ia32_Load(node)) {
			const ir_mode     *mode  = get_ia32_ls_mode(node);
			const ia32_attr_t *attr  = get_ia32_attr_const(node);
			int                align = get_mode_size_bytes(mode);

			if(attr->data.need_64bit_stackent) {
				mode = mode_Ls;
			}
			if(attr->data.need_32bit_stackent) {
				mode = mode_Is;
			}
			be_node_needs_frame_entity(env, node, mode, align);
		} else if (is_ia32_vfild(node) || is_ia32_xLoad(node)
		           || is_ia32_vfld(node)) {
			const ir_mode *mode = get_ia32_ls_mode(node);
			int align = 4;
			be_node_needs_frame_entity(env, node, mode, align);
		} else if(is_ia32_FldCW(node)) {
			const ir_mode *mode = ia32_reg_classes[CLASS_ia32_fp_cw].mode;
			int align = 4;
			be_node_needs_frame_entity(env, node, mode, align);
		} else {
#ifndef NDEBUG
			assert(is_ia32_St(node) ||
 				   is_ia32_xStoreSimple(node) ||
				   is_ia32_vfst(node) ||
				   is_ia32_vfist(node) ||
			       is_ia32_FnstCW(node));
#endif
		}
	}
}

/**
 * We transform Spill and Reload here. This needs to be done before
 * stack biasing otherwise we would miss the corrected offset for these nodes.
 */
static void ia32_after_ra(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph *irg = cg->irg;
	be_fec_env_t *fec_env = be_new_frame_entity_coalescer(cg->birg);

	/* create and coalesce frame entities */
	irg_walk_graph(irg, NULL, ia32_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env);
	be_free_frame_entity_coalescer(fec_env);

	irg_block_walk_graph(irg, NULL, ia32_after_ra_walker, cg);

	ia32_finish_irg(irg, cg);
}

/**
 * Last touchups for the graph before emit: x87 simulation to replace the
 * virtual with real x87 instructions, creating a block schedule and peephole
 * optimisations.
 */
static void ia32_finish(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;

	/* we might have to rewrite x87 virtual registers */
	if (cg->do_x87_sim) {
		x87_simulate_graph(cg->arch_env, cg->birg);
	}

	/* create block schedule, this also removes empty blocks which might
	 * produce critical edges */
	cg->blk_sched = be_create_block_schedule(irg, cg->birg->exec_freq);

	/* do peephole optimisations */
	ia32_peephole_optimization(irg, cg);
}

/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ia32_codegen(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;

	ia32_gen_routine(cg, irg);

	cur_reg_set = NULL;

	/* remove it from the isa */
	cg->isa->cg = NULL;

	assert(ia32_current_cg == cg);
	ia32_current_cg = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(cg);
}

static void *ia32_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t ia32_code_gen_if = {
	ia32_cg_init,
	NULL,                /* before abi introduce hook */
	ia32_prepare_graph,
	NULL,                /* spill */
	ia32_before_sched,   /* before scheduling hook */
	ia32_before_ra,      /* before register allocation hook */
	ia32_after_ra,       /* after register allocation hook */
	ia32_finish,         /* called before codegen */
	ia32_codegen         /* emit && done */
};

/**
 * Initializes a IA32 code generator.
 */
static void *ia32_cg_init(be_irg_t *birg) {
	ia32_isa_t      *isa = (ia32_isa_t *)birg->main_env->arch_env->isa;
	ia32_code_gen_t *cg  = xcalloc(1, sizeof(*cg));

	cg->impl      = &ia32_code_gen_if;
	cg->irg       = birg->irg;
	cg->reg_set   = new_set(ia32_cmp_irn_reg_assoc, 1024);
	cg->arch_env  = birg->main_env->arch_env;
	cg->isa       = isa;
	cg->birg      = birg;
	cg->blk_sched = NULL;
	cg->fp_kind   = isa->fp_kind;
	cg->dump      = (birg->main_env->options->dump_flags & DUMP_BE) ? 1 : 0;

	/* copy optimizations from isa for easier access */
	cg->opt      = isa->opt;
	cg->arch     = isa->arch;
	cg->opt_arch = isa->opt_arch;

	/* enter it */
	isa->cg = cg;

#ifndef NDEBUG
	if (isa->name_obst) {
		obstack_free(isa->name_obst, NULL);
		obstack_init(isa->name_obst);
	}
#endif /* NDEBUG */

	cur_reg_set = cg->reg_set;

	ia32_irn_ops.cg = cg;

	assert(ia32_current_cg == NULL);
	ia32_current_cg = cg;

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
 * Set output modes for GCC
 */
static const tarval_mode_info mo_integer = {
	TVO_HEX,
	"0x",
	NULL,
};

/*
 * set the tarval output mode of all integer modes to decimal
 */
static void set_tarval_output_modes(void)
{
	int i;

	for (i = get_irp_n_modes() - 1; i >= 0; --i) {
		ir_mode *mode = get_irp_mode(i);

		if (mode_is_int(mode))
			set_tarval_mode_output_option(mode, &mo_integer);
	}
}

const arch_isa_if_t ia32_isa_if;

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
		NULL,                    /* main environment */
		7,                       /* costs for a spill instruction */
		5,                       /* costs for a reload instruction */
	},
	NULL_EMITTER,                /* emitter environment */
	NULL,                    /* 16bit register names */
	NULL,                    /* 8bit register names */
	NULL,                    /* 8bit register names high */
	NULL,                    /* types */
	NULL,                    /* tv_ents */
	(0                 |
	IA32_OPT_INCDEC    |     /* optimize add 1, sub 1 into inc/dec               default: on */
	IA32_OPT_DOAM      |     /* optimize address mode                            default: on */
	IA32_OPT_LEA       |     /* optimize for LEAs                                default: on */
	IA32_OPT_PLACECNST |     /* place constants immediately before instructions, default: on */
	IA32_OPT_IMMOPS    |     /* operations can use immediates,                   default: on */
	IA32_OPT_PUSHARGS),      /* create pushs for function argument passing,      default: on */
	arch_pentium_4,          /* instruction architecture */
	arch_pentium_4,          /* optimize for architecture */
	fp_x87,                  /* floating point mode */
	NULL,                    /* current code generator */
#ifndef NDEBUG
	NULL,                    /* name obstack */
	0                        /* name obst size */
#endif
};

static void set_arch_costs(enum cpu_support arch);

/**
 * Initializes the backend ISA.
 */
static void *ia32_init(FILE *file_handle) {
	static int inited = 0;
	ia32_isa_t *isa;

	if (inited)
		return NULL;
	inited = 1;

	set_tarval_output_modes();

	isa = xmalloc(sizeof(*isa));
	memcpy(isa, &ia32_isa_template, sizeof(*isa));

	if(mode_fpcw == NULL) {
		mode_fpcw = new_ir_mode("Fpcw", irms_int_number, 16, 0, irma_none, 0);
	}

	ia32_register_init();
	ia32_create_opcodes();

	set_arch_costs(isa->opt_arch);

	if ((ARCH_INTEL(isa->arch) && isa->arch < arch_pentium_4) ||
	    (ARCH_AMD(isa->arch) && isa->arch < arch_athlon))
		/* no SSE2 for these cpu's */
		isa->fp_kind = fp_x87;

	if (ARCH_INTEL(isa->opt_arch) && isa->opt_arch >= arch_pentium_4) {
		/* Pentium 4 don't like inc and dec instructions */
		isa->opt &= ~IA32_OPT_INCDEC;
	}

	be_emit_init_env(&isa->emit, file_handle);
	isa->regs_16bit     = pmap_create();
	isa->regs_8bit      = pmap_create();
	isa->regs_8bit_high = pmap_create();
	isa->types          = pmap_create();
	isa->tv_ent         = pmap_create();
	isa->cpu            = ia32_init_machine_description();

	ia32_build_16bit_reg_map(isa->regs_16bit);
	ia32_build_8bit_reg_map(isa->regs_8bit);
	ia32_build_8bit_reg_map_high(isa->regs_8bit_high);

#ifndef NDEBUG
	isa->name_obst = xmalloc(sizeof(*isa->name_obst));
	obstack_init(isa->name_obst);
#endif /* NDEBUG */

	ia32_handle_intrinsics();

	/* needed for the debug support */
	be_gas_emit_switch_section(&isa->emit, GAS_SECTION_TEXT);
	be_emit_cstring(&isa->emit, ".Ltext0:\n");
	be_emit_write_line(&isa->emit);

	/* we mark referenced global entities, so we can only emit those which
	 * are actually referenced. (Note: you mustn't use the type visited flag
	 * elsewhere in the backend)
	 */
	inc_master_type_visited();

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void ia32_done(void *self) {
	ia32_isa_t *isa = self;

	/* emit now all global declarations */
	be_gas_emit_decls(&isa->emit, isa->arch_isa.main_env, 1);

	pmap_destroy(isa->regs_16bit);
	pmap_destroy(isa->regs_8bit);
	pmap_destroy(isa->regs_8bit_high);
	pmap_destroy(isa->tv_ent);
	pmap_destroy(isa->types);

#ifndef NDEBUG
	obstack_free(isa->name_obst, NULL);
#endif /* NDEBUG */

	be_emit_destroy_env(&isa->emit);

	free(self);
}


/**
 * Return the number of register classes for this architecture.
 * We report always these:
 *  - the general purpose registers
 *  - the SSE floating point register set
 *  - the virtual floating point registers
 *  - the SSE vector register set
 */
static int ia32_get_n_reg_class(const void *self) {
	(void) self;
	return N_CLASSES;
}

/**
 * Return the register class for index i.
 */
static const arch_register_class_t *ia32_get_reg_class(const void *self, int i)
{
	(void) self;
	assert(i >= 0 && i < N_CLASSES);
	return &ia32_reg_classes[i];
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
	unsigned  cc;
	int       n, i, regnum;
	be_abi_call_flags_t call_flags = be_abi_call_get_flags(abi);

	unsigned use_push = !IS_P6_ARCH(isa->opt_arch);

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;  /* always last arg first on stack */
	call_flags.bits.store_args_sequential = use_push;
	/* call_flags.bits.try_omit_fp                 not changed: can handle both settings */
	call_flags.bits.fp_free               = 0;  /* the frame pointer is fixed in IA32 */
	call_flags.bits.call_has_imm          = 1;  /* IA32 calls can have immediate address */

	/* set parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &ia32_abi_callbacks);

	if (get_method_variadicity(method_type) == variadicity_variadic) {
		/* pass all parameters of a variadic function on the stack */
		cc = cc_cdecl_set;
	} else {
		cc = get_method_calling_convention(method_type);
		if (get_method_additional_properties(method_type) & mtp_property_private) {
			/* set the calling conventions to register parameter */
			cc = (cc & ~cc_bits) | cc_reg_param;
		}
	}
	n = get_method_n_params(method_type);
	for (i = regnum = 0; i < n; i++) {
		const ir_mode         *mode;
		const arch_register_t *reg = NULL;

		tp   = get_method_param_type(method_type, i);
		mode = get_type_mode(tp);
		if (mode != NULL) {
			reg  = ia32_get_RegParam_reg(isa->cg, cc, regnum, mode);
		}
		if (reg != NULL) {
			be_abi_call_param_reg(abi, i, reg);
			++regnum;
		} else {
			be_abi_call_param_stack(abi, i, 4, 0, 0);
		}
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

		assert(!mode_is_float(mode) && "mixed INT, FP results not supported");

		be_abi_call_res_reg(abi, 0, &ia32_gp_regs[REG_EAX]);
		be_abi_call_res_reg(abi, 1, &ia32_gp_regs[REG_EDX]);
	}
	else if (n == 1) {
		const arch_register_t *reg;

		tp   = get_method_res_type(method_type, 0);
		assert(is_atomic_type(tp));
		mode = get_type_mode(tp);

		reg = mode_is_float(mode) ? &ia32_vfp_regs[REG_VF0] : &ia32_gp_regs[REG_EAX];

		be_abi_call_res_reg(abi, 0, reg);
	}
}


static const void *ia32_get_irn_ops(const arch_irn_handler_t *self,
                                    const ir_node *irn)
{
	(void) self;
	(void) irn;
	return &ia32_irn_ops;
}

const arch_irn_handler_t ia32_irn_handler = {
	ia32_get_irn_ops
};

const arch_irn_handler_t *ia32_get_irn_handler(const void *self)
{
	(void) self;
	return &ia32_irn_handler;
}

int ia32_to_appear_in_schedule(void *block_env, const ir_node *irn)
{
	(void) block_env;

	if(!is_ia32_irn(irn)) {
		return -1;
	}

	if(is_ia32_NoReg_GP(irn) || is_ia32_NoReg_VFP(irn) || is_ia32_NoReg_XMM(irn)
		|| is_ia32_Unknown_GP(irn) || is_ia32_Unknown_XMM(irn)
		|| is_ia32_Unknown_VFP(irn) || is_ia32_ChangeCW(irn)
		|| is_ia32_Immediate(irn))
		return 0;

	return 1;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *ia32_get_code_generator_if(void *self)
{
	(void) self;
	return &ia32_code_gen_if;
}

/**
 * Returns the estimated execution time of an ia32 irn.
 */
static sched_timestep_t ia32_sched_exectime(void *env, const ir_node *irn) {
	const arch_env_t *arch_env = env;
	return is_ia32_irn(irn) ? ia32_get_op_estimated_cost(arch_get_irn_ops(arch_env, irn), irn) : 1;
}

list_sched_selector_t ia32_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *ia32_get_list_sched_selector(
		const void *self, list_sched_selector_t *selector)
{
	(void) self;
	memcpy(&ia32_sched_selector, selector, sizeof(ia32_sched_selector));
	ia32_sched_selector.exectime              = ia32_sched_exectime;
	ia32_sched_selector.to_appear_in_schedule = ia32_to_appear_in_schedule;
	return &ia32_sched_selector;
}

static const ilp_sched_selector_t *ia32_get_ilp_sched_selector(const void *self)
{
	(void) self;
	return NULL;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int ia32_get_reg_class_alignment(const void *self,
                                        const arch_register_class_t *cls)
{
	ir_mode *mode = arch_register_class_mode(cls);
	int bytes     = get_mode_size_bytes(mode);
	(void) self;

	if (mode_is_float(mode) && bytes > 8)
		return 16;
	return bytes;
}

static const be_execution_unit_t ***ia32_get_allowed_execution_units(
		const void *self, const ir_node *irn)
{
	static const be_execution_unit_t *_allowed_units_BRANCH[] = {
		&ia32_execution_units_BRANCH[IA32_EXECUNIT_TP_BRANCH_BRANCH1],
		&ia32_execution_units_BRANCH[IA32_EXECUNIT_TP_BRANCH_BRANCH2],
		NULL,
	};
	static const be_execution_unit_t *_allowed_units_GP[] = {
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_EAX],
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_EBX],
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_ECX],
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_EDX],
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_ESI],
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_EDI],
		&ia32_execution_units_GP[IA32_EXECUNIT_TP_GP_GP_EBP],
		NULL,
	};
	static const be_execution_unit_t *_allowed_units_DUMMY[] = {
		&be_machine_execution_units_DUMMY[0],
		NULL,
	};
	static const be_execution_unit_t **_units_callret[] = {
		_allowed_units_BRANCH,
		NULL
	};
	static const be_execution_unit_t **_units_other[] = {
		_allowed_units_GP,
		NULL
	};
	static const be_execution_unit_t **_units_dummy[] = {
		_allowed_units_DUMMY,
		NULL
	};
	const be_execution_unit_t ***ret;
	(void) self;

	if (is_ia32_irn(irn)) {
		ret = get_ia32_exec_units(irn);
	}
	else if (is_be_node(irn)) {
		if (be_is_Call(irn) || be_is_Return(irn)) {
			ret = _units_callret;
		}
		else if (be_is_Barrier(irn)) {
			ret = _units_dummy;
		}
		else {
			 ret = _units_other;
		}
	}
	else {
		ret = _units_dummy;
	}

	return ret;
}

/**
 * Return the abstract ia32 machine.
 */
static const be_machine_t *ia32_get_machine(const void *self) {
	const ia32_isa_t *isa = self;
	return isa->cpu;
}

/**
 * Return irp irgs in the desired order.
 */
static ir_graph **ia32_get_irg_list(const void *self, ir_graph ***irg_list)
{
	(void) self;
	(void) irg_list;
	return NULL;
}

/**
 * Allows or disallows the creation of Psi nodes for the given Phi nodes.
 * @return 1 if allowed, 0 otherwise
 */
static int ia32_is_psi_allowed(ir_node *sel, ir_node *phi_list, int i, int j)
{
	ir_node *phi;

	(void)sel;
	(void)i;
	(void)j;

#if 1
	if(is_Proj(sel)) {
		ir_node *pred = get_Proj_pred(sel);
		if(is_Cmp(pred)) {
			ir_node *left     = get_Cmp_left(pred);
			ir_mode *cmp_mode = get_irn_mode(left);
			if(mode_is_float(cmp_mode))
				return 0;
		}
	}
#endif

	/* check the Phi nodes */
	for (phi = phi_list; phi; phi = get_irn_link(phi)) {
		ir_mode *mode = get_irn_mode(phi);

		if (mode_is_float(mode) || get_mode_size_bits(mode) > 32)
			return 0;
	}

	return 1;
}

typedef struct insn_const {
	int add_cost;       /**< cost of an add instruction */
	int lea_cost;       /**< cost of a lea instruction */
	int const_shf_cost; /**< cost of a constant shift instruction */
	int cost_mul_start; /**< starting cost of a multiply instruction */
	int cost_mul_bit;   /**< cost of multiply for every set bit */
} insn_const;

/* costs for the i386 */
static const insn_const i386_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	2,   /* cost of a constant shift instruction */
	6,   /* starting cost of a multiply instruction */
	1    /* cost of multiply for every set bit */
};

/* costs for the i486 */
static const insn_const i486_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	2,   /* cost of a constant shift instruction */
	12,  /* starting cost of a multiply instruction */
	1    /* cost of multiply for every set bit */
};

/* costs for the Pentium */
static const insn_const pentium_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	11,  /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Pentium Pro */
static const insn_const pentiumpro_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	4,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the K6 */
static const insn_const k6_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	3,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Athlon */
static const insn_const athlon_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	5,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Pentium 4 */
static const insn_const pentium4_cost = {
	1,   /* cost of an add instruction */
	3,   /* cost of a lea instruction */
	4,   /* cost of a constant shift instruction */
	15,  /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the Core */
static const insn_const core_cost = {
	1,   /* cost of an add instruction */
	1,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	10,  /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

/* costs for the generic */
static const insn_const generic_cost = {
	1,   /* cost of an add instruction */
	2,   /* cost of a lea instruction */
	1,   /* cost of a constant shift instruction */
	4,   /* starting cost of a multiply instruction */
	0    /* cost of multiply for every set bit */
};

static const insn_const *arch_costs = &generic_cost;

static void set_arch_costs(enum cpu_support arch) {
	switch (arch) {
	case arch_i386:
		arch_costs = &i386_cost;
		break;
	case arch_i486:
		arch_costs = &i486_cost;
		break;
	case arch_pentium:
	case arch_pentium_mmx:
		arch_costs = &pentium_cost;
		break;
	case arch_pentium_pro:
	case arch_pentium_2:
	case arch_pentium_3:
		arch_costs = &pentiumpro_cost;
		break;
	case arch_pentium_4:
		arch_costs = &pentium4_cost;
		break;
	case arch_pentium_m:
		arch_costs = &pentiumpro_cost;
		break;
	case arch_core:
		arch_costs = &core_cost;
		break;
	case arch_k6:
		arch_costs = &k6_cost;
		break;
	case arch_athlon:
	case arch_athlon_64:
	case arch_opteron:
		arch_costs = &athlon_cost;
		break;
	case arch_generic:
	default:
		arch_costs = &generic_cost;
	}
}

/**
 * Evaluate a given simple instruction.
 */
static int ia32_evaluate_insn(insn_kind kind, tarval *tv) {
	int cost;

	switch (kind) {
	case MUL:
		cost =  arch_costs->cost_mul_start;
		if (arch_costs->cost_mul_bit > 0) {
			char *bitstr = get_tarval_bitpattern(tv);
			int i;

			for (i = 0; bitstr[i] != '\0'; ++i) {
				if (bitstr[i] == '1') {
					cost += arch_costs->cost_mul_bit;
				}
			}
			free(bitstr);
		}
		return cost;
	case LEA:
		return arch_costs->lea_cost;
	case ADD:
	case SUB:
		return arch_costs->add_cost;
	case SHIFT:
		return arch_costs->const_shf_cost;
	default:
		return 1;
	}
}

static ia32_intrinsic_env_t intrinsic_env = {
	NULL,    /**< the irg, these entities belong to */
	NULL,    /**< entity for first div operand (move into FPU) */
	NULL,    /**< entity for second div operand (move into FPU) */
	NULL,    /**< entity for converts ll -> d */
	NULL,    /**< entity for converts d -> ll */
	NULL,    /**< entity for __divdi3 library call */
	NULL,    /**< entity for __moddi3 library call */
	NULL,    /**< entity for __udivdi3 library call */
	NULL,    /**< entity for __umoddi3 library call */
};

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *ia32_get_libfirm_params(void) {
	static const ir_settings_if_conv_t ifconv = {
		4,                    /* maxdepth, doesn't matter for Psi-conversion */
		ia32_is_psi_allowed   /* allows or disallows Psi creation for given selector */
	};
	static const ir_settings_arch_dep_t ad = {
		1,                   /* also use subs */
		4,                   /* maximum shifts */
		31,                  /* maximum shift amount */
		ia32_evaluate_insn,  /* evaluate the instruction sequence */

		1,  /* allow Mulhs */
		1,  /* allow Mulus */
		32  /* Mulh allowed up to 32 bit */
	};
	static backend_params p = {
		1,     /* need dword lowering */
		1,     /* support inline assembly */
		NULL,  /* no additional opcodes */
		NULL,  /* will be set later */
		ia32_create_intrinsic_fkt,
		&intrinsic_env,  /* context for ia32_create_intrinsic_fkt */
		NULL,  /* will be set below */
	};

	p.dep_param    = &ad;
	p.if_conv_info = &ifconv;
	return &p;
}

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
	{ "generic",    arch_generic, },
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

static const lc_opt_enum_int_items_t gas_items[] = {
	{ "normal",  GAS_FLAVOUR_NORMAL },
	{ "mingw",   GAS_FLAVOUR_MINGW  },
	{ NULL,      0 }
};

static lc_opt_enum_int_var_t gas_var = {
	(int*) &be_gas_flavour, gas_items
};

static const lc_opt_table_entry_t ia32_options[] = {
	LC_OPT_ENT_ENUM_INT("arch",      "select the instruction architecture", &arch_var),
	LC_OPT_ENT_ENUM_INT("opt",       "optimize for instruction architecture", &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpunit",    "select the floating point unit", &fp_unit_var),
	LC_OPT_ENT_NEGBIT("noaddrmode",  "do not use address mode", &ia32_isa_template.opt, IA32_OPT_DOAM),
	LC_OPT_ENT_NEGBIT("nolea",       "do not optimize for LEAs", &ia32_isa_template.opt, IA32_OPT_LEA),
	LC_OPT_ENT_NEGBIT("noplacecnst", "do not place constants", &ia32_isa_template.opt, IA32_OPT_PLACECNST),
	LC_OPT_ENT_NEGBIT("noimmop",     "no operations with immediates", &ia32_isa_template.opt, IA32_OPT_IMMOPS),
	LC_OPT_ENT_NEGBIT("nopushargs",  "do not create pushs for function arguments", &ia32_isa_template.opt, IA32_OPT_PUSHARGS),
	LC_OPT_ENT_ENUM_INT("gasmode",   "set the GAS compatibility mode", &gas_var),
	LC_OPT_LAST
};

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
	ia32_get_ilp_sched_selector,
	ia32_get_reg_class_alignment,
	ia32_get_libfirm_params,
	ia32_get_allowed_execution_units,
	ia32_get_machine,
	ia32_get_irg_list,
};

void ia32_init_emitter(void);
void ia32_init_finish(void);
void ia32_init_optimize(void);
void ia32_init_transform(void);
void ia32_init_x87(void);

void be_init_arch_ia32(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_options);
	be_register_isa_if("ia32", &ia32_isa_if);

	FIRM_DBG_REGISTER(dbg, "firm.be.ia32.cg");

	ia32_init_emitter();
	ia32_init_finish();
	ia32_init_optimize();
	ia32_init_transform();
	ia32_init_x87();
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_ia32);
