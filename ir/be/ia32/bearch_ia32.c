/**
 * This is the main ia32 firm backend driver.
 * @author Christian Wuerdig
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

#include <math.h>

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irbitset.h"
#include "pdeq.h"
#include "debug.h"

#include "../beabi.h"                 /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "../be.h"
#include "../be_t.h"
#include "../beirgmod.h"
#include "../be_dbgout.h"
#include "../beblocksched.h"
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
#include "ia32_finish.h"
#include "ia32_util.h"

#define DEBUG_MODULE "firm.be.ia32.isa"

/* TODO: ugly */
static set *cur_reg_set = NULL;

/* Creates the unique per irg GP NoReg node. */
ir_node *ia32_new_NoReg_gp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi, &ia32_gp_regs[REG_GP_NOREG]);
}

/* Creates the unique per irg FP NoReg node. */
ir_node *ia32_new_NoReg_fp(ia32_code_gen_t *cg) {
	return be_abi_get_callee_save_irn(cg->birg->abi,
		USE_SSE2(cg) ? &ia32_xmm_regs[REG_XMM_NOREG] : &ia32_vfp_regs[REG_VFP_NOREG]);
}

/**
 * Returns gp_noreg or fp_noreg, depending in input requirements.
 */
ir_node *ia32_get_admissible_noreg(ia32_code_gen_t *cg, ir_node *irn, int pos) {
	arch_register_req_t       req;
	const arch_register_req_t *p_req;

	p_req = arch_get_register_req(cg->arch_env, &req, irn, pos);
	assert(p_req && "Missing register requirements");
	if (p_req->cls == &ia32_reg_classes[CLASS_ia32_gp])
		return ia32_new_NoReg_gp(cg);
	else
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
		if(pos >= 0) {
			DBG((mod, LEVEL_1, "ignoring request IN requirements for node %+F\n", irn));
			return NULL;
		}

		node_pos = (pos == -1) ? get_Proj_proj(irn) : pos;
		irn      = skip_Proj(irn);

		DB((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", irn, node_pos));
	}

	if (is_ia32_irn(irn)) {
		irn_req = (pos >= 0) ? get_ia32_in_req(irn, pos) : get_ia32_out_req(irn, node_pos);
		if (irn_req == NULL) {
			/* no requirements */
			return NULL;
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
		pos = get_Proj_proj(irn);
		irn = skip_Proj(irn);
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

		pos = get_Proj_proj(irn);
		irn = skip_Proj(irn);
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
	arch_irn_class_t classification = arch_irn_class_normal;

	irn = skip_Proj(irn);

	if (is_cfop(irn))
		classification |= arch_irn_class_branch;

	if (! is_ia32_irn(irn))
		return classification & ~arch_irn_class_normal;

	if (is_ia32_Cnst(irn))
		classification |= arch_irn_class_const;

	if (is_ia32_Ld(irn))
		classification |= arch_irn_class_load;

	if (is_ia32_St(irn) || is_ia32_Store8Bit(irn))
		classification |= arch_irn_class_store;

	if (is_ia32_got_reload(irn))
		classification |= arch_irn_class_reload;

	return classification;
}

static arch_irn_flags_t ia32_get_flags(const void *self, const ir_node *irn) {
	arch_irn_flags_t flags;
	ir_node          *pred = is_Proj(irn) && mode_is_datab(get_irn_mode(irn)) ? get_Proj_pred(irn) : NULL;

	if (is_Unknown(irn))
		flags = arch_irn_flags_ignore;
	else {
	    /* pred is only set, if we have a Proj */
		flags = pred && is_ia32_irn(pred) ? get_ia32_out_flags(pred, get_Proj_proj(irn)) : arch_irn_flags_none;

		irn = skip_Proj(irn);
		if (is_ia32_irn(irn))
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

static entity *ia32_get_frame_entity(const void *self, const ir_node *irn) {
	return is_ia32_irn(irn) ? get_ia32_frame_ent(irn) : NULL;
}

static void ia32_set_frame_entity(const void *self, ir_node *irn, entity *ent) {
	set_ia32_frame_ent(irn, ent);
}

static void ia32_set_frame_offset(const void *self, ir_node *irn, int bias) {
	char buf[64];
	const ia32_irn_ops_t *ops = self;

	if (get_ia32_frame_ent(irn)) {
		ia32_am_flavour_t am_flav = get_ia32_am_flavour(irn);

		if(is_ia32_Pop(irn)) {
			int omit_fp = be_abi_omit_fp(ops->cg->birg->abi);
			if (omit_fp) {
				/* Pop nodes modify the stack pointer before calculating the destination
				 * address, so fix this here
				 */
				bias -= 4;
			}
		}

		DBG((ops->cg->mod, LEVEL_1, "stack biased %+F with %d\n", irn, bias));

		snprintf(buf, sizeof(buf), "%d", bias);

		if (get_ia32_op_type(irn) == ia32_Normal) {
			set_ia32_cnst(irn, buf);
		} else {
			add_ia32_am_offs(irn, buf);
			am_flav |= ia32_O;
			set_ia32_am_flavour(irn, am_flav);
		}
	}
}

static int ia32_get_sp_bias(const void *self, const ir_node *irn) {
	if(is_Proj(irn)) {
		long proj = get_Proj_proj(irn);
		ir_node *pred = get_Proj_pred(irn);

		if (proj == pn_ia32_Push_stack && is_ia32_Push(pred))
			return 4;
		if (proj == pn_ia32_Pop_stack && is_ia32_Pop(pred))
			return -4;
	}

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

	if (! env->flags.try_omit_fp) {
		ir_node *bl      = get_irg_start_block(env->irg);
		ir_node *curr_sp = be_abi_reg_map_get(reg_map, env->isa->sp);
		ir_node *curr_bp = be_abi_reg_map_get(reg_map, env->isa->bp);
		ir_node *noreg = be_abi_reg_map_get(reg_map, &ia32_gp_regs[REG_GP_NOREG]);
		ir_node *push;

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
	}
	else {
		const ia32_isa_t *isa     = (ia32_isa_t *)env->isa;
		ir_mode          *mode_bp = env->isa->bp->reg_class->mode;

		/* gcc always emits a leave at the end of a routine */
		if (1 || ARCH_AMD(isa->opt_arch)) {
			ir_node *leave;

			/* leave */
			leave   = new_rd_ia32_Leave(NULL, env->irg, bl, curr_sp, curr_bp);
			set_ia32_flags(leave, arch_irn_flags_ignore);
			curr_bp = new_r_Proj(current_ir_graph, bl, leave, mode_bp, pn_ia32_Leave_frame);
			curr_sp = new_r_Proj(current_ir_graph, bl, leave, get_irn_mode(curr_sp), pn_ia32_Leave_stack);
			*mem    = new_r_Proj(current_ir_graph, bl, leave, mode_M, pn_ia32_Leave_M);
		}
		else {
			ir_node *noreg = be_abi_reg_map_get(reg_map, &ia32_gp_regs[REG_GP_NOREG]);
			ir_node *pop;

			/* copy ebp to esp */
			curr_sp = be_new_SetSP(env->isa->sp, env->irg, bl, curr_sp, curr_bp, *mem);

			/* pop ebp */
			pop     = new_rd_ia32_Pop(NULL, env->irg, bl, noreg, noreg, curr_sp, *mem);
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

	if ( !between_type) {
		entity *old_bp_ent;
		entity *ret_addr_ent;
		entity *omit_fp_ret_addr_ent;

		ir_type *old_bp_type   = new_type_primitive(IDENT("bp"), mode_P);
		ir_type *ret_addr_type = new_type_primitive(IDENT("return_addr"), mode_P);

		between_type           = new_type_struct(IDENT("ia32_between_type"));
		old_bp_ent             = new_entity(between_type, IDENT("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, IDENT("ret_addr"), ret_addr_type);

		set_entity_offset_bytes(old_bp_ent, 0);
		set_entity_offset_bytes(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
		set_type_state(between_type, layout_fixed);

		omit_fp_between_type = new_type_struct(IDENT("ia32_between_type_omit_fp"));
		omit_fp_ret_addr_ent = new_entity(omit_fp_between_type, IDENT("ret_addr"), ret_addr_type);

		set_entity_offset_bytes(omit_fp_ret_addr_ent, 0);
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

	assert(is_ia32_irn(irn));

	cost  = get_ia32_latency(irn);
	op_tp = get_ia32_op_type(irn);

	if (is_ia32_CopyB(irn)) {
		cost = 250;
		if (ARCH_INTEL(ops->cg->arch))
			cost += 150;
	}
	else if (is_ia32_CopyB_i(irn)) {
		int size = get_tarval_long(get_ia32_Immop_tarval(irn));
		cost     = 20 + (int)ceil((4/3) * size);
		if (ARCH_INTEL(ops->cg->arch))
			cost += 150;
	}
	/* in case of address mode operations add additional cycles */
	else if (op_tp == ia32_AddrModeD || op_tp == ia32_AddrModeS) {
		/*
			In case of stack access add 5 cycles (we assume stack is in cache),
			other memory operations cost 20 cycles.
		*/
		cost += is_ia32_use_frame(irn) ? 5 : 20;
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
	ir_node  *block, *noreg, *nomem;
	int      pnc;

	/* we cannot invert non-ia32 irns */
	if (! is_ia32_irn(irn))
		return NULL;

	/* operand must always be a real operand (not base, index or mem) */
	if (i != 2 && i != 3)
		return NULL;

	/* we don't invert address mode operations */
	if (get_ia32_op_type(irn) != ia32_Normal)
		return NULL;

	irg   = get_irn_irg(irn);
	block = get_nodes_block(irn);
	mode  = get_ia32_res_mode(irn);
	noreg = get_irn_n(irn, 0);
	nomem = new_r_NoMem(irg);

	/* initialize structure */
	inverse->nodes = obstack_alloc(obst, 2 * sizeof(inverse->nodes[0]));
	inverse->costs = 0;
	inverse->n     = 2;

	switch (get_ia32_irn_opcode(irn)) {
		case iro_ia32_Add:
			if (get_ia32_immop_type(irn) == ia32_ImmConst) {
				/* we have an add with a const here */
				/* invers == add with negated const */
				inverse->nodes[0] = new_rd_ia32_Add(NULL, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				pnc               = pn_ia32_Add_res;
				inverse->costs   += 1;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
				set_ia32_Immop_tarval(inverse->nodes[0], tarval_neg(get_ia32_Immop_tarval(irn)));
				set_ia32_commutative(inverse->nodes[0]);
			}
			else if (get_ia32_immop_type(irn) == ia32_ImmSymConst) {
				/* we have an add with a symconst here */
				/* invers == sub with const */
				inverse->nodes[0] = new_rd_ia32_Sub(NULL, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				pnc               = pn_ia32_Sub_res;
				inverse->costs   += 2;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
			}
			else {
				/* normal add: inverse == sub */
				ir_node *proj = ia32_get_res_proj(irn);
				assert(proj);

				inverse->nodes[0] = new_rd_ia32_Sub(NULL, irg, block, noreg, noreg, proj, get_irn_n(irn, i ^ 1), nomem);
				pnc               = pn_ia32_Sub_res;
				inverse->costs   += 2;
			}
			break;
		case iro_ia32_Sub:
			if (get_ia32_immop_type(irn) != ia32_ImmNone) {
				/* we have a sub with a const/symconst here */
				/* invers == add with this const */
				inverse->nodes[0] = new_rd_ia32_Add(NULL, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				pnc               = pn_ia32_Add_res;
				inverse->costs   += (get_ia32_immop_type(irn) == ia32_ImmSymConst) ? 5 : 1;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
			}
			else {
				/* normal sub */
				ir_node *proj = ia32_get_res_proj(irn);
				assert(proj);

				if (i == 2) {
					inverse->nodes[0] = new_rd_ia32_Add(NULL, irg, block, noreg, noreg, proj, get_irn_n(irn, 3), nomem);
				}
				else {
					inverse->nodes[0] = new_rd_ia32_Sub(NULL, irg, block, noreg, noreg, get_irn_n(irn, 2), proj, nomem);
				}
				pnc             = pn_ia32_Sub_res;
				inverse->costs += 1;
			}
			break;
		case iro_ia32_Eor:
			if (get_ia32_immop_type(irn) != ia32_ImmNone) {
				/* xor with const: inverse = xor */
				inverse->nodes[0] = new_rd_ia32_Eor(NULL, irg, block, noreg, noreg, get_irn_n(irn, i), noreg, nomem);
				pnc               = pn_ia32_Eor_res;
				inverse->costs   += (get_ia32_immop_type(irn) == ia32_ImmSymConst) ? 5 : 1;
				copy_ia32_Immop_attr(inverse->nodes[0], (ir_node *)irn);
			}
			else {
				/* normal xor */
				inverse->nodes[0] = new_rd_ia32_Eor(NULL, irg, block, noreg, noreg, (ir_node *)irn, get_irn_n(irn, i), nomem);
				pnc               = pn_ia32_Eor_res;
				inverse->costs   += 1;
			}
			break;
		case iro_ia32_Not: {
			ir_node *proj = ia32_get_res_proj(irn);
			assert(proj);

			inverse->nodes[0] = new_rd_ia32_Not(NULL, irg, block, noreg, noreg, proj, nomem);
			pnc = pn_ia32_Not_res;
			inverse->costs   += 1;
			break;
		}
		case iro_ia32_Minus: {
			ir_node *proj = ia32_get_res_proj(irn);
			assert(proj);

			inverse->nodes[0] = new_rd_ia32_Minus(NULL, irg, block, noreg, noreg, proj, nomem);
			pnc               = pn_ia32_Minus_res;
			inverse->costs   += 1;
			break;
		}
		default:
			/* inverse operation not supported */
			return NULL;
	}

	set_ia32_res_mode(inverse->nodes[0], mode);
	inverse->nodes[1] = new_r_Proj(irg, block, inverse->nodes[0], mode, pnc);

	return inverse;
}

/**
 * Check if irn can load it's operand at position i from memory (source addressmode).
 * @param self   Pointer to irn ops itself
 * @param irn    The irn to be checked
 * @param i      The operands position
 * @return Non-Zero if operand can be loaded
 */
static int ia32_possible_memory_operand(const void *self, const ir_node *irn, unsigned int i) {
	if (! is_ia32_irn(irn)                            ||  /* must be an ia32 irn */
		get_irn_arity(irn) != 5                       ||  /* must be a binary operation */
		get_ia32_op_type(irn) != ia32_Normal          ||  /* must not already be a addressmode irn */
		! (get_ia32_am_support(irn) & ia32_am_Source) ||  /* must be capable of source addressmode */
		(i != 2 && i != 3)                            ||  /* a "real" operand position must be requested */
		(i == 2 && ! is_ia32_commutative(irn))        ||  /* if first operand requested irn must be commutative */
		is_ia32_use_frame(irn))                           /* must not already use frame */
		return 0;

	return 1;
}

static void ia32_perform_memory_operand(const void *self, ir_node *irn, ir_node *spill, unsigned int i) {
	const ia32_irn_ops_t *ops = self;
	ia32_code_gen_t      *cg  = ops->cg;

	assert(ia32_possible_memory_operand(self, irn, i) && "Cannot perform memory operand change");

	if (i == 2) {
		ir_node *tmp = get_irn_n(irn, 3);
		set_irn_n(irn, 3, get_irn_n(irn, 2));
		set_irn_n(irn, 2, tmp);
	}

	set_ia32_am_support(irn, ia32_am_Source);
	set_ia32_op_type(irn, ia32_AddrModeS);
	set_ia32_am_flavour(irn, ia32_B);
	set_ia32_ls_mode(irn, get_irn_mode(get_irn_n(irn, i)));
	set_ia32_use_frame(irn);
	set_ia32_got_reload(irn);

	set_irn_n(irn, 0, get_irg_frame(get_irn_irg(irn)));
	set_irn_n(irn, 4, spill);

	/*
		Input at position one is index register, which is NoReg.
		We would need cg object to get a real noreg, but we cannot
		access it from here.
	 */
	set_irn_n(irn, 3, ia32_get_admissible_noreg(cg, irn, 3));

	//FIXME DBG_OPT_AM_S(reload, irn);
}

static const be_abi_callbacks_t ia32_abi_callbacks = {
	ia32_abi_init,
	ia32_abi_done,
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

static void ia32_kill_convs(ia32_code_gen_t *cg) {
	ir_node *irn;

	/* BEWARE: the Projs are inserted in the set */
	foreach_nodeset(cg->kill_conv, irn) {
		ir_node *in = get_irn_n(get_Proj_pred(irn), 2);
		edges_reroute(irn, in, cg->birg->irg);
	}
}

/**
 * Transform the Thread Local Store base.
 */
static void transform_tls(ir_graph *irg) {
	ir_node *irn = get_irg_tls(irg);

	if (irn) {
		dbg_info *dbg = get_irn_dbg_info(irn);
		ir_node  *blk = get_nodes_block(irn);
		ir_node  *newn;
		newn = new_rd_ia32_LdTls(dbg, irg, blk, get_irn_mode(irn));

		exchange(irn, newn);
	}
}

/**
 * Transforms the standard firm graph into
 * an ia32 firm graph
 */
static void ia32_prepare_graph(void *self) {
	ia32_code_gen_t *cg = self;
	dom_front_info_t *dom;
	DEBUG_ONLY(firm_dbg_module_t *old_mod = cg->mod;)

	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.transform");

	/* 1st: transform constants and psi condition trees */
	ia32_pre_transform_phase(cg);

	/* 2nd: transform all remaining nodes */
	ia32_register_transformers();
	dom = be_compute_dominance_frontiers(cg->irg);

	cg->kill_conv = new_nodeset(5);
	transform_tls(cg->irg);
	irg_walk_blkwise_graph(cg->irg, NULL, ia32_transform_node, cg);
	ia32_kill_convs(cg);
	del_nodeset(cg->kill_conv);

	be_free_dominance_frontiers(dom);

	if (cg->dump)
		be_dump(cg->irg, "-transformed", dump_ir_block_graph_sched);

	/* 3rd: optimize address mode */
	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.am");
	ia32_optimize_addressmode(cg);

	if (cg->dump)
		be_dump(cg->irg, "-am", dump_ir_block_graph_sched);

	DEBUG_ONLY(cg->mod = old_mod;)
}

/**
 * Dummy functions for hooks we don't need but which must be filled.
 */
static void ia32_before_sched(void *self) {
}

static void remove_unused_nodes(ir_node *irn, bitset_t *already_visited) {
	int i, arity;
	ir_mode *mode;
	ir_node *mem_proj = NULL;

	if (is_Block(irn))
		return;

	mode = get_irn_mode(irn);

	/* check if we already saw this node or the node has more than one user */
	if (bitset_contains_irn(already_visited, irn) || get_irn_n_edges(irn) > 1) {
		return;
	};

	/* mark irn visited */
	bitset_add_irn(already_visited, irn);

	/* non-Tuple nodes with one user: ok, return */
	if (get_irn_n_edges(irn) >= 1 && mode != mode_T) {
		return;
	}

	/* tuple node has one user which is not the mem proj-> ok */
	if (mode == mode_T && get_irn_n_edges(irn) == 1) {
		mem_proj = ia32_get_proj_for_mode(irn, mode_M);
		if (mem_proj == NULL) {
			return;
		}
	}

	arity = get_irn_arity(irn);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(irn, i);

		/* do not follow memory edges or we will accidentally remove stores */
		if (get_irn_mode(pred) == mode_M) {
			if(mem_proj != NULL) {
				edges_reroute(mem_proj, pred, get_irn_irg(mem_proj));
				mem_proj = NULL;
			}
			continue;
		}

		set_irn_n(irn, i, new_Bad());

		/*
			The current node is about to be removed: if the predecessor
			has only this node as user, it need to be removed as well.
		*/
		if (get_irn_n_edges(pred) <= 1)
			remove_unused_nodes(pred, already_visited);
	}

	// we need to set the presd to Bad again to also get the memory edges
	arity = get_irn_arity(irn);
	for (i = 0; i < arity; ++i) {
		set_irn_n(irn, i, new_Bad());
	}

	if (sched_is_scheduled(irn)) {
		sched_remove(irn);
	}
}

static void remove_unused_loads_walker(ir_node *irn, void *env) {
	bitset_t *already_visited = env;
	if (is_ia32_Ld(irn) && ! bitset_contains_irn(already_visited, irn))
		remove_unused_nodes(irn, env);
}

/**
 * Called before the register allocator.
 * Calculate a block schedule here. We need it for the x87
 * simulator and the emitter.
 */
static void ia32_before_ra(void *self) {
	ia32_code_gen_t *cg              = self;
	bitset_t        *already_visited = bitset_irg_alloca(cg->irg);

	/*
		Handle special case:
		There are sometimes unused loads, only pinned by memory.
		We need to remove those Loads and all other nodes which won't be used
		after removing the Load from schedule.
	*/
	irg_walk_graph(cg->irg, NULL, remove_unused_loads_walker, already_visited);
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
	else
		new_op = new_rd_ia32_Load(env->dbg, env->irg, env->block, ptr, noreg, mem);

	set_ia32_am_support(new_op, ia32_am_Source);
	set_ia32_op_type(new_op, ia32_AddrModeS);
	set_ia32_am_flavour(new_op, ia32_B);
	set_ia32_ls_mode(new_op, mode);
	set_ia32_frame_ent(new_op, ent);
	set_ia32_use_frame(new_op);

	DBG_OPT_RELOAD2LD(irn, new_op);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, new_op, mode, pn_ia32_Load_res);

	if (sched_point) {
		sched_add_after(sched_point, new_op);
		sched_add_after(new_op, proj);

		sched_remove(irn);
	}

	/* copy the register from the old node to the new Load */
	reg = arch_get_irn_register(env->cg->arch_env, irn);
	arch_set_irn_register(env->cg->arch_env, new_op, reg);

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, irn));

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
		sched_remove(irn);
	}

	SET_IA32_ORIG_NODE(new_op, ia32_get_old_node_name(env->cg, irn));

	exchange(irn, proj);
}

static ir_node *create_push(ia32_transform_env_t *env, ir_node *schedpoint, ir_node *sp, ir_node *mem, entity *ent) {
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *frame = get_irg_frame(env->irg);

	ir_node *push = new_rd_ia32_Push(env->dbg, env->irg, env->block, frame, noreg, noreg, sp, mem);

	set_ia32_frame_ent(push, ent);
	set_ia32_use_frame(push);
	set_ia32_op_type(push, ia32_AddrModeS);
	set_ia32_am_flavour(push, ia32_B);
	set_ia32_ls_mode(push, mode_Is);

	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ia32_transform_env_t *env, ir_node *schedpoint, ir_node *sp, entity *ent) {
	ir_node *noreg = ia32_new_NoReg_gp(env->cg);
	ir_node *frame = get_irg_frame(env->irg);

	ir_node *pop = new_rd_ia32_Pop(env->dbg, env->irg, env->block, frame, noreg, sp, new_NoMem());

	set_ia32_frame_ent(pop, ent);
	set_ia32_use_frame(pop);
	set_ia32_op_type(pop, ia32_AddrModeD);
	set_ia32_am_flavour(pop, ia32_B);
	set_ia32_ls_mode(pop, mode_Is);

	sched_add_before(schedpoint, pop);

	return pop;
}

static ir_node* create_spproj(ia32_transform_env_t *env, ir_node *pred, int pos, ir_node *schedpoint) {
	ir_mode *spmode = mode_Iu;
	const arch_register_t *spreg = &ia32_gp_regs[REG_ESP];
	ir_node *sp;

	sp = new_rd_Proj(env->dbg, env->irg, env->block, pred, spmode, pos);
	arch_set_irn_register(env->cg->arch_env, sp, spreg);
	sched_add_before(schedpoint, sp);

	return sp;
}

/**
 * Transform memperm, currently we do this the ugly way and produce
 * push/pop into/from memory cascades. This is possible without using
 * any registers.
 */
static void transform_MemPerm(ia32_transform_env_t *env) {
	ir_node *node = env->irn;
	int i, arity;
	ir_node *sp = be_abi_get_ignore_irn(env->cg->birg->abi, &ia32_gp_regs[REG_ESP]);
	const ir_edge_t *edge;
	const ir_edge_t *next;
	ir_node **pops;

	arity = be_get_MemPerm_entity_arity(node);
	pops = alloca(arity * sizeof(pops[0]));

	// create pushs
	for(i = 0; i < arity; ++i) {
		entity *ent = be_get_MemPerm_in_entity(node, i);
		ir_type *enttype = get_entity_type(ent);
		int entbits = get_type_size_bits(enttype);
		ir_node *mem = get_irn_n(node, i + 1);
		ir_node *push;

		assert( (entbits == 32 || entbits == 64) && "spillslot on x86 should be 32 or 64 bit");

		push = create_push(env, node, sp, mem, ent);
		sp = create_spproj(env, push, 0, node);
		if(entbits == 64) {
			// add another push after the first one
			push = create_push(env, node, sp, mem, ent);
			add_ia32_am_offs_int(push, 4);
			sp = create_spproj(env, push, 0, node);
		}

		set_irn_n(node, i, new_Bad());
	}

	// create pops
	for(i = arity - 1; i >= 0; --i) {
		entity *ent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(ent);
		int entbits = get_type_size_bits(enttype);

		ir_node *pop;

		assert( (entbits == 32 || entbits == 64) && "spillslot on x86 should be 32 or 64 bit");

		pop = create_pop(env, node, sp, ent);
		if(entbits == 64) {
			// add another pop after the first one
			sp = create_spproj(env, pop, 1, node);
			pop = create_pop(env, node, sp, ent);
			add_ia32_am_offs_int(pop, 4);
		}
		sp = create_spproj(env, pop, 1, node);

		pops[i] = pop;
	}

	// exchange memprojs
	foreach_out_edge_safe(node, edge, next) {
		ir_node *proj = get_edge_src_irn(edge);
		int p = get_Proj_proj(proj);

		assert(p < arity);

		set_Proj_pred(proj, pops[p]);
		set_Proj_proj(proj, 3);
	}

	// remove memperm
	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		set_irn_n(node, i, new_Bad());
	}
	sched_remove(node);
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
			ir_node *spillval = get_irn_n(node, be_pos_Spill_val);
			/* we always spill the whole register  */
			tenv.dbg  = get_irn_dbg_info(node);
			tenv.irn  = node;
			tenv.mode = fix_spill_mode(cg, get_irn_mode(spillval));
			transform_to_Store(&tenv);
		}
		else if(be_is_MemPerm(node)) {
			tenv.dbg = get_irn_dbg_info(node);
			tenv.irn = node;
			transform_MemPerm(&tenv);
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
	ir_graph *irg = cg->irg;

	irg_block_walk_graph(irg, NULL, ia32_after_ra_walker, cg);

	ia32_finish_irg(irg, cg);
}

/**
 * Last touchups for the graph before emit
 */
static void ia32_finish(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;

	//be_remove_empty_blocks(irg);
	cg->blk_sched = be_create_block_schedule(irg, cg->birg->execfreqs);

	//cg->blk_sched = sched_create_block_schedule(cg->irg, cg->birg->execfreqs);

	/* if we do x87 code generation, rewrite all the virtual instructions and registers */
	if (cg->used_fp == fp_x87 || cg->force_sim) {
		x87_simulate_graph(cg->arch_env, irg, cg->blk_sched);
	}

	ia32_peephole_optimization(irg, cg);
}

/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ia32_codegen(void *self) {
	ia32_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;

	ia32_gen_routine(cg->isa->out, irg, cg);

	cur_reg_set = NULL;

	/* remove it from the isa */
	cg->isa->cg = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(cg);
}

static void *ia32_cg_init(const be_irg_t *birg);

static const arch_code_generator_if_t ia32_code_gen_if = {
	ia32_cg_init,
	NULL,                /* before abi introduce hook */
	ia32_prepare_graph,
	ia32_before_sched,   /* before scheduling hook */
	ia32_before_ra,      /* before register allocation hook */
	ia32_after_ra,       /* after register allocation hook */
	ia32_finish,         /* called before codegen */
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
	cg->dump      = (birg->main_env->options->dump_flags & DUMP_BE) ? 1 : 0;

	FIRM_DBG_REGISTER(cg->mod, "firm.be.ia32.cg");

	/* copy optimizations from isa for easier access */
	cg->opt      = isa->opt;
	cg->arch     = isa->arch;
	cg->opt_arch = isa->opt_arch;

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
 * Set output modes for GCC
 */
static const tarval_mode_info mo_integer = {
	TVO_DECIMAL,
	NULL,
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
	},
	NULL,                    /* 16bit register names */
	NULL,                    /* 8bit register names */
	NULL,                    /* types */
	NULL,                    /* tv_ents */
	(0                 |
	IA32_OPT_INCDEC    |     /* optimize add 1, sub 1 into inc/dec               default: on */
	IA32_OPT_DOAM      |     /* optimize address mode                            default: on */
	IA32_OPT_LEA       |     /* optimize for LEAs                                default: on */
	IA32_OPT_PLACECNST |     /* place constants immediately before instructions, default: on */
	IA32_OPT_IMMOPS    |     /* operations can use immediates,                   default: on */
	IA32_OPT_EXTBB     |     /* use extended basic block scheduling,             default: on */
	IA32_OPT_PUSHARGS),      /* create pushs for function argument passing,      default: on */
	arch_pentium_4,          /* instruction architecture */
	arch_pentium_4,          /* optimize for architecture */
	fp_sse2,                 /* use sse2 unit */
	NULL,                    /* current code generator */
	NULL,                    /* output file */
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

	set_tarval_output_modes();

	isa = xmalloc(sizeof(*isa));
	memcpy(isa, &ia32_isa_template, sizeof(*isa));

	ia32_register_init(isa);
	ia32_create_opcodes();

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

	ia32_handle_intrinsics();
	ia32_switch_section(isa->out, NO_SECTION);
	fprintf(isa->out, "\t.intel_syntax\n");

	/* needed for the debug support */
	ia32_switch_section(isa->out, SECTION_TEXT);
	fprintf(isa->out, ".Ltext0:\n");

	inited = 1;

	return isa;
}



/**
 * Closes the output file and frees the ISA structure.
 */
static void ia32_done(void *self) {
	ia32_isa_t *isa = self;

	/* emit now all global declarations */
	ia32_gen_decls(isa->out, isa->arch_isa.main_env);

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
 *  - the SSE floating point register set
 *  - the virtual floating point registers
 */
static int ia32_get_n_reg_class(const void *self) {
	return 3;
}

/**
 * Return the register class for index i.
 */
static const arch_register_class_t *ia32_get_reg_class(const void *self, int i) {
	assert(i >= 0 && i < 3 && "Invalid ia32 register class requested.");
	if (i == 0)
		return &ia32_reg_classes[CLASS_ia32_gp];
	else if (i == 1)
		return &ia32_reg_classes[CLASS_ia32_xmm];
	else
		return &ia32_reg_classes[CLASS_ia32_vfp];
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

	unsigned use_push = !IS_P6_ARCH(isa->opt_arch);

	/* set abi flags for calls */
	call_flags.bits.left_to_right         = 0;  /* always last arg first on stack */
	call_flags.bits.store_args_sequential = use_push;
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
		/* parameters on the stack are 32 bit aligned */
		be_abi_call_param_stack(abi, i, 4, 0, 0);
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
	return is_ia32_irn(irn) ? 1 : -1;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *ia32_get_code_generator_if(void *self) {
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
static const list_sched_selector_t *ia32_get_list_sched_selector(const void *self, list_sched_selector_t *selector) {
	memcpy(&ia32_sched_selector, selector, sizeof(ia32_sched_selector));
	ia32_sched_selector.exectime              = ia32_sched_exectime;
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

/**
 * Allows or disallows the creation of a Psi for the given Cond selector.
 * @return 1 if allowed, 0 otherwise
 */
static int ia32_is_psi_allowed(ir_node *sel, ir_node *phi_list, int i, int j)
{
	ir_node *cmp, *cmp_a;
	ir_mode *mode;

	if (get_irn_mode(sel) != mode_b)
		return 0;

	cmp   = get_Proj_pred(sel);
	cmp_a = get_Cmp_left(cmp);
	mode  = get_irn_mode(cmp_a);

	/* we don't want long long an floating point Psi */
	return ! mode_is_float(mode) && get_mode_size_bits(mode) <= 32;
}

static ia32_intrinsic_env_t intrinsic_env = {
	NULL,    /**< the irg, these entities belong to */
	NULL,    /**< entity for first div operand (move into FPU) */
	NULL,    /**< entity for second div operand (move into FPU) */
	NULL,    /**< entity for converts ll -> d */
	NULL,    /**< entity for converts d -> ll */
};

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *ia32_get_libfirm_params(void) {
	static const opt_if_conv_info_t ifconv = {
		4,                    /* maxdepth, doesn't matter for Psi-conversion */
		ia32_is_psi_allowed   /* allows or disallows Psi creation for given selector */
	};
	static const arch_dep_params_t ad = {
		1,  /* also use subs */
		4,  /* maximum shifts */
		31, /* maximum shift amount */

		1,  /* allow Mulhs */
		1,  /* allow Mulus */
		32  /* Mulh allowed up to 32 bit */
	};
	static backend_params p = {
		NULL,  /* no additional opcodes */
		NULL,  /* will be set later */
		1,     /* need dword lowering */
		ia32_create_intrinsic_fkt,
		&intrinsic_env,  /* context for ia32_create_intrinsic_fkt */
		NULL,  /* will be set later */
	};

	p.dep_param    = &ad;
	p.if_conv_info = &ifconv;
	return &p;
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

static const lc_opt_enum_int_items_t gas_items[] = {
	{ "linux",   ASM_LINUX_GAS },
	{ "mingw",   ASM_MINGW_GAS },
	{ NULL,      0 }
};

static lc_opt_enum_int_var_t gas_var = {
	(int *)&asm_flavour, gas_items
};

static const lc_opt_table_entry_t ia32_options[] = {
	LC_OPT_ENT_ENUM_INT("arch",      "select the instruction architecture", &arch_var),
	LC_OPT_ENT_ENUM_INT("opt",       "optimize for instruction architecture", &opt_arch_var),
	LC_OPT_ENT_ENUM_INT("fpunit",    "select the floating point unit", &fp_unit_var),
	LC_OPT_ENT_NEGBIT("noaddrmode",  "do not use address mode", &ia32_isa_template.opt, IA32_OPT_DOAM),
	LC_OPT_ENT_NEGBIT("nolea",       "do not optimize for LEAs", &ia32_isa_template.opt, IA32_OPT_LEA),
	LC_OPT_ENT_NEGBIT("noplacecnst", "do not place constants", &ia32_isa_template.opt, IA32_OPT_PLACECNST),
	LC_OPT_ENT_NEGBIT("noimmop",     "no operations with immediates", &ia32_isa_template.opt, IA32_OPT_IMMOPS),
	LC_OPT_ENT_NEGBIT("noextbb",     "do not use extended basic block scheduling", &ia32_isa_template.opt, IA32_OPT_EXTBB),
	LC_OPT_ENT_NEGBIT("nopushargs",  "do not create pushs for function arguments", &ia32_isa_template.opt, IA32_OPT_PUSHARGS),
	LC_OPT_ENT_ENUM_INT("gasmode",   "set the GAS compatibility mode", &gas_var),
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
 * ia32-nolea        do not optimize for LEAs
 * ia32-noplacecnst  do not place constants,
 * ia32-noimmop      no operations with immediates
 * ia32-noextbb      do not use extended basic block scheduling
 * ia32-nopushargs   do not create pushs for function argument passing
 * ia32-gasmode      set the GAS compatibility mode
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
	ia32_get_libfirm_params,
#ifdef WITH_LIBCORE
	ia32_register_options
#endif
};
