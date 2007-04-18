/* The codegenrator (transform FIRM into mips FIRM */
/* $Id$ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"

#include "../benode_t.h"
#include "../beabi.h"
#include "../besched.h"
#include "../besched_t.h"
#include "../beirg_t.h"
#include "bearch_mips_t.h"

#include "mips_nodes_attr.h"
#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "mips_transform.h"
#include "mips_new_nodes.h"
#include "mips_map_regs.h"
#include "mips_util.h"
#include "mips_emitter.h"

#include "gen_mips_regalloc_if.h"

/****************************************************************************************************
 *                  _        _                        __                           _   _
 *                 | |      | |                      / _|                         | | (_)
 *  _ __   ___   __| | ___  | |_ _ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___   __ _| |_ _  ___  _ __
 * | '_ \ / _ \ / _` |/ _ \ | __| '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \ / _` | __| |/ _ \| '_ \
 * | | | | (_) | (_| |  __/ | |_| | | (_| | | | \__ \ || (_) | |  | | | | | | (_| | |_| | (_) | | | |
 * |_| |_|\___/ \__,_|\___|  \__|_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|\__,_|\__|_|\___/|_| |_|
 *
 ****************************************************************************************************/

#define MIPS_GENBINFUNC(mips_nodetype)															\
	static ir_node* mips_gen_##mips_nodetype(mips_transform_env_t *env, ir_node *op1, ir_node *op2) {\
		ASSERT_NO_FLOAT(env->mode);																\
		/*assert(get_irn_mode(op1) == get_irn_mode(op2));*/										\
		/*assert(get_irn_mode(op1) == env->mode);*/												\
		assert(get_mode_size_bits(env->mode) == 32);											\
		return new_rd_mips_##mips_nodetype(env->dbg, env->irg, env->block, op1, op2);           \
	}

MIPS_GENBINFUNC(addu)
MIPS_GENBINFUNC(sub)
MIPS_GENBINFUNC(and)
MIPS_GENBINFUNC(or)
MIPS_GENBINFUNC(xor)
MIPS_GENBINFUNC(sl)
MIPS_GENBINFUNC(sr)
MIPS_GENBINFUNC(sra)

#define MIPS_GENUNFUNC(mips_nodetype)															\
	static ir_node *mips_gen_##mips_nodetype(mips_transform_env_t *env, ir_node *op) {			\
		ASSERT_NO_FLOAT(env->mode);																\
		assert(get_irn_mode(op) == env->mode);													\
		assert(get_mode_size_bits(env->mode) == 32);											\
		return new_rd_mips_##mips_nodetype(env->dbg, env->irg, env->block, op);		            \
	}

MIPS_GENUNFUNC(not)

static ir_node* mips_get_reg_node(mips_transform_env_t *env, const arch_register_t *reg) {
	return be_abi_get_callee_save_irn(env->cg->birg->abi, reg);
}

static ir_node* gen_zero_node(mips_transform_env_t *env, dbg_info *ebg, ir_graph *irg, ir_node *block)
{
	ir_node *zero = be_abi_get_callee_save_irn(env->cg->birg->abi, &mips_gp_regs[REG_ZERO]);
	// TODO make zero nodes work
	//ir_node *unknown = new_rd_mips_zero(dbg, irg, block, mode);

	return zero;
}

static ir_node* gen_node_for_Const(mips_transform_env_t *env, dbg_info *dbg, ir_graph *irg, ir_node *block, ir_node *constant)
{
	tarval* tv = get_Const_tarval(constant);
	ir_node *lui;
	ir_node *lli;
	mips_attr_t *attr;
	ir_mode* mode = get_irn_mode(constant);
	unsigned long val, lower, upper;

	val = get_tarval_long(tv);
	if(val == 0)
		return gen_zero_node(env, dbg, irg, block);

	lower = val & 0xffff;
	upper = (val >> 16) & 0xffff;
	if(upper == 0) {
		ir_node *zero = gen_zero_node(env, dbg, irg, block);
		ir_node *lli = new_rd_mips_lli(dbg, irg, block, zero);
		attr = get_mips_attr(lli);
		attr->tv = new_tarval_from_long(val, mode);

		return lli;
	}

	lui = new_rd_mips_lui(dbg, irg, block);
	attr = get_mips_attr(lui);
	attr->tv = new_tarval_from_long(val, mode);

	if(lower == 0)
		return lui;

	lli = new_rd_mips_lli(dbg, irg, block, lui);
	attr = get_mips_attr(lli);
	attr->tv = new_tarval_from_long(val, mode);

	return lli;
}

static ir_node* exchange_node_for_Const(mips_transform_env_t *env, ir_node* pred, int n) {
	ir_node *node = env->irn;
	dbg_info *dbg = get_irn_dbg_info(pred);
	ir_graph *irg = get_irn_irg(node);
	ir_node *block;

	if(get_irn_opcode(node) == iro_Phi) {
		ir_node *phipred = get_nodes_block(node);
		block = get_Block_cfgpred_block(phipred, n);
	} else {
		block = get_nodes_block(node);
	}

	return gen_node_for_Const(env, dbg, irg, block, pred);
}

static ir_node* gen_node_for_SymConst(mips_transform_env_t *env, ir_node* pred, int n) {
	ir_node *result;
	symconst_kind kind;
	mips_attr_t *attr;
	ir_node *node = env->irn;
	dbg_info *dbg = get_irn_dbg_info(pred);
	ir_graph *irg = get_irn_irg(node);
	ir_node *block;

	if (is_Phi(node)) {
		ir_node *phipred = get_nodes_block(node);
		block = get_Block_cfgpred_block(phipred, n);
	} else {
		block = get_nodes_block(node);
	}

	kind = get_SymConst_kind(pred);
	if(kind == symconst_addr_ent) {
		result = new_rd_mips_la(dbg, irg, block);
		attr = get_mips_attr(result);
		attr->symconst_id = get_entity_ld_ident(get_SymConst_entity(pred));
		return result;
	} else if(kind == symconst_addr_name) {
		result = new_rd_mips_la(dbg, irg, block);
		attr = get_mips_attr(result);
		attr->symconst_id = get_SymConst_name(pred);
		return result;
	}

	// TODO
	assert(0);
	return NULL;
}

/**
 * Generates a mips node for a firm Load node
 */
static ir_node *gen_node_for_Load(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *result = NULL;
	ir_mode *mode;
	ir_node *load_ptr;
	mips_attr_t *attr;

	ASSERT_NO_FLOAT(get_irn_mode(node));

	mode = get_Load_mode(node);
	assert(mode->vector_elem == 1);
	assert(mode->sort == irms_int_number || mode->sort == irms_reference);

	load_ptr = get_Load_ptr(node);
	assert(get_mode_sort(mode) == irms_reference || get_mode_sort(mode) == irms_int_number);
	result = new_rd_mips_load_r(env->dbg, env->irg, env->block,
			get_Load_mem(node), load_ptr, get_irn_mode(node));

	attr = get_mips_attr(result);
	attr->tv = new_tarval_from_long(0, mode_Iu);
	attr->modes.load_store_mode = mode;

	return result;
}

/**
 * Generates a mips node for a firm Store node
 */
static ir_node *gen_node_for_Store(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *result = NULL;
	ir_mode *mode;
	mips_attr_t *attr;
	ir_node *store_ptr;

	ASSERT_NO_FLOAT(env->mode);

	store_ptr = get_Store_ptr(node);
	mode = get_irn_mode(store_ptr);
	assert(mode->vector_elem == 1);
	assert(mode->sort == irms_int_number || mode->sort == irms_reference);

	if(get_irn_opcode(store_ptr) == iro_SymConst) {
		result = new_rd_mips_store_i(env->dbg, env->irg, env->block, get_Store_mem(node),
			get_Store_ptr(node), get_Store_value(node), env->mode);
	} else {
		result = new_rd_mips_store_r(env->dbg, env->irg, env->block, get_Store_mem(node),
			get_Store_ptr(node), get_Store_value(node), env->mode);
	}
	attr = get_mips_attr(result);
	attr->tv = new_tarval_from_long(0, mode_Iu);
	attr->modes.load_store_mode = mode;

	return result;
}

static ir_node *gen_node_for_div_Proj(mips_transform_env_t *env) {
	ir_node *proj = env->irn;
	ir_node *new_proj;
	ir_node *pred = get_irn_n(proj, 0);
	mips_attr_t *attr;
	long n;

	n = get_Proj_proj(proj);

	// set the div mode to the DivMod node
	attr = get_mips_attr(pred);
	assert(attr->modes.original_mode == NULL || attr->modes.original_mode == env->mode);
	attr->modes.original_mode = env->mode;

	// we have to construct a new proj here, to avoid circular refs that
	// happen when we reuse the old one
	new_proj = new_ir_node(env->dbg, env->irg, env->block, op_Proj, mode_ANY, 1, &pred);
	set_Proj_proj(new_proj, n);

	if(n == pn_DivMod_res_div) {
		return new_rd_mips_mflo(env->dbg, env->irg, env->block, new_proj);
	} else if(n == pn_DivMod_res_mod) {
		return new_rd_mips_mfhi(env->dbg, env->irg, env->block, new_proj);
	}

	return proj;
}

static ir_node *make_jmp_or_fallthrough(mips_transform_env_t *env)
{
	const ir_edge_t *edge;
	ir_node *node = env->irn;
	ir_node *next_block;
	int our_block_sched_nr = mips_get_block_sched_nr(get_nodes_block(node));

	edge = get_irn_out_edge_first(node);
	next_block = get_edge_src_irn(edge);

	if(mips_get_sched_block(env->cg, our_block_sched_nr + 1) == next_block) {
		return new_rd_mips_fallthrough(env->dbg, env->irg, env->block, mode_X);
	}

	return new_rd_mips_b(env->dbg, env->irg, env->block, mode_X);
}

static ir_node *gen_node_for_Cond_Proj(mips_transform_env_t *env, ir_node* node, int true_false)
{
	// we can't use get_Cond_selector here because the selector is already
	// replaced by a mips_ compare node
	ir_node *proj = get_Cond_selector(node);
	ir_node *original_cmp = get_irn_n(proj, 0);
	ir_node *cmp;
	ir_node *condjmp;
	ir_node *op1, *op2;
	dbg_info *dbg = env->dbg;
	ir_graph *irg = env->irg;
	ir_node *block = env->block;
	long n;

	n = get_Proj_proj(proj);
	assert(n < 8 && "Only ordered comps supported");

	assert(get_irn_opcode(original_cmp) == iro_Cmp);
	op1 = get_Cmp_left(original_cmp);
	op2 = get_Cmp_right(original_cmp);

	switch(n) {
	case pn_Cmp_False:
		if(true_false)
			return NULL;

		return make_jmp_or_fallthrough(env);

	case pn_Cmp_Eq:
		if(!true_false)
			return make_jmp_or_fallthrough(env);

		condjmp = new_rd_mips_beq(dbg, irg, block, op1, op2, mode_T);
		return new_rd_Proj(dbg, irg, block, condjmp, mode_X, 1);

	case pn_Cmp_Lt:
		if(!true_false)
			return make_jmp_or_fallthrough(env);

		cmp = new_rd_mips_slt(dbg, irg, block, op1, op2);
		condjmp = new_rd_mips_bgtz(dbg, irg, block, cmp, mode_T);
		return new_rd_Proj(dbg, irg, block, condjmp, mode_X, 1);

	case pn_Cmp_Le:
		if(!true_false)
			return make_jmp_or_fallthrough(env);

		cmp = new_rd_mips_slt(dbg, irg, block, op2, op1);
		condjmp = new_rd_mips_blez(dbg, irg, block, cmp, mode_T);
		return new_rd_Proj(dbg, irg, block, condjmp, mode_X, 1);

	case pn_Cmp_Gt:
		if(!true_false)
			return make_jmp_or_fallthrough(env);

		cmp = new_rd_mips_slt(dbg, irg, block, op2, op1);
		condjmp = new_rd_mips_bgtz(dbg, irg, block, cmp, mode_T);
		return new_rd_Proj(dbg, irg, block, condjmp, mode_X, 1);

	case pn_Cmp_Ge:
		if(!true_false)
			return make_jmp_or_fallthrough(env);

		cmp = new_rd_mips_slt(dbg, irg, block, op1, op2);
		condjmp = new_rd_mips_blez(dbg, irg, block, cmp, mode_T);
		return new_rd_Proj(dbg, irg, block, condjmp, mode_X, 1);

	case pn_Cmp_Lg:
		if(!true_false)
			return make_jmp_or_fallthrough(env);

		condjmp = new_rd_mips_bne(dbg, irg, block, op1, op2, mode_T);
		return new_rd_Proj(dbg, irg, block, condjmp, mode_X, 1);

	case pn_Cmp_Leg:
		if(!true_false)
			return NULL;

		return make_jmp_or_fallthrough(env);

	default:
		assert(0);
	}

	return NULL;
}

static ir_node *gen_node_for_Proj(mips_transform_env_t *env)
{
	ir_node *proj = env->irn;
	long n;
	ir_node *predecessor = get_Proj_pred(proj);

	// all DivMods, Div, Mod should be replaced by now
	assert(get_irn_opcode(predecessor) != iro_DivMod);
	assert(get_irn_opcode(predecessor) != iro_Div);
	assert(get_irn_opcode(predecessor) != iro_Mod);

	if(is_mips_div(predecessor))
		return gen_node_for_div_Proj(env);

	if(get_irn_opcode(predecessor) == iro_Cond) {
		ir_node *selector = get_Cond_selector(predecessor);
		ir_mode *mode = get_irn_mode(selector);
		n = get_Proj_proj(proj);

		if(get_mode_sort(mode) == irms_internal_boolean) {
			assert(n == pn_Cond_true || n == pn_Cond_false);
			return gen_node_for_Cond_Proj(env, predecessor, n == pn_Cond_true);
		}
	}

	return proj;
}

static ir_node *gen_node_for_Cond(mips_transform_env_t *env)
{
	ir_node *selector = get_Cond_selector(env->irn);
	ir_mode *selector_mode = get_irn_mode(selector);
	ir_node *node = env->irn;
	dbg_info *dbg = env->dbg;
	ir_graph *irg = env->irg;
	ir_node *block = env->block;
	ir_node *sub, *sltu, *minval_const, *max_const, *switchjmp;
	ir_node *defaultproj, *defaultproj_succ;
	ir_node *beq, *sl;
	long pn, minval, maxval, defaultprojn;
	const ir_edge_t *edge;
	ir_node *zero, *two_const, *add, *la, *load, *proj;
	ir_mode *unsigned_mode;
	mips_attr_t *attr;

	// mode_b conds are handled by gen_node_for_Proj
	if(get_mode_sort(selector_mode) != irms_int_number)
		return env->irn;

	assert(get_mode_size_bits(selector_mode) == 32);

	defaultproj = NULL;
	defaultprojn = get_Cond_defaultProj(node);

	// go over all projs to find min-&maxval of the switch
	minval = INT_MAX;
	maxval = INT_MIN;
	foreach_out_edge(node, edge) {
		ir_node* proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pn = get_Proj_proj(proj);
		if(pn == defaultprojn) {
			defaultproj = proj;
			continue;
		}

		if(pn < minval)
			minval = pn;
		if(pn > maxval)
			maxval = pn;
	}
	assert(defaultproj != NULL);

	// subtract minval from the switch value

	if(minval != 0) {
		minval_const = new_rd_Const(dbg, irg, block, selector_mode, new_tarval_from_long(minval, selector_mode));
		minval_const = gen_node_for_Const(env, dbg, irg, block, minval_const);
		sub = new_rd_mips_sub(dbg, irg, block, selector, minval_const);
	} else {
		sub = selector;
	}

	// compare if we're above maxval-minval or below zero.
	// we can do this with 1 compare because we use unsigned mode
	unsigned_mode = new_ir_mode(get_mode_name(selector_mode),
			get_mode_sort(selector_mode), get_mode_size_bits(selector_mode),
			0, get_mode_arithmetic(selector_mode), get_mode_modulo_shift(selector_mode));

	max_const = new_rd_Const(dbg, irg, block, unsigned_mode, new_tarval_from_long(maxval - minval + 1, unsigned_mode));
	max_const = gen_node_for_Const(env, dbg, irg, block, max_const);
	sltu = new_rd_mips_slt(dbg, irg, block, sub, max_const);

	zero = gen_zero_node(env, dbg, irg, block);
	beq = new_rd_mips_beq(dbg, irg, block, sltu, zero, mode_T);

	// attach defaultproj to beq now
	set_irn_n(defaultproj, 0, beq);
	set_Proj_proj(defaultproj, 1);

	two_const = new_rd_Const(dbg, irg, block, unsigned_mode, new_tarval_from_long(2, unsigned_mode));
	two_const = gen_node_for_Const(env, dbg, irg, block, two_const);
	sl = new_rd_mips_sl(dbg, irg, block, sub, two_const);

	la = new_rd_mips_la(dbg, irg, block);
	add = new_rd_mips_addu(dbg, irg, block, sl, la);
	load = new_rd_mips_load_r(dbg, irg, block, new_rd_NoMem(irg), add, mode_T);
	attr = get_mips_attr(load);
	attr->modes.load_store_mode = mode_Iu;
	attr->tv = new_tarval_from_long(0, mode_Iu);

	proj = new_rd_Proj(dbg, irg, block, load, mode_Iu, pn_Load_res);

	switchjmp = new_rd_mips_SwitchJump(dbg, irg, block, proj, mode_T);
	attr = get_mips_attr(switchjmp);
	attr->switch_default_pn = defaultprojn;

	edge = get_irn_out_edge_first(defaultproj);
	defaultproj_succ = get_edge_src_irn(edge);
	attr->symconst_id = new_id_from_str(mips_get_block_label(defaultproj_succ));

	attr = get_mips_attr(la);
	attr->symconst_id = new_id_from_str(mips_get_jumptbl_label(switchjmp));

	return switchjmp;
}

static ir_node *create_conv_store_load(mips_transform_env_t *env, ir_mode* srcmode, ir_mode* dstmode) {
	ir_node *nomem, *store, *mem_proj, *value_proj, *load;
	ir_entity *mem_entity;
	ir_node *node = env->irn;
	ir_node *pred = get_Conv_op(node);
	ir_node *sp;
	// TODO HACK make this global...
	ident* id;
	ir_type *i32type;
	ir_type *ptr_i32type;
	mips_attr_t* attr;

	id = new_id_from_str("__conv0");
	i32type = new_type_primitive(new_id_from_str("ptr32"), mode_Iu);
	ptr_i32type = new_d_type_pointer(id, i32type, mode_P, env->dbg);
	mem_entity = new_d_entity(get_irg_frame_type(env->irg), id, ptr_i32type, env->dbg);

	sp = mips_get_reg_node(env, &mips_gp_regs[REG_SP]);
	nomem = new_ir_node(env->dbg, env->irg, env->block, op_NoMem, mode_M, 0, NULL);

	store = new_rd_mips_store_r(env->dbg, env->irg, env->block, nomem, sp, pred, mode_T);
	attr = get_mips_attr(store);
	attr->tv = new_tarval_from_long(0, mode_Iu);
	attr->modes.load_store_mode = srcmode;
	attr->stack_entity = mem_entity;

	mem_proj = new_ir_node(env->dbg, env->irg, env->block, op_Proj, mode_M, 1, &store);
	set_Proj_proj(mem_proj, pn_Store_M);

	load = new_rd_mips_load_r(env->dbg, env->irg, env->block, mem_proj, sp, mode_T);
	attr = get_mips_attr(load);
	attr->tv = new_tarval_from_long(0, mode_Iu);
	attr->modes.load_store_mode = dstmode;
	attr->stack_entity = mem_entity;

	value_proj = new_ir_node(env->dbg, env->irg, env->block, op_Proj, env->mode, 1, &load);
	set_Proj_proj(value_proj, pn_Load_res);

	return value_proj;
}

static ir_node *create_conv_and(mips_transform_env_t *env, long immediate) {
	ir_node *node = env->irn;
	ir_node *pred;
	ir_node *result;
	mips_attr_t *attr;

	pred = get_Conv_op(node);
	result = new_rd_mips_andi(env->dbg, env->irg, env->block, pred);
	attr = get_mips_attr(result);
	attr->tv = new_tarval_from_long(immediate, mode_Iu);

	return result;
}

static ir_node *gen_node_for_Conv(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *pred;
	ir_mode *srcmode;
	ir_mode *destmode;
	int dst_size, src_size;

	pred = get_Conv_op(node);
	srcmode = get_irn_mode(pred);
	destmode = get_irn_mode(node);

	dst_size = get_mode_size_bits(destmode);
	src_size = get_mode_size_bits(srcmode);

	if(srcmode->size >= destmode->size) {
		assert(srcmode->size > destmode->size || srcmode->sign != destmode->sign);
		return new_rd_mips_reinterpret_conv(env->dbg, env->irg, env->block, pred);
	}
	if(srcmode->sign) {
		if(srcmode->size == 8) {
			return create_conv_store_load(env, mode_Bs, mode_Bs);
		} else if(srcmode->size == 16) {
			return create_conv_store_load(env, mode_Hs, mode_Hs);
		}
	} else {
		if(src_size == 8) {
			return create_conv_and(env, 0xff);
		} else if(src_size == 16) {
			return create_conv_and(env, 0xffff);
		}
	}

	assert(0);
	return NULL;
}

static ir_node *gen_node_mips_div(mips_transform_env_t *env, ir_node* op1, ir_node* op2, long p_div, long p_mod,
								  long p_m, long p_x)
{
	ir_node *node = env->irn;
	ir_node *div;
	const ir_edge_t *edge;
	ir_mode *mode = get_irn_mode(node);

	if(mode_is_signed(mode)) {
		div = new_rd_mips_div(env->dbg, env->irg, env->block, op1, op2);
	} else {
		div = new_rd_mips_divu(env->dbg, env->irg, env->block, op1, op2);
	}

	// Adjust div projs
	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long n = get_Proj_proj(proj);
		assert(is_Proj(proj) && "non-Proj from Mod node");
		if (n == p_div) {
			set_Proj_proj(proj, pn_DivMod_res_div);
		} else if (n == p_mod) {
			set_Proj_proj(proj, pn_DivMod_res_mod);
		} else if(n == p_m) {
			set_Proj_proj(proj, pn_DivMod_M);
		} else if(n == p_x) {
			set_Proj_proj(proj, pn_DivMod_X_except);
		} else {
			assert(!"invalid proj");
		}
	}

	return div;
}

static ir_node *gen_node_for_DivMod(mips_transform_env_t *env) {
	ir_node *node = env->irn;

	return gen_node_mips_div(env, get_DivMod_left(node), get_DivMod_right(node), pn_DivMod_res_div,
							 pn_DivMod_res_mod, pn_DivMod_M, pn_DivMod_X_except);
}

static ir_node *gen_node_for_Div(mips_transform_env_t *env) {
	ir_node *node = env->irn;

	return gen_node_mips_div(env, get_Div_left(node), get_Div_right(node), pn_Div_res, -1,
							 pn_Div_M, pn_Div_X_except);
}

static ir_node *gen_node_for_Mod(mips_transform_env_t *env) {
	ir_node *node = env->irn;

	return gen_node_mips_div(env, get_Mod_left(node), get_Mod_right(node), -1, pn_Mod_res,
							 pn_Mod_M, pn_Mod_X_except);
}

static ir_node *gen_node_for_Mul(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *mul;
	ir_node *mflo;
	ir_node *op1, *op2;
	ir_mode *mode = get_irn_mode(node);

	op1 = get_Mul_left(node);
	op2 = get_Mul_right(node);

	assert(get_mode_size_bits(env->mode) == 32);
	assert(get_mode_size_bits(get_irn_mode(op1)) == get_mode_size_bits(env->mode));
	assert(get_mode_size_bits(get_irn_mode(op2)) == get_mode_size_bits(env->mode));

	if(mode_is_signed(mode)) {
		mul = new_rd_mips_mult(env->dbg, env->irg, env->block, get_Mul_left(node), get_Mul_right(node));
	} else {
		mul = new_rd_mips_multu(env->dbg, env->irg, env->block, get_Mul_left(node), get_Mul_right(node));
	}
	mflo = new_rd_mips_mflo(env->dbg, env->irg, env->block, mul);

	return mflo;
}

static ir_node *gen_node_for_IJmp(mips_transform_env_t *env) {
	ir_node *node = env->irn;

	return new_rd_mips_j(env->dbg, env->irg, env->block, get_IJmp_target(node), node->mode);
}

static ir_node *gen_node_for_Jmp(mips_transform_env_t *env) {
	return make_jmp_or_fallthrough(env);
}

static ir_node *gen_node_for_Abs(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *sra, *add, *xor;
	mips_attr_t *attr;

	// TODO for other bit sizes...
	assert(get_mode_size_bits(env->mode) == 32);
	sra = new_rd_mips_srai(env->dbg, env->irg, env->block, get_Abs_op(node));
	attr = get_mips_attr(sra);
	attr->tv = new_tarval_from_long(31, mode_Iu);
	add = new_rd_mips_addu(env->dbg, env->irg, env->block, get_Abs_op(node), sra);
	xor = new_rd_mips_xor(env->dbg, env->irg, env->block, sra, add);

	return xor;
}

static ir_node *gen_node_for_Rot(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *subu, *srlv, *sllv, *or;

	subu = new_rd_mips_subuzero(env->dbg, env->irg, env->block, get_Rot_right(node));
	srlv = new_rd_mips_srlv(env->dbg, env->irg, env->block, get_Rot_left(node), subu);
	sllv = new_rd_mips_sllv(env->dbg, env->irg, env->block, get_Rot_left(node), get_Rot_right(node));
	or = new_rd_mips_or(env->dbg, env->irg, env->block, sllv, srlv);

	return or;
}

static ir_node *gen_node_for_Unknown(mips_transform_env_t *env)
{
	return gen_zero_node(env, env->dbg, env->irg, env->block);
}

/*
 * lower a copyB into standard Firm assembler :-)
 */
ir_node *gen_code_for_CopyB(ir_node *block, ir_node *node) {
	ir_node *cnt, *sub;
	ir_node *dst = get_CopyB_dst(node);
	ir_node *src = get_CopyB_src(node);
	ir_type *type = get_CopyB_type(node);
	ir_node *mem = get_CopyB_mem(node);
	ir_node *mm[4];
	ir_node *result = NULL;
	int size = get_type_size_bytes(type);
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_graph *irg = get_irn_irg(block);
	mips_attr_t *attr;
	int i, n;

	if (size > 16) {
		ir_node     *phi, *projT, *projF, *cmp, *proj, *cond, *jmp, *in[2];
		ir_node     *new_bl, *src_phi, *dst_phi, *mem_phi, *add;
		ir_mode     *p_mode = get_irn_mode(src);
		ir_node     *ld[4];

		/* build the control loop */
		in[0] = in[1] = new_r_Unknown(irg, mode_X);

		new_bl = new_r_Block(irg, 2, in);

		in[0] = cnt = new_Const_long(mode_Is, (size >> 4));
        in[1] = new_r_Unknown(irg, mode_Is);
		phi   = new_r_Phi(irg, new_bl, 2, in, mode_Is);

		sub = new_rd_Sub(dbg, irg, new_bl, phi, new_Const_long(mode_Is, -1), mode_Is);
		set_Phi_pred(phi, 1, sub);

		cmp = new_rd_Cmp(dbg, irg, new_bl, sub, new_Const_long(mode_Is, 0));
		proj = new_r_Proj(irg, new_bl, cmp, mode_b, pn_Cmp_Lg);
		cond = new_rd_Cond(dbg, irg, new_bl, proj);

		projT = new_r_Proj(irg, new_bl, cond, mode_X, pn_Cond_true);
		projF = new_r_Proj(irg, new_bl, cond, mode_X, pn_Cond_false);

		jmp = get_Block_cfgpred(block, 0);
		set_Block_cfgpred(block, 0, projF);

		set_Block_cfgpred(new_bl, 0, jmp);
		set_Block_cfgpred(new_bl, 1, projT);

		size &= 0xF;

		/* build the copy */
		in[0]   = src;
        in[1]   = new_r_Unknown(irg, p_mode);
		src_phi = new_r_Phi(irg, new_bl, 2, in, p_mode);

		in[0]   = dst;
		dst_phi = new_r_Phi(irg, new_bl, 2, in, p_mode);

		add = new_rd_Add(dbg, irg, new_bl, src_phi, new_Const_long(mode_Is, 16), p_mode);
		set_Phi_pred(src_phi, 1, add);
		add = new_rd_Add(dbg, irg, new_bl, dst_phi, new_Const_long(mode_Is, 16), p_mode);
		set_Phi_pred(dst_phi, 1, add);

		in[0]   = mem;
        in[1]   = new_r_Unknown(irg, mode_M);
		mem_phi = new_r_Phi(irg, new_bl, 2, in, mode_M);

		src = src_phi;
		dst = dst_phi;

		/* create 4 parallel loads */
		for (i = 0; i < 4; ++i) {
			ir_node *load;

			load = new_rd_mips_load_r(dbg, irg, new_bl, mem_phi, src, mode_T);
			attr = get_mips_attr(load);
			attr->modes.load_store_mode = mode_Iu;
			attr->tv = new_tarval_from_long(i * 4, mode_Iu);

			ld[i] = new_rd_Proj(dbg, irg, new_bl, load, mode_Iu, pn_Load_res);
		}

		/* create 4 parallel stores */
		for (i = 0; i < 4; ++i) {
			ir_node *store;

			store = new_rd_mips_store_r(dbg, irg, new_bl, mem_phi, dst, ld[i], mode_T);
			attr = get_mips_attr(store);
			attr->modes.load_store_mode = mode_Iu;
			attr->tv = new_tarval_from_long(i * 4, mode_Iu);

			mm[i] = new_rd_Proj(dbg, irg, new_bl, store, mode_M, pn_Store_M);
		}
		mem = new_r_Sync(irg, new_bl, 4, mm);
		result = mem;
		set_Phi_pred(mem_phi, 1, mem);
	}

	// output store/loads manually
	n = 0;
	for(i = size; i > 0; ) {
		ir_mode *mode;
		ir_node *load, *store, *projv;
		int offset = size - i;
		if(i >= 4) {
			mode = mode_Iu;
			i -= 4;
		} else if(i >= 2) {
			mode = mode_Hu;
			i -= 2;
		} else {
			mode = mode_Bu;
			i -= 1;
		}

		load = new_rd_mips_load_r(dbg, irg, block, mem, src, mode_T);
		attr = get_mips_attr(load);
		attr->modes.load_store_mode = mode;
		attr->tv = new_tarval_from_long(offset, mode_Iu);

		projv = new_rd_Proj(dbg, irg, block, load, mode, pn_Load_res);

		store = new_rd_mips_store_r(dbg, irg, block, mem, dst, projv, mode_T);
		attr = get_mips_attr(store);
		attr->modes.load_store_mode = mode;
		attr->tv = new_tarval_from_long(offset, mode_Iu);

		mm[n] = new_rd_Proj(dbg, irg, block, store, mode_M, pn_Store_M);
		n++;
	}

	if(n > 0) {
		result = new_r_Sync(irg, block, n, mm);
	} else if(n == 1) {
		result = mm[0];
	}

	return result;
}

static void mips_fix_CopyB_Proj(mips_transform_env_t* env) {
	ir_node *node = env->irn;
	long n = get_Proj_proj(node);

	if(n == pn_CopyB_M_except) {
		assert(0);
	} else if(n == pn_CopyB_M_regular) {
		set_Proj_proj(node, pn_Store_M);
	} else if(n == pn_CopyB_M_except) {
		set_Proj_proj(node, pn_Store_X_except);
	}
}

static void mips_transform_Spill(mips_transform_env_t* env) {
	ir_node   *node = env->irn;
	ir_node   *sched_point = NULL;
	ir_node   *store, *proj;
	ir_node   *nomem = new_rd_NoMem(env->irg);
	ir_node   *ptr   = get_irn_n(node, 0);
	ir_node   *val   = get_irn_n(node, 1);
	ir_entity *ent   = be_get_frame_entity(node);
	mips_attr_t *attr;

	if(sched_is_scheduled(node)) {
		sched_point = sched_prev(node);
	}

	store = new_rd_mips_store_r(env->dbg, env->irg, env->block, nomem, ptr, val, mode_T);
	attr = get_mips_attr(store);
	attr->stack_entity = ent;
	attr->modes.load_store_mode = get_irn_mode(val);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_M, pn_Store_M);

	if (sched_point) {
		sched_add_after(sched_point, store);
		sched_add_after(store, proj);

		sched_remove(node);
	}

	exchange(node, proj);
}

static void mips_transform_Reload(mips_transform_env_t* env) {
	ir_node   *node = env->irn;
	ir_node   *sched_point = NULL;
	ir_node   *load, *proj;
	ir_node   *ptr   = get_irn_n(node, 0);
	ir_node   *mem   = get_irn_n(node, 1);
	ir_mode   *mode  = get_irn_mode(node);
	ir_entity *ent   = be_get_frame_entity(node);
	const arch_register_t* reg;
	mips_attr_t *attr;

	if(sched_is_scheduled(node)) {
		sched_point = sched_prev(node);
	}

	load = new_rd_mips_load_r(env->dbg, env->irg, env->block, mem, ptr, mode_T);
	attr = get_mips_attr(load);
	attr->stack_entity = ent;
	attr->modes.load_store_mode = mode;

	proj = new_rd_Proj(env->dbg, env->irg, env->block, load, mode, pn_Load_res);

	if (sched_point) {
		sched_add_after(sched_point, load);
		sched_add_after(load, proj);

		sched_remove(node);
	}

	/* copy the register from the old node to the new Load */
	reg = arch_get_irn_register(env->cg->arch_env, node);
	arch_set_irn_register(env->cg->arch_env, proj, reg);

	exchange(node, proj);
}

static ir_node *gen_node_for_StackParam(mips_transform_env_t *env)
{
	ir_node *node = env->irn;
	ir_node *sp = get_irn_n(node, 0);
	ir_node *load;
	ir_node *nomem = new_rd_NoMem(env->irg);
	ir_node *proj;
	mips_attr_t *attr;

	load = new_rd_mips_load_r(env->dbg, env->irg, env->block, nomem, sp, mode_T);
	attr = get_mips_attr(load);
	attr->stack_entity = be_get_frame_entity(node);
	attr->modes.load_store_mode = env->mode;

	proj = new_rd_Proj(env->dbg, env->irg, env->block, load, env->mode, pn_Load_res);

	return proj;
}

static ir_node *gen_node_for_AddSP(mips_transform_env_t *env)
{
	ir_node *node = env->irn;
	ir_node *op1, *op2;
	ir_node *add;
	const arch_register_t *reg;

	op1 = get_irn_n(node, 0);
	op2 = get_irn_n(node, 1);

	add = new_rd_mips_addu(env->dbg, env->irg, env->block, op1, op2);

	/* copy the register requirements from the old node to the new node */
	reg = arch_get_irn_register(env->cg->arch_env, node);
	arch_set_irn_register(env->cg->arch_env, add, reg);

	return add;
}

/*********************************************************
 *                  _             _      _
 *                 (_)           | |    (_)
 *  _ __ ___   __ _ _ _ __     __| |_ __ ___   _____ _ __
 * | '_ ` _ \ / _` | | '_ \   / _` | '__| \ \ / / _ \ '__|
 * | | | | | | (_| | | | | | | (_| | |  | |\ V /  __/ |
 * |_| |_| |_|\__,_|_|_| |_|  \__,_|_|  |_| \_/ \___|_|
 *
 *********************************************************/


/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void mips_transform_node(ir_node *node, void *env) {
	mips_code_gen_t *cgenv = (mips_code_gen_t *)env;
	ir_opcode code         = get_irn_opcode(node);
	ir_node *asm_node      = node;
	mips_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	DEBUG_ONLY(tenv.mod      = cgenv->mod;)
	tenv.mode     = get_irn_mode(node);
	tenv.cg		  = cgenv;

#define UNOP(firm_opcode, mips_nodetype)        case iro_##firm_opcode: asm_node = mips_gen_##mips_nodetype(&tenv, get_##firm_opcode##_op(node)); break
#define BINOP(firm_opcode, mips_nodetype)       case iro_##firm_opcode: asm_node = mips_gen_##mips_nodetype(&tenv, get_##firm_opcode##_left(node), get_##firm_opcode##_right(node)); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

	switch (code) {
 		BINOP(Add, addu);
		BINOP(Sub, sub);
		BINOP(And, and);
		BINOP(Or, or);
		BINOP(Eor, xor);
		UNOP(Not, not);
		BINOP(Shl, sl);
		BINOP(Shr, sr);
		BINOP(Shrs, sra);

	case iro_Abs:
		asm_node = gen_node_for_Abs(&tenv);
		break;

	case iro_Rot:
		asm_node = gen_node_for_Rot(&tenv);
		break;

	case iro_Div:
		asm_node = gen_node_for_Div(&tenv);
		break;

	case iro_Mod:
		asm_node = gen_node_for_Mod(&tenv);
		break;

	case iro_Load:
		asm_node = gen_node_for_Load(&tenv);
		break;

	case iro_Store:
		asm_node = gen_node_for_Store(&tenv);
		break;

	case iro_Proj:
		asm_node = gen_node_for_Proj(&tenv);
		break;

	case iro_Conv:
		asm_node = gen_node_for_Conv(&tenv);
		break;

	case iro_DivMod:
		asm_node = gen_node_for_DivMod(&tenv);
		break;

	case iro_Mul:
		asm_node = gen_node_for_Mul(&tenv);
		break;

	case iro_Jmp:
		asm_node = gen_node_for_Jmp(&tenv);
		break;

	case iro_IJmp:
		asm_node = gen_node_for_IJmp(&tenv);
		break;

	case iro_Unknown:
		asm_node = gen_node_for_Unknown(&tenv);
		break;

	case iro_Cond:
		asm_node = gen_node_for_Cond(&tenv);
		break;

		/* TODO: implement these nodes */
		BAD(Mux);

		/* You probably don't need to handle the following nodes */

		// call is handled in the emit phase
		IGN(Call);
		// Cmp is handled together with Cond
		IGN(Cmp);
		IGN(Alloc);

		IGN(Block);
		IGN(Start);
		IGN(End);
		IGN(NoMem);
		IGN(Phi);
		IGN(Break);
		IGN(Sync);

		IGN(Const);
		IGN(SymConst);

		BAD(Raise);
		BAD(Sel);
		BAD(InstOf);
		BAD(Cast);
		BAD(Free);
		BAD(Tuple);
		BAD(Id);
		BAD(Bad);
		BAD(Confirm);
		BAD(Filter);
		BAD(CallBegin);
		BAD(EndReg);
		BAD(EndExcept);

		default:
			if(be_is_StackParam(node)) {
				asm_node = gen_node_for_StackParam(&tenv);
			} else if(be_is_AddSP(node)) {
				asm_node = gen_node_for_AddSP(&tenv);
			}
			break;

bad:
		fprintf(stderr, "Not implemented: %s\n", get_irn_opname(node));
		assert(0);
	}

	if (asm_node != node) {
		exchange(node, asm_node);
		DB((tenv.mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	} else {
		DB((tenv.mod, LEVEL_1, "ignored\n"));
	}
}

void mips_pre_transform_node(ir_node *node, void *env) {
	mips_code_gen_t *cgenv = (mips_code_gen_t *)env;
	int i;

	mips_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	DEBUG_ONLY(tenv.mod      = cgenv->mod;)
	tenv.mode     = get_irn_mode(node);
	tenv.cg		  = cgenv;

	if(is_Proj(node)) {
		ir_node* pred = get_Proj_pred(node);

		if(get_irn_opcode(pred) == iro_CopyB) {
			mips_fix_CopyB_Proj(&tenv);
		}
	}

	for(i = 0; i < get_irn_arity(node); ++i) {
		ir_node* pred = get_irn_n(node, i);

		if (is_Const(pred)) {
			ir_node* constnode = exchange_node_for_Const(&tenv, pred, i);
			set_irn_n(node, i, constnode);
		} else if (get_irn_op(pred) == op_SymConst) {
			ir_node* constnode = gen_node_for_SymConst(&tenv, pred, i);
			set_irn_n(node, i, constnode);
		}
	}
}

/**
 * Calls the transform functions for Spill and Reload.
 */
void mips_after_ra_walker(ir_node *node, void *env) {
	mips_code_gen_t *cg = env;
	mips_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block = get_nodes_block(node);
	tenv.dbg   = get_irn_dbg_info(node);
	tenv.irg   = current_ir_graph;
	tenv.irn   = node;
	DEBUG_ONLY(tenv.mod   = cg->mod;)
	tenv.mode  = get_irn_mode(node);
	tenv.cg    = cg;

	/* be_is_StackParam(node) || */
	if (be_is_Reload(node)) {
		mips_transform_Reload(&tenv);
	} else if (be_is_Spill(node)) {
		mips_transform_Spill(&tenv);
	}
}
