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

/* mips emitter */
/* $Id$ */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog_t.h"
#include "irouts.h"
#include "error.h"

#include "../besched.h"
#include "../benode_t.h"
#include "../beutil.h"

#include "mips_emitter.h"
#include "gen_mips_emitter.h"
#include "mips_nodes_attr.h"
#include "mips_new_nodes.h"
#include "mips_map_regs.h"

#define SNPRINTF_BUF_LEN 128

static const arch_env_t *arch_env = NULL;


/*************************************************************
 *             _       _    __   _          _
 *            (_)     | |  / _| | |        | |
 *  _ __  _ __ _ _ __ | |_| |_  | |__   ___| |_ __   ___ _ __
 * | '_ \| '__| | '_ \| __|  _| | '_ \ / _ \ | '_ \ / _ \ '__|
 * | |_) | |  | | | | | |_| |   | | | |  __/ | |_) |  __/ |
 * | .__/|_|  |_|_| |_|\__|_|   |_| |_|\___|_| .__/ \___|_|
 * | |                                       | |
 * |_|                                       |_|
 *************************************************************/

static const char *get_symconst_str(ir_node *node)
{
	ident *id;

	switch(get_SymConst_kind(node)) {
	case symconst_addr_name:
		id = get_SymConst_name(node);
		return get_id_str(id);
	case symconst_addr_ent:
		id = get_entity_ident(get_SymConst_entity(node));
		return get_id_str(id);
	default:
		assert(0);
	}

	return NULL;
}

/**
 * Return a const or symconst as string.
 */
static const char *node_const_to_str(ir_node *n)
{
	static char buf[64];
	const mips_attr_t *attr = get_mips_attr(n);
	long val;

	if(is_mips_load_r(n) || is_mips_store_r(n)) {
		mips_attr_t *attr = get_mips_attr(n);
		ir_node *symconst;

		if(attr->tv != NULL) {
			val = get_tarval_long(attr->tv);
			snprintf(buf, sizeof(buf), "%ld", val);

			return buf;
		}
		if(attr->stack_entity != NULL) {
			snprintf(buf, sizeof(buf), "%d", attr->stack_entity_offset);
			return buf;
		}

		symconst = get_irn_n(n, 1);
		assert(get_irn_opcode(symconst) == iro_SymConst);

		return get_symconst_str(symconst);
	} else if(is_mips_la(n)) {
		snprintf(buf, sizeof(buf), "%s", get_id_str(attr->symconst_id));
		return buf;
	} else if(is_mips_lli(n)) {
		assert(attr->tv != NULL);
		if(get_mode_sign(get_tarval_mode(attr->tv))) {
			long val = get_tarval_long(attr->tv);
			snprintf(buf, sizeof(buf), "0x%04lX", val & 0xffff);
		} else {
			unsigned long val = get_tarval_long(attr->tv);
			snprintf(buf, sizeof(buf), "0x%04lX", val & 0xffff);
		}

		return buf;
	} else if(is_mips_lui(n)) {
		assert(attr->tv != NULL);
		if(get_mode_sign(get_tarval_mode(attr->tv))) {
			long val = get_tarval_long(attr->tv);
			val = (val & 0xffff0000) >> 16;
			snprintf(buf, sizeof(buf), "0x%04lX", val & 0xffff);
		} else {
			unsigned long val = get_tarval_long(attr->tv);
			val = (val & 0xffff0000) >> 16;
			snprintf(buf, sizeof(buf), "0x%04lX", val & 0xffff);
		}

		return buf;
	}

	assert(attr->tv != NULL);
	val = get_tarval_long(attr->tv);
	snprintf(buf, sizeof(buf), "%ld", val);

	return buf;
}

/**
 * Returns node's offset as string.
 */
static const char *node_offset_to_str(ir_node *n)
{
	return "";
}

/* We always pass the ir_node which is a pointer. */
static int mips_get_arg_type(const lc_arg_occ_t *occ) {
	return lc_arg_type_ptr;
}


/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(ir_node *irn, int pos)
{
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");
	return reg;
}

/**
 * Returns the register at out position pos.
 */
static const arch_register_t *get_out_reg(ir_node *irn, int pos)
{
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		reg = arch_get_irn_register(arch_env, irn);
	}
	else if (is_mips_irn(irn)) {
		reg = get_mips_out_reg(irn, pos);
	}
	else {
		const ir_edge_t *edge;

		foreach_out_edge(irn, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "non-Proj from mode_T node");
			if (get_Proj_proj(proj) == pos) {
				reg = arch_get_irn_register(arch_env, proj);
				break;
			}
		}
	}

	assert(reg && "no out register found");
	return reg;
}

/**
 * Returns the number of the in register at position pos.
 */
int get_mips_reg_nr(ir_node *irn, int pos, int in_out)
{
	const arch_register_t *reg;

	if (in_out == 1) {
		reg = get_in_reg(irn, pos);
	}
	else {
		reg = get_out_reg(irn, pos);
	}

	return arch_register_get_index(reg);
}

/**
 * Returns the name of the in register at position pos.
 */
const char *get_mips_reg_name(ir_node *irn, int pos, int in_out)
{
	const arch_register_t *reg;

	if (in_out == 1) {
		reg = get_in_reg(irn, pos);
	}
	else {
		reg = get_out_reg(irn, pos);
	}

	return arch_register_get_name(reg);
}

/**
 * Get the register name for a node.
 */
static int mips_get_reg_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	int res;
	ir_node    *X  = arg->v_ptr;
	int         nr = occ->width - 1;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'S') {
		buf = get_mips_reg_name(X, nr, 1);
	}
	else { /* 'D' */
		buf = get_mips_reg_name(X, nr, 0);
	}

	res = lc_appendable_chadd(app, '$');
	res += lc_appendable_snadd(app, buf, strlen(buf));
	return res;
}

/**
 * Returns the tarval or offset of an mips node as a string.
 */
static int mips_const_to_str(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X = arg->v_ptr;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'C') {
		buf = node_const_to_str(X);
	}
	else { /* 'O' */
		buf = node_offset_to_str(X);
	}

	return lc_arg_append(app, occ, buf, strlen(buf));
}

/**
 * Determines the SSE suffix depending on the mode.
 */
static int mips_get_mode_suffix(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	ir_node *X = arg->v_ptr;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (get_mode_size_bits(get_irn_mode(X)) == 32)
		return lc_appendable_chadd(app, 's');
	else
		return lc_appendable_chadd(app, 'd');
}

/**
 * Return the mips printf arg environment.
 * We use the firm environment with some additional handlers.
 */
const lc_arg_env_t *mips_get_arg_env(void)
{
	static lc_arg_env_t *env = NULL;

	static const lc_arg_handler_t mips_reg_handler   = { mips_get_arg_type, mips_get_reg_name };
	static const lc_arg_handler_t mips_const_handler = { mips_get_arg_type, mips_const_to_str };
	static const lc_arg_handler_t mips_mode_handler  = { mips_get_arg_type, mips_get_mode_suffix };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();
			//lc_arg_new_env();

		lc_arg_register(env, "mips:sreg", 'S', &mips_reg_handler);
		lc_arg_register(env, "mips:dreg", 'D', &mips_reg_handler);
		lc_arg_register(env, "mips:cnst", 'C', &mips_const_handler);
		lc_arg_register(env, "mips:offs", 'O', &mips_const_handler);
		lc_arg_register(env, "mips:mode", 'M', &mips_mode_handler);
	}

	return env;
}

/*
 * Add a number to a prefix. This number will not be used a second time.
 */
static char *get_unique_label(char *buf, size_t buflen, const char *prefix)
{
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%lu", prefix, ++id);
	return buf;
}

/************************************************************************/
/* ABI Handling                                                         */
/************************************************************************/

static void mips_emit_IncSP(const ir_node *node, mips_emit_env_t *env)
{
	FILE *F      = env->out;
	int   offset = be_get_IncSP_offset(node);

	if(offset == 0) {
		fprintf(F, "\t\t\t\t # omitted IncSP with 0\n");
		return;
	}

	if(offset > 0xffff || offset < -0xffff) {
		panic("stackframe > 2^16 bytes not supported yet\n");
	}

	if(offset > 0) {
		fprintf(F, "\tsubu $sp, $sp, %d\n", offset);
	} else {
		fprintf(F, "\taddu $sp, $sp, %d\n", -offset);
	}
}

static void mips_emit_Copy(const ir_node *node, mips_emit_env_t *env)
{
	FILE *F = env->out;

	lc_efprintf(mips_get_arg_env(), F, "\tmove %1D, %1S\t\t\t# copy\n", node, node);
}

static void mips_emit_Return(const ir_node* node, mips_emit_env_t *env)
{
	FILE *F = env->out;
	fprintf(F, "\tj $ra\t\t\t\t# return\n");
}

static void mips_emit_nops(FILE* F, int n)
{
	int i;

	for(i = 0; i < n; ++i) {
		fprintf(F, "\tnop\n");
	}
}

static void mips_emit_Perm(const ir_node *node, mips_emit_env_t *env)
{
	FILE *F = env->out;

	assert(get_irn_arity(node) == 2);

	lc_efprintf(mips_get_arg_env(), F, "\txor %1S, %1S, %2S\t\t\t# perm\n", node, node, node);
	mips_emit_nops(F, 3);
	lc_efprintf(mips_get_arg_env(), F, "\txor %2S, %2S, %1S\n", node, node, node);
	mips_emit_nops(F, 3);
	lc_efprintf(mips_get_arg_env(), F, "\txor %1S, %1S, %2S\n", node, node, node);
	mips_emit_nops(F, 3);
}

static void mips_emit_Spill(const ir_node* node, mips_emit_env_t *env)
{
	FILE      *F   = env->out;
	ir_entity *ent = be_get_frame_entity(node);

	lc_efprintf(mips_get_arg_env(), F, "\tsw %1S, %d($fp)\n", node, get_entity_offset(ent));
}

static void mips_emit_Reload(const ir_node* node, mips_emit_env_t *env)
{
	FILE      *F   = env->out;
	ir_entity *ent = be_get_frame_entity(node);

	lc_efprintf(mips_get_arg_env(), F, "\tlw %1D, %d($fp)\n", node, get_entity_offset(ent));
}

/************************************************************************/
/* Calls                                                                */
/************************************************************************/

static void mips_emit_Call(ir_node *node, mips_emit_env_t *env)
{
	FILE *F = env->out;
	const arch_register_t *callee_reg;

	// call to imediate value (label)
	ir_entity *callee = be_Call_get_entity(node);
	if(callee != NULL) {
		fprintf(F, "\tjal %s\n", get_entity_name(callee));
		return;
	}

	// call to function pointer
	callee_reg = get_in_reg(node, be_pos_Call_ptr);
	assert(callee_reg != NULL);

	fprintf(F, "\tjal %s\n", arch_register_get_name(callee_reg));
}

/************************************************************************
 *      _
 *     | |_   _ _ __ ___  _ __  ___
 *  _  | | | | | '_ ` _ \| '_ \/ __|
 * | |_| | |_| | | | | | | |_) \__ \
 *  \___/ \__,_|_| |_| |_| .__/|___/
 *                       |_|
 ************************************************************************/

const char* mips_get_block_label(const ir_node* block)
{
	static char buf[64];
	snprintf(buf, sizeof(buf), "BLOCK_%ld", get_irn_node_nr(block));

	return buf;
}

static void mips_emit_Jump(ir_node *node, mips_emit_env_t *env)
{
	FILE *F = env->out;
	const ir_node *block = get_irn_link(node);

	assert(is_Block(block));

	fprintf(F, "\tb %s\n", mips_get_block_label(block));
}

ir_node *mips_get_jump_block(const ir_node* node, int projn)
{
	const ir_edge_t *oute;
	for(oute = get_irn_out_edge_first(node); oute != NULL;
	    oute = get_irn_out_edge_next(node, oute)) {
		ir_node *proj = get_edge_src_irn(oute);
		long n;
		assert(is_Proj(proj));

		n = get_Proj_proj(proj);
		if(n == projn)
			return get_irn_link(proj);
	}

	return NULL;
}

/************************************************************************
 *  ____          _ _       _         _									*
 * / ___|_      _(_) |_ ___| |__     | |_   _ _ __ ___  _ __			*
 * \___ \ \ /\ / / | __/ __| '_ \ _  | | | | | '_ ` _ \| '_ \			*
 *  ___) \ V  V /| | || (__| | | | |_| | |_| | | | | | | |_) |			*
 * |____/ \_/\_/ |_|\__\___|_| |_|\___/ \__,_|_| |_| |_| .__/			*
 *                                                     |_|				*
 *                                                                      *
 ************************************************************************/

/* jump table entry (target and corresponding number) */
typedef struct _branch_t {
	ir_node *target;
	int      value;
} branch_t;

/* jump table for switch generation */
typedef struct _jmp_tbl_t {
	ir_node  *defBlock;        /**< default target */
	int       min_value;       /**< smallest switch case */
	int       max_value;       /**< largest switch case */
	int       num_branches;    /**< number of jumps */
	char     *label;           /**< label of the jump table */
	branch_t *branches;        /**< jump array */
} jmp_tbl_t;

/**
 * Compare two variables of type branch_t. Used to sort all switch cases
 */
static int mips_cmp_branch_t(const void *a, const void *b) {
	branch_t *b1 = (branch_t *)a;
	branch_t *b2 = (branch_t *)b;

	if (b1->value <= b2->value)
		return -1;
	else
		return 1;
}

const char* mips_get_jumptbl_label(const ir_node* switchjmp)
{
	static char buf[64];
	snprintf(buf, sizeof(buf), "__JUMPTBL%ld", get_irn_node_nr(switchjmp));

	return buf;
}

/**
 * Emits code for a SwitchJmp (creates a jump table if
 * possible otherwise a cmp-jmp cascade). Stolen from ia32
 */
void emit_mips_jump_table(const ir_node *irn, FILE* F) {
	int              lastval, i, i2, pn;
	jmp_tbl_t        tbl;
	ir_node         *proj;
	const ir_edge_t *edge;
	mips_attr_t     *attr = get_mips_attr(irn);

	/* fill the table structure */
	tbl.label        = xmalloc(SNPRINTF_BUF_LEN);
	tbl.label        = get_unique_label(tbl.label, SNPRINTF_BUF_LEN, "JMPTBL_");
	tbl.defBlock     = NULL;
	tbl.num_branches = get_irn_n_edges(irn);
	tbl.branches     = xcalloc(tbl.num_branches, sizeof(tbl.branches[0]));
	tbl.min_value    = INT_MAX;
	tbl.max_value    = INT_MIN;

	i = 0;
	/* go over all proj's and collect them */
	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pn = get_Proj_proj(proj);

		/* create branch entry */
		tbl.branches[i].target = get_irn_link(proj);
		tbl.branches[i].value  = pn;

		tbl.min_value = pn < tbl.min_value ? pn : tbl.min_value;
		tbl.max_value = pn > tbl.max_value ? pn : tbl.max_value;

		i++;
	}

	/* sort the branches by their number */
	qsort(tbl.branches, tbl.num_branches, sizeof(tbl.branches[0]), mips_cmp_branch_t);

	fprintf(F, "%s:\n", mips_get_jumptbl_label(irn));
	lastval = tbl.min_value;
	for(i = 0; i < tbl.num_branches; ++i) {
		const branch_t *branch = &tbl.branches[i];
		int value = branch->value;

		for(i2 = lastval + 1; i2 < value; ++i2) {
			fprintf(F, "\t.word %s\n", get_id_str(attr->symconst_id));
		}

		fprintf(F, "\t.word %s\n", mips_get_block_label(branch->target));
		lastval = branch->value;
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

static void dump_jump_tables(ir_node* node, void *env)
{
	FILE* F = (FILE*) env;

	// emit jump tables
	if(is_mips_SwitchJump(node)) {
		fprintf(F, ".data\n");
		emit_mips_jump_table(node, F);
		fprintf(F, ".text\n");
	}
}

/***********************************************************************************
 *                  _          __                                             _
 *                 (_)        / _|                                           | |
 *  _ __ ___   __ _ _ _ __   | |_ _ __ __ _ _ __ ___   _____      _____  _ __| | __
 * | '_ ` _ \ / _` | | '_ \  |  _| '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
 * | | | | | | (_| | | | | | | | | | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
 * |_| |_| |_|\__,_|_|_| |_| |_| |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
 *
 ***********************************************************************************/

static void mips_emit_nothing(ir_mode *mode, mips_emit_env_t *env)
{
}

static void mips_emit_this_shouldnt_happen(ir_mode *mode, mips_emit_env_t *env)
{
	assert(0);
}

/**
 * Register emitter functions for mips backend
 */
void mips_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	mips_register_spec_emitters();

	/* benode emitter */
	op_be_IncSP->ops.generic = (op_func) mips_emit_IncSP;
	op_be_SetSP->ops.generic = (op_func) mips_emit_this_shouldnt_happen;
	op_be_AddSP->ops.generic = (op_func) mips_emit_this_shouldnt_happen;
	op_be_Call->ops.generic = (op_func) mips_emit_Call;
	op_be_Keep->ops.generic = (op_func) mips_emit_nothing;
	op_be_Copy->ops.generic = (op_func) mips_emit_Copy;
	op_be_Return->ops.generic = (op_func) mips_emit_Return;
	op_be_RegParams->ops.generic = (op_func) mips_emit_nothing;
	op_be_Spill->ops.generic = (op_func) mips_emit_Spill;
	op_be_Reload->ops.generic = (op_func) mips_emit_Reload;
	op_be_Perm->ops.generic = (op_func) mips_emit_Perm;

	op_Start->ops.generic = (op_func) mips_emit_nothing;
	op_Proj->ops.generic = (op_func) mips_emit_nothing;
	op_SymConst->ops.generic = (op_func) mips_emit_nothing;
	op_Jmp->ops.generic = (op_func) mips_emit_Jump;
	op_Cmp->ops.generic = (op_func) mips_emit_this_shouldnt_happen;
	op_Cond->ops.generic = (op_func) mips_emit_this_shouldnt_happen;
}

typedef void (*emit_func) (const ir_node *, mips_emit_env_t *);

/**
 * Emits assembly for a single node
 */
static void mips_emit_node(ir_node *irn, mips_emit_env_t* env)
{
	mips_emit_env_t   *emit_env = env;
	FILE              *F        = emit_env->out;
	ir_op             *op       = get_irn_op(irn);
	DEBUG_ONLY(firm_dbg_module_t *mod = emit_env->mod;)

	DBG((mod, LEVEL_1, "emitting code for %+F\n", irn));

	if (op->ops.generic) {
		emit_func emit = (emit_func) op->ops.generic;
		(*emit) (irn, env);

#if 0
		if(emit != (emit_func) mips_emit_nothing)
			mips_emit_nops(F, 5);
#endif
	} else {
		ir_fprintf(F, "\t\t\t\t\t# %+F\n", irn);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
void mips_gen_block(ir_node *block, void *env)
{
	FILE *F = ((mips_emit_env_t *)env)->out;
	ir_node *irn;

	if (! is_Block(block))
		return;

	fprintf(F, "%s:\n", mips_get_block_label(block));
	sched_foreach(block, irn) {
		mips_emit_node(irn, env);
	}
	fprintf(F, "\n");
}

/**
 * Emits code for function start.
 */
void mips_emit_start(FILE *F, ir_graph *irg)
{
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	// dump jump tables
	irg_walk_graph(irg, NULL, dump_jump_tables, F);

	fprintf(F, "\n\n");
	fprintf(F, "\t.balign\t4\n");
	fprintf(F, "\t.global\t%s\n", irg_name);
	fprintf(F, "\t.set\tnomips16\n");
	fprintf(F, "\t.ent\t%s\n", irg_name);
	fprintf(F, "%s:\n", irg_name);
	fprintf(F, "\t.frame\t$fp, 24, $ra\n");
	fprintf(F, "\t.mask\t0xc0000000, -4\n");
	fprintf(F, "\t.fmask\t0x00000000, 0\n");
}

/**
 * Emits code for function end
 */
void mips_emit_end(FILE *F, ir_graph *irg)
{
	const char *irg_name = get_entity_name(get_irg_entity(irg));
	fprintf(F, "\t.end\t%s\n", irg_name);
}

/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void mips_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Main driver
 */
void mips_gen_routine(FILE *F, ir_graph *irg, const mips_code_gen_t *cg)
{
	mips_emit_env_t emit_env;
	int i, n;

	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.mips.emit");

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	irg_block_walk_graph(irg, mips_gen_labels, NULL, &emit_env);
	mips_emit_start(F, irg);
//	irg_walk_blkwise_graph(irg, NULL, mips_gen_block, &emit_env);

	dump_ir_block_graph_sched(irg, "-kaputtelist");

	for (i = 0, n = mips_get_sched_n_blocks(cg); i < n; ++i) {
		ir_node *block = mips_get_sched_block(cg, i);
		mips_gen_block(block, &emit_env);
	}

	mips_emit_end(F, irg);
}
