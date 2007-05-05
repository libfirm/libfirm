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
 * @brief   implementation of mips assembly emitter
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "xmalloc.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog_t.h"
#include "irouts.h"
#include "tv.h"
#include "error.h"

#include "../besched.h"
#include "../benode_t.h"
#include "../beutil.h"
#include "../begnuas.h"

#include "mips_emitter.h"
#include "gen_mips_emitter.h"
#include "mips_nodes_attr.h"
#include "mips_new_nodes.h"
#include "mips_map_regs.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define SNPRINTF_BUF_LEN 128

/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const arch_env_t *arch_env,
                                         const ir_node *node, int pos)
{
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(node) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(node, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");
	return reg;
}

/**
 * Returns the register at out position pos.
 */
static const arch_register_t *get_out_reg(const arch_env_t *arch_env,
                                          const ir_node *node, int pos)
{
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(node) != mode_T) {
		reg = arch_get_irn_register(arch_env, node);
	} else if (is_mips_irn(node)) {
		reg = get_mips_out_reg(node, pos);
	} else {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
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

/**
 * Emit the name of the source register at given input position.
 */
void mips_emit_source_register(mips_emit_env_t *env, const ir_node *node,
                               int pos)
{
	const arch_register_t *reg = get_in_reg(env->arch_env, node, pos);
	be_emit_char(env->emit, '$');
	be_emit_string(env->emit, arch_register_get_name(reg));
}

/**
 * Emit the name of the destination register at given output position.
 */
void mips_emit_dest_register(mips_emit_env_t *env, const ir_node *node,
                             int pos)
{
	const arch_register_t *reg = get_out_reg(env->arch_env, node, pos);
	be_emit_char(env->emit, '$');
	be_emit_string(env->emit, arch_register_get_name(reg));
}

#if 0
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
#endif

void mips_emit_immediate(mips_emit_env_t *env, const ir_node *node)
{
	const mips_attr_t *attr;

	attr = get_mips_attr(node);
	be_emit_tarval(env->emit, attr->tv);
}

/*
 * Add a number to a prefix. This number will not be used a second time.
 */
static
char *get_unique_label(char *buf, size_t buflen, const char *prefix)
{
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%lu", prefix, ++id);
	return buf;
}

/************************************************************************/
/* ABI Handling                                                         */
/************************************************************************/

static
void mips_emit_IncSP(mips_emit_env_t *env, const ir_node *node)
{
	int   offset = be_get_IncSP_offset(node);

	if(offset == 0) {
		be_emit_cstring(env->emit, "\t/* omitted IncSP with 0 */");
		be_emit_finish_line_gas(env->emit, node);
		return;
	}

	if(offset > 0xffff || offset < -0xffff) {
		panic("stackframe > 2^16 bytes not supported yet\n");
	}

	if(offset > 0) {
		be_emit_irprintf(env->emit, "\tsubu $sp, $sp, %d", offset);
	} else {
		be_emit_irprintf(env->emit, "\taddu $sp, $sp, %d", -offset);
	}
	be_emit_finish_line_gas(env->emit, node);
}

static void mips_emit_Copy(mips_emit_env_t *env, const ir_node *node)
{
	be_emit_cstring(env->emit, "\tmove ");
	mips_emit_dest_register(env, node, 0);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 0);
	be_emit_finish_line_gas(env->emit, node);
}

static void mips_emit_Return(mips_emit_env_t *env, const ir_node* node)
{
	be_emit_cstring(env->emit, "\tj $ra");
	be_emit_finish_line_gas(env->emit, node);
}

static __attribute__((unused))
void mips_emit_nops(mips_emit_env_t *env, int n)
{
	int i;

	for(i = 0; i < n; ++i) {
		be_emit_cstring(env->emit, "\tnop\n");
		be_emit_write_line(env->emit);
	}
}

static void mips_emit_Perm(mips_emit_env_t *env, const ir_node *node)
{
	assert(get_irn_arity(node) == 2);

	be_emit_cstring(env->emit, "\txor ");
	mips_emit_source_register(env, node, 0);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 0);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 1);
	be_emit_finish_line_gas(env->emit, node);

	/* mips_emit_nops(env, 3); */

	be_emit_cstring(env->emit, "\txor ");
	mips_emit_source_register(env, node, 1);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 1);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 0);
	be_emit_finish_line_gas(env->emit, node);

	/* mips_emit_nops(env, 3); */

	be_emit_cstring(env->emit, "\txor ");
	mips_emit_source_register(env, node, 0);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 0);
	be_emit_cstring(env->emit, ", ");
	mips_emit_source_register(env, node, 1);
	be_emit_finish_line_gas(env->emit, node);

	/* mips_emit_nops(env, 3); */
}

/************************************************************************/
/* Calls                                                                */
/************************************************************************/

static void mips_emit_Call(mips_emit_env_t *env, const ir_node *node)
{
	ir_entity *callee;

	be_emit_cstring(env->emit, "\tjal ");

	/* call of immediate value (label) */
	callee = be_Call_get_entity(node);
	if(callee != NULL) {
		be_emit_ident(env->emit, get_entity_ld_ident(callee));
	} else {
		mips_emit_source_register(env, node, be_pos_Call_ptr);
	}
	be_emit_finish_line_gas(env->emit, node);
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

/**
 * Emits a block label from the given block.
 */
static
void mips_emit_block_label(mips_emit_env_t *env, const ir_node *block)
{
	be_emit_irprintf(env->emit, "BLOCK_%ld", get_irn_node_nr(block));
}

static void mips_emit_Jump(mips_emit_env_t *env, const ir_node *node)
{
	const ir_node *block = get_irn_link(node);
	assert(is_Block(block));

	be_emit_cstring(env->emit, "\tb ");
	mips_emit_block_label(env, block);
	be_emit_finish_line_gas(env->emit, node);
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

void mips_emit_jump_target_proj(mips_emit_env_t *env, const ir_node *node, int projn)
{
	ir_node *jumpblock = mips_get_jump_block(node, projn);
	assert(jumpblock != NULL);

	mips_emit_block_label(env, jumpblock);
}

void mips_emit_jump_target(mips_emit_env_t *env, const ir_node *node)
{
	ir_node *jumpblock = get_irn_link(node);
	assert(jumpblock != NULL);

	mips_emit_block_label(env, jumpblock);
}

/************************************************************************
 *  ____          _ _       _         _                                 *
 * / ___|_      _(_) |_ ___| |__     | |_   _ _ __ ___  _ __            *
 * \___ \ \ /\ / / | __/ __| '_ \ _  | | | | | '_ ` _ \| '_ \           *
 *  ___) \ V  V /| | || (__| | | | |_| | |_| | | | | | | |_) |          *
 * |____/ \_/\_/ |_|\__\___|_| |_|\___/ \__,_|_| |_| |_| .__/           *
 *                                                     |_|              *
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
void emit_mips_jump_table(mips_emit_env_t *env, const ir_node *irn) {
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

	be_emit_string(env->emit, mips_get_jumptbl_label(irn));
	be_emit_cstring(env->emit, ":\n");
	be_emit_write_line(env->emit);
	lastval = tbl.min_value;
	for(i = 0; i < tbl.num_branches; ++i) {
		const branch_t *branch = &tbl.branches[i];
		int value = branch->value;

		for(i2 = lastval + 1; i2 < value; ++i2) {
			be_emit_cstring(env->emit, "\t.word ");
			be_emit_ident(env->emit, attr->symconst_id);
			be_emit_char(env->emit, '\n');
			be_emit_write_line(env->emit);
		}

		be_emit_cstring(env->emit, "\t.word ");
		mips_emit_block_label(env, branch->target);
		be_emit_char(env->emit, '\n');
		be_emit_write_line(env->emit);

		lastval = branch->value;
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

static
void dump_jump_tables(ir_node* node, void *data)
{
	mips_emit_env_t *env = data;

	// emit jump tables
	if(is_mips_SwitchJump(node)) {
		be_emit_cstring(env->emit, ".data\n");
		be_emit_write_line(env->emit);

		emit_mips_jump_table(env, node);

		be_emit_cstring(env->emit, ".text\n");
		be_emit_write_line(env->emit);
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

static void mips_emit_nothing(mips_emit_env_t *env, const ir_node *node)
{
	(void) env;
	(void) node;
}

static void mips_emit_this_shouldnt_happen(mips_emit_env_t *env, const ir_node *node)
{
	(void) env;
	panic("Found non-lowered node %+F while emitting", node);
}

/**
 * The type of a emitter function.
 */
typedef void (*emit_func) (mips_emit_env_t *, const ir_node *);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static void register_emitter(ir_op *op, emit_func func) {
	op->ops.generic = (op_func) func;
}

/**
 * Register emitter functions for mips backend
 */
void mips_register_emitters(void) {
	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	mips_register_spec_emitters();

	/* benode emitter */
	register_emitter(op_be_IncSP, mips_emit_IncSP);
	register_emitter(op_be_SetSP, mips_emit_this_shouldnt_happen);
	register_emitter(op_be_AddSP, mips_emit_this_shouldnt_happen);
	register_emitter(op_be_Call, mips_emit_Call);
	register_emitter(op_be_Copy, mips_emit_Copy);
	register_emitter(op_be_Keep, mips_emit_nothing);
	register_emitter(op_be_Barrier, mips_emit_nothing);
	register_emitter(op_be_Return, mips_emit_Return);
	register_emitter(op_be_RegParams, mips_emit_nothing);
	register_emitter(op_be_Spill, mips_emit_this_shouldnt_happen);
	register_emitter(op_be_Reload, mips_emit_this_shouldnt_happen);
	register_emitter(op_be_Perm, mips_emit_Perm);

	register_emitter(op_Start, mips_emit_nothing);
	register_emitter(op_Proj, mips_emit_nothing);
	register_emitter(op_SymConst, mips_emit_this_shouldnt_happen);
	register_emitter(op_Const, mips_emit_this_shouldnt_happen);
	register_emitter(op_Jmp, mips_emit_Jump);
	register_emitter(op_Cmp, mips_emit_this_shouldnt_happen);
	register_emitter(op_Cond, mips_emit_this_shouldnt_happen);
}

/**
 * Emits assembly for a single node
 */
static void mips_emit_node(mips_emit_env_t *env, const ir_node *node)
{
	ir_op *op = get_irn_op(node);

	if (op->ops.generic) {
		emit_func emit = (emit_func) op->ops.generic;
		(*emit) (env, node);
	} else {
		panic("No emitter defined for node %+F", node);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
void mips_gen_block(mips_emit_env_t *env, ir_node *block)
{
	ir_node *node;

	if (! is_Block(block))
		return;

	mips_emit_block_label(env, block);
	be_emit_cstring(env->emit, ":\n");
	be_emit_write_line(env->emit);

	sched_foreach(block, node) {
		mips_emit_node(env, node);
	}

	be_emit_char(env->emit, '\n');
	be_emit_write_line(env->emit);
}

/**
 * Emits code for function start.
 */
void mips_emit_func_prolog(mips_emit_env_t *env, ir_graph *irg)
{
	ident *irg_ident = get_entity_ld_ident(get_irg_entity(irg));
	be_emit_env_t *eenv = env->emit;

	// dump jump tables
	irg_walk_graph(irg, NULL, dump_jump_tables, env);

	be_emit_write_line(eenv);
	be_gas_emit_switch_section(eenv, GAS_SECTION_TEXT);

	be_emit_cstring(eenv, "\t.balign\t4\n");

	be_emit_cstring(eenv, "\t.global\t");
	be_emit_ident(eenv, irg_ident);
	be_emit_char(eenv, '\n');

	be_emit_cstring(eenv, "\t.set\tnomips16\n");

	be_emit_cstring(eenv, "\t.ent\t");
	be_emit_ident(eenv, irg_ident);
	be_emit_char(eenv, '\n');

	be_emit_ident(eenv, irg_ident);
	be_emit_cstring(eenv, ":\n");

	be_emit_cstring(eenv, "\t.frame\t$fp, 24, $ra\n");
	be_emit_cstring(eenv, "\t.mask\t0xc0000000, -4\n");
	be_emit_cstring(eenv, "\t.fmask\t0x00000000, 0\n");

	be_emit_write_line(eenv);
}

/**
 * Emits code for function end
 */
void mips_emit_func_epilog(mips_emit_env_t *env, ir_graph *irg)
{
	ident *irg_ident = get_entity_ident(get_irg_entity(irg));

	be_emit_cstring(env->emit, "\t.end\t");
	be_emit_ident(env->emit, irg_ident);
	be_emit_char(env->emit, '\n');
	be_emit_write_line(env->emit);
}

/**
 * Sets labels for control flow nodes (jump target)
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
void mips_gen_routine(mips_code_gen_t *cg, ir_graph *irg)
{
	mips_emit_env_t  env;
	int i, n;

	env.isa      = (mips_isa_t*) cg->arch_env->isa;
	env.emit     = &env.isa->emit;
	env.arch_env = cg->arch_env;
	env.cg       = cg;

	mips_register_emitters();

	irg_block_walk_graph(irg, mips_gen_labels, NULL, &env);

	mips_emit_func_prolog(&env, irg);

	dump_ir_block_graph_sched(irg, "-kaputtelist");

	for (i = 0, n = mips_get_sched_n_blocks(cg); i < n; ++i) {
		ir_node *block = mips_get_sched_block(cg, i);
		mips_gen_block(&env, block);
	}

	mips_emit_func_epilog(&env, irg);
}

void mips_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.mips.emitter");
}
