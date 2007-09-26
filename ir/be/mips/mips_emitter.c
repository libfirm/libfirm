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

#define BLOCK_PREFIX ".L"

#define SNPRINTF_BUF_LEN 128

static const mips_isa_t      *isa;
static const arch_env_t      *arch_env;
static const mips_code_gen_t *cg;

/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const ir_node *node, int pos)
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
static const arch_register_t *get_out_reg(const ir_node *node, int pos)
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
void mips_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(node, pos);
	be_emit_char('$');
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Emit the name of the destination register at given output position.
 */
void mips_emit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_out_reg(node, pos);
	be_emit_char('$');
	be_emit_string(arch_register_get_name(reg));
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

void mips_emit_load_store_address(const ir_node *node, int pos)
{
	const mips_load_store_attr_t *attr = get_mips_load_store_attr_const(node);

	be_emit_irprintf("%d(", attr->offset);
	mips_emit_source_register(node, pos);
	be_emit_char(')');
}

void mips_emit_immediate_suffix(const ir_node *node, int pos)
{
	ir_node *op = get_irn_n(node, pos);
	if(is_mips_Immediate(op))
		be_emit_char('i');
}

void mips_emit_immediate(const ir_node *node)
{
	const mips_immediate_attr_t *attr = get_mips_immediate_attr_const(node);

	switch(attr->imm_type) {
	case MIPS_IMM_CONST:
		be_emit_irprintf("%d", attr->val);
		break;
	case MIPS_IMM_SYMCONST_LO:
		be_emit_cstring("%lo($");
		be_emit_ident(get_entity_ld_ident(attr->entity));
		if(attr->val != 0) {
			be_emit_irprintf("%+d", attr->val);
		}
		be_emit_char(')');
		break;
	case MIPS_IMM_SYMCONST_HI:
		be_emit_cstring("%hi($");
		be_emit_ident(get_entity_ld_ident(attr->entity));
		if(attr->val != 0) {
			be_emit_irprintf("%+d", attr->val);
		}
		be_emit_char(')');
		break;
	default:
		panic("invalid immediate type found");
	}
}

/**
 * Emit the name of the destination register at given output position.
 */
void mips_emit_source_register_or_immediate(const ir_node *node, int pos)
{
	const ir_node *op = get_irn_n(node, pos);
	if(is_mips_Immediate(op)) {
		mips_emit_immediate(op);
	} else {
		mips_emit_source_register(node, pos);
	}
}

#if 0
/*
 * Add a number to a prefix. This number will not be used a second time.
 */
static char *get_unique_label(char *buf, size_t buflen, const char *prefix)
{
	static unsigned long id = 0;
	snprintf(buf, buflen, "%s%lu", prefix, ++id);
	return buf;
}
#endif

/************************************************************************/
/* ABI Handling                                                         */
/************************************************************************/

static
void mips_emit_IncSP(const ir_node *node)
{
	int   offset = be_get_IncSP_offset(node);

	if(offset == 0) {
		be_emit_cstring("\t/* omitted IncSP with 0 */");
		be_emit_finish_line_gas(node);
		return;
	}

	if(offset > 0xffff || offset < -0xffff) {
		panic("stackframe > 2^16 bytes not supported yet\n");
	}

	if(offset > 0) {
		be_emit_irprintf("\tsubu $sp, $sp, %d", offset);
	} else {
		be_emit_irprintf("\taddu $sp, $sp, %d", -offset);
	}
	be_emit_finish_line_gas(node);
}

static void mips_emit_Copy(const ir_node *node)
{
	be_emit_cstring("\tmove ");
	mips_emit_dest_register(node, 0);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 0);
	be_emit_finish_line_gas(node);
}

static void mips_emit_Return(const ir_node* node)
{
	be_emit_cstring("\tj $ra");
	be_emit_finish_line_gas(node);
}

static __attribute__((unused))
void mips_emit_nops(int n)
{
	int i;

	for(i = 0; i < n; ++i) {
		be_emit_cstring("\tnop\n");
		be_emit_write_line();
	}
}

static void mips_emit_Perm(const ir_node *node)
{
	assert(get_irn_arity(node) == 2);

	be_emit_cstring("\txor ");
	mips_emit_source_register(node, 0);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 0);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 1);
	be_emit_finish_line_gas(node);

	/* mips_emit_nops(3); */

	be_emit_cstring("\txor ");
	mips_emit_source_register(node, 1);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 1);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 0);
	be_emit_finish_line_gas(node);

	/* mips_emit_nops(3); */

	be_emit_cstring("\txor ");
	mips_emit_source_register(node, 0);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 0);
	be_emit_cstring(", ");
	mips_emit_source_register(node, 1);
	be_emit_finish_line_gas(node);

	/* mips_emit_nops(3); */
}

/************************************************************************/
/* Calls                                                                */
/************************************************************************/

static void mips_emit_Call(const ir_node *node)
{
	ir_entity *callee;

	be_emit_cstring("\tjal ");

	/* call of immediate value (label) */
	callee = be_Call_get_entity(node);
	if(callee != NULL) {
		be_emit_ident(get_entity_ld_ident(callee));
	} else {
		mips_emit_source_register(node, be_pos_Call_ptr);
	}
	be_emit_finish_line_gas(node);
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
static void mips_emit_block_label(const ir_node *block)
{
	if (has_Block_label(block)) {
		be_emit_string(be_gas_label_prefix());
		be_emit_irprintf("%lu", get_Block_label(block));
	} else {
		be_emit_cstring(BLOCK_PREFIX);
		be_emit_irprintf("%d", get_irn_node_nr(block));

	}
}

static void mips_emit_Jump(const ir_node *node)
{
	const ir_node *block = get_irn_link(node);
	assert(is_Block(block));

	be_emit_cstring("\tb ");
	mips_emit_block_label(block);
	be_emit_finish_line_gas(node);
}

ir_node *mips_get_jump_block(const ir_node* node, long projn)
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

void mips_emit_jump_target_proj(const ir_node *node, long projn)
{
	ir_node *jumpblock = mips_get_jump_block(node, projn);
	assert(jumpblock != NULL);

	mips_emit_block_label(jumpblock);
}

void mips_emit_jump_target(const ir_node *node)
{
	ir_node *jumpblock = get_irn_link(node);
	assert(jumpblock != NULL);

	mips_emit_block_label(jumpblock);
}

void mips_emit_jump_or_fallthrough(const ir_node *node, long pn)
{
	ir_node *jumpblock = mips_get_jump_block(node, pn);
	assert(jumpblock != NULL);

	/* TODO: use fallthrough when possible */
	be_emit_cstring("b ");
	mips_emit_block_label(jumpblock);
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

#if 0
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
static int mips_cmp_branch_t(const void *a, const void *b)
{
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
void emit_mips_jump_table(const ir_node *irn)
{
	int                lastval, i, i2, pn;
	jmp_tbl_t          tbl;
	ir_node           *proj;
	const ir_edge_t   *edge;
	const mips_attr_t *attr = get_mips_attr_const(irn);

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

	be_emit_string(mips_get_jumptbl_label(irn));
	be_emit_cstring(":\n");
	be_emit_write_line();
	lastval = tbl.min_value;
	for(i = 0; i < tbl.num_branches; ++i) {
		const branch_t *branch = &tbl.branches[i];
		int value = branch->value;

		for(i2 = lastval + 1; i2 < value; ++i2) {
			be_emit_cstring("\t.word ");
			be_emit_ident(get_entity_ld_ident(attr->symconst));
			be_emit_char('\n');
			be_emit_write_line();
		}

		be_emit_cstring("\t.word ");
		mips_emit_block_label(branch->target);
		be_emit_char('\n');
		be_emit_write_line();

		lastval = branch->value;
	}

	if (tbl.label)
		free(tbl.label);
	if (tbl.branches)
		free(tbl.branches);
}

static void dump_jump_tables(ir_node* node, void *data)
{
	(void) data;

	// emit jump tables
	if(is_mips_SwitchJump(node)) {
		be_emit_cstring(".data\n");
		be_emit_write_line();

		emit_mips_jump_table(node);

		be_emit_cstring(".text\n");
		be_emit_write_line();
	}
}
#endif

/***********************************************************************************
 *                  _          __                                             _
 *                 (_)        / _|                                           | |
 *  _ __ ___   __ _ _ _ __   | |_ _ __ __ _ _ __ ___   _____      _____  _ __| | __
 * | '_ ` _ \ / _` | | '_ \  |  _| '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
 * | | | | | | (_| | | | | | | | | | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
 * |_| |_| |_|\__,_|_|_| |_| |_| |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
 *
 ***********************************************************************************/

static void mips_emit_nothing(const ir_node *node)
{
	(void) node;
}

static void mips_emit_this_shouldnt_happen(const ir_node *node)
{
	panic("Found non-lowered node %+F while emitting", node);
}

/**
 * The type of a emitter function.
 */
typedef void (*emit_func) (const ir_node *);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static void register_emitter(ir_op *op, emit_func func)
{
	op->ops.generic = (op_func) func;
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
	register_emitter(op_be_IncSP, mips_emit_IncSP);
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
	register_emitter(op_Phi, mips_emit_nothing);
}

/**
 * Emits assembly for a single node
 */
static void mips_emit_node(const ir_node *node)
{
	ir_op *op = get_irn_op(node);

	if (op->ops.generic) {
		emit_func emit = (emit_func) op->ops.generic;
		(*emit) (node);
	} else {
		panic("No emitter defined for node %+F", node);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
void mips_gen_block(const ir_node *block)
{
	ir_node *node;

	if (! is_Block(block))
		return;

	mips_emit_block_label(block);
	be_emit_cstring(":\n");
	be_emit_write_line();

	sched_foreach(block, node) {
		mips_emit_node(node);
	}

	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Emits code for function start.
 */
void mips_emit_func_prolog(ir_graph *irg)
{
	ident *irg_ident = get_entity_ld_ident(get_irg_entity(irg));

	// dump jump tables
	//irg_walk_graph(irg, NULL, dump_jump_tables, env);

	be_emit_write_line();
	be_gas_emit_switch_section(GAS_SECTION_TEXT);

	be_emit_cstring("\t.balign\t4\n");

	be_emit_cstring("\t.global\t");
	be_emit_ident(irg_ident);
	be_emit_char('\n');

	be_emit_cstring("\t.set\tnomips16\n");

	be_emit_cstring("\t.ent\t");
	be_emit_ident(irg_ident);
	be_emit_char('\n');

	be_emit_ident(irg_ident);
	be_emit_cstring(":\n");

	be_emit_cstring("\t.frame\t$fp, 24, $ra\n");
	be_emit_cstring("\t.mask\t0xc0000000, -4\n");
	be_emit_cstring("\t.fmask\t0x00000000, 0\n");

	be_emit_write_line();
}

/**
 * Emits code for function end
 */
void mips_emit_func_epilog(ir_graph *irg)
{
	ident *irg_ident = get_entity_ident(get_irg_entity(irg));

	be_emit_cstring("\t.end\t");
	be_emit_ident(irg_ident);
	be_emit_char('\n');
	be_emit_write_line();
}

/**
 * Sets labels for control flow nodes (jump target)
 */
void mips_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Main driver
 */
void mips_gen_routine(mips_code_gen_t *mips_cg, ir_graph *irg)
{
	int i, n;

	cg       = mips_cg;
	isa      = (const mips_isa_t*) cg->arch_env->isa;
	arch_env = cg->arch_env;

	mips_register_emitters();

	irg_block_walk_graph(irg, mips_gen_labels, NULL, NULL);

	mips_emit_func_prolog(irg);

	dump_ir_block_graph_sched(irg, "-kaputtelist");

	for (i = 0, n = mips_get_sched_n_blocks(cg); i < n; ++i) {
		ir_node *block = mips_get_sched_block(cg, i);
		mips_gen_block(block);
	}

	mips_emit_func_epilog(irg);
}

void mips_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.mips.emitter");
}
