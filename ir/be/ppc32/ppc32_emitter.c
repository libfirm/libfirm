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
 * @brief   ppc emitter
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
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
#include "irnode_t.h"
#include "irargs_t.h"

#include "../besched_t.h"
#include "../benode_t.h"

#include "ppc32_emitter.h"
#include "gen_ppc32_emitter.h"
#include "gen_ppc32_regalloc_if.h"
#include "ppc32_nodes_attr.h"
#include "ppc32_new_nodes.h"
#include "ppc32_map_regs.h"

#define SNPRINTF_BUF_LEN 128

static const arch_env_t *arch_env = NULL;
static char printbuf[SNPRINTF_BUF_LEN];
static char printbuf2[SNPRINTF_BUF_LEN];

extern int isleaf;


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

const char *ppc32_rlwimi_emit_helper(const ir_node *n, ppc32_emit_env_t *env) {
	rlwimi_const_t *rlwimi_const = get_ppc32_rlwimi_const(n);
	snprintf(printbuf, SNPRINTF_BUF_LEN, "%i, %i, %i", rlwimi_const->shift,
		rlwimi_const->maskA, rlwimi_const->maskB);
	return printbuf;
}


/**
 * Return a const or symconst as string.
 */
static const char *node_const_to_str(ir_node *n) {
	const char *buf;
	switch(get_ppc32_type(n))
	{
		case ppc32_ac_Const:
			tarval_snprintf(printbuf, SNPRINTF_BUF_LEN, get_ppc32_constant_tarval(n));
			buf=printbuf;
			break;
		case ppc32_ac_SymConst:
			buf=get_id_str(get_ppc32_symconst_ident(n));
			break;
		case ppc32_ac_Offset:
			snprintf(printbuf, SNPRINTF_BUF_LEN, "%i", get_ppc32_offset(n));
			return printbuf;
		default:
			assert(0 && "node_const_to_str(): Illegal offset type");
			return 0;
	}
	switch(get_ppc32_offset_mode(n))
	{
		case ppc32_ao_None:
			return buf;
		case ppc32_ao_Lo16:
			snprintf(printbuf2, SNPRINTF_BUF_LEN, "lo16(%s)", buf);
			return printbuf2;
		case ppc32_ao_Hi16:
			snprintf(printbuf2, SNPRINTF_BUF_LEN, "hi16(%s)", buf);
			return printbuf2;
		case ppc32_ao_Ha16:
			snprintf(printbuf2, SNPRINTF_BUF_LEN, "ha16(%s)", buf);
			return printbuf2;
		default:
			assert(0 && "node_const_to_str(): Illegal offset mode");
			return 0;
	}
}

/**
 * Returns node's offset as string.
 */
static const char *node_offset_to_str(ir_node *n) {
	const char *buf;
	if(get_ppc32_type(n)==ppc32_ac_None) return "0";
	switch(get_ppc32_type(n))
	{
		case ppc32_ac_Const:
			tarval_snprintf(printbuf, SNPRINTF_BUF_LEN, get_ppc32_constant_tarval(n));
			buf=printbuf;
			break;
		case ppc32_ac_SymConst:
			buf=get_id_str(get_ppc32_symconst_ident(n));
			break;
		case ppc32_ac_Offset:
			snprintf(printbuf, SNPRINTF_BUF_LEN, "%i", get_ppc32_offset(n));
			return printbuf;
		default:
			assert(0 && "node_offset_to_str(): Illegal offset type");
			return 0;
	}
	switch(get_ppc32_offset_mode(n))
	{
		case ppc32_ao_None:
			return buf;
		case ppc32_ao_Lo16:
			snprintf(printbuf2, SNPRINTF_BUF_LEN, "lo16(%s)", buf);
			return printbuf2;
		case ppc32_ao_Hi16:
			snprintf(printbuf2, SNPRINTF_BUF_LEN, "hi16(%s)", buf);
			return printbuf2;
		case ppc32_ao_Ha16:
			snprintf(printbuf2, SNPRINTF_BUF_LEN, "ha16(%s)", buf);
			return printbuf2;
		default:
			assert(0 && "node_offset_to_str(): Illegal offset mode");
			return 0;
	}
}

/* We always pass the ir_node which is a pointer. */
static int ppc32_get_arg_type(const lc_arg_occ_t *occ) {
	return lc_arg_type_ptr;
}


/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(ir_node *irn, int pos) {
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
static const arch_register_t *get_out_reg(ir_node *irn, int pos) {
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	assert(get_irn_n_edges(irn) > pos && "Invalid OUT position");

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		reg = arch_get_irn_register(arch_env, irn);
	}
	else if (is_ppc32_irn(irn)) {
		reg = get_ppc32_out_reg(irn, pos);
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
int get_ppc32_reg_nr(ir_node *irn, int pos, int in_out) {
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
const char *get_ppc32_reg_name(ir_node *irn, int pos, int in_out) {
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
static int ppc32_get_reg_name(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	const char *buf;
	ir_node    *X  = arg->v_ptr;
	int         nr = occ->width - 1;

	if (!X)
		return lc_arg_append(app, occ, "(null)", 6);

	if (occ->conversion == 'S') {
		buf = get_ppc32_reg_name(X, nr, 1);
	}
	else { /* 'D' */
		buf = get_ppc32_reg_name(X, nr, 0);
	}

//	lc_appendable_chadd(app, '%');
	return lc_arg_append(app, occ, buf, strlen(buf));
}

/**
 * Returns the tarval or offset of an ppc node as a string.
 */
static int ppc32_const_to_str(lc_appendable_t *app,
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
static int ppc32_get_mode_suffix(lc_appendable_t *app,
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
 * Return the ppc printf arg environment.
 * We use the firm environment with some additional handlers.
 */
const lc_arg_env_t *ppc32_get_arg_env(void) {
	static lc_arg_env_t *env = NULL;

	static const lc_arg_handler_t ppc32_reg_handler   = { ppc32_get_arg_type, ppc32_get_reg_name };
	static const lc_arg_handler_t ppc32_const_handler = { ppc32_get_arg_type, ppc32_const_to_str };
	static const lc_arg_handler_t ppc32_mode_handler  = { ppc32_get_arg_type, ppc32_get_mode_suffix };

	if(env == NULL) {
		/* extend the firm printer */
		env = firm_get_arg_env();
			//lc_arg_new_env();

		lc_arg_register(env, "ppc:sreg", 'S', &ppc32_reg_handler);
		lc_arg_register(env, "ppc:dreg", 'D', &ppc32_reg_handler);
		lc_arg_register(env, "ppc:cnst", 'C', &ppc32_const_handler);
		lc_arg_register(env, "ppc:offs", 'O', &ppc32_const_handler);
		lc_arg_register(env, "ppc:mode", 'M', &ppc32_mode_handler);
	}

	return env;
}

/**
 * Returns the target label for a control flow node.
 */
static char *get_cfop_target(const ir_node *irn, char *buf) {
	ir_node *bl = get_irn_link(irn);

	snprintf(buf, SNPRINTF_BUF_LEN, "BLOCK_%ld", get_irn_node_nr(bl));
	return buf;
}

/**
 * Emits code for a unconditional jump.
 */
static void emit_Jmp(const ir_node *irn, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	ir_node *block;

	block = get_nodes_block(irn);
	if(get_irn_link(irn) != get_irn_link(block))
		ir_fprintf(F, "\tb %s\t\t\t/* Branch(%+F) */\n", get_cfop_target(irn, printbuf), get_irn_link(irn));
	else
		ir_fprintf(F, "\t\t\t\t\t\t/* fallthrough(%+F) */\n", get_irn_link(irn));
}

/**
 * Emits code for a call
 */
static void emit_be_Call(const ir_node *irn, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	ir_entity *call_ent = be_Call_get_entity(irn);

	if(call_ent)
	{
		ir_fprintf(F, "\tbl      %s\t\t\t/* Branch and link(%+F) */\n", get_entity_name(call_ent), irn);
	}
	else
	{
		ir_node *node = get_irn_n(irn, be_pos_Call_ptr);
		lc_efprintf(ppc32_get_arg_env(), F, "\tmtlr %1D\t\t\t/* Move to link register */\n", node);
		ir_fprintf(F, "\tblrl\t\t\t/* Branch to link register and link(%+F) */\n", irn);
	}
}

char *branchops[8] = { 0, "beq", "blt", "ble", "bgt", "bge", "bne", "b" };

static void emit_ppc32_Branch(const ir_node *n, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	int projnum = get_ppc32_proj_nr(n);

	const ir_edge_t *edge = get_irn_out_edge_first(n);
	ir_node *proj = get_edge_src_irn(edge);

	int opind;

	if(get_Proj_proj(proj) == pn_Cond_true)
		opind = projnum;
	else
		opind = 7-projnum;

	assert(opind>=0 && opind<8);

	if(opind)
	{
		get_cfop_target(proj, printbuf);
		lc_efprintf(ppc32_get_arg_env(), F, "\t%-8s%1S, %s\t\t\t/* Branch(%1S) to %s */\n",
			branchops[opind], n, printbuf, n, printbuf);
	}

	edge = get_irn_out_edge_next(n, edge);

	if(edge)
	{
		ir_node *irn = get_edge_src_irn(edge);
		lc_efprintf(ppc32_get_arg_env(), F, "\tb       %s\t\t\t/* Branch(%+F) */\n",
			get_cfop_target(irn, printbuf), get_irn_link(irn));
	}
}

static void emit_ppc32_LoopCopy(const ir_node *n, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	fprintf(F, "LOOP_%ld:\n", get_irn_node_nr(n));
	lc_efprintf(ppc32_get_arg_env(), F, "\tlwzu    %5D, 4(%2S)\t\t\t/* Load with update */\n",n,n);
	lc_efprintf(ppc32_get_arg_env(), F, "\tstwu    %5D, 4(%3S)\t\t\t/* Store with update */\n",n,n);
	lc_efprintf(ppc32_get_arg_env(), F, "\tbdnz    LOOP_%i\t\t\t/* Branch with decrement if CTR != 0 */\n",
		get_irn_node_nr(n));
}

static void emit_ppc32_Switch(const ir_node *n, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	ir_node *proj,*defproj=NULL;
	int pn;

	const ir_edge_t* edge;
	foreach_out_edge(n, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at Switch");
		if(get_irn_mode(proj) != mode_X) continue;

		pn = get_Proj_proj(proj);
		/* check for default proj */
		if (pn == get_ppc32_proj_nr(n)) {
			assert(defproj == NULL && "found two defProjs at Switch");
			defproj = proj;
		}
		else
		{

			lc_efprintf(ppc32_get_arg_env(), F, "\taddis   %2S, 0, hi16(%i)\t\t\t/* Load upper immediate */\n",n,pn);
			lc_efprintf(ppc32_get_arg_env(), F, "\tori     %2S, %2S, lo16(%i)\t\t\t/* Load lower immediate */\n",n,n,pn);
			lc_efprintf(ppc32_get_arg_env(), F, "\tcmp     %3S, %1S, %2S\t\t\t/* Compare */\n",n,n,n);
			lc_efprintf(ppc32_get_arg_env(), F, "\tbeq     %3S, %s\t\t\t/* Branch if equal */\n",
				n,get_cfop_target(proj, printbuf));
		}
	}
	assert(defproj != NULL && "didn't find defProj at Switch");
	lc_efprintf(ppc32_get_arg_env(), F, "\tb       %s\t\t\t/* Default case */\n", get_cfop_target(defproj, printbuf));
}

/**
 * Emits code for a backend Copy node
 */
static void emit_be_Copy(const ir_node *n, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	const arch_register_class_t *regclass = arch_get_irn_reg_class(env->arch_env, n, 0);

	if (regclass == &ppc32_reg_classes[CLASS_ppc32_gp])
	{
		lc_efprintf(ppc32_get_arg_env(), F, "\tmr      %1D, %1S\t\t\t/* Move register */\n",n,n);
	}
	else if (regclass == &ppc32_reg_classes[CLASS_ppc32_fp])
	{
		lc_efprintf(ppc32_get_arg_env(), F, "\tfmr     %1D, %1S\t\t\t/* Move register */\n",n,n);
	}
	else if (regclass == &ppc32_reg_classes[CLASS_ppc32_condition])
	{
		lc_efprintf(ppc32_get_arg_env(), F, "\tmcrf    %1D, %1S\t\t\t/* Move register */\n",n,n);
	}
	else assert(0 && "Illegal register class for Copy");
}

/**
 * Emits code for a backend Perm node
 */
static void emit_be_Perm(const ir_node *n, ppc32_emit_env_t *env) {
	FILE *F = env->out;
	const arch_register_class_t *regclass = arch_get_irn_reg_class(env->arch_env, n, 0);

	if (regclass == &ppc32_reg_classes[CLASS_ppc32_gp])
	{
		lc_efprintf(ppc32_get_arg_env(), F, "\txor     %1S, %1S, %2S\t\t\t/* Swap %1S, %2S with XOR */\n",n,n,n,n,n);
		lc_efprintf(ppc32_get_arg_env(), F, "\txor     %2S, %1S, %2S\t\t\t/* (continued) */\n",n,n,n);
		lc_efprintf(ppc32_get_arg_env(), F, "\txor     %1S, %1S, %2S\t\t\t/* (continued) */\n",n,n,n);
	}
	else if (regclass == &ppc32_reg_classes[CLASS_ppc32_fp])
	{
		lc_efprintf(ppc32_get_arg_env(), F, "\tfmr     f0, %1S\t\t\t/* Swap %1S, %2S with moves */\n",n,n,n);
		lc_efprintf(ppc32_get_arg_env(), F, "\tfmr     %1S, %2S\t\t\t/* (continued) */\n",n,n);
		lc_efprintf(ppc32_get_arg_env(), F, "\tfmr     %2S, f0\t\t\t/* (continued) */\n",n);
	}
	else if (regclass == &ppc32_reg_classes[CLASS_ppc32_condition])
	{
		lc_efprintf(ppc32_get_arg_env(), F, "\tmcrf    cr7, %1S\t\t\t/* Swap %1S, %2S with moves */\n",n,n,n);
		lc_efprintf(ppc32_get_arg_env(), F, "\tmcrf    %1S, %2S\t\t\t/* (continued) */\n",n,n);
		lc_efprintf(ppc32_get_arg_env(), F, "\tmcrf    %2S, cr7\t\t\t/* (continued) */\n",n);
	}
	else assert(0 && "Illegal register class for Perm");

}


/**
 * Emits code for a proj -> node
 */
static void emit_Proj(const ir_node *irn, ppc32_emit_env_t *env) {
	ir_node *pred = get_Proj_pred(irn);

	if (get_irn_op(pred) == op_Start) {
		switch(get_Proj_proj(irn)) {
			case pn_Start_X_initial_exec:
				emit_Jmp(irn, env);
				break;
			default:
				break;
		}
	}
}

static void emit_be_IncSP(const ir_node *irn, ppc32_emit_env_t *emit_env) {
	FILE          *F    = emit_env->out;
	int offs = be_get_IncSP_offset(irn);

	fprintf(F, "\t\t\t\t\t/* ignored IncSP with %d */\n", -offs);

//	if (offs) {
//		assert(offs<=0x7fff);
//		lc_efprintf(ppc32_get_arg_env(), F, "\taddi    %1S, %1S, %d\t\t\t/* %+F (IncSP) */\n", irn, irn,
//			-offs, irn);
//	}
//	else {
//		fprintf(F, "\t\t\t\t\t/* omitted IncSP with 0 */\n");
//	}
}

/*static void emit_Spill(const ir_node *irn, ppc32_emit_env_t *emit_env) {
	ir_node *context = be_get_Spill_context(irn);
	ir_entity *entity = be_get_spill_entity(irn);
}*/

/***********************************************************************************
 *                  _          __                                             _
 *                 (_)        / _|                                           | |
 *  _ __ ___   __ _ _ _ __   | |_ _ __ __ _ _ __ ___   _____      _____  _ __| | __
 * | '_ ` _ \ / _` | | '_ \  |  _| '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
 * | | | | | | (_| | | | | | | | | | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
 * |_| |_| |_|\__,_|_|_| |_| |_| |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
 *
 ***********************************************************************************/

static void ppc32_register_emitters(void) {
	/* first clear generic function pointers */
	clear_irp_opcodes_generic_func();

	/* register generated emitter functions */
	ppc32_register_spec_emitters();

#define EMIT(a) op_##a->ops.generic = (op_func)emit_##a

	EMIT(ppc32_Branch);
	EMIT(ppc32_LoopCopy);
	EMIT(ppc32_Switch);
	EMIT(be_Call);
	EMIT(Jmp);
	EMIT(Proj);
	EMIT(be_IncSP);
	EMIT(be_Copy);
	EMIT(be_Perm);
//	EMIT(Spill);
//	EMIT(Reload);
}

/**
 * Emits code for a node.
 */
static void ppc32_emit_node(ir_node *irn, void *env) {
	ppc32_emit_env_t  *emit_env = env;
	FILE              *F        = emit_env->out;
	ir_op             *op       = get_irn_op(irn);
	DEBUG_ONLY(firm_dbg_module_t *mod = emit_env->mod;)

	DBG((mod, LEVEL_1, "emitting code for %+F\n", irn));

	if (op->ops.generic) {
	    void (*emit)(ir_node *, void *) = (void (*)(ir_node *, void *))op->ops.generic;
		(*emit)(irn, env);
	}
	else {
		ir_fprintf(F, "\t\t\t\t\t/* %+F */\n", irn);
	}
}


/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ppc32_gen_block(ir_node *block, void *env) {
	ir_node *irn;

	if (! is_Block(block))
		return;

	fprintf(((ppc32_emit_env_t *)env)->out, "BLOCK_%ld:\n", get_irn_node_nr(block));
	sched_foreach(block, irn) {
		ppc32_emit_node(irn, env);
	}
}


/**
 * Emits code for function start.
 */
void ppc32_emit_start(FILE *F, ir_graph *irg, ppc32_emit_env_t *env) {
	const char *irg_name  = get_entity_ld_name(get_irg_entity(irg));
	int         framesize = get_type_size_bytes(get_irg_frame_type(env->cg->irg));

	if(! strcmp(irg_name, "main"))						   // XXX: underscore hack
	{
		fprintf(F, "\t.text\n");
		fprintf(F, "\t.globl _main\n");
		fprintf(F, "\t.align 4\n");
		fprintf(F, "_main:\n");
	}
	else
	{
		fprintf(F, "\t.text\n");
		fprintf(F, "\t.globl %s\n", irg_name);
		fprintf(F, "\t.align 4\n");
		fprintf(F, "%s:\n", irg_name);
	}

	if(framesize > 24)
	{
		fprintf(F, "\tmflr    r0\n");
		fprintf(F, "\tstw     r0, 8(r1)\n");
		fprintf(F, "\tstwu    r1, -%i(r1)\n", framesize);
	}
	else
	{
		fprintf(F, "\t\t\t\t\t/* set new frame (%d) omitted */\n", framesize);
	}


/*	if(!isleaf)
	{
		// store link register in linkage area (TODO: if needed)

		fprintf(F, "\tmflr    r0\n");
		fprintf(F, "\tstwu    r0, -4(r1)\n");   // stw r0, 8(SP)
	}*/
}

/**
 * Emits code for function end
 */
void ppc32_emit_end(FILE *F, ir_graph *irg, ppc32_emit_env_t *env) {
	int framesize = get_type_size_bytes(get_irg_frame_type(env->cg->irg));

/*	if(!isleaf)
	{
		// restore link register

		fprintf(F, "\tlwz     r0, 0(r1)\n");
		fprintf(F, "\taddi    r1, r1, 4\n");
		fprintf(F, "\tmtlr    r0\n");
	}*/
	if(framesize > 24)
	{
		fprintf(F, "\tlwz     r1, 0(r1)\n");
		fprintf(F, "\tlwz     r0, 8(r1)\n");
		fprintf(F, "\tmtlr    r0\n");
	}
	fprintf(F, "\tblr\n\n");
}

/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void ppc32_gen_labels(ir_node *block, void *env) {
	ir_node *pred;
	int n;

	for (n = get_Block_n_cfgpreds(block) - 1; n >= 0; --n) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Main driver: generates code for one routine
 */
void ppc32_gen_routine(FILE *F, ir_graph *irg, const ppc32_code_gen_t *cg) {
	ppc32_emit_env_t emit_env;
	ir_node *block;
	int i, n;

	emit_env.out      = F;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.ppc.emit");

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	ppc32_register_emitters();

	ppc32_emit_start(F, irg, &emit_env);
	irg_block_walk_graph(irg, ppc32_gen_labels, NULL, &emit_env);

	n = ARR_LEN(cg->blk_sched);
	for (i = 0; i < n;) {
		ir_node *next_bl;

		block   = cg->blk_sched[i];
		++i;
		next_bl = i < n ? cg->blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		ppc32_gen_block(block, &emit_env);
	}
	ppc32_emit_end(F, irg, &emit_env);
}
