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
#include "error.h"

#include "../besched_t.h"
#include "../benode_t.h"

#include "ppc32_emitter.h"
#include "gen_ppc32_emitter.h"
#include "gen_ppc32_regalloc_if.h"
#include "ppc32_nodes_attr.h"
#include "ppc32_new_nodes.h"
#include "ppc32_map_regs.h"

#define SNPRINTF_BUF_LEN 128

static char printbuf[SNPRINTF_BUF_LEN];

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
/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const arch_env_t *arch_env, const ir_node *irn, int pos) {
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
static const arch_register_t *get_out_reg(const arch_env_t *arch_env, const ir_node *irn, int pos) {
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	assert(get_irn_n_edges(irn) > pos && "Invalid OUT position");

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(irn) != mode_T) {
		reg = arch_get_irn_register(arch_env, irn);
	} else if (is_ppc32_irn(irn)) {
		reg = get_ppc32_out_reg(irn, pos);
	} else {
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
 * Emit the name of the source register at given input position.
 */
void ppc32_emit_source_register(ppc32_emit_env_t *env, const ir_node *node, int pos) {
	const arch_register_t *reg = get_in_reg(env->arch_env, node, pos);
	be_emit_string(env->emit, arch_register_get_name(reg));
}

/**
 * Emit the name of the destination register at given output position.
 */
void ppc32_emit_dest_register(ppc32_emit_env_t *env, const ir_node *node, int pos) {
	const arch_register_t *reg = get_out_reg(env->arch_env, node, pos);
	be_emit_string(env->emit, arch_register_get_name(reg));
}

void ppc32_emit_rlwimi_helper(ppc32_emit_env_t *env, const ir_node *n) {
	rlwimi_const_t *rlwimi_const = get_ppc32_rlwimi_const(n);

	be_emit_irprintf(env->emit, "%i, %i, %i", rlwimi_const->shift,
		rlwimi_const->maskA, rlwimi_const->maskB);
}

/**
 * Emit a const or symconst.
 */
void ppc32_emit_immediate(ppc32_emit_env_t *env, const ir_node *n) {
	const char *buf;

	switch (get_ppc32_type(n)) {
	case ppc32_ac_Const:
		tarval_snprintf(printbuf, SNPRINTF_BUF_LEN, get_ppc32_constant_tarval(n));
		buf = printbuf;
		break;
	case ppc32_ac_SymConst:
		buf = get_id_str(get_ppc32_symconst_ident(n));
		break;
	case ppc32_ac_Offset:
		be_emit_irprintf(env->emit, "%i", get_ppc32_offset(n));
		return;
	default:
		assert(0 && "node_const_to_str(): Illegal offset type");
		return;
	}
	switch (get_ppc32_offset_mode(n)) {
	case ppc32_ao_None:
		be_emit_string(env->emit, buf);
		return;
	case ppc32_ao_Lo16:
		be_emit_irprintf(env->emit, "lo16(%s)", buf);
		return;
	case ppc32_ao_Hi16:
		be_emit_irprintf(env->emit, "hi16(%s)", buf);
		return;
	case ppc32_ao_Ha16:
		be_emit_irprintf(env->emit, "ha16(%s)", buf);
		return;
	default:
		assert(0 && "node_const_to_str(): Illegal offset mode");
		return;
	}
}

/**
 * Emits a node's offset.
 */
void ppc32_emit_offset(ppc32_emit_env_t *env, const ir_node *n) {
	const char *buf;
	if (get_ppc32_type(n) == ppc32_ac_None) {
		be_emit_char(env->emit, '0');
		return;
	}

	switch (get_ppc32_type(n)) {
	case ppc32_ac_Const:
		tarval_snprintf(printbuf, SNPRINTF_BUF_LEN, get_ppc32_constant_tarval(n));
		buf = printbuf;
		break;
	case ppc32_ac_SymConst:
		buf = get_id_str(get_ppc32_symconst_ident(n));
		break;
	case ppc32_ac_Offset:
		be_emit_irprintf(env->emit, "%i", get_ppc32_offset(n));
		return;
	default:
		assert(0 && "node_offset_to_str(): Illegal offset type");
		return;
	}
	switch (get_ppc32_offset_mode(n)) {
	case ppc32_ao_None:
		be_emit_string(env->emit, buf);
		return;
	case ppc32_ao_Lo16:
		be_emit_irprintf(env->emit, "lo16(%s)", buf);
		return;
	case ppc32_ao_Hi16:
		be_emit_irprintf(env->emit, "hi16(%s)", buf);
		return;
	case ppc32_ao_Ha16:
		be_emit_irprintf(env->emit, "ha16(%s)", buf);
		return;
	default:
		assert(0 && "node_offset_to_str(): Illegal offset mode");
		return;
	}
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
static void emit_Jmp(ppc32_emit_env_t *env, const ir_node *irn) {
	ir_node *block = get_nodes_block(irn);

	if (get_irn_link(irn) != get_irn_link(block)) {
		be_emit_irprintf(env->emit, "\tb %s", get_cfop_target(irn, printbuf));
	} else {
		be_emit_irprintf(env->emit, "/* fallthrough(%+F) */", get_irn_link(irn));
	}
	be_emit_finish_line_gas(env->emit, irn);
}

/**
 * Emits code for a call
 */
static void emit_be_Call(ppc32_emit_env_t *env, const ir_node *irn) {
	ir_entity *call_ent = be_Call_get_entity(irn);

	if (call_ent) {
		set_entity_backend_marked(call_ent, 1);
		be_emit_irprintf(env->emit, "\tbl %s", get_entity_ld_name(call_ent));
	} else {
		be_emit_cstring(env->emit, "\tmtlr ");
		ppc32_emit_source_register(env, irn, be_pos_Call_ptr);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* Move to link register and link */\n");
		be_emit_write_line(env->emit);
		be_emit_cstring(env->emit, "\tblrl");
	}
	be_emit_finish_line_gas(env->emit, irn);
}

static void emit_ppc32_Branch(ppc32_emit_env_t *env, const ir_node *irn) {
	static const char *branchops[8] = { 0, "beq", "blt", "ble", "bgt", "bge", "bne", "b" };
	int projnum = get_ppc32_proj_nr(irn);

	const ir_edge_t *edge = get_irn_out_edge_first(irn);
	ir_node *proj = get_edge_src_irn(edge);

	int opind;

	if (get_Proj_proj(proj) == pn_Cond_true)
		opind = projnum;
	else
		opind = 7 - projnum;

	assert(opind>=0 && opind<8);

	if (opind){
		get_cfop_target(proj, printbuf);
		be_emit_irprintf(env->emit, "\t%8s", branchops[opind]);
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		be_emit_string(env->emit, printbuf);
		be_emit_finish_line_gas(env->emit, irn);
	}

	edge = get_irn_out_edge_next(irn, edge);
	if (edge) {
		ir_node *blk = get_edge_src_irn(edge);
		be_emit_cstring(env->emit, "\tb ");
		be_emit_string(env->emit, get_cfop_target(blk, printbuf));
		be_emit_finish_line_gas(env->emit, irn);
	}
}

static void emit_ppc32_LoopCopy(ppc32_emit_env_t *env, const ir_node *irn) {
	be_emit_irprintf(env->emit, "LOOP_%ld:\n", get_irn_node_nr(irn));
	be_emit_write_line(env->emit);

	be_emit_cstring(env->emit, "\tlwzu ");
	ppc32_emit_dest_register(env, irn, 4);
	be_emit_cstring(env->emit, ", 4(");
	ppc32_emit_source_register(env, irn, 1);
	be_emit_char(env->emit, ')');
	be_emit_pad_comment(env->emit);
	be_emit_cstring(env->emit, "/* Load with update */\n");
	be_emit_write_line(env->emit);

	be_emit_cstring(env->emit, "\tstwu ");
	ppc32_emit_dest_register(env, irn, 4);
	be_emit_cstring(env->emit, ", 4(");
	ppc32_emit_source_register(env, irn, 2);
	be_emit_char(env->emit, ')');
	be_emit_pad_comment(env->emit);
	be_emit_cstring(env->emit, "/* Store with update */\n");
	be_emit_write_line(env->emit);

	be_emit_irprintf(env->emit, "\tbdnz LOOP_%i", get_irn_node_nr(irn));
	be_emit_finish_line_gas(env->emit, irn);
}

static void emit_ppc32_Switch(ppc32_emit_env_t *env, const ir_node *irn) {
	ir_node *proj, *defproj = NULL;
	int pn;

	const ir_edge_t* edge;
	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at Switch");
		if (get_irn_mode(proj) != mode_X) continue;

		pn = get_Proj_proj(proj);
		/* check for default proj */
		if (pn == get_ppc32_proj_nr(irn)) {
			assert(defproj == NULL && "found two defProjs at Switch");
			defproj = proj;
		} else {
			be_emit_cstring(env->emit, "\taddis ");
			ppc32_emit_source_register(env, irn, 1);
			be_emit_irprintf(env->emit, ", 0, hi16(%i)", pn);
			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* Load upper immediate */\n");
			be_emit_write_line(env->emit);

			be_emit_cstring(env->emit, "\tori ");
			ppc32_emit_source_register(env, irn, 1);
			be_emit_cstring(env->emit, ", ");
			ppc32_emit_source_register(env, irn, 1);
			be_emit_irprintf(env->emit, ", lo16(%i)", pn);
			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* Load lower immediate */\n");
			be_emit_write_line(env->emit);

			be_emit_cstring(env->emit, "\tcmp ");
			ppc32_emit_source_register(env, irn, 2);
			be_emit_cstring(env->emit, ", ");
			ppc32_emit_source_register(env, irn, 0);
			be_emit_cstring(env->emit, ", ");
			ppc32_emit_source_register(env, irn, 1);
			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* Compare */\n");
			be_emit_write_line(env->emit);

			be_emit_cstring(env->emit, "\tbeq ");
			ppc32_emit_source_register(env, irn, 2);
			be_emit_irprintf(env->emit, ", %s", get_cfop_target(proj, printbuf));
			be_emit_cstring(env->emit, "/* Branch if equal */\n");
			be_emit_write_line(env->emit);
		}
	}
	assert(defproj != NULL && "didn't find defProj at Switch");
	be_emit_irprintf(env->emit, "\tb %s", get_cfop_target(defproj, printbuf));
	be_emit_finish_line_gas(env->emit, irn);
}

/**
 * Emits code for a backend Copy node
 */
static void emit_be_Copy(ppc32_emit_env_t *env,const ir_node *irn) {
	const arch_register_class_t *regclass = arch_get_irn_reg_class(env->arch_env, irn, 0);

	if (regclass == &ppc32_reg_classes[CLASS_ppc32_gp]) {
		be_emit_cstring(env->emit, "\tmr ");
	} else if (regclass == &ppc32_reg_classes[CLASS_ppc32_fp]) {
		be_emit_cstring(env->emit, "\tfmr ");
	} else if (regclass == &ppc32_reg_classes[CLASS_ppc32_condition]) {
		be_emit_cstring(env->emit, "\tmcrf ");
	} else {
		assert(0 && "Illegal register class for Copy");
		panic("ppc32 Emitter: Illegal register class for Copy");
	}
	ppc32_emit_dest_register(env, irn, 0);
	be_emit_cstring(env->emit, ", ");
	ppc32_emit_source_register(env, irn, 0);
	be_emit_finish_line_gas(env->emit, irn);
}

/**
 * Emits code for a backend Perm node
 */
static void emit_be_Perm(ppc32_emit_env_t *env, const ir_node *irn) {
	const arch_register_class_t *regclass = arch_get_irn_reg_class(env->arch_env, irn, 0);

	if (regclass == &ppc32_reg_classes[CLASS_ppc32_gp]) {
		be_emit_cstring(env->emit, "\txor ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* Swap with XOR */\n");
		be_emit_write_line(env->emit);

		be_emit_cstring(env->emit, "\txor ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* (continued) */\n");
		be_emit_write_line(env->emit);

		be_emit_cstring(env->emit, "\txor ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 1);
	} else if (regclass == &ppc32_reg_classes[CLASS_ppc32_fp]) {
		be_emit_cstring(env->emit, "\tfmr f0, ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* Swap with moves */\n");
		be_emit_write_line(env->emit);

		be_emit_cstring(env->emit, "\tfmr ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* (continued) */\n");
		be_emit_write_line(env->emit);

		be_emit_cstring(env->emit, "\tfmr ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, ", f0");
	} else if (regclass == &ppc32_reg_classes[CLASS_ppc32_condition]) {
		be_emit_cstring(env->emit, "\tmcrf cr7, ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* Swap with moves */\n");
		be_emit_write_line(env->emit);

		be_emit_cstring(env->emit, "\tmcrf ");
		ppc32_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* (continued) */\n");
		be_emit_write_line(env->emit);

		be_emit_cstring(env->emit, "\tmcrf ");
		ppc32_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, ", cr7");
	} else {
		assert(0 && "Illegal register class for Perm");
		panic("ppc32 Emitter: Illegal register class for Perm");
	}
	be_emit_finish_line_gas(env->emit, irn);
}


/**
 * Emits code for a proj -> node
 */
static void emit_Proj(ppc32_emit_env_t *env, const ir_node *irn) {
	ir_node *pred = get_Proj_pred(irn);

	if (get_irn_op(pred) == op_Start) {
		if (get_Proj_proj(irn) == pn_Start_X_initial_exec) {
			emit_Jmp(env, irn);
		}
	}
}

static void emit_be_IncSP(ppc32_emit_env_t *env, const ir_node *irn) {
	int offs = be_get_IncSP_offset(irn);

	be_emit_irprintf(env->emit, "\t/* ignored IncSP with %d */", -offs);
	be_emit_finish_line_gas(env->emit, irn);

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
/**
 * The type of a emitter function.
 */
typedef void (emit_func)(ppc32_emit_env_t *env, const ir_node *irn);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static INLINE void set_emitter(ir_op *op, emit_func ppc32_emit_node) {
	op->ops.generic = (op_func)ppc32_emit_node;
}

static void ppc32_register_emitters(void) {
	/* first clear generic function pointers */
	clear_irp_opcodes_generic_func();

	/* register generated emitter functions */
	ppc32_register_spec_emitters();

	set_emitter(op_ppc32_Branch, emit_ppc32_Branch);
	set_emitter(op_ppc32_LoopCopy, emit_ppc32_LoopCopy);
	set_emitter(op_ppc32_Switch, emit_ppc32_Switch);
	set_emitter(op_be_Call, emit_be_Call);
	set_emitter(op_Jmp, emit_Jmp);
	set_emitter(op_Proj, emit_Proj);
	set_emitter(op_be_IncSP, emit_be_IncSP);
	set_emitter(op_be_Copy, emit_be_Copy);
	set_emitter(op_be_Perm, emit_be_Perm);
//	set_emitter(op_Spill, emit_Spill);
//	set_emitter(op_Reload, emit_Reload);
}

/**
 * Emits code for a node.
 */
static void ppc32_emit_node(ppc32_emit_env_t *env, const ir_node *irn) {
	ir_op *op = get_irn_op(irn);

	if (op->ops.generic) {
		emit_func *emit = (emit_func *)op->ops.generic;
		(*emit)(env, irn);
	} else {
		be_emit_cstring(env->emit, "\t/* TODO */");
		be_emit_finish_line_gas(env->emit, irn);
	}
}


/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void ppc32_gen_block(ppc32_emit_env_t *env, const ir_node *block) {
	ir_node *irn;

	if (! is_Block(block))
		return;

	be_emit_irprintf(env->emit, "BLOCK_%ld:\n", get_irn_node_nr(block));
	be_emit_write_line(env->emit);
	sched_foreach(block, irn) {
		ppc32_emit_node(env, irn);
	}
}


/**
 * Emits code for function start.
 */
static void ppc32_emit_start(ppc32_emit_env_t *env, ir_graph *irg) {
	const char *irg_name  = get_entity_ld_name(get_irg_entity(irg));
	int         framesize = get_type_size_bytes(get_irg_frame_type(env->cg->irg));

	if(! strcmp(irg_name, "main")) {					   // XXX: underscore hack
		irg_name = "_main";
	}

	be_emit_irprintf(env->emit, "\t.text\n\t.globl %s\n\t.align 4\n%s:\n", irg_name, irg_name);

	if (framesize > 24) {
		be_emit_cstring(env->emit, "\tmflr    r0\n");
		be_emit_cstring(env->emit, "\tstw     r0, 8(r1)\n");
		be_emit_irprintf(env->emit, "\tstwu    r1, -%i(r1)\n", framesize);
	} else {
		be_emit_irprintf(env->emit, "\t/* set new frame (%d) omitted */\n", framesize);
	}
	be_emit_write_line(env->emit);

/*	if(!isleaf) {
		// store link register in linkage area (TODO: if needed)

		be_emit_cstring(env->emit, "\tmflr    r0\n");
		be_emit_cstring(env->emit, "\tstwu    r0, -4(r1)\n");   // stw r0, 8(SP)
		be_emit_write_line(env->emit);
	}*/
}

/**
 * Emits code for function end
 */
static void ppc32_emit_end(ppc32_emit_env_t *env, ir_graph *irg) {
	int framesize = get_type_size_bytes(get_irg_frame_type(env->cg->irg));

/*	if(!isleaf) {
		// restore link register

		be_emit_cstring(env->emit, "\tlwz     r0, 0(r1)\n");
		be_emit_cstring(env->emit, "\taddi    r1, r1, 4\n");
		be_emit_cstring(env->emit, "\tmtlr    r0\n");
		be_emit_write_line(env->emit);
	}*/
	if(framesize > 24) {
		be_emit_cstring(env->emit, "\tlwz    r1, 0(r1)\n");
		be_emit_cstring(env->emit, "\tlwz    r0, 8(r1)\n");
		be_emit_cstring(env->emit, "\tmtlr   r0\n");
		be_emit_write_line(env->emit);
	}
	be_emit_cstring(env->emit, "\tblr\n\n");
	be_emit_write_line(env->emit);
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
void ppc32_gen_routine(const ppc32_code_gen_t *cg, ir_graph *irg)
{
	ppc32_emit_env_t emit_env;
	ir_node *block;
	int i, n;

	emit_env.emit     = &cg->isa->emit;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.ppc.emit");

	ppc32_register_emitters();

	ppc32_emit_start(&emit_env, irg);
	irg_block_walk_graph(irg, ppc32_gen_labels, NULL, &emit_env);

	n = ARR_LEN(cg->blk_sched);
	for (i = 0; i < n;) {
		ir_node *next_bl;

		block   = cg->blk_sched[i];
		++i;
		next_bl = i < n ? cg->blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		ppc32_gen_block(&emit_env, block);
	}
	ppc32_emit_end(&emit_env, irg);
}
