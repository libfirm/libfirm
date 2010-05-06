/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   emit assembler for a backend graph
 * @version $Id: amd64_emitter.c 26746 2009-11-27 08:53:15Z matze $
 */
#include "config.h"

#include <limits.h>

#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog.h"

#include "../besched.h"
#include "../begnuas.h"
#include "../beblocksched.h"
#include "../be_dbgout.h"

#include "amd64_emitter.h"
#include "gen_amd64_emitter.h"
#include "amd64_nodes_attr.h"
#include "amd64_new_nodes.h"

#define SNPRINTF_BUF_LEN 128

#include "../benode.h"

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

	reg = arch_get_irn_register(op);

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
		reg = arch_get_irn_register(node);
	} else if (is_amd64_irn(node)) {
		reg = arch_irn_get_register(node, pos);
	} else {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "non-Proj from mode_T node");
			if (get_Proj_proj(proj) == pos) {
				reg = arch_get_irn_register(proj);
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

void amd64_emit_immediate(const ir_node *node)
{
	const amd64_immediate_attr_t *attr = get_amd64_immediate_attr_const (node);
	be_emit_char('$');
	be_emit_irprintf("0x%X", attr->imm_value);
}

void amd64_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

void amd64_emit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_out_reg(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Returns the target label for a control flow node.
 */
/*
static void amd64_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_irn_link(node);

	be_emit_irprintf("BLOCK_%ld", get_irn_node_nr(block));
}
*/

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
 * Default emitter for anything that we don't want to generate code for.
 */
static void emit_nothing(const ir_node *node)
{
	(void) node;
}

/**
 * Emit a SymConst.
 */
static void emit_amd64_SymConst(const ir_node *irn)
{
	const amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr_const(irn);
//	sym_or_tv_t key, *entry;
//	unsigned label;
//
//	key.u.id     = get_entity_ld_ident(attr->entity);
//	key.is_ident = 1;
//	key.label    = 0;
//	entry = (sym_or_tv_t *)set_insert(sym_or_tv, &key, sizeof(key), HASH_PTR(key.u.generic));
//	if (entry->label == 0) {
//		/* allocate a label */
//		entry->label = get_unique_label();
//	}
//	label = entry->label;


	be_gas_emit_entity(attr->entity);
	be_emit_char(':');
	be_emit_finish_line_gas(irn);
	be_emit_cstring("\t.long 0x0");
	be_emit_finish_line_gas(irn);
}


/**
 * Emits code for a return.
 */
static void emit_be_Return(const ir_node *node)
{
	be_emit_cstring("\tret");
	be_emit_finish_line_gas(node);
}

/**
 * The type of a emitter function.
 */
typedef void (emit_func)(const ir_node *irn);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static inline void set_emitter(ir_op *op, emit_func arm_emit_node)
{
	op->ops.generic = (op_func)arm_emit_node;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void amd64_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	amd64_register_spec_emitters();

	set_emitter(op_be_Return, emit_be_Return);
	set_emitter(op_be_Start,    emit_nothing);
	set_emitter(op_be_Barrier,  emit_nothing);
	set_emitter(op_be_IncSP,    emit_nothing);
}

typedef void (*emit_func_ptr) (const ir_node *);

/**
 * Emits code for a node.
 */
static void amd64_emit_node(const ir_node *node)
{
	ir_op               *op       = get_irn_op(node);

	if (op->ops.generic) {
		emit_func_ptr func = (emit_func_ptr) op->ops.generic;
		(*func) (node);
	} else {
		ir_fprintf(stderr, "No emitter for node %+F\n", node);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void amd64_gen_block(ir_node *block, void *data)
{
	ir_node *node;
	(void) data;

	if (! is_Block(block))
		return;

	be_gas_emit_block_name(block);
	be_emit_char(':');

	be_emit_write_line();

	sched_foreach(block, node) {
		amd64_emit_node(node);
	}
}


/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
static void amd64_gen_labels(ir_node *block, void *env)
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
void amd64_gen_routine(const amd64_code_gen_t *cg, ir_graph *irg)
{
	(void)cg;
	ir_entity *entity = get_irg_entity(irg);
	ir_node  **blk_sched;
	int i, n;

	/* register all emitter functions */
	amd64_register_emitters();

	blk_sched = be_create_block_schedule(irg);

	be_dbg_method_begin(entity, be_abi_get_stack_layout(cg->birg->abi));
	be_gas_emit_function_prolog(entity, 4);

	irg_block_walk_graph(irg, amd64_gen_labels, NULL, NULL);

	n = ARR_LEN(blk_sched);
	for (i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];
		ir_node *prev  = i > 0 ? blk_sched[i-1] : NULL;

		set_irn_link(block, prev);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];

		amd64_gen_block(block, 0);
	}

	be_gas_emit_function_epilog(entity);
	be_dbg_method_end();
	be_emit_char('\n');
	be_emit_write_line();
}
