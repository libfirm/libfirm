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
 * @version $Id$
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
#include "../benode.h"

#include "TEMPLATE_emitter.h"
#include "gen_TEMPLATE_emitter.h"
#include "TEMPLATE_nodes_attr.h"
#include "TEMPLATE_new_nodes.h"

#define SNPRINTF_BUF_LEN 128

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
	} else if (is_TEMPLATE_irn(node)) {
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



void TEMPLATE_emit_immediate(const ir_node *node)
{
	const TEMPLATE_attr_t *attr = get_TEMPLATE_attr_const(node);
	be_emit_tarval(attr->value);
}

void TEMPLATE_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(node, pos);
	be_emit_string(arch_register_get_name(reg));
}

void TEMPLATE_emit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_out_reg(node, pos);
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Returns the target label for a control flow node.
 */
static void TEMPLATE_emit_cfop_target(const ir_node *node)
{
	ir_node *block = (ir_node*)get_irn_link(node);
	be_gas_emit_block_name(block);
}



/**
 * Emits code for a unconditional jump.
 */
static void emit_TEMPLATE_Jmp(const ir_node *node)
{
	be_emit_cstring("\tjmp ");
	TEMPLATE_emit_cfop_target(node);
	be_emit_finish_line_gas(node);
}

static void emit_be_IncSP(const ir_node *node)
{
	int offset = be_get_IncSP_offset(node);

	if (offset == 0)
		return;

	/* downwards growing stack */
	if (offset > 0) {
		be_emit_cstring("\tsub ");
	} else {
		be_emit_cstring("\tadd ");
		offset = -offset;
	}

	TEMPLATE_emit_source_register(node, 0);
	be_emit_irprintf(", %d, ", offset);
	TEMPLATE_emit_dest_register(node, 0);
	be_emit_finish_line_gas(node);
}

static void emit_be_Return(const ir_node *node)
{
	be_emit_cstring("\tret");
	be_emit_finish_line_gas(node);
}

static void emit_nothing(const ir_node *node)
{
	(void) node;
}

/**
 * The type of a emitter function.
 */
typedef void (emit_func)(const ir_node *node);

static inline void set_emitter(ir_op *op, emit_func func)
{
	op->ops.generic = (op_func)func;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void TEMPLATE_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	TEMPLATE_register_spec_emitters();

	/* custom emitters not provided by the spec */
	set_emitter(op_TEMPLATE_Jmp,   emit_TEMPLATE_Jmp);
	set_emitter(op_be_Return,      emit_be_Return);
	set_emitter(op_be_IncSP,       emit_be_IncSP);

	/* no need to emit anything for the following nodes */
	set_emitter(op_Phi,            emit_nothing);
	set_emitter(op_be_Keep,        emit_nothing);
	set_emitter(op_be_Start,       emit_nothing);
}

typedef void (*emit_func_ptr) (const ir_node *);

/**
 * Emits code for a node.
 */
static void TEMPLATE_emit_node(const ir_node *node)
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
static void TEMPLATE_emit_block(ir_node *block)
{
	ir_node *node;

	be_gas_emit_block_name(block);
	be_emit_cstring(":\n");
	be_emit_write_line();

	sched_foreach(block, node) {
		TEMPLATE_emit_node(node);
	}
}

/**
 * Sets labels for control flow nodes (jump target)
 */
static void TEMPLATE_gen_labels(ir_node *block, void *env)
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
void TEMPLATE_emit_routine(ir_graph *irg)
{
	ir_node   **block_schedule;
	ir_entity  *entity = get_irg_entity(irg);
	size_t      i;
	size_t      n;

	/* register all emitter functions */
	TEMPLATE_register_emitters();

	/* create the block schedule */
	block_schedule = be_create_block_schedule(irg);

	/* emit assembler prolog */
	be_gas_emit_function_prolog(entity, 4);

	/* populate jump link fields with their destinations */
	irg_block_walk_graph(irg, TEMPLATE_gen_labels, NULL, NULL);

	n = ARR_LEN(block_schedule);
	for (i = 0; i < n; ++i) {
		ir_node *block = block_schedule[i];
		TEMPLATE_emit_block(block);
	}
	be_gas_emit_function_epilog(entity);
}
