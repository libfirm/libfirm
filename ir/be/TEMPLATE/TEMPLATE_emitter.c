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
 * @brief   emit assembler for a backend graph
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
#include "irargs_t.h"
#include "irprog.h"

#include "../besched.h"

#include "TEMPLATE_emitter.h"
#include "gen_TEMPLATE_emitter.h"
#include "TEMPLATE_nodes_attr.h"
#include "TEMPLATE_new_nodes.h"
#include "TEMPLATE_map_regs.h"

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
	} else if (is_TEMPLATE_irn(node)) {
		reg = get_TEMPLATE_out_reg(node, pos);
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

void TEMPLATE_emit_immediate(TEMPLATE_emit_env_t *env, const ir_node *node)
{
	/* TODO */
}

void TEMPLATE_emit_source_register(TEMPLATE_emit_env_t *env, const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(env->arch_env, node, pos);
	be_emit_string(env->emit, arch_register_get_name(reg));
}

void TEMPLATE_emit_dest_register(TEMPLATE_emit_env_t *env, const ir_node *node, int pos)
{
	const arch_register_t *reg = get_out_reg(env->arch_env, node, pos);
	be_emit_string(env->emit, arch_register_get_name(reg));
}

/**
 * Returns the target label for a control flow node.
 */
static void TEMPLATE_emit_cfop_target(TEMPLATE_emit_env_t *env, const ir_node *node) {
	ir_node *block = get_irn_link(node);

	be_emit_irprintf(env->emit, "BLOCK_%ld", get_irn_node_nr(block));
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

/**
 * Emits code for a unconditional jump.
 */
static void emit_Jmp(TEMPLATE_emit_env_t *env, const ir_node *node) {
	ir_node *block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	be_emit_cstring(env->emit, "\tjmp ");
	TEMPLATE_emit_cfop_target(env, node);
	be_emit_finish_line_gas(env->emit, node);
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void TEMPLATE_register_emitters(void) {

/* some convienience macros to register additional emitter functions
   (other than the generated ones) */
#define TEMPLATE_EMIT(a) op_TEMPLATE_##a->ops.generic = (op_func)emit_TEMPLATE_##a
#define EMIT(a)          op_##a->ops.generic = (op_func)emit_##a
#define BE_EMIT(a)       op_be_##a->ops.generic = (op_func)emit_be_##a

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	TEMPLATE_register_spec_emitters();

	/* register addtional emitter functions if needed */
	EMIT(Jmp);

#undef TEMPLATE_EMIT
#undef BE_EMIT
#undef EMIT
}

typedef void (*emit_func_ptr) (TEMPLATE_emit_env_t *, const ir_node *);

/**
 * Emits code for a node.
 */
void TEMPLATE_emit_node(TEMPLATE_emit_env_t *env, const ir_node *node) {
	ir_op               *op       = get_irn_op(node);
	DEBUG_ONLY(firm_dbg_module_t *mod = env->mod;)

	DBG((mod, LEVEL_1, "emitting code for %+F\n", node));

	if (op->ops.generic) {
		emit_func_ptr func = (emit_func_ptr) op->ops.generic;
		(*func) (env, node);
	} else {
		ir_fprintf(stderr, "No emitter for node %+F\n", node);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
void TEMPLATE_gen_block(ir_node *block, void *data) {
	TEMPLATE_emit_env_t *env = data;
	ir_node *node;

	if (! is_Block(block))
		return;

	be_emit_cstring(env->emit, "BLOCK_");
	be_emit_irprintf(env->emit, "%ld:\n", get_irn_node_nr(block));
	be_emit_write_line(env->emit);

	sched_foreach(block, node) {
		TEMPLATE_emit_node(env, node);
	}
}


/**
 * Emits code for function start.
 */
void TEMPLATE_emit_func_prolog(TEMPLATE_emit_env_t *env, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	/* TODO: emit function header */
	be_emit_cstring(env->emit, "/* start of ");
	be_emit_string(env->emit, irg_name);
	be_emit_cstring(env->emit, " */\n");
	be_emit_write_line(env->emit);
}

/**
 * Emits code for function end
 */
void TEMPLATE_emit_func_epilog(TEMPLATE_emit_env_t *env, ir_graph *irg) {
	const char *irg_name = get_entity_name(get_irg_entity(irg));

	/* TODO: emit function end */
	be_emit_cstring(env->emit, "/* end of ");
	be_emit_string(env->emit, irg_name);
	be_emit_cstring(env->emit, " */\n");
	be_emit_write_line(env->emit);
}

/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void TEMPLATE_gen_labels(ir_node *block, void *env) {
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
void TEMPLATE_gen_routine(const TEMPLATE_code_gen_t *cg, ir_graph *irg) {
	TEMPLATE_emit_env_t env;

	env.isa      = (TEMPLATE_isa_t *) cg->arch_env->isa;
	env.emit     = &env.isa->emit;
	env.arch_env = cg->arch_env;
	env.cg       = cg;
	FIRM_DBG_REGISTER(env.mod, "firm.be.TEMPLATE.emit");

	/* register all emitter functions */
	TEMPLATE_register_emitters();

	TEMPLATE_emit_func_prolog(&env, irg);
	irg_block_walk_graph(irg, TEMPLATE_gen_labels, NULL, &env);
	irg_walk_blkwise_graph(irg, NULL, TEMPLATE_gen_block, &env);
	TEMPLATE_emit_func_epilog(&env, irg);
}
