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
 * @brief   arm emitter
 * @author  Oliver Richter, Tobias Gneist
 * @version $Id$
 */
#define SILENCER

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irtools.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "irargs_t.h"
#include "error.h"

#include "../besched.h"
#include "../beblocksched.h"
#include "../beirg_t.h"
#include "../begnuas.h"

#include "arm_emitter.h"
#include "gen_arm_emitter.h"
#include "arm_nodes_attr.h"
#include "arm_new_nodes.h"
#include "arm_map_regs.h"
#include "gen_arm_regalloc_if.h"

#include "../benode_t.h"

#define SNPRINTF_BUF_LEN 128

static const arch_env_t *arch_env = NULL;

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
    } else if (is_arm_irn(node)) {
        reg = get_arm_out_reg(node, pos);
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
 * Emits a block label from the given block.
 */
static void arm_emit_block_label(arm_emit_env_t *env, const ir_node *block) {
	be_emit_irprintf(env->emit, "BLOCK_%u", get_irn_node_nr(block));
}

/**
 * Emit the name of the source register at given input position.
 */
void arm_emit_source_register(arm_emit_env_t *env, const ir_node *node, int pos) {
	const arch_register_t *reg = get_in_reg(env->arch_env, node, pos);
	be_emit_string(env->emit, arch_register_get_name(reg));
}

/**
 * Emit the name of the destination register at given output position.
 */
void arm_emit_dest_register(arm_emit_env_t *env, const ir_node *node, int pos) {
	const arch_register_t *reg = get_out_reg(env->arch_env, node, pos);
	be_emit_string(env->emit, arch_register_get_name(reg));
}

/**
 * Emit a node's offset.
 */
void arm_emit_offset(arm_emit_env_t *env, const ir_node *node) {
	int offset = 0;
	ir_op *irn_op = get_irn_op(node);

	if (irn_op == op_be_StackParam) {
		ir_entity *ent = be_get_frame_entity(node);
		offset = get_entity_offset(ent);
	} else if (irn_op == op_be_Reload || irn_op == op_be_Spill) {
		ir_entity *ent = be_get_frame_entity(node);
		offset = get_entity_offset(ent);
	} else if (irn_op == op_be_IncSP) {
		offset = - be_get_IncSP_offset(node);
	} else {
		assert(!"unimplemented arm_emit_offset for this node type");
		panic("unimplemented arm_emit_offset for this node type");
	}
	be_emit_irprintf(env->emit, "%d", offset);
}

/**
 * Emit the instruction suffix depending on the mode.
 */
void arm_emit_mode(arm_emit_env_t *env, const ir_node *node) {
	arm_attr_t *attr;
	ir_mode *mode;
	int bits;

	attr = get_arm_attr(node);
	mode = attr->op_mode ? attr->op_mode : get_irn_mode(node);
	bits = get_mode_size_bits(mode);

	if (bits == 32)
		be_emit_char(env->emit, 's');
	else if (bits == 64)
		be_emit_char(env->emit, 'd');
	else
		be_emit_char(env->emit, 'e');
}


/**
 * Returns non-zero if a mode has a Immediate attribute.
 */
int is_immediate_node(const ir_node *irn) {
	arm_attr_t *attr = get_arm_attr(irn);
	return ARM_GET_SHF_MOD(attr) == ARM_SHF_IMM;
}

/**
 * Emit a const or SymConst value.
 */
void arm_emit_immediate(arm_emit_env_t *env, const ir_node *node) {
	if (is_immediate_node(node)) {
		be_emit_irprintf(env->emit, "#0x%X", arm_decode_imm_w_shift(get_arm_value(node)));
	} else if (is_arm_SymConst(node))
		be_emit_ident(env->emit, get_arm_symconst_id(node));
	else {
		assert(!"not a Constant");
	}
}

/**
 * Returns the tarval or offset of an arm node as a string.
 */
void arm_emit_shift(arm_emit_env_t *env, const ir_node *node) {
	arm_shift_modifier mod;

	mod = get_arm_shift_modifier(node);
	if (ARM_HAS_SHIFT(mod)) {
		long v = get_tarval_long(get_arm_value(node));

		be_emit_irprintf(env->emit, ", %s #%ld", arm_shf_mod_name(mod), v);
	}
}

/**
 * Returns a unique label. This number will not be used a second time.
 */
static unsigned get_unique_label(void) {
	static unsigned id = 0;
	return ++id;
}

/**
 * Emit a SymConst.
 */
static void emit_arm_SymConst(arm_emit_env_t *env, const ir_node *irn) {
	ident *id = get_arm_symconst_id(irn);
	pmap_entry *entry = pmap_find(env->symbols, id);
	unsigned label;

	if (entry == NULL) {
		/* allocate a label */
		label = get_unique_label();
		pmap_insert(env->symbols, id, INT_TO_PTR(label));
	} else {
		label = PTR_TO_INT(entry->value);
	}

	/* load the symbol indirect */
	be_emit_cstring(env->emit, "\tldr ");
	arm_emit_dest_register(env, irn, 0);
	be_emit_irprintf(env->emit, ", .L%u", label);
	be_emit_finish_line_gas(env->emit, irn);
}

/**
 * Returns the next block in a block schedule.
 */
static ir_node *sched_next_block(ir_node *block) {
    return get_irn_link(block);
}

/**
 * Emit a conditional jump.
 */
static void emit_arm_CondJmp(arm_emit_env_t *env, const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node *true_block = NULL;
	ir_node *false_block = NULL;
	ir_node *op1 = get_irn_n(irn, 0);
	ir_mode *opmode = get_irn_mode(op1);
	const char *suffix;
	int proj_num = get_arm_proj_num(irn);

	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		ir_node *block = get_irn_link(proj);
		if (nr == pn_Cond_true) {
			true_block = block;
		} else {
			false_block = block;
		}
	}

	if (proj_num == pn_Cmp_False) {
		/* always false: should not happen */
		be_emit_cstring(env->emit, "\tb ");
		arm_emit_block_label(env, false_block);
		be_emit_finish_line_gas(env->emit, irn);
	} else if (proj_num == pn_Cmp_True) {
		/* always true: should not happen */
		be_emit_cstring(env->emit, "\tb ");
		arm_emit_block_label(env, true_block);
		be_emit_finish_line_gas(env->emit, irn);
	} else {
		ir_node *block = get_nodes_block(irn);

		if (mode_is_float(opmode)) {
			suffix = "ICHWILLIMPLEMENTIERTWERDEN";

			be_emit_cstring(env->emit, "\tfcmp ");
			arm_emit_source_register(env, irn, 0);
			be_emit_cstring(env->emit, ", ");
			arm_emit_source_register(env, irn, 1);
			be_emit_finish_line_gas(env->emit, irn);

			be_emit_cstring(env->emit, "\tfmstat");
			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* FCSPR -> CPSR */");
			be_emit_finish_line_gas(env->emit, NULL);
		} else {
			if (true_block == sched_next_block(block)) {
				/* negate it */
				proj_num = get_negated_pnc(proj_num, opmode);
			}
			switch (proj_num) {
				case pn_Cmp_Eq:  suffix = "eq"; break;
				case pn_Cmp_Lt:  suffix = "lt"; break;
				case pn_Cmp_Le:  suffix = "le"; break;
				case pn_Cmp_Gt:  suffix = "gt"; break;
				case pn_Cmp_Ge:  suffix = "ge"; break;
				case pn_Cmp_Lg:  suffix = "ne"; break;
				case pn_Cmp_Leg: suffix = "al"; break;
				default: assert(!"Cmp unsupported"); suffix = "al";
			}
			be_emit_cstring(env->emit, "\tcmp ");
			arm_emit_source_register(env, irn, 0);
			be_emit_cstring(env->emit, ", ");
			arm_emit_source_register(env, irn, 1);
			be_emit_finish_line_gas(env->emit, irn);
		}

		if (true_block == sched_next_block(block)) {
			be_emit_irprintf(env->emit, "\tb%s ", suffix);
			arm_emit_block_label(env, true_block);
			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* false case */");
			be_emit_finish_line_gas(env->emit, NULL);

			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* fallthrough ");
			arm_emit_block_label(env, false_block);
			be_emit_cstring(env->emit, " */");
			be_emit_finish_line_gas(env->emit, NULL);
		} else {
			be_emit_irprintf(env->emit, "\tb%s ", suffix);
			arm_emit_block_label(env, true_block);
			be_emit_pad_comment(env->emit);
			be_emit_cstring(env->emit, "/* true case */");
			be_emit_finish_line_gas(env->emit, NULL);

			if (false_block == sched_next_block(block)) {
				be_emit_pad_comment(env->emit);
				be_emit_cstring(env->emit, "/* fallthrough ");
				arm_emit_block_label(env, false_block);
				be_emit_cstring(env->emit, " */");
				be_emit_finish_line_gas(env->emit, NULL);
			} else {
				be_emit_cstring(env->emit, "b ");
				arm_emit_block_label(env, false_block);
				be_emit_pad_comment(env->emit);
				be_emit_cstring(env->emit, "/* false case */");
				be_emit_finish_line_gas(env->emit, NULL);
			}
		}
	}
}

/**
 * Create the CopyB instruction sequence.
 */
static void emit_arm_CopyB(arm_emit_env_t *env, const ir_node *irn) {
	unsigned int size = get_tarval_long(get_arm_value(irn));

	be_emit_cstring(env->emit, "/* MemCopy (");
	arm_emit_source_register(env, irn, 1);
	be_emit_cstring(env->emit, ")->(");
	arm_emit_source_register(env, irn, 0);
	be_emit_irprintf(env->emit, " [%d bytes], Uses ", size);
	arm_emit_source_register(env, irn, 2);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 3);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 4);
	be_emit_cstring(env->emit, ", and %r12 */");
	be_emit_finish_line_gas(env->emit, NULL);

	assert ( size > 0 && "CopyB needs size > 0" );
	if (size & 3)
		size += 4;
	size >>= 2;
	switch(size & 3) {
	case 0:
		break;
	case 1:
		be_emit_cstring(env->emit, "\tldr %%r12, [");
		arm_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, ", #0]!");
		be_emit_finish_line_gas(env->emit, NULL);

		be_emit_cstring(env->emit, "\tstr %%r12, [");
		arm_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", #0]!");
		be_emit_finish_line_gas(env->emit, irn);
		break;
	case 2:
		be_emit_cstring(env->emit, "\tldmia ");
		arm_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, "!, {%r12, ");
		arm_emit_source_register(env, irn, 2);
		be_emit_char(env->emit, '}');
		be_emit_finish_line_gas(env->emit, NULL);

		be_emit_cstring(env->emit, "\tstmia ");
		arm_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, "!, {%r12, ");
		arm_emit_source_register(env, irn, 2);
		be_emit_char(env->emit, '}');
		be_emit_finish_line_gas(env->emit, irn);
		break;
	case 3:
		be_emit_cstring(env->emit, "\tldmia ");
		arm_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, "!, {%r12, ");
		arm_emit_source_register(env, irn, 2);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 3);
		be_emit_char(env->emit, '}');
		be_emit_finish_line_gas(env->emit, NULL);

		be_emit_cstring(env->emit, "\tstmia ");
		arm_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, "!, {%r12, ");
		arm_emit_source_register(env, irn, 2);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 3);
		be_emit_char(env->emit, '}');
		be_emit_finish_line_gas(env->emit, irn);
		break;
	}
	size >>= 2;
	while (size) {
		be_emit_cstring(env->emit, "\tldmia ");
		arm_emit_source_register(env, irn, 1);
		be_emit_cstring(env->emit, "!, {%r12, ");
		arm_emit_source_register(env, irn, 2);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 3);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 4);
		be_emit_char(env->emit, '}');
		be_emit_finish_line_gas(env->emit, NULL);

		be_emit_cstring(env->emit, "\tstmia ");
		arm_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, "!, {%r12, ");
		arm_emit_source_register(env, irn, 2);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 3);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 4);
		be_emit_char(env->emit, '}');
		be_emit_finish_line_gas(env->emit, irn);
		--size;
	}
}

static void emit_arm_SwitchJmp(arm_emit_env_t *env, const ir_node *irn) {
	const ir_edge_t    *edge;
	ir_node            *proj;
	int i;
	ir_node **projs;
	int n_projs;
	int block_nr;
	ir_node *default_block = NULL;

	block_nr = get_irn_node_nr(irn);
	n_projs = get_arm_n_projs(irn);

	projs = xcalloc(n_projs , sizeof(ir_node*));

	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		if (get_Proj_proj(proj) == get_arm_default_proj_num(irn))
			default_block = get_irn_link(proj);

		projs[get_Proj_proj(proj)] = proj;
	}
	assert(default_block != NULL);

	/*
	   CMP %1S, n_projs - 1
	   BHI default
	*/

	be_emit_cstring(env->emit, "\tcmp ");
	arm_emit_source_register(env, irn, 0);
	be_emit_irprintf(env->emit, ", #%u", n_projs - 1);
	be_emit_finish_line_gas(env->emit, irn);

	be_emit_cstring(env->emit, "\tbhi ");
	arm_emit_block_label(env, default_block);
	be_emit_finish_line_gas(env->emit, NULL);

	/*
	   LDR %r12, .TABLE_X_START
	   ADD %r12, %r12, [%1S, LSL #2]
	   LDR %r15, %r12
	 */

	be_emit_irprintf(env->emit, "\tldr %%r12, TABLE_%d_START", block_nr);
	be_emit_finish_line_gas(env->emit, NULL);

	be_emit_irprintf(env->emit, "\tadd %%r12, %%r12, ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", LSL #2");
	be_emit_finish_line_gas(env->emit, NULL);

	be_emit_cstring(env->emit, "\tldr %r15, [%r12, #0]");
	be_emit_finish_line_gas(env->emit, NULL);

	be_emit_irprintf(env->emit, "TABLE_%d_START:\n\t.word\tTABLE_%d", block_nr, block_nr);
	be_emit_finish_line_gas(env->emit, NULL);
	be_emit_irprintf(env->emit, "\t.align 2");
	be_emit_finish_line_gas(env->emit, NULL);
	be_emit_irprintf(env->emit, "TABLE_%d:", block_nr);
	be_emit_finish_line_gas(env->emit, NULL);

	for (i = 0; i < n_projs; ++i) {
		ir_node *block;
		proj = projs[i];
		if ( proj ) {
			block = get_irn_link(proj);
		} else {
			block = get_irn_link(projs[get_arm_default_proj_num(irn)]);
		}
		be_emit_cstring(env->emit, "\t.word\t");
		arm_emit_block_label(env, block);
		be_emit_finish_line_gas(env->emit, NULL);
	}
	be_emit_irprintf(env->emit, "\t.align 2\n");
	be_emit_finish_line_gas(env->emit, NULL);
	xfree(projs);
}

/************************************************************************/
/* emit_be                                                              */
/************************************************************************/

static void emit_be_Call(arm_emit_env_t *env, const ir_node *irn) {
	ir_entity *ent = be_Call_get_entity(irn);

	be_emit_cstring(env->emit, "\tbl ");
	if (ent) {
		mark_entity_visited(ent);
		be_emit_ident(env->emit, get_entity_ld_ident(ent));
	} else {
		arm_emit_source_register(env, irn, be_pos_Call_ptr);
	}
	be_emit_finish_line_gas(env->emit, irn);
}

/** Emit an IncSP node */
static void emit_be_IncSP(arm_emit_env_t *env, const ir_node *irn) {
	int offs = be_get_IncSP_offset(irn);

	if (offs != 0) {
		be_emit_cstring(env->emit, "\tadd ");
		arm_emit_dest_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, ", #");
		arm_emit_offset(env, irn);
	} else {
		be_emit_cstring(env->emit, "\t/* omitted IncSP(");
		arm_emit_offset(env, irn);
		be_emit_cstring(env->emit,") */");
	}
	be_emit_finish_line_gas(env->emit, irn);
}

static void emit_be_Copy(arm_emit_env_t *env, const ir_node *irn) {
	ir_mode *mode = get_irn_mode(irn);

	if (get_in_reg(env->arch_env, irn, 0) == get_out_reg(env->arch_env, irn, 0)) {
		be_emit_cstring(env->emit, "\t/* omitted Copy: ");
		arm_emit_source_register(env, irn, 0);
		be_emit_cstring(env->emit, " -> ");
		arm_emit_dest_register(env, irn, 0);
		be_emit_finish_line_gas(env->emit, irn);
		return;
	}

	if (mode_is_float(mode)) {
		if (USE_FPA(env->cg->isa)) {
			be_emit_cstring(env->emit, "\tmvf");
			arm_emit_mode(env, irn);
			be_emit_char(env->emit, ' ');
			arm_emit_dest_register(env, irn, 0);
			be_emit_cstring(env->emit, ", ");
			arm_emit_source_register(env, irn, 0);
			be_emit_finish_line_gas(env->emit, irn);
		} else {
			assert(0 && "move not supported for this mode");
			panic("emit_be_Copy: move not supported for this mode");
		}
	} else if (mode_is_numP(mode)) {
		be_emit_cstring(env->emit, "\tmov ");
		arm_emit_dest_register(env, irn, 0);
		be_emit_cstring(env->emit, ", ");
		arm_emit_source_register(env, irn, 0);
			be_emit_finish_line_gas(env->emit, irn);
	} else {
		assert(0 && "move not supported for this mode");
		panic("emit_be_Copy: move not supported for this mode");
	}
}

/**
 * Emit code for a Spill.
 */
static void emit_be_Spill(arm_emit_env_t *env, const ir_node *irn) {
	ir_mode *mode = get_irn_mode(irn);

	if (mode_is_float(mode)) {
		if (USE_FPA(env->cg->isa)) {
			be_emit_cstring(env->emit, "\tstf ");
		} else {
			assert(0 && "spill not supported for this mode");
			panic("emit_be_Spill: spill not supported for this mode");
		}
	} else if (mode_is_dataM(mode)) {
		be_emit_cstring(env->emit, "\tstr ");
	} else {
		assert(0 && "spill not supported for this mode");
		panic("emit_be_Spill: spill not supported for this mode");
	}
	arm_emit_source_register(env, irn, 1);
	be_emit_cstring(env->emit, ", [");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", #");
	arm_emit_offset(env, irn);
	be_emit_char(env->emit, ']');
	be_emit_finish_line_gas(env->emit, irn);
}

/**
 * Emit code for a Reload.
 */
static void emit_be_Reload(arm_emit_env_t *env, const ir_node *irn) {
	ir_mode *mode = get_irn_mode(irn);

	if (mode_is_float(mode)) {
		if (USE_FPA(env->cg->isa)) {
			be_emit_cstring(env->emit, "\tldf ");
		} else {
			assert(0 && "reload not supported for this mode");
			panic("emit_be_Reload: reload not supported for this mode");
		}
	} else if (mode_is_dataM(mode)) {
		be_emit_cstring(env->emit, "\tldr ");
	} else {
		assert(0 && "reload not supported for this mode");
		panic("emit_be_Reload: reload not supported for this mode");
	}
	arm_emit_dest_register(env, irn, 0);
	be_emit_cstring(env->emit, ", [");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", #");
	arm_emit_offset(env, irn);
	be_emit_char(env->emit, ']');
	be_emit_finish_line_gas(env->emit, irn);
}

static void emit_be_Perm(arm_emit_env_t *env, const ir_node *irn) {
	be_emit_cstring(env->emit, "\teor ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 1);
	be_emit_finish_line_gas(env->emit, NULL);

	be_emit_cstring(env->emit, "\teor ");
	arm_emit_source_register(env, irn, 1);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 1);
	be_emit_finish_line_gas(env->emit, NULL);

	be_emit_cstring(env->emit, "\teor ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", ");
	arm_emit_source_register(env, irn, 1);
	be_emit_finish_line_gas(env->emit, irn);
}

static void emit_be_StackParam(arm_emit_env_t *env, const ir_node *irn) {
	ir_mode *mode = get_irn_mode(irn);

	if (mode_is_float(mode)) {
		if (USE_FPA(env->cg->isa)) {
			be_emit_cstring(env->emit,"\tldf ");
		} else {
			assert(0 && "stackparam not supported for this mode");
			panic("emit_be_StackParam: stackparam not supported for this mode");
		}
	} else {
		be_emit_cstring(env->emit,"\tldr ");
	}
	arm_emit_dest_register(env, irn, 0);
	be_emit_cstring(env->emit, ", [");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit,", #");
	arm_emit_offset(env, irn);
	be_emit_finish_line_gas(env->emit, irn);
}

/************************************************************************/
/* emit                                                                 */
/************************************************************************/

static void emit_Jmp(arm_emit_env_t *env, const ir_node *irn) {
	const ir_edge_t *edge = get_irn_out_edge_first(irn);
	ir_node *target_block = get_edge_src_irn(edge);
	ir_node *block = get_nodes_block(irn);

	if (target_block == sched_next_block(block)) {
		be_emit_pad_comment(env->emit);
		be_emit_cstring(env->emit, "/* fallthrough ");
		arm_emit_block_label(env, target_block);
		be_emit_cstring(env->emit, " */");
		be_emit_finish_line_gas(env->emit, NULL);
	} else {
		be_emit_cstring(env->emit, "\tb ");
		arm_emit_block_label(env, target_block);
		be_emit_finish_line_gas(env->emit, irn);
	}
}

static void emit_arm_fpaDbl2GP(arm_emit_env_t *env, const ir_node *irn) {
	be_emit_cstring(env->emit, "\tstfd ");
	arm_emit_source_register(env, irn, 0);
	be_emit_cstring(env->emit, ", [sp, #-8]!");
	be_emit_pad_comment(env->emit);
	be_emit_cstring(env->emit, "/* Push fp to stack */");
	be_emit_finish_line_gas(env->emit, NULL);

	be_emit_cstring(env->emit, "\tldmfd sp!, {");
	arm_emit_dest_register(env, irn, 1);
	be_emit_cstring(env->emit, ", ");
	arm_emit_dest_register(env, irn, 0);
	be_emit_char(env->emit, '}');
	be_emit_pad_comment(env->emit);
	be_emit_cstring(env->emit, "/* Pop destination */");
	be_emit_finish_line_gas(env->emit, irn);
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

static void emit_silence(arm_emit_env_t *env, const ir_node *irn) {
	/* Do nothing. */
}

/**
 * The type of a emitter function.
 */
typedef void (emit_func)(arm_emit_env_t *env, const ir_node *irn);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static INLINE void set_emitter(ir_op *op, emit_func arm_emit_node) {
	op->ops.generic = (op_func)arm_emit_node;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void arm_register_emitters(void) {

#define ARM_EMIT(a)  set_emitter(op_arm_##a, emit_arm_##a)
#define EMIT(a)      set_emitter(op_##a, emit_##a)
#define BE_EMIT(a)   set_emitter(op_be_##a, emit_be_##a)
#define SILENCE(a)   set_emitter(op_##a, emit_silence)

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	arm_register_spec_emitters();

	/* other emitter functions */
	ARM_EMIT(CondJmp);
	ARM_EMIT(CopyB);
// 	ARM_EMIT(CopyB_i);
//	ARM_EMIT(Const);
	ARM_EMIT(SymConst);
	ARM_EMIT(SwitchJmp);
	ARM_EMIT(fpaDbl2GP);

	/* benode emitter */
 	BE_EMIT(Call);
 	BE_EMIT(IncSP);
// 	BE_EMIT(AddSP);
	BE_EMIT(Copy);
	BE_EMIT(Spill);
	BE_EMIT(Reload);
	BE_EMIT(Perm);
	BE_EMIT(StackParam);

	/* firm emitter */
	EMIT(Jmp);

	/* noisy stuff */
#ifdef SILENCER
	SILENCE(Start);
	SILENCE(Proj);
	SILENCE(Phi);
	SILENCE(be_Keep);
	SILENCE(be_CopyKeep);
	SILENCE(be_RegParams);
	SILENCE(be_Barrier);
	SILENCE(be_Return);
#endif

#undef ARM_EMIT
#undef BE_EMIT
#undef EMIT
#undef SILENCE
}

/**
 * Emits code for a node.
 */
static void arm_emit_node(arm_emit_env_t *env, const ir_node *irn) {
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
void arm_gen_block(ir_node *block, void *ctx) {
	arm_emit_env_t *env = ctx;
	ir_node *irn;

	arm_emit_block_label(env, block);
	be_emit_cstring(env->emit, ":\n");
	be_emit_write_line(env->emit);

	sched_foreach(block, irn) {
		arm_emit_node(env, irn);
	}
}


/**
 * Emits code for function start.
 */
void arm_func_prolog(arm_emit_env_t *env, ir_graph *irg) {
	be_emit_env_t *eenv = env->emit;
	ir_entity *ent = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(ent);

	be_emit_write_line(eenv);
	be_gas_emit_switch_section(eenv, GAS_SECTION_TEXT);
	be_emit_cstring(eenv, "\t.align  2\n");

	if (get_entity_visibility(ent) == visibility_external_visible)
		be_emit_irprintf(eenv, "\t.global %s\n", irg_name);
	be_emit_irprintf(eenv, "%s:\n", irg_name);
}

/**
 * Emits code for function end
 */
void arm_emit_end(FILE *F, ir_graph *irg) {
	fprintf(F, "\t.ident \"firmcc\"\n");
}

/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void arm_gen_labels(ir_node *block, void *env) {
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}


/**
 * Main driver. Emits the code for one routine.
 */
void arm_gen_routine(const arm_code_gen_t *cg, ir_graph *irg)
{
	arm_emit_env_t emit_env;
	ir_node **blk_sched;
	int i, n;
	pmap_entry *entry;

	emit_env.emit     = &cg->isa->emit;
	emit_env.arch_env = cg->arch_env;
	emit_env.cg       = cg;
	emit_env.symbols  = pmap_create();
	FIRM_DBG_REGISTER(emit_env.mod, "firm.be.arm.emit");

	/* set the global arch_env (needed by print hooks) */
	arch_env = cg->arch_env;

	arm_register_emitters();

	/* create the block schedule. For now, we don't need it earlier. */
	blk_sched = be_create_block_schedule(cg->irg, cg->birg->exec_freq);

	arm_func_prolog(&emit_env, irg);
	irg_block_walk_graph(irg, arm_gen_labels, NULL, &emit_env);

	n = ARR_LEN(blk_sched);
	for (i = 0; i < n;) {
		ir_node *block, *next_bl;

		block   = blk_sched[i];
		++i;
		next_bl = i < n ? blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		arm_gen_block(block, &emit_env);
	}

	/* emit SymConst values */
	if (pmap_count(emit_env.symbols) > 0) {
		be_emit_cstring(emit_env.emit, "\t.align 2\n");

		pmap_foreach(emit_env.symbols, entry) {
			ident *id = entry->key;

			be_emit_irprintf(emit_env.emit, ".L%u:\n", PTR_TO_INT(entry->value));
			be_emit_cstring(emit_env.emit, "\t.word\t");
			be_emit_ident(emit_env.emit, id);
			be_emit_char(emit_env.emit, '\n');
			be_emit_write_line(emit_env.emit);
		}
		be_emit_char(emit_env.emit, '\n');
		be_emit_write_line(emit_env.emit);
	}
	pmap_destroy(emit_env.symbols);
}
