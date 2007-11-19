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
 * @author  Oliver Richter, Tobias Gneist, Michael Beck
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
#include "raw_bitset.h"
#include "dbginfo.h"

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

#define BLOCK_PREFIX ".L"

#define SNPRINTF_BUF_LEN 128

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static const arch_env_t     *arch_env = NULL;
static const arm_code_gen_t *cg;
static const arm_isa_t      *isa;
static set                  *sym_or_tv;

/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const ir_node *irn, int pos) {
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(irn) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(irn, pos);

	reg = arch_get_irn_register(arch_env, op);

	assert(reg && "no in register found");

	/* in case of a joker register: just return a valid register */
	if (arch_register_type_is(reg, joker)) {
		const arch_register_req_t *req;

		/* ask for the requirements */
		req = arch_get_register_req(arch_env, irn, pos);

		if (arch_register_req_is(req, limited)) {
			/* in case of limited requirements: get the first allowed register */
			unsigned idx = rbitset_next(req->limited, 0, 1);
			reg = arch_register_for_index(req->cls, idx);
		} else {
			/* otherwise get first register in class */
			reg = arch_register_for_index(req->cls, 0);
		}
	}
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
 * Emit the name of the source register at given input position.
 */
void arm_emit_source_register(const ir_node *node, int pos) {
	const arch_register_t *reg = get_in_reg(node, pos);
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Emit the name of the destination register at given output position.
 */
void arm_emit_dest_register(const ir_node *node, int pos) {
	const arch_register_t *reg = get_out_reg(node, pos);
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Emit a node's offset.
 */
void arm_emit_offset(const ir_node *node) {
	int offset = 0;
	ir_op *irn_op = get_irn_op(node);

	if (irn_op == op_be_Reload || irn_op == op_be_Spill) {
		ir_entity *ent = be_get_frame_entity(node);
		offset = get_entity_offset(ent);
	} else if (irn_op == op_be_IncSP) {
		offset = - be_get_IncSP_offset(node);
	} else {
		assert(!"unimplemented arm_emit_offset for this node type");
		panic("unimplemented arm_emit_offset for this node type");
	}
	be_emit_irprintf("%d", offset);
}

/**
 * Emit the arm fpa instruction suffix depending on the mode.
 */
static void arm_emit_fpa_postfix(const ir_mode *mode) {
	int bits = get_mode_size_bits(mode);
	if (bits == 32)
		be_emit_char('s');
	else if (bits == 64)
		be_emit_char('d');
	else
		be_emit_char('e');
}

/**
 * Emit the instruction suffix depending on the mode.
 */
void arm_emit_mode(const ir_node *node) {
	ir_mode *mode;

	if (is_arm_irn(node)) {
		const arm_attr_t *attr = get_arm_attr_const(node);
		mode = attr->op_mode ? attr->op_mode : get_irn_mode(node);
	} else {
		mode = get_irn_mode(node);
	}
	arm_emit_fpa_postfix(mode);
}

/**
 * Emit a const or SymConst value.
 */
void arm_emit_immediate(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);

	if (ARM_GET_SHF_MOD(attr) == ARM_SHF_IMM) {
		be_emit_irprintf("#0x%X", arm_decode_imm_w_shift(get_arm_value(node)));
	} else if (ARM_GET_FPA_IMM(attr)) {
		be_emit_irprintf("#0x%F", get_arm_value(node));
	} else if (is_arm_SymConst(node))
		be_emit_ident(get_arm_symconst_id(node));
	else {
		assert(!"not a Constant");
	}
}

/**
 * Returns the tarval or offset of an arm node as a string.
 */
void arm_emit_shift(const ir_node *node) {
	arm_shift_modifier mod;

	mod = get_arm_shift_modifier(node);
	if (ARM_HAS_SHIFT(mod)) {
		long v = get_tarval_long(get_arm_value(node));

		be_emit_irprintf(", %s #%l", arm_shf_mod_name(mod), v);
	}
}

/** An entry in the sym_or_tv set. */
typedef struct sym_or_tv_t {
	union {
		ident  *id;          /**< An ident. */
		tarval *tv;          /**< A tarval. */
		const void *generic; /**< For generic compare. */
	} u;
	unsigned label;      /**< the associated label. */
	char is_ident;       /**< Non-zero if an ident is stored. */
} sym_or_tv_t;

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
static void emit_arm_SymConst(const ir_node *irn) {
	sym_or_tv_t key, *entry;
	unsigned label;

	key.u.id     = get_arm_symconst_id(irn);
	key.is_ident = 1;
	key.label    = 0;
	entry = (sym_or_tv_t *)set_insert(sym_or_tv, &key, sizeof(key), HASH_PTR(key.u.generic));
	if (entry->label == 0) {
		/* allocate a label */
		entry->label = get_unique_label();
	}
	label = entry->label;

	/* load the symbol indirect */
	be_emit_cstring("\tldr ");
	arm_emit_dest_register(irn, 0);
	be_emit_irprintf(", .L%u", label);
	be_emit_finish_line_gas(irn);
}

/**
 * Emit a floating point fpa constant.
 */
static void emit_arm_fpaConst(const ir_node *irn) {
	sym_or_tv_t key, *entry;
	unsigned label;
	ir_mode *mode;

	key.u.tv     = get_arm_value(irn);
	key.is_ident = 0;
	key.label    = 0;
	entry = (sym_or_tv_t *)set_insert(sym_or_tv, &key, sizeof(key), HASH_PTR(key.u.generic));
	if (entry->label == 0) {
		/* allocate a label */
		entry->label = get_unique_label();
	}
	label = entry->label;

	/* load the tarval indirect */
	mode = get_irn_mode(irn);
	be_emit_cstring("\tldf");
	arm_emit_fpa_postfix(mode);
	be_emit_char(' ');

	arm_emit_dest_register(irn, 0);
	be_emit_irprintf(", .L%u", label);
	be_emit_finish_line_gas(irn);
}

/**
 * Returns the next block in a block schedule.
 */
static ir_node *sched_next_block(const ir_node *block) {
    return get_irn_link(block);
}

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn) {
	return get_irn_link(irn);
}

/**
 * Emits a block label for the given block.
 */
static void arm_emit_block_name(const ir_node *block) {
	if (has_Block_label(block)) {
		be_emit_string(be_gas_label_prefix());
		be_emit_irprintf("%lu", get_Block_label(block));
	} else {
		be_emit_cstring(BLOCK_PREFIX);
		be_emit_irprintf("%d", get_irn_node_nr(block));
	}
}

/**
 * Emit the target label for a control flow node.
 */
static void arm_emit_cfop_target(const ir_node *irn) {
	ir_node *block = get_cfop_target_block(irn);

	arm_emit_block_name(block);
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_arm_CmpBra(const ir_node *irn) {
	const ir_edge_t *edge;
	const ir_node *proj_true  = NULL;
	const ir_node *proj_false = NULL;
	const ir_node *block;
	const ir_node *next_block;
	ir_node *op1 = get_irn_n(irn, 0);
	ir_mode *opmode = get_irn_mode(op1);
	const char *suffix;
	int proj_num = get_arm_CondJmp_proj_num(irn);

	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_block = sched_next_block(block);

	if (proj_num == pn_Cmp_False) {
		/* always false: should not happen */
		be_emit_cstring("\tb ");
		arm_emit_cfop_target(proj_false);
		be_emit_finish_line_gas(proj_false);
	} else if (proj_num == pn_Cmp_True) {
		/* always true: should not happen */
		be_emit_cstring("\tb ");
		arm_emit_cfop_target(proj_true);
		be_emit_finish_line_gas(proj_true);
	} else {
		if (mode_is_float(opmode)) {
			suffix = "ICHWILLIMPLEMENTIERTWERDEN";

			be_emit_cstring("\tfcmp ");
			arm_emit_source_register(irn, 0);
			be_emit_cstring(", ");
			arm_emit_source_register(irn, 1);
			be_emit_finish_line_gas(irn);

			be_emit_cstring("\tfmstat");
			be_emit_pad_comment();
			be_emit_cstring("/* FCSPR -> CPSR */");
			be_emit_finish_line_gas(NULL);
		} else {
			if (get_cfop_target_block(proj_true) == next_block) {
				/* exchange both proj's so the second one can be omitted */
				const ir_node *t = proj_true;

				proj_true  = proj_false;
				proj_false = t;
				proj_num   = get_negated_pnc(proj_num, mode_Iu);
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
			be_emit_cstring("\tcmp ");
			arm_emit_source_register(irn, 0);
			be_emit_cstring(", ");
			arm_emit_source_register(irn, 1);
			be_emit_finish_line_gas(irn);
		}

		/* emit the true proj */
		be_emit_irprintf("\tb%s ", suffix);
		arm_emit_cfop_target(proj_true);
		be_emit_finish_line_gas(proj_true);

		if (get_cfop_target_block(proj_false) == next_block) {
			be_emit_cstring("\t/* fallthrough to ");
			arm_emit_cfop_target(proj_false);
			be_emit_cstring(" */");
			be_emit_finish_line_gas(proj_false);
		} else {
			be_emit_cstring("b ");
			arm_emit_cfop_target(proj_false);
			be_emit_finish_line_gas(proj_false);
		}
	}
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_arm_fpaCmfBra(const ir_node *irn) {
	(void) irn;
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_arm_fpaCmfeBra(const ir_node *irn) {
	(void) irn;
}

/** Sort register in ascending order. */
static int reg_cmp(const void *a, const void *b) {
	const arch_register_t * const *ra = a;
	const arch_register_t * const *rb = b;

	return *ra < *rb ? -1 : (*ra != *rb);
}

/**
 * Create the CopyB instruction sequence.
 */
static void emit_arm_CopyB(const ir_node *irn) {
	unsigned int size = get_tarval_long(get_arm_value(irn));

	const char *tgt = arch_register_get_name(get_in_reg(irn, 0));
	const char *src = arch_register_get_name(get_in_reg(irn, 1));
	const char *t0, *t1, *t2, *t3;

	const arch_register_t *tmpregs[4];

	/* collect the temporary registers and sort them, we need ascending order */
	tmpregs[0] = get_in_reg(irn, 2);
	tmpregs[1] = get_in_reg(irn, 3);
	tmpregs[2] = get_in_reg(irn, 4);
	tmpregs[3] = &arm_gp_regs[REG_R12];

	/* Note: R12 is always the last register because the RA did not assign higher ones */
	qsort((void *)tmpregs, 3, sizeof(tmpregs[0]), reg_cmp);

	/* need ascending order */
	t0 = arch_register_get_name(tmpregs[0]);
	t1 = arch_register_get_name(tmpregs[1]);
	t2 = arch_register_get_name(tmpregs[2]);
	t3 = arch_register_get_name(tmpregs[3]);

	be_emit_cstring("/* MemCopy (");
	be_emit_string(src);
	be_emit_cstring(")->(");
	arm_emit_source_register(irn, 0);
	be_emit_irprintf(" [%d bytes], Uses ", size);
	be_emit_string(t0);
	be_emit_cstring(", ");
	be_emit_string(t1);
	be_emit_cstring(", ");
	be_emit_string(t2);
	be_emit_cstring(", and ");
	be_emit_string(t3);
	be_emit_cstring("*/");
	be_emit_finish_line_gas(NULL);

	assert(size > 0 && "CopyB needs size > 0" );

	if (size & 3) {
		assert(!"strange hack enabled: copy more bytes than needed!");
		size += 4;
	}

	size >>= 2;
	switch (size & 3) {
	case 0:
		break;
	case 1:
		be_emit_cstring("\tldr ");
		be_emit_string(t3);
		be_emit_cstring(", [");
		be_emit_string(src);
		be_emit_cstring(", #0]");
		be_emit_finish_line_gas(NULL);

		be_emit_cstring("\tstr ");
		be_emit_string(t3);
		be_emit_cstring(", [");
		be_emit_string(tgt);
		be_emit_cstring(", #0]");
		be_emit_finish_line_gas(irn);
		break;
	case 2:
		be_emit_cstring("\tldmia ");
		be_emit_string(src);
		be_emit_cstring("!, {");
		be_emit_string(t0);
		be_emit_cstring(", ");
		be_emit_string(t1);
		be_emit_char('}');
		be_emit_finish_line_gas(NULL);

		be_emit_cstring("\tstmia ");
		be_emit_string(tgt);
		be_emit_cstring("!, {");
		be_emit_string(t0);
		be_emit_cstring(", ");
		be_emit_string(t1);
		be_emit_char('}');
		be_emit_finish_line_gas(irn);
		break;
	case 3:
		be_emit_cstring("\tldmia ");
		be_emit_string(src);
		be_emit_cstring("!, {");
		be_emit_string(t0);
		be_emit_cstring(", ");
		be_emit_string(t1);
		be_emit_cstring(", ");
		be_emit_string(t2);
		be_emit_char('}');
		be_emit_finish_line_gas(NULL);

		be_emit_cstring("\tstmia ");
		be_emit_string(tgt);
		be_emit_cstring("!, {");
		be_emit_string(t0);
		be_emit_cstring(", ");
		be_emit_string(t1);
		be_emit_cstring(", ");
		be_emit_string(t2);
		be_emit_char('}');
		be_emit_finish_line_gas(irn);
		break;
	}
	size >>= 2;
	while (size) {
		be_emit_cstring("\tldmia ");
		be_emit_string(src);
		be_emit_cstring("!, {");
		be_emit_string(t0);
		be_emit_cstring(", ");
		be_emit_string(t1);
		be_emit_cstring(", ");
		be_emit_string(t2);
		be_emit_cstring(", ");
		be_emit_string(t3);
		be_emit_char('}');
		be_emit_finish_line_gas(NULL);

		be_emit_cstring("\tstmia ");
		be_emit_string(tgt);
		be_emit_cstring("!, {");
		be_emit_string(t0);
		be_emit_cstring(", ");
		be_emit_string(t1);
		be_emit_cstring(", ");
		be_emit_string(t2);
		be_emit_cstring(", ");
		be_emit_string(t3);
		be_emit_char('}');
		be_emit_finish_line_gas(irn);
		--size;
	}
}

static void emit_arm_SwitchJmp(const ir_node *irn) {
	const ir_edge_t    *edge;
	ir_node            *proj;
	int i;
	ir_node **projs;
	int n_projs;
	int block_nr;
	ir_node *default_proj = NULL;

	block_nr = get_irn_node_nr(irn);
	n_projs = get_arm_SwitchJmp_n_projs(irn);

	projs = xcalloc(n_projs , sizeof(ir_node*));

	foreach_out_edge(irn, edge) {
		proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		if (get_Proj_proj(proj) == get_arm_SwitchJmp_default_proj_num(irn))
			default_proj = proj;

		projs[get_Proj_proj(proj)] = proj;
	}
	assert(default_proj != NULL && "SwitchJmp should have a Default Proj");

	/*
	   CMP %1S, n_projs - 1
	   BHI default
	*/

	be_emit_cstring("\tcmp ");
	arm_emit_source_register(irn, 0);
	be_emit_irprintf(", #%u", n_projs - 1);
	be_emit_finish_line_gas(irn);

	be_emit_cstring("\tbhi ");
	arm_emit_cfop_target(default_proj);
	be_emit_finish_line_gas(default_proj);

	/*
	   LDR %r12, .TABLE_X_START
	   ADD %r12, %r12, [%1S, LSL #2]
	   LDR %r15, %r12
	 */

	be_emit_irprintf("\tldr %%r12, TABLE_%d_START", block_nr);
	be_emit_finish_line_gas(NULL);

	be_emit_irprintf("\tadd %%r12, %%r12, ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", LSL #2");
	be_emit_finish_line_gas(NULL);

	be_emit_cstring("\tldr %r15, [%r12, #0]");
	be_emit_finish_line_gas(NULL);

	be_emit_irprintf("TABLE_%d_START:\n\t.word\tTABLE_%d", block_nr, block_nr);
	be_emit_finish_line_gas(NULL);
	be_emit_irprintf("\t.align 2");
	be_emit_finish_line_gas(NULL);
	be_emit_irprintf("TABLE_%d:", block_nr);
	be_emit_finish_line_gas(NULL);

	for (i = 0; i < n_projs; ++i) {
		proj = projs[i];
		if (proj == NULL) {
			proj = projs[get_arm_SwitchJmp_default_proj_num(irn)];
		}
		be_emit_cstring("\t.word\t");
		arm_emit_cfop_target(proj);
		be_emit_finish_line_gas(proj);
	}
	be_emit_irprintf("\t.align 2\n");
	be_emit_finish_line_gas(NULL);
	xfree(projs);
}

/************************************************************************/
/* emit_be                                                              */
/************************************************************************/

static void emit_be_Call(const ir_node *irn) {
	ir_entity *ent = be_Call_get_entity(irn);

	be_emit_cstring("\tbl ");
	if (ent) {
		set_entity_backend_marked(ent, 1);
		be_emit_ident(get_entity_ld_ident(ent));
	} else {
		arm_emit_source_register(irn, be_pos_Call_ptr);
	}
	be_emit_finish_line_gas(irn);
}

/** Emit an IncSP node */
static void emit_be_IncSP(const ir_node *irn) {
	int offs = be_get_IncSP_offset(irn);

	if (offs != 0) {
		be_emit_cstring("\tadd ");
		arm_emit_dest_register(irn, 0);
		be_emit_cstring(", ");
		arm_emit_source_register(irn, 0);
		be_emit_cstring(", #");
		arm_emit_offset(irn);
	} else {
		be_emit_cstring("\t/* omitted IncSP(");
		arm_emit_offset(irn);
		be_emit_cstring(") */");
	}
	be_emit_finish_line_gas(irn);
}

static void emit_be_Copy(const ir_node *irn) {
	ir_mode *mode = get_irn_mode(irn);

	if (get_in_reg(irn, 0) == get_out_reg(irn, 0)) {
		be_emit_cstring("\t/* omitted Copy: ");
		arm_emit_source_register(irn, 0);
		be_emit_cstring(" -> ");
		arm_emit_dest_register(irn, 0);
		be_emit_finish_line_gas(irn);
		return;
	}

	if (mode_is_float(mode)) {
		if (USE_FPA(isa)) {
			be_emit_cstring("\tmvf");
			arm_emit_mode(irn);
			be_emit_char(' ');
			arm_emit_dest_register(irn, 0);
			be_emit_cstring(", ");
			arm_emit_source_register(irn, 0);
			be_emit_finish_line_gas(irn);
		} else {
			assert(0 && "move not supported for this mode");
			panic("emit_be_Copy: move not supported for this mode");
		}
	} else if (mode_is_data(mode)) {
		be_emit_cstring("\tmov ");
		arm_emit_dest_register(irn, 0);
		be_emit_cstring(", ");
		arm_emit_source_register(irn, 0);
			be_emit_finish_line_gas(irn);
	} else {
		assert(0 && "move not supported for this mode");
		panic("emit_be_Copy: move not supported for this mode");
	}
}

/**
 * Emit code for a Spill.
 */
static void emit_be_Spill(const ir_node *irn) {
	ir_mode *mode = get_irn_mode(be_get_Spill_val(irn));

	if (mode_is_float(mode)) {
		if (USE_FPA(cg->isa)) {
			be_emit_cstring("\tstf");
			arm_emit_fpa_postfix(mode);
			be_emit_char(' ');
		} else {
			assert(0 && "spill not supported for this mode");
			panic("emit_be_Spill: spill not supported for this mode");
		}
	} else if (mode_is_dataM(mode)) {
		be_emit_cstring("\tstr ");
	} else {
		assert(0 && "spill not supported for this mode");
		panic("emit_be_Spill: spill not supported for this mode");
	}
	arm_emit_source_register(irn, 1);
	be_emit_cstring(", [");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", #");
	arm_emit_offset(irn);
	be_emit_char(']');
	be_emit_finish_line_gas(irn);
}

/**
 * Emit code for a Reload.
 */
static void emit_be_Reload(const ir_node *irn) {
	ir_mode *mode = get_irn_mode(irn);

	if (mode_is_float(mode)) {
		if (USE_FPA(cg->isa)) {
			be_emit_cstring("\tldf");
			arm_emit_fpa_postfix(mode);
			be_emit_char(' ');
		} else {
			assert(0 && "reload not supported for this mode");
			panic("emit_be_Reload: reload not supported for this mode");
		}
	} else if (mode_is_dataM(mode)) {
		be_emit_cstring("\tldr ");
	} else {
		assert(0 && "reload not supported for this mode");
		panic("emit_be_Reload: reload not supported for this mode");
	}
	arm_emit_dest_register(irn, 0);
	be_emit_cstring(", [");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", #");
	arm_emit_offset(irn);
	be_emit_char(']');
	be_emit_finish_line_gas(irn);
}

static void emit_be_Perm(const ir_node *irn) {
	be_emit_cstring("\teor ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	arm_emit_source_register(irn, 1);
	be_emit_finish_line_gas(NULL);

	be_emit_cstring("\teor ");
	arm_emit_source_register(irn, 1);
	be_emit_cstring(", ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	arm_emit_source_register(irn, 1);
	be_emit_finish_line_gas(NULL);

	be_emit_cstring("\teor ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	arm_emit_source_register(irn, 1);
	be_emit_finish_line_gas(irn);
}

/************************************************************************/
/* emit                                                                 */
/************************************************************************/

static void emit_Jmp(const ir_node *node) {
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = sched_next_block(block);
	if (get_cfop_target_block(node) != next_block) {
		be_emit_cstring("\tb ");
		arm_emit_cfop_target(node);
	} else {
		be_emit_cstring("\t/* fallthrough to ");
		arm_emit_cfop_target(node);
		be_emit_cstring(" */");
	}
	be_emit_finish_line_gas(node);
}

static void emit_arm_fpaDbl2GP(const ir_node *irn) {
	be_emit_cstring("\tstfd ");
	arm_emit_source_register(irn, 0);
	be_emit_cstring(", [sp, #-8]!");
	be_emit_pad_comment();
	be_emit_cstring("/* Push fp to stack */");
	be_emit_finish_line_gas(NULL);

	be_emit_cstring("\tldmfd sp!, {");
	arm_emit_dest_register(irn, 1);
	be_emit_cstring(", ");
	arm_emit_dest_register(irn, 0);
	be_emit_char('}');
	be_emit_pad_comment();
	be_emit_cstring("/* Pop destination */");
	be_emit_finish_line_gas(irn);
}

static void emit_arm_LdTls(const ir_node *irn) {
	(void) irn;
	panic("TLS not supported for this target\n");
	/* Er... our gcc does not support it... Install a newer toolchain. */
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

static void emit_silence(const ir_node *irn) {
	(void) irn;
	/* Do nothing. */
}

/**
 * The type of a emitter function.
 */
typedef void (emit_func)(const ir_node *irn);

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
	ARM_EMIT(CmpBra);
	ARM_EMIT(fpaCmfBra);
	ARM_EMIT(fpaCmfeBra);
	ARM_EMIT(CopyB);
// 	ARM_EMIT(CopyB_i);
//	ARM_EMIT(Const);
	ARM_EMIT(SymConst);
	ARM_EMIT(SwitchJmp);
	ARM_EMIT(fpaDbl2GP);
	ARM_EMIT(fpaConst);
	ARM_EMIT(LdTls);

	/* benode emitter */
 	BE_EMIT(Call);
 	BE_EMIT(IncSP);
	BE_EMIT(Copy);
	BE_EMIT(Spill);
	BE_EMIT(Reload);
	BE_EMIT(Perm);

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

static const char *last_name = NULL;
static unsigned last_line = -1;
static unsigned num = -1;

/**
 * Emit the debug support for node node.
 */
static void arm_emit_dbg(const ir_node *irn) {
	dbg_info *db = get_irn_dbg_info(irn);
	unsigned lineno;
	const char *fname = ir_retrieve_dbg_info(db, &lineno);

	if (! cg->birg->main_env->options->stabs_debug_support)
		return;

	if (fname) {
		if (last_name != fname) {
			last_line = -1;
			be_dbg_include_begin(cg->birg->main_env->db_handle, fname);
			last_name = fname;
		}
		if (last_line != lineno) {
			char name[64];

			snprintf(name, sizeof(name), ".LM%u", ++num);
			last_line = lineno;
			be_dbg_line(cg->birg->main_env->db_handle, lineno, name);
			be_emit_string(name);
			be_emit_cstring(":\n");
			be_emit_write_line();
		}
	}
}

/**
 * Emits code for a node.
 */
static void arm_emit_node(const ir_node *irn) {
	ir_op *op = get_irn_op(irn);

	if (op->ops.generic) {
		emit_func *emit = (emit_func *)op->ops.generic;
		arm_emit_dbg(irn);
		(*emit)(irn);
	} else {
		be_emit_cstring("\t/* TODO */");
		be_emit_finish_line_gas(irn);
	}
}

/**
 * emit the block label if needed.
 */
static void arm_emit_block_header(ir_node *block, ir_node *prev)
{
	int           n_cfgpreds;
	int           need_label;
	int           i, arity;
	ir_exec_freq  *exec_freq = cg->birg->exec_freq;

	need_label = 0;
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 1) {
		ir_node *pred       = get_Block_cfgpred(block, 0);
		ir_node *pred_block = get_nodes_block(pred);

		/* we don't need labels for fallthrough blocks, however switch-jmps
		 * are no fallthroughs */
		if (pred_block == prev &&
				!(is_Proj(pred) && is_arm_SwitchJmp(get_Proj_pred(pred)))) {
			need_label = 0;
		} else {
			need_label = 1;
		}
	} else {
		need_label = 1;
	}

	if (need_label) {
		arm_emit_block_name(block);
		be_emit_char(':');

		be_emit_pad_comment();
		be_emit_cstring("   /* preds:");

		/* emit list of pred blocks in comment */
		arity = get_irn_arity(block);
		for (i = 0; i < arity; ++i) {
			ir_node *predblock = get_Block_cfgpred_block(block, i);
			be_emit_irprintf(" %d", get_irn_node_nr(predblock));
		}
	} else {
		be_emit_cstring("\t/* ");
		arm_emit_block_name(block);
		be_emit_cstring(": ");
	}
	if (exec_freq != NULL) {
		be_emit_irprintf(" freq: %f",
		                 get_block_execfreq(exec_freq, block));
	}
	be_emit_cstring(" */\n");
	be_emit_write_line();
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void arm_gen_block(ir_node *block, ir_node *prev_block) {
	ir_node *irn;

	arm_emit_block_header(block, prev_block);
	arm_emit_dbg(block);
	sched_foreach(block, irn) {
		arm_emit_node(irn);
	}
}

/**
 * Emits code for function start.
 */
void arm_func_prolog(ir_graph *irg) {
	ir_entity *ent = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(ent);

	be_emit_write_line();
	be_gas_emit_switch_section(GAS_SECTION_TEXT);
	be_emit_cstring("\t.align  2\n");

	if (get_entity_visibility(ent) == visibility_external_visible)
		be_emit_irprintf("\t.global %s\n", irg_name);
	be_emit_irprintf("%s:\n", irg_name);
}

/**
 * Emits code for function end
 */
void arm_emit_end(FILE *F, ir_graph *irg) {
	(void) irg;
	fprintf(F, "\t.ident \"firmcc\"\n");
}

/**
 * Block-walker:
 * Sets labels for control flow nodes (jump target)
 */
static void arm_gen_labels(ir_node *block, void *env) {
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void)env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Compare two entries of the symbol or tarval set.
 */
static int cmp_sym_or_tv(const void *elt, const void *key, size_t size) {
	const sym_or_tv_t *p1 = elt;
	const sym_or_tv_t *p2 = key;
	(void) size;

	/* as an identifier NEVER can point to a tarval, it's enough
	   to compare it this way */
	return p1->u.generic != p2->u.generic;
}

/**
 * Main driver. Emits the code for one routine.
 */
void arm_gen_routine(const arm_code_gen_t *arm_cg, ir_graph *irg) {
	ir_node **blk_sched;
	int i, n;
	ir_node *last_block = NULL;

	cg        = arm_cg;
	arch_env  = cg->arch_env;
	sym_or_tv = new_set(cmp_sym_or_tv, 8);


	arm_register_emitters();

	/* create the block schedule. For now, we don't need it earlier. */
	blk_sched = be_create_block_schedule(cg->irg, cg->birg->exec_freq);

	arm_func_prolog(irg);
	irg_block_walk_graph(irg, arm_gen_labels, NULL, NULL);

	n = ARR_LEN(blk_sched);
	for (i = 0; i < n;) {
		ir_node *block, *next_bl;

		block   = blk_sched[i];
		++i;
		next_bl = i < n ? blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		arm_gen_block(block, last_block);
		last_block = block;
	}

	/* emit SymConst values */
	if (set_count(sym_or_tv) > 0) {
		sym_or_tv_t *entry;

		be_emit_cstring("\t.align 2\n");

		foreach_set(sym_or_tv, entry) {
			be_emit_irprintf(".L%u:\n", entry->label);

			if (entry->is_ident) {
				be_emit_cstring("\t.word\t");
				be_emit_ident(entry->u.id);
				be_emit_char('\n');
				be_emit_write_line();
			} else {
				tarval *tv = entry->u.tv;
				int i, size = get_mode_size_bytes(get_tarval_mode(tv));
				unsigned v;

				/* beware: ARM fpa uses big endian format */
				for (i = ((size + 3) & ~3) - 4; i >= 0; i -= 4) {
					/* get 32 bits */
					v =            get_tarval_sub_bits(tv, i+3);
					v = (v << 8) | get_tarval_sub_bits(tv, i+2);
					v = (v << 8) | get_tarval_sub_bits(tv, i+1);
					v = (v << 8) | get_tarval_sub_bits(tv, i+0);
					be_emit_irprintf("\t.word\t%u\n", v);
					be_emit_write_line();
				}
			}
		}
		be_emit_char('\n');
		be_emit_write_line();
	}
	del_set(sym_or_tv);
}

void arm_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.arm.emit");
}
