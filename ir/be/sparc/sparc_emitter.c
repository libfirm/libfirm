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
 * @version $Id: sparc_emitter.c 26542 2009-09-18 09:18:32Z matze $
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
#include "irargs_t.h"
#include "error.h"
#include "raw_bitset.h"
#include "dbginfo.h"

#include "../besched.h"
#include "../beblocksched.h"
#include "../beirg.h"
#include "../begnuas.h"
#include "../be_dbgout.h"
#include "../benode.h"

#include "sparc_emitter.h"
#include "gen_sparc_emitter.h"
#include "sparc_nodes_attr.h"
#include "sparc_new_nodes.h"

#define SNPRINTF_BUF_LEN 128
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

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
	} else if (is_sparc_irn(node)) {
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

void sparc_emit_immediate(const ir_node *node)
{
	// TODO: make sure it's a valid simm13 ?
	const sparc_attr_t *attr = get_sparc_attr_const(node);
	be_emit_irprintf("%d", attr->immediate_value);
}

void sparc_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

void sparc_emit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_out_reg(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Emits either a imm or register depending on arity of node
 * @param node
 * @param register no (-1 if no register)
 */
void sparc_emit_reg_or_imm(const ir_node *node, int pos)
{
	if (get_irn_arity(node) > pos) {
		// we have reg input
		sparc_emit_source_register(node, pos);
	} else {
		// we have a imm input
		sparc_emit_immediate(node);
	}
}

/**
 * emit SP offset
 */
void sparc_emit_offset(const ir_node *node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
	assert(attr->base.is_load_store);
	if (attr->offset > 0)
		be_emit_irprintf("+0x%X", attr->offset);
}

/**
 *  Emit load mode char
 */
void sparc_emit_load_mode(const ir_node *node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
    ir_mode *mode      = attr->load_store_mode;
    int      bits      = get_mode_size_bits(mode);
    bool     is_signed = mode_is_signed(mode);

    if (bits == 16) {
        be_emit_string(is_signed ? "sh" : "uh");
    } else if (bits == 8) {
        be_emit_string(is_signed ? "sb" : "ub");
    } else if (bits == 64) {
        be_emit_string("d");
    } else {
        assert(bits == 32);
    }
}

/**
 * Emit store mode char
 */
void sparc_emit_store_mode(const ir_node *node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
    ir_mode *mode      = attr->load_store_mode;
    int      bits      = get_mode_size_bits(mode);

    if (bits == 16) {
        be_emit_string("h");
    } else if (bits == 8) {
        be_emit_string("b");
    } else if (bits == 64) {
        be_emit_string("d");
    } else {
        assert(bits == 32);
    }
}

/**
 * Returns the target label for a control flow node.
 */
static void sparc_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_irn_link(node);
	be_gas_emit_block_name(block);
}

/**
 * Emit single entity
 */
static void sparc_emit_entity(ir_entity *entity)
{
	be_emit_ident(get_entity_ld_ident(entity));
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
 * Emits code for stack space management
 */
static void emit_be_IncSP(const ir_node *irn)
{
	int offs = -be_get_IncSP_offset(irn);

	if (offs != 0) {
		/* SPARC stack grows downwards */
		if (offs < 0) {
			be_emit_cstring("\tsub ");
			offs = -offs;
		} else {
			be_emit_cstring("\tadd ");
		}

		sparc_emit_source_register(irn, 0);
		be_emit_irprintf(", %d", offs);
		be_emit_cstring(", ");
		sparc_emit_dest_register(irn, 0);
		be_emit_finish_line_gas(irn);
	} else {
		// ignore IncSP(0)
		//be_emit_cstring("\t/* IncSP(0) skipped */");

//		be_emit_cstring("\t/* ");
//		be_emit_cstring("sub ");
//		offs = -offs;
//		sparc_emit_source_register(irn, 0);
//		be_emit_irprintf(", %d", offs);
//		be_emit_cstring(", ");
//		sparc_emit_dest_register(irn, 0);
//		be_emit_cstring(" ignored */ ");
//		be_emit_finish_line_gas(irn);
	}


}

/**
 * Emits code for return node
 */
static void emit_be_Return(const ir_node *irn)
{
	be_emit_cstring("\tret");
	be_emit_finish_line_gas(irn);
}

/**
 * Emits code for Call node
 */
static void emit_be_Call(const ir_node *irn)
{
	ir_entity *entity = be_Call_get_entity(irn);

	if (entity != NULL) {
		be_emit_cstring("\tcall ");
	    sparc_emit_entity(entity);
		be_emit_finish_line_gas(irn);
		be_emit_cstring("\tnop");
		be_emit_pad_comment();
		be_emit_cstring("/* TODO: use delay slot */\n");
	} else {
		be_emit_cstring("\tnop\n");
		be_emit_pad_comment();
		be_emit_cstring("/* TODO: Entity == NULL */\n");
		be_emit_finish_line_gas(irn);
	}
}

/**
 * Emit a SymConst.
 */
static void emit_sparc_SymConst(const ir_node *irn)
{
	const sparc_symconst_attr_t *attr = get_sparc_symconst_attr_const(irn);
	//const char *entity_name = get_entity_ld_name(attr->entity);
	ident *id_symconst = get_entity_ident(attr->entity);
	const char *label = get_id_str(id_symconst);

	//sethi %hi(const32),%reg
	//or    %reg,%lo(const32),%reg

	be_emit_irprintf("\tsethi %%hi(%s), ", label);
	sparc_emit_dest_register(irn, 0);
	be_emit_cstring("\n ");
	be_emit_cstring("\tor ");
	sparc_emit_dest_register(irn, 0);
	be_emit_irprintf(", %%lo(%s), ", label);
	sparc_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);
}


/**
 * Emits code for FrameAddr fix
 */
static void emit_sparc_FrameAddr(const ir_node *irn)
{
	const sparc_symconst_attr_t *attr = get_irn_generic_attr_const(irn);
	be_emit_cstring("\tadd ");
	sparc_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	be_emit_irprintf("0x%X", attr->fp_offset);
	be_emit_cstring(", ");
	sparc_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);
}


/**
 * Emits code for Branch
 */
static void emit_sparc_Branch(const ir_node *irn)
{
	const ir_edge_t *edge;
	const ir_node *proj_true  = NULL;
	const ir_node *proj_false = NULL;
	const ir_node *block;
	const ir_node *next_block;
	ir_node *op1 = get_irn_n(irn, 0);
	const char *suffix;
	int proj_num = get_sparc_jmp_cond_proj_num(irn);
	const sparc_cmp_attr_t *cmp_attr = get_irn_generic_attr_const(op1);
	// bool is_signed = !cmp_attr->is_unsigned;

	assert(is_sparc_Cmp(op1) || is_sparc_Tst(op1));

	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	if (cmp_attr->ins_permuted) {
		proj_num = get_mirrored_pnc(proj_num);
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_block = get_irn_link(block);

	assert(proj_num != pn_Cmp_False);
	assert(proj_num != pn_Cmp_True);

	if (get_irn_link(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		proj_num   = get_negated_pnc(proj_num, mode_Iu);
	}

	switch (proj_num) {
		case pn_Cmp_Eq:  suffix = "e"; break;
		case pn_Cmp_Lt:  suffix = "l"; break;
		case pn_Cmp_Le:  suffix = "le"; break;
		case pn_Cmp_Gt:  suffix = "g"; break;
		case pn_Cmp_Ge:  suffix = "ge"; break;
		case pn_Cmp_Lg:  suffix = "ne"; break;
		case pn_Cmp_Leg: suffix = "a"; break;
		default: panic("Cmp has unsupported pnc");
	}

	/* emit the true proj */
	be_emit_irprintf("\tb%s ", suffix);
	sparc_emit_cfop_target(proj_true);
	be_emit_finish_line_gas(proj_true);

	be_emit_cstring("\tnop");
	be_emit_pad_comment();
	be_emit_cstring("/* TODO: use delay slot */\n");

	if (get_irn_link(proj_false) == next_block) {
		be_emit_cstring("\t/* false-fallthrough to ");
		sparc_emit_cfop_target(proj_false);
		be_emit_cstring(" */");
		be_emit_finish_line_gas(proj_false);
	} else {
		be_emit_cstring("\tba ");
		sparc_emit_cfop_target(proj_false);
		be_emit_finish_line_gas(proj_false);
		be_emit_cstring("\tnop\t\t/* TODO: use delay slot */");
		be_emit_write_line();
	}
}

/**
 * emit Jmp (which actually is a branch always)
 */
static void emit_sparc_Jmp(const ir_node *node)
{
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = get_irn_link(block);
	if (get_irn_link(node) != next_block) {
		be_emit_cstring("\tba ");
		sparc_emit_cfop_target(node);
	} else {
		be_emit_cstring("\t/* fallthrough to ");
		sparc_emit_cfop_target(node);
		be_emit_cstring(" */");
	}
	be_emit_finish_line_gas(node);
}

static void emit_be_Copy(const ir_node *irn)
{
	ir_mode *mode = get_irn_mode(irn);

	if (get_in_reg(irn, 0) == get_out_reg(irn, 0)) {
		/* omitted Copy */
		return;
	}

	if (mode_is_float(mode)) {
		panic("emit_be_Copy: move not supported for FP");
	} else if (mode_is_data(mode)) {
		be_emit_cstring("\tmov ");
		sparc_emit_source_register(irn, 0);
		be_emit_cstring(", ");
		sparc_emit_dest_register(irn, 0);
		be_emit_finish_line_gas(irn);
	} else {
		panic("emit_be_Copy: move not supported for this mode");
	}
}


/**
 * dummy emitter for ignored nodes
 */
static void emit_nothing(const ir_node *irn)
{
	(void) irn;
}



/**
 * type of emitter function
 */
typedef void (*emit_func) (const ir_node *);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static inline void set_emitter(ir_op *op, emit_func sparc_emit_node)
{
	op->ops.generic = (op_func)sparc_emit_node;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void sparc_register_emitters(void)
{

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	sparc_register_spec_emitters();

	/* custom emitter */
    set_emitter(op_be_IncSP,       emit_be_IncSP);
    set_emitter(op_be_Return,      emit_be_Return);
    set_emitter(op_be_Call,        emit_be_Call);
    set_emitter(op_sparc_FrameAddr,  emit_sparc_FrameAddr);
    set_emitter(op_sparc_Branch,   emit_sparc_Branch);
    set_emitter(op_sparc_SymConst,   emit_sparc_SymConst);
    set_emitter(op_sparc_Jmp,        emit_sparc_Jmp);

    set_emitter(op_be_Copy,        emit_be_Copy);
    set_emitter(op_be_CopyKeep,    emit_be_Copy);

/*
    set_emitter(op_arm_B,          emit_arm_B);
    set_emitter(op_arm_CopyB,      emit_arm_CopyB);
    set_emitter(op_arm_fpaConst,   emit_arm_fpaConst);
    set_emitter(op_arm_fpaDbl2GP,  emit_arm_fpaDbl2GP);
    set_emitter(op_arm_LdTls,      emit_arm_LdTls);
    set_emitter(op_arm_SwitchJmp,  emit_arm_SwitchJmp);
    set_emitter(op_be_MemPerm,     emit_be_MemPerm);
    set_emitter(op_be_Perm,        emit_be_Perm);
*/
    /* no need to emit anything for the following nodes */
	set_emitter(op_Phi,            emit_nothing);
	set_emitter(op_be_Keep,        emit_nothing);
	set_emitter(op_be_Start,       emit_nothing);
	set_emitter(op_be_Barrier,     emit_nothing);

}

/**
 * Emits code for a node.
 */
static void sparc_emit_node(const ir_node *node)
{
	ir_op               *op       = get_irn_op(node);

	if (op->ops.generic) {
		emit_func func = (emit_func) op->ops.generic;
		be_dbg_set_dbg_info(get_irn_dbg_info(node));
		(*func) (node);
	} else {
		panic("Error: No emit handler for node %+F (graph %+F)\n",
			node, current_ir_graph);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void sparc_gen_block(ir_node *block, void *data)
{
	ir_node *node;
	(void) data;

	if (! is_Block(block))
		return;

	be_gas_emit_block_name(block);
	be_emit_cstring(":\n");
	be_emit_write_line();

	sched_foreach(block, node) {
		sparc_emit_node(node);
	}
}


/**
 * Emits code for function start.
 */
static void sparc_emit_func_prolog(ir_graph *irg)
{
	ir_entity *ent = get_irg_entity(irg);

	be_gas_emit_function_prolog(ent, 4);
	// TODO: fetch reg names via API func
	// TODO: move value to SPARC_MIN_STACKSIZE const
	be_emit_cstring("\tsave %sp, -64, %sp");
	be_emit_cstring("\t/* incr CWP and alloc min. required stack space */\n");
	be_emit_write_line();
}

/**
 * Emits code for function end
 */
static void sparc_emit_func_epilog(ir_graph *irg)
{
	ir_entity *ent = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(ent);

	be_emit_cstring("\trestore");
	be_emit_cstring("\t/* decr CWP */\n");
	be_emit_irprintf("\t.size  %s, .-%s\n", irg_name, irg_name);
	be_emit_cstring("# -- End ");
	be_emit_string(irg_name);
	be_emit_cstring("\n");
	be_emit_write_line();
}

/**
 * Block-walker:
 * TODO: Sets labels for control flow nodes (jump target).
 * Links control predecessors to there destination blocks.
 */
static void sparc_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block); // link the pred of a block (which is a jmp)
	}
}


/**
 * Main driver
 */
void sparc_gen_routine(const sparc_code_gen_t *cg, ir_graph *irg)
{
	ir_node **blk_sched;
	ir_node *last_block = NULL;
	ir_entity *entity     = get_irg_entity(irg);
	int i, n;

	be_gas_elf_type_char = '#';

	/* register all emitter functions */
	sparc_register_emitters();
	be_dbg_method_begin(entity, be_abi_get_stack_layout(cg->birg->abi));

	/* create the block schedule. For now, we don't need it earlier. */
	blk_sched = be_create_block_schedule(cg->irg, cg->birg->exec_freq);

	// emit function prolog
	sparc_emit_func_prolog(irg);

	// generate BLOCK labels
	irg_block_walk_graph(irg, sparc_gen_labels, NULL, NULL);

	// inject block scheduling links & emit code of each block
	n = ARR_LEN(blk_sched);
	for (i = 0; i < n;) {
		ir_node *block, *next_bl;

		block = blk_sched[i];
		++i;
		next_bl = i < n ? blk_sched[i] : NULL;

		/* set here the link. the emitter expects to find the next block here */
		set_irn_link(block, next_bl);
		sparc_gen_block(block, last_block);
		last_block = block;
	}


	//irg_walk_blkwise_graph(irg, NULL, sparc_gen_block, NULL);

	// emit function epilog
	sparc_emit_func_epilog(irg);
}

void sparc_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.emit");
}
