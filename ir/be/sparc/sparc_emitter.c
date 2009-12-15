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

static set *sym_or_tv;

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

void sparc_emit_offset(const ir_node *node)
{
	(void) node;
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
	be_emit_irprintf("BLOCK_%ld", get_irn_node_nr(block));
}

/**
 * Emit single entity
 */
static void sparc_emit_entity(ir_entity *entity)
{
	set_entity_backend_marked(entity, 1);
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
 * Emits code for a unconditional jump.
 */
static void emit_Jmp(const ir_node *node)
{
	ir_node *block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	be_emit_cstring("\tjmp ");
	sparc_emit_cfop_target(node);
	be_emit_finish_line_gas(node);
}


/**
 * Emits code for stack space management
 */
static void emit_be_IncSP(const ir_node *irn)
{
	int offs = -be_get_IncSP_offset(irn);

	if (offs != 0) {
		/* SPARC stack grows downwards */
		if (offs < 0) {
			be_emit_cstring("\tadd ");
			offs = -offs;
		} else {
			be_emit_cstring("\tsub ");
		}

		sparc_emit_source_register(irn, 0);
		be_emit_irprintf(", %d", offs);
		be_emit_cstring(", ");
		sparc_emit_dest_register(irn, 0);
	} else {
		// ignore IncSP(0)
		//be_emit_cstring("\t/* IncSP(0) skipped */");
		be_emit_cstring("\t/* ");
		be_emit_cstring("sub ");
		offs = -offs;
		sparc_emit_source_register(irn, 0);
		be_emit_irprintf(", %d", offs);
		be_emit_cstring(", ");
		sparc_emit_dest_register(irn, 0);
		be_emit_cstring(" ignored */ ");
	}

	be_emit_finish_line_gas(irn);
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
		be_emit_cstring("\tnop\t /* TODO: use delay slot */\n ");
	} else {
		be_emit_cstring("\tnop\t /* TODO: Entity == NULL */\n ");
		/*
		be_emit_cstring("\tmov lr, pc");
		be_emit_finish_line_gas(irn);
		be_emit_cstring("\tmov pc, ");
		sparc_emit_source_register(irn, be_pos_Call_ptr);
		*/
		be_emit_finish_line_gas(irn);
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
static void emit_sparc_SymConst(const ir_node *irn)
{
	const sparc_symconst_attr_t *attr = get_sparc_symconst_attr_const(irn);
	sym_or_tv_t key, *entry;
	unsigned label;

	set_entity_backend_marked(attr->entity, 1);

	key.u.id     = get_entity_ld_ident(attr->entity);
	key.is_ident = 1;
	key.label    = 0;
	entry = (sym_or_tv_t *)set_insert(sym_or_tv, &key, sizeof(key), HASH_PTR(key.u.generic));
	if (entry->label == 0) {
		/* allocate a label */
		entry->label = get_unique_label();
	}

	label = entry->label;

	/* load the symbol indirect */
	be_emit_cstring("\tld ");
	be_emit_irprintf(".L%u, ", label);
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
	sparc_emit_dest_register(irn, 0);
	be_emit_cstring(", ");
	be_emit_irprintf("#0x%X", attr->fp_offset);
	be_emit_finish_line_gas(irn);
}


/**
 * Emits code for Branch
 */
static void emit_sparc_Branch(const ir_node *irn)
{
	(void) irn;
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

/*
    set_emitter(op_arm_B,          emit_arm_B);
    set_emitter(op_arm_CopyB,      emit_arm_CopyB);
    set_emitter(op_arm_fpaConst,   emit_arm_fpaConst);
    set_emitter(op_arm_fpaDbl2GP,  emit_arm_fpaDbl2GP);
    set_emitter(op_arm_Jmp,        emit_arm_Jmp);
    set_emitter(op_arm_LdTls,      emit_arm_LdTls);
    set_emitter(op_arm_SwitchJmp,  emit_arm_SwitchJmp);
    set_emitter(op_be_Copy,        emit_be_Copy);
    set_emitter(op_be_CopyKeep,    emit_be_Copy);
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
void sparc_emit_node(const ir_node *node)
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
void sparc_gen_block(ir_node *block, void *data) {
	ir_node *node;
	(void) data;

	if (! is_Block(block))
		return;
/*
	be_emit_cstring("BLOCK_");
	be_emit_irprintf("%ld:\n", get_irn_node_nr(block));
	be_emit_write_line();
*/
	sched_foreach(block, node) {
		sparc_emit_node(node);
	}
}


/**
 * Emits code for function start.
 */
void sparc_emit_func_prolog(ir_graph *irg) {
	ir_entity *ent = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(ent);

	/* TODO: emit function header */
	be_emit_cstring("# -- Begin ");
	be_emit_string(irg_name);
	be_emit_cstring("\n");


	be_emit_write_line();
	be_gas_emit_switch_section(GAS_SECTION_TEXT);
	be_emit_cstring("\t.align  4\n");

	if (get_entity_visibility(ent) == visibility_external_visible)
		be_emit_irprintf("\t.global %s\n", irg_name);

	be_emit_cstring("\t/* .proc  n - n specifies which registers will contain the return value upon return from the procedure */\n");
	be_emit_irprintf("\t.type %s, #function\n", irg_name);

	be_emit_irprintf("%s:\n", irg_name);
	// TODO: fetch reg names via API func
	// TODO: move value to SPARC_MIN_STACKSIZE const
	be_emit_cstring("\tsave %r14, -64, %r14");
	be_emit_cstring("\t/* incr CWP and alloc min. required stack space */\n");
	be_emit_write_line();
}

/**
 * Emits code for function end
 */
void sparc_emit_func_epilog(ir_graph *irg) {
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
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
void sparc_gen_labels(ir_node *block, void *env) {
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

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

void gen_symconst_values()
{
	sym_or_tv = new_set(cmp_sym_or_tv, 8);

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

				/* TODO: beware: ARM fpa uses big endian format */
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

/**
 * Main driver
 */
void sparc_gen_routine(const sparc_code_gen_t *cg, ir_graph *irg)
{
	ir_entity *entity     = get_irg_entity(irg);

	/* register all emitter functions */
	sparc_register_emitters();
	be_dbg_method_begin(entity, be_abi_get_stack_layout(cg->birg->abi));
	sparc_emit_func_prolog(irg);
	irg_block_walk_graph(irg, sparc_gen_labels, NULL, NULL);
	irg_walk_blkwise_graph(irg, NULL, sparc_gen_block, NULL);
	sparc_emit_func_epilog(irg);

	gen_symconst_values();
}

void sparc_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.emit");
}
