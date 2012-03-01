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
 * @brief   Representation of opcode of intermediate operation.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "config.h"

#include <string.h>

#include "irop_t.h"
#include "irnode_t.h"
#include "irhooks.h"
#include "irbackedge_t.h"
#include "irnodehashmap.h"

#include "iropt_t.h"
#include "irverify_t.h"
#include "reassoc_t.h"

#include "xmalloc.h"
#include "benode.h"

static ir_op **opcodes;
/** the available next opcode */
static unsigned next_iro = iro_MaxOpcode;

void default_copy_attr(ir_graph *irg, const ir_node *old_node,
                       ir_node *new_node)
{
	unsigned size = firm_add_node_size;
	(void) irg;

	assert(get_irn_op(old_node) == get_irn_op(new_node));
	memcpy(&new_node->attr, &old_node->attr, get_op_attr_size(get_irn_op(old_node)));

	if (size > 0) {
		/* copy additional node data */
		memcpy(get_irn_data(new_node, void, size), get_irn_data(old_node, void, size), size);
	}
}

/**
 * Copies all Call attributes stored in the old node to the new node.
 */
static void call_copy_attr(ir_graph *irg, const ir_node *old_node,
                           ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	remove_Call_callee_arr(new_node);
}

/**
 * Copies all Block attributes stored in the old node to the new node.
 */
static void block_copy_attr(ir_graph *irg, const ir_node *old_node,
                            ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	new_node->attr.block.irg.irg       = irg;
	new_node->attr.block.phis          = NULL;
	new_node->attr.block.cg_backedge   = NULL;
	new_node->attr.block.backedge      = new_backedge_arr(irg->obst, get_irn_arity(new_node));
	new_node->attr.block.block_visited = 0;
	memset(&new_node->attr.block.dom, 0, sizeof(new_node->attr.block.dom));
	memset(&new_node->attr.block.pdom, 0, sizeof(new_node->attr.block.pdom));
	/* It should be safe to copy the entity here, as it has no back-link to the old block.
	 * It serves just as a label number, so copying a labeled block results in an exact copy.
	 * This is at least what we need for DCE to work. */
	new_node->attr.block.entity         = old_node->attr.block.entity;
	new_node->attr.block.phis           = NULL;
	INIT_LIST_HEAD(&new_node->attr.block.succ_head);
}

/**
 * Copies all phi attributes stored in old node to the new node
 */
static void phi_copy_attr(ir_graph *irg, const ir_node *old_node,
                          ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	new_node->attr.phi.next       = NULL;
	new_node->attr.phi.u.backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
}

/**
 * Copies all ASM attributes stored in old node to the new node
 */
static void ASM_copy_attr(ir_graph *irg, const ir_node *old_node,
                          ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	new_node->attr.assem.input_constraints  = DUP_ARR_D(ir_asm_constraint, irg->obst, old_node->attr.assem.input_constraints);
	new_node->attr.assem.output_constraints = DUP_ARR_D(ir_asm_constraint, irg->obst, old_node->attr.assem.output_constraints);
	new_node->attr.assem.clobbers = DUP_ARR_D(ident*, irg->obst, old_node->attr.assem.clobbers);
}

static void switch_copy_attr(ir_graph *irg, const ir_node *old_node,
                             ir_node *new_node)
{
	const ir_switch_table *table = get_Switch_table(old_node);
	new_node->attr.switcha.table = ir_switch_table_duplicate(irg, table);
	new_node->attr.switcha.n_outs = old_node->attr.switcha.n_outs;
}

static ir_node *duplicate_loop_node(ir_node *node, ir_nodehashmap_t *hashmap)
{
	ir_node *new = ir_nodehashmap_get(hashmap, node);
	if (new == NULL) {
		int arity = get_irn_arity(node);
		int i;
		new = vl_exact_copy(node);
		ir_nodehashmap_insert(hashmap, node, new);

		for (i = 0; i < arity; ++i) {
			ir_node *pred     = get_irn_n(node, i);
			ir_node *new_pred = duplicate_loop_node(pred, hashmap);
			set_irn_n(new, i, new_pred);
		}
	}
	return new;
}

static void loop_copy_attr(ir_graph *irg, const ir_node *old_node,
                           ir_node *new_node)
{
	(void)irg;
	ir_nodehashmap_t hashmap;
	ir_node         *new_eta;
	int              arity = get_irn_arity(old_node);
	int              i;

	/* initialize next-pointer of new loop (don't copy the old one) */
	set_Loop_next(new_node, new_node);

	ir_nodehashmap_init(&hashmap);

	for (i = 0; i < arity; ++i) {
		ir_node *in = get_irn_n(old_node, i);
		ir_nodehashmap_insert(&hashmap, in, in);
	}

	new_eta = duplicate_loop_node(get_Loop_eta(old_node), &hashmap);
	set_Loop_eta(new_node, new_eta);

	ir_nodehashmap_destroy(&hashmap);
}

/**
 * Sets the default copy_attr operation for an ir_ops
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
static void firm_set_default_copy_attr(unsigned code, ir_op_ops *ops)
{
	switch (code) {
	case iro_Call:   ops->copy_attr = call_copy_attr;   break;
	case iro_Block:  ops->copy_attr = block_copy_attr;  break;
	case iro_Phi:    ops->copy_attr = phi_copy_attr;    break;
	case iro_ASM:    ops->copy_attr = ASM_copy_attr;    break;
	case iro_Switch: ops->copy_attr = switch_copy_attr; break;
	case iro_Loop:   ops->copy_attr = loop_copy_attr;   break;
	default:
		if (ops->copy_attr == NULL)
			ops->copy_attr = default_copy_attr;
	}
}

/*
 * Sets the default operation for an ir_ops.
 */
static void set_default_operations(unsigned code, ir_op_ops *ops)
{
	firm_set_default_hash(code, ops);
	firm_set_default_computed_value(code, ops);
	firm_set_default_equivalent_node(code, ops);
	firm_set_default_transform_node(code, ops);
	firm_set_default_node_cmp_attr(code, ops);
	firm_set_default_get_type_attr(code, ops);
	firm_set_default_get_entity_attr(code, ops);
	firm_set_default_copy_attr(code, ops);
	firm_set_default_verifier(code, ops);
	firm_set_default_reassoc(code, ops);
}

ir_op *new_ir_op(unsigned code, const char *name, op_pin_state p,
                 unsigned flags, op_arity opar, int op_index, size_t attr_size,
                 const ir_op_ops *ops)
{
	ir_op *res = XMALLOCZ(ir_op);

	res->code      = code;
	res->name      = new_id_from_chars(name, strlen(name));
	res->pin_state = p;
	res->attr_size = attr_size;
	res->flags     = flags;
	res->opar      = opar;
	res->op_index  = op_index;
	res->tag       = 0;

	if (ops)
		res->ops = *ops;
	else /* no given ops, set all operations to NULL */
		memset(&res->ops, 0, sizeof(res->ops));

	set_default_operations(code, &res->ops);

	{
		size_t len = ARR_LEN(opcodes);
		if ((size_t)code >= len) {
			ARR_RESIZE(ir_op*, opcodes, (size_t)code+1);
			memset(&opcodes[len], 0, (code-len+1) * sizeof(opcodes[0]));
		}
		if (opcodes[code] != NULL)
			panic("opcode registered twice");
		opcodes[code] = res;
	}

	hook_new_ir_op(res);
	return res;
}

void free_ir_op(ir_op *code)
{
	hook_free_ir_op(code);

	assert(opcodes[code->code] == code);
	opcodes[code->code] = NULL;

	free(code);
}

unsigned ir_get_n_opcodes(void)
{
	return ARR_LEN(opcodes);
}

ir_op *ir_get_opcode(unsigned code)
{
	assert((size_t)code < ARR_LEN(opcodes));
	return opcodes[code];
}

void ir_clear_opcodes_generic_func(void)
{
	size_t n = ir_get_n_opcodes();
	size_t i;

	for (i = 0; i < n; ++i) {
		ir_op *op = ir_get_opcode(i);
		if (op != NULL)
			op->ops.generic = (op_func)NULL;
	}
}

void ir_op_set_memory_index(ir_op *op, int memory_index)
{
	assert(op->flags & irop_flag_uses_memory);
	op->memory_index = memory_index;
}

void ir_op_set_fragile_indices(ir_op *op, int pn_x_regular, int pn_x_except)
{
	assert(op->flags & irop_flag_fragile);
	op->pn_x_regular = pn_x_regular;
	op->pn_x_except = pn_x_except;
}

const char *get_op_name (const ir_op *op)
{
	return get_id_str(op->name);
}

unsigned (get_op_code)(const ir_op *op)
{
  return get_op_code_(op);
}

ident *(get_op_ident)(const ir_op *op)
{
  return get_op_ident_(op);
}

const char *get_op_pin_state_name(op_pin_state s)
{
	switch (s) {
#define XXX(s) case s: return #s
	XXX(op_pin_state_floats);
	XXX(op_pin_state_pinned);
	XXX(op_pin_state_exc_pinned);
	XXX(op_pin_state_mem_pinned);
#undef XXX
	}
	return "<none>";
}

op_pin_state (get_op_pinned)(const ir_op *op)
{
	return get_op_pinned_(op);
}

void set_op_pinned(ir_op *op, op_pin_state pinned)
{
	if (op == op_Block || op == op_Phi || is_op_cfopcode(op)) return;
	op->pin_state = pinned;
}

unsigned get_next_ir_opcode(void)
{
	return next_iro++;
}

unsigned get_next_ir_opcodes(unsigned num)
{
	unsigned base = next_iro;
	next_iro += num;
	return base;
}

op_func (get_generic_function_ptr)(const ir_op *op)
{
	return get_generic_function_ptr_(op);
}

void (set_generic_function_ptr)(ir_op *op, op_func func)
{
	set_generic_function_ptr_(op, func);
}

const ir_op_ops *(get_op_ops)(const ir_op *op)
{
	return get_op_ops_(op);
}

irop_flags get_op_flags(const ir_op *op)
{
	return (irop_flags)op->flags;
}

static void generated_init_op(void);
static void generated_finish_op(void);

void firm_init_op(void)
{
	opcodes = NEW_ARR_F(ir_op*, 0);
	generated_init_op();
	be_init_op();
}

void firm_finish_op(void)
{
	be_finish_op();
	generated_finish_op();
	DEL_ARR_F(opcodes);
	opcodes = NULL;
}

#include "gen_irop.c.inl"
