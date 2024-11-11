/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of opcode of intermediate operation.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "irop_t.h"

#include "benode.h"
#include "cgana.h"
#include "irbackedge_t.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irverify_t.h"
#include "panic.h"
#include "reassoc_t.h"
#include "xmalloc.h"
#include <string.h>

static ir_op **opcodes;
/** the available next opcode */
static unsigned next_iro = iro_last+1;

static ir_type *default_get_type_attr(const ir_node *node);
static ir_entity *default_get_entity_attr(const ir_node *node);
static unsigned default_hash_node(const ir_node *node);

int attrs_equal_false(const ir_node *a, const ir_node *b)
{
	(void)a;
	(void)b;
	return false;
}

static int attrs_equal_true(const ir_node *a, const ir_node *b)
{
	(void)a;
	(void)b;
	return true;
}

ir_op *new_ir_op(unsigned code, const char *name, op_pin_state p,
                 irop_flags flags, op_arity opar, int op_index,
                 size_t attr_size)
{
	ir_op *res = XMALLOCZ(ir_op);

	res->code      = code;
	res->name      = name;
	res->pin_state = p;
	res->attr_size = attr_size;
	res->flags     = flags;
	res->opar      = opar;
	res->op_index  = op_index;
	res->tag       = 0;

	memset(&res->ops, 0, sizeof(res->ops));
	res->ops.hash            = default_hash_node;
	res->ops.copy_attr       = default_copy_attr;
	res->ops.attrs_equal     = attrs_equal_true;
	res->ops.get_type_attr   = default_get_type_attr;
	res->ops.get_entity_attr = default_get_entity_attr;

	size_t len = ARR_LEN(opcodes);
	if ((size_t)code >= len) {
		ARR_RESIZE(ir_op*, opcodes, (size_t)code+1);
		memset(&opcodes[len], 0, (code-len+1) * sizeof(opcodes[0]));
	}
	if (opcodes[code] != NULL)
		panic("opcode registered twice");
	opcodes[code] = res;

	return res;
}

void free_ir_op(ir_op *code)
{
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
	for (size_t i = 0, n = ir_get_n_opcodes(); i < n; ++i) {
		ir_op *op = ir_get_opcode(i);
		if (op == NULL)
			continue;
		op->ops.generic  = (op_func)NULL;
		op->ops.generic1 = (op_func)NULL;
		op->ops.generic2 = (op_func)NULL;
	}
}

void ir_op_set_memory_index(ir_op *op, int memory_index)
{
	assert(is_op_uses_memory(op));
	op->memory_index = memory_index;
}

void ir_op_set_fragile_indices(ir_op *op, unsigned pn_x_regular,
                               unsigned pn_x_except)
{
	assert(is_op_fragile(op));
	op->pn_x_regular = pn_x_regular;
	op->pn_x_except = pn_x_except;
}

const char *get_op_name (const ir_op *op)
{
	return op->name;
}

unsigned (get_op_code)(const ir_op *op)
{
  return get_op_code_(op);
}

const char *get_op_pin_state_name(op_pin_state s)
{
	switch (s) {
#define XXX(s) case s: return #s
	XXX(op_pin_state_floats);
	XXX(op_pin_state_pinned);
	XXX(op_pin_state_exc_pinned);
#undef XXX
	}
	return "<none>";
}

op_pin_state (get_op_pinned)(const ir_op *op)
{
	return get_op_pinned_(op);
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

irop_flags get_op_flags(const ir_op *op)
{
	return (irop_flags)op->flags;
}

static ir_type *default_get_type_attr(const ir_node *node)
{
	(void)node;
	return get_unknown_type();
}

static ir_entity *default_get_entity_attr(const ir_node *node)
{
	(void)node;
	return NULL;
}

static unsigned default_hash_node(const ir_node *node)
{
	/* hash table value = 9*(9*(9*(9*(9*arity+in[0])+in[1])+ ...)+mode)+code */
	int      arity          = get_irn_arity(node);
	unsigned hash           = (unsigned)arity;
	bool     consider_block = is_cfop(node) || (!get_opt_global_cse() && !is_Block(node));

	/* consider all in nodes... except the block if not a control flow. */
	for (int i = consider_block ? -1 : 0;  i < arity;  ++i) {
		ir_node *pred = get_irn_n(node, i);
		hash = 9*hash + hash_ptr(pred);
	}

	/* ...mode,... */
	hash = 9*hash + hash_ptr(get_irn_mode(node));
	/* ...and code */
	hash = 9*hash + hash_ptr(get_irn_op(node));

	return hash;
}

/**
 * Calculate a hash value of a Const node.
 */
static unsigned hash_Const(const ir_node *node)
{
	/* special value for const, as they only differ in their tarval. */
	unsigned hash = hash_ptr(node->attr.con.tarval);
	return hash;
}

/**
 * Calculate a hash value of an Address/Offset node.
 */
static unsigned hash_entconst(const ir_node *node)
{
	unsigned hash = hash_ptr(node->attr.entc.entity);
	return hash;
}

/**
 * Calculate a hash value of an Align/Size node.
 */
static unsigned hash_typeconst(const ir_node *node)
{
	unsigned hash = hash_ptr(node->attr.typec.type);
	return hash;
}

/** Compares two exception attributes */
static bool except_attrs_equal(const except_attr *a, const except_attr *b)
{
	return a->pinned == b->pinned;
}

/** Compares the attributes of two Const nodes. */
static int attrs_equal_Const(const ir_node *a, const ir_node *b)
{
	return get_Const_tarval(a) == get_Const_tarval(b);
}

/** Compares the attributes of two Proj nodes. */
static int attrs_equal_Proj(const ir_node *a, const ir_node *b)
{
	return a->attr.proj.num == b->attr.proj.num;
}

/** Compares the attributes of two Alloc nodes. */
static int attrs_equal_Alloc(const ir_node *a, const ir_node *b)
{
	const alloc_attr *pa = &a->attr.alloc;
	const alloc_attr *pb = &b->attr.alloc;
	return pa->alignment == pb->alignment;
}

/** Compares the attributes of two Address/Offset nodes. */
static int attrs_equal_entconst(const ir_node *a, const ir_node *b)
{
	const entconst_attr *pa = &a->attr.entc;
	const entconst_attr *pb = &b->attr.entc;
	return pa->entity == pb->entity;
}

/** Compares the attributes of two Align/Size nodes. */
static int attrs_equal_typeconst(const ir_node *a, const ir_node *b)
{
	const typeconst_attr *pa = &a->attr.typec;
	const typeconst_attr *pb = &b->attr.typec;
	return pa->type == pb->type;
}

/** Compares the attributes of two Call nodes. */
static int attrs_equal_Call(const ir_node *a, const ir_node *b)
{
	const call_attr *pa = &a->attr.call;
	const call_attr *pb = &b->attr.call;
	return pa->type == pb->type && except_attrs_equal(&pa->exc, &pb->exc);
}

/** Compares the attributes of two Sel nodes. */
static int attrs_equal_Sel(const ir_node *a, const ir_node *b)
{
	const ir_type *a_type = get_Sel_type(a);
	const ir_type *b_type = get_Sel_type(b);
	return a_type == b_type;
}

static int attrs_equal_Member(const ir_node *a, const ir_node *b)
{
	const ir_entity *a_ent = get_Member_entity(a);
	const ir_entity *b_ent = get_Member_entity(b);
	return a_ent == b_ent;
}

/** Compares the attributes of two Phi nodes. */
static int attrs_equal_Phi(const ir_node *a, const ir_node *b)
{
	(void)b;
	/* do not CSE Phi-nodes without any inputs when building new graphs */
	if (get_irn_arity(a) == 0 &&
		irg_is_constrained(get_irn_irg(a), IR_GRAPH_CONSTRAINT_CONSTRUCTION))
	    return false;
	return true;
}

/** Compares the attributes of two Load nodes. */
static int attrs_equal_Load(const ir_node *a, const ir_node *b)
{
	const load_attr *attr_a = &a->attr.load;
	const load_attr *attr_b = &b->attr.load;
	return attr_a->volatility == attr_b->volatility
	    && attr_a->unaligned == attr_b->unaligned
	    && attr_a->mode == attr_b->mode
	    && except_attrs_equal(&attr_a->exc, &attr_b->exc);
}

/** Compares the attributes of two Store nodes. */
static int attrs_equal_Store(const ir_node *a, const ir_node *b)
{
	const store_attr *attr_a = &a->attr.store;
	const store_attr *attr_b = &b->attr.store;
	return attr_a->unaligned == attr_b->unaligned
	    && attr_a->volatility == attr_b->volatility
	    && except_attrs_equal(&attr_a->exc, &attr_b->exc);
}

static int attrs_equal_CopyB(const ir_node *a, const ir_node *b)
{
	const copyb_attr *attr_a = &a->attr.copyb;
	const copyb_attr *attr_b = &b->attr.copyb;
	return attr_a->type == attr_b->type
	    && attr_a->volatility == attr_b->volatility;
}

/** Compares the attributes of two Div nodes. */
static int attrs_equal_Div(const ir_node *a, const ir_node *b)
{
	const div_attr *attr_a = &a->attr.div;
	const div_attr *attr_b = &b->attr.div;
	return attr_a->resmode == attr_b->resmode
	    && attr_a->no_remainder == attr_b->no_remainder
	    && except_attrs_equal(&attr_a->exc, &attr_b->exc);
}

/** Compares the attributes of two Mod nodes. */
static int attrs_equal_Mod(const ir_node *a, const ir_node *b)
{
	const mod_attr *attr_a = &a->attr.mod;
	const mod_attr *attr_b = &b->attr.mod;
	return attr_a->resmode == attr_b->resmode
	    && except_attrs_equal(&attr_a->exc, &attr_b->exc);
}

static int attrs_equal_Cmp(const ir_node *a, const ir_node *b)
{
	const cmp_attr *attr_a = &a->attr.cmp;
	const cmp_attr *attr_b = &b->attr.cmp;
	return attr_a->relation == attr_b->relation;
}

/** Compares the attributes of two Confirm nodes. */
static int attrs_equal_Confirm(const ir_node *a, const ir_node *b)
{
	const confirm_attr *attr_a = &a->attr.confirm;
	const confirm_attr *attr_b = &b->attr.confirm;
	return attr_a->relation == attr_b->relation;
}

/** Compares the attributes of two Builtin nodes. */
static int attrs_equal_Builtin(const ir_node *a, const ir_node *b)
{
	const builtin_attr *attr_a = &a->attr.builtin;
	const builtin_attr *attr_b = &b->attr.builtin;
	return attr_a->kind == attr_b->kind && attr_a->type == attr_b->type
	    && except_attrs_equal(&attr_a->exc, &attr_b->exc);
}

/** Compares the attributes of two ASM nodes. */
static int attrs_equal_ASM(const ir_node *a, const ir_node *b)
{
	const asm_attr *attr_a = &a->attr.assem;
	const asm_attr *attr_b = &b->attr.assem;
	if (attr_a->text != attr_b->text)
		return false;

	int n_inputs = get_ASM_n_inputs(a);
	if (n_inputs != get_ASM_n_inputs(b))
		return false;

	size_t const n_constraints = get_ASM_n_constraints(a);
	if (n_constraints != get_ASM_n_constraints(b))
		return false;

	ir_asm_constraint const *const cons_a = get_ASM_constraints(a);
	ir_asm_constraint const *const cons_b = get_ASM_constraints(b);
	for (size_t i = 0; i < n_constraints; ++i) {
		if (cons_a[i].in_pos     != cons_b[i].in_pos ||
		    cons_a[i].out_pos    != cons_b[i].out_pos ||
		    cons_a[i].constraint != cons_b[i].constraint ||
		    cons_a[i].mode       != cons_b[i].mode)
			return false;
	}

	size_t n_clobbers = get_ASM_n_clobbers(a);
	if (n_clobbers != get_ASM_n_clobbers(b))
		return false;

	ident **cla = get_ASM_clobbers(a);
	ident **clb = get_ASM_clobbers(b);
	for (size_t i = 0; i < n_clobbers; ++i) {
		if (cla[i] != clb[i])
			return false;
	}

	return except_attrs_equal(&attr_a->exc, &attr_b->exc);
}

void default_copy_attr(ir_graph *irg, ir_node const *old_node, ir_node *new_node)
{
	(void)irg;
	assert(get_irn_op(old_node) == get_irn_op(new_node));
	size_t size = get_op_attr_size(get_irn_op(old_node));
	memcpy(&new_node->attr, &old_node->attr, size);
}

/**
 * Copies all Call attributes stored in the old node to the new node.
 */
static void call_copy_attr(ir_graph *irg, const ir_node *old_node,
                           ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	cg_remove_call_callee_arr(new_node);
}

/**
 * Copies all Block attributes stored in the old node to the new node.
 */
static void block_copy_attr(ir_graph *irg, const ir_node *old_node,
                            ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	new_node->attr.block.phis          = NULL;
	new_node->attr.block.backedge      = new_backedge_arr(get_irg_obstack(irg), get_irn_arity(new_node));
	new_node->attr.block.block_visited = 0;
	memset(&new_node->attr.block.dom, 0, sizeof(new_node->attr.block.dom));
	memset(&new_node->attr.block.pdom, 0, sizeof(new_node->attr.block.pdom));
	/* It should be safe to copy the entity here, as it has no back-link to the
	 * old block. It serves just as a label number, so copying a labeled block
	 * results in an exact copy. This is at least what we need for DCE to work.
	 */
	new_node->attr.block.entity         = old_node->attr.block.entity;
	new_node->attr.block.phis           = NULL;
}

/**
 * Copies all phi attributes stored in old node to the new node
 */
static void phi_copy_attr(ir_graph *irg, const ir_node *old_node,
                          ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	new_node->attr.phi.next       = NULL;
	new_node->attr.phi.u.backedge = new_backedge_arr(get_irg_obstack(irg), get_irn_arity(new_node));
}

/**
 * Copies all ASM attributes stored in old node to the new node
 */
static void ASM_copy_attr(ir_graph *irg, const ir_node *old_node,
                          ir_node *new_node)
{
	default_copy_attr(irg, old_node, new_node);
	struct obstack *const obst = get_irg_obstack(irg);
	new_node->attr.assem.constraints = DUP_ARR_D(ir_asm_constraint, obst, old_node->attr.assem.constraints);
	new_node->attr.assem.clobbers    = DUP_ARR_D(ident*,            obst, old_node->attr.assem.clobbers);
}

static void switch_copy_attr(ir_graph *irg, const ir_node *old_node,
                             ir_node *new_node)
{
	const ir_switch_table *table = get_Switch_table(old_node);
	new_node->attr.switcha.table = ir_switch_table_duplicate(irg, table);
	new_node->attr.switcha.n_outs = old_node->attr.switcha.n_outs;
}

void set_op_hash(ir_op *op, hash_func func)
{
	op->ops.hash = func;
}

void set_op_computed_value(ir_op *op, computed_value_func func)
{
	op->ops.computed_value = func;
}

void set_op_computed_value_proj(ir_op *op, computed_value_func func)
{
	op->ops.computed_value_Proj = func;
}

void set_op_equivalent_node(ir_op *op, equivalent_node_func func)
{
	op->ops.equivalent_node = func;
}

void set_op_equivalent_node_proj(ir_op *op, equivalent_node_func func)
{
	op->ops.equivalent_node_Proj = func;
}

void set_op_transform_node(ir_op *op, transform_node_func func)
{
	op->ops.transform_node = func;
}

void set_op_transform_node_proj(ir_op *op, transform_node_func func)
{
	op->ops.transform_node_Proj = func;
}

void set_op_attrs_equal(ir_op *op, node_attrs_equal_func func)
{
	op->ops.attrs_equal = func;
}

void set_op_reassociate(ir_op *op, reassociate_func func)
{
	op->ops.reassociate = func;
}

void set_op_copy_attr(ir_op *op, copy_attr_func func)
{
	op->ops.copy_attr = func;
}

void set_op_get_type_attr(ir_op *op, get_type_attr_func func)
{
	op->ops.get_type_attr = func;
}

void set_op_get_entity_attr(ir_op *op, get_entity_attr_func func)
{
	op->ops.get_entity_attr = func;
}

void set_op_verify(ir_op *op, verify_node_func func)
{
	op->ops.verify_node = func;
}

void set_op_verify_proj(ir_op *op, verify_proj_node_func func)
{
	op->ops.verify_proj_node = func;
}

void set_op_dump(ir_op *op, dump_node_func func)
{
	op->ops.dump_node = func;
}

void firm_init_op(void)
{
	opcodes = NEW_ARR_F(ir_op*, 0);
	ir_init_opcodes();
	be_init_op();

	set_op_attrs_equal(op_ASM,     attrs_equal_ASM);
	set_op_attrs_equal(op_Address, attrs_equal_entconst);
	set_op_attrs_equal(op_Align,   attrs_equal_typeconst);
	set_op_attrs_equal(op_Alloc,   attrs_equal_Alloc);
	set_op_attrs_equal(op_Builtin, attrs_equal_Builtin);
	set_op_attrs_equal(op_Call,    attrs_equal_Call);
	set_op_attrs_equal(op_Cmp,     attrs_equal_Cmp);
	set_op_attrs_equal(op_Confirm, attrs_equal_Confirm);
	set_op_attrs_equal(op_Const,   attrs_equal_Const);
	set_op_attrs_equal(op_CopyB,   attrs_equal_CopyB);
	set_op_attrs_equal(op_Div,     attrs_equal_Div);
	set_op_attrs_equal(op_Dummy,   attrs_equal_false);
	set_op_attrs_equal(op_Load,    attrs_equal_Load);
	set_op_attrs_equal(op_Member,  attrs_equal_Member);
	set_op_attrs_equal(op_Mod,     attrs_equal_Mod);
	set_op_attrs_equal(op_Offset,  attrs_equal_entconst);
	set_op_attrs_equal(op_Phi,     attrs_equal_Phi);
	set_op_attrs_equal(op_Proj,    attrs_equal_Proj);
	set_op_attrs_equal(op_Sel,     attrs_equal_Sel);
	set_op_attrs_equal(op_Size,    attrs_equal_typeconst);
	set_op_attrs_equal(op_Store,   attrs_equal_Store);
	set_op_attrs_equal(op_Unknown, attrs_equal_false);

	set_op_hash(op_Address, hash_entconst);
	set_op_hash(op_Align,   hash_typeconst);
	set_op_hash(op_Const,   hash_Const);
	set_op_hash(op_Offset,  hash_entconst);
	set_op_hash(op_Size,    hash_typeconst);

	set_op_copy_attr(op_Call,   call_copy_attr);
	set_op_copy_attr(op_Block,  block_copy_attr);
	set_op_copy_attr(op_Phi,    phi_copy_attr);
	set_op_copy_attr(op_ASM,    ASM_copy_attr);
	set_op_copy_attr(op_Switch, switch_copy_attr);

	ir_register_opt_node_ops();
	ir_register_reassoc_node_ops();
	ir_register_verify_node_ops();
	ir_register_getter_ops();
}

void firm_finish_op(void)
{
	be_finish_op();
	ir_finish_opcodes();
	DEL_ARR_F(opcodes);
	opcodes = NULL;
}
