/**
 * @file   benode.c
 * @date   17.05.2005
 * @author Sebastian Hack
 *
 * Backend node support.
 *
 * This file provdies Perm, Copy, Spill and Reload nodes.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "obst.h"
#include "set.h"
#include "pmap.h"
#include "util.h"
#include "debug.h"
#include "fourcc.h"

#include "irop_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irprintf.h"

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "benode_t.h"

#include "beirgmod.h"

/* Sometimes we want to put const nodes into get_irn_generic_attr ... */
#define get_irn_attr(irn) get_irn_generic_attr((ir_node *) (irn))

static unsigned be_node_tag = FOURCC('B', 'E', 'N', 'O');

typedef enum _node_kind_t {
	node_kind_spill,
	node_kind_reload,
	node_kind_perm,
	node_kind_copy,
	node_kind_kill,
	node_kind_last
} node_kind_t;

typedef struct {
	node_kind_t kind;
	const arch_register_class_t *cls;
	ir_op *op;
	int n_pos;
	int *pos;
} be_op_t;

typedef enum {
	be_req_kind_old_limited,
	be_req_kind_negate_old_limited,
	be_req_kind_single_reg
} be_req_kind_t;

typedef struct {
	arch_register_req_t req;
	be_req_kind_t       kind;
	arch_irn_flags_t    flags;
	union {
		struct {
			void (*old_limited)(void *ptr, bitset_t *bs);
			void *old_limited_env;
		} old_limited;

		const arch_register_t *single_reg;
	} x;
} be_req_t;

typedef struct {
	const arch_register_t *reg;
	be_req_t              req;
	be_req_t              in_req;
} be_reg_data_t;

typedef struct {
	int                         max_reg_data;
	const arch_register_class_t *cls;
	be_reg_data_t               *reg_data;
} be_node_attr_t;

typedef struct {
	be_node_attr_t node_attr;
	ir_node *spill_ctx;  /**< The node in whose context this spill was introduced. */
	entity *ent;     /**< The entity in the stack frame the spill writes to. */
} be_spill_attr_t;

typedef struct {
	be_node_attr_t node_attr;
	int offset;           /**< The offset by which the stack shall be increased/decreased. */
	be_stack_dir_t dir;   /**< The direction in which the stack shall be modified (along or in the other direction). */
} be_stack_attr_t;

static ir_op *op_Spill;
static ir_op *op_Reload;
static ir_op *op_Perm;
static ir_op *op_Copy;
static ir_op *op_Keep;
static ir_op *op_Call;
static ir_op *op_Return;
static ir_op *op_IncSP;
static ir_op *op_AddSP;
static ir_op *op_RegParams;
static ir_op *op_StackParam;
static ir_op *op_NoReg;

static int beo_base = -1;

static const ir_op_ops be_node_op_ops;

#define N   irop_flag_none
#define L   irop_flag_labeled
#define C   irop_flag_commutative
#define X   irop_flag_cfopcode
#define I   irop_flag_ip_cfopcode
#define F   irop_flag_fragile
#define Y   irop_flag_forking
#define H   irop_flag_highlevel
#define c   irop_flag_constlike
#define K   irop_flag_keep

void be_node_init(void) {
	static int inited = 0;

	if(inited)
		return;

	inited = 1;

	/* Acquire all needed opcodes. */
	beo_base = get_next_ir_opcodes(beo_Last - 1);

	op_Spill      = new_ir_op(beo_base + beo_Spill,      "Spill",      op_pin_state_mem_pinned, N, oparity_unary,    0, sizeof(be_spill_attr_t), &be_node_op_ops);
	op_Reload     = new_ir_op(beo_base + beo_Reload,     "Reload",     op_pin_state_mem_pinned, N, oparity_zero,     0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Perm       = new_ir_op(beo_base + beo_Perm,       "Perm",       op_pin_state_pinned,     N, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Copy       = new_ir_op(beo_base + beo_Copy,       "Copy",       op_pin_state_floats,     N, oparity_unary,    0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Keep       = new_ir_op(beo_base + beo_Keep,       "Keep",       op_pin_state_pinned,     K, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_NoReg      = new_ir_op(beo_base + beo_NoReg,      "NoReg",      op_pin_state_floats,     N, oparity_zero,     0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Call       = new_ir_op(beo_base + beo_Call,       "Call",       op_pin_state_pinned,     N, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Return     = new_ir_op(beo_base + beo_Return,     "Return",     op_pin_state_pinned,     X, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_AddSP      = new_ir_op(beo_base + beo_AddSP,      "AddSP",      op_pin_state_pinned,     N, oparity_unary,    0, sizeof(be_stack_attr_t), &be_node_op_ops);
	op_IncSP      = new_ir_op(beo_base + beo_IncSP,      "IncSP",      op_pin_state_pinned,     N, oparity_binary,   0, sizeof(be_stack_attr_t), &be_node_op_ops);
	op_RegParams  = new_ir_op(beo_base + beo_RegParams,  "RegParams",  op_pin_state_pinned,     N, oparity_zero,     0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_StackParam = new_ir_op(beo_base + beo_StackParam, "StackParam", op_pin_state_pinned,     N, oparity_unary,    0, sizeof(be_stack_attr_t), &be_node_op_ops);

	set_op_tag(op_Spill,      &be_node_tag);
	set_op_tag(op_Reload,     &be_node_tag);
	set_op_tag(op_Perm,       &be_node_tag);
	set_op_tag(op_Copy,       &be_node_tag);
	set_op_tag(op_Keep,       &be_node_tag);
	set_op_tag(op_NoReg,      &be_node_tag);
	set_op_tag(op_Call,       &be_node_tag);
	set_op_tag(op_Return,     &be_node_tag);
	set_op_tag(op_AddSP,      &be_node_tag);
	set_op_tag(op_IncSP,      &be_node_tag);
	set_op_tag(op_RegParams,  &be_node_tag);
	set_op_tag(op_StackParam, &be_node_tag);
}

static void *init_node_attr(ir_node* irn, const arch_register_class_t *cls, ir_graph *irg, int max_reg_data)
{
	be_node_attr_t *a = get_irn_attr(irn);

	a->max_reg_data = max_reg_data;
	a->cls          = cls;
	a->reg_data     = NULL;

	if(max_reg_data > 0) {
		int i;

		a->reg_data = NEW_ARR_D(be_reg_data_t, get_irg_obstack(irg), max_reg_data);
		memset(a->reg_data, 0, max_reg_data * sizeof(a->reg_data[0]));
		for(i = 0; i < max_reg_data; ++i) {
			a->reg_data[i].req.req.cls  = cls;
			a->reg_data[i].req.req.type = arch_register_req_type_normal;
		}
	}

	return a;
}

static INLINE int is_be_node(const ir_node *irn)
{
	return get_op_tag(get_irn_op(irn)) == &be_node_tag;
}

be_opcode_t be_get_irn_opcode(const ir_node *irn)
{
	return is_be_node(irn) ? get_irn_opcode(irn) - beo_base : beo_NoBeOp;
}

static int redir_proj(const ir_node **node, int pos)
{
	const ir_node *n = *node;

	if(is_Proj(n)) {
		assert(pos == -1 && "Illegal pos for a Proj");
		*node = get_Proj_pred(n);
		return get_Proj_proj(n);
	}

	return 0;
}

static void
be_node_set_irn_reg(const void *_self, ir_node *irn, const arch_register_t *reg)
{
	int out_pos;
	be_node_attr_t *a;

	out_pos = redir_proj((const ir_node **) &irn, -1);
	a       = get_irn_attr(irn);

	assert(is_be_node(irn));
	assert(out_pos < a->max_reg_data && "position too high");
	a->reg_data[out_pos].reg = reg;
}


ir_node *be_new_Spill(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *to_spill, ir_node *ctx)
{
	be_spill_attr_t *a;
	ir_node *in[1];
	ir_node *res;

	in[0] = to_spill;
	res   = new_ir_node(NULL, irg, bl, op_Spill, mode_M, 1, in);
	a     = init_node_attr(res, cls, irg, 0);
	a->ent       = NULL;
	a->spill_ctx = ctx;
	return res;
}

ir_node *be_new_Reload(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_mode *mode, ir_node *mem)
{
	ir_node *in[1];
	ir_node *res;

	in[0] = mem;
	res   = new_ir_node(NULL, irg, bl, op_Reload, mode, 1, in);
	init_node_attr(res, cls, irg, 1);
	return res;
}

ir_node *be_new_Perm(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	ir_node *irn = new_ir_node(NULL, irg, bl, op_Perm, mode_T, n, in);
	init_node_attr(irn, cls, irg, n);
	return irn;
}

ir_node *be_new_Copy(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *op)
{
	ir_node *in[1];
	ir_node *res;

	in[0] = op;
	res   = new_ir_node(NULL, irg, bl, op_Copy, get_irn_mode(op), 1, in);
	init_node_attr(res, cls, irg, 1);
	return res;
}

ir_node *be_new_Keep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	ir_node *irn;

	irn = new_ir_node(NULL, irg, bl, op_Keep, mode_ANY, n, in);
	init_node_attr(irn, cls, irg, 0);
	keep_alive(irn);
	return irn;
}

ir_node *be_new_Call(ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *sp, ir_node *ptr, int n_outs, int n, ir_node *in[])
{
	int real_n = 3 + n;
	ir_node *irn;
	ir_node **real_in;

	real_in = malloc(sizeof(real_in[0]) * (real_n));

	real_in[0] = mem;
	real_in[1] = sp;
	real_in[2] = ptr;
	memcpy(&real_in[3], in, n * sizeof(in[0]));

	irn = new_ir_node(NULL, irg, bl, op_Call, mode_T, real_n, real_in);
	init_node_attr(irn, NULL, irg, (n_outs > real_n ? n_outs : real_n));
	return irn;
}

ir_node *be_new_Return(ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	ir_node *irn = new_ir_node(NULL, irg, bl, op_Return, mode_X, n, in);
	init_node_attr(irn, NULL, irg, n);

	return irn;
}

ir_node *be_new_IncSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *mem, unsigned offset, be_stack_dir_t dir)
{
	be_stack_attr_t *a;
	ir_node *irn;
	ir_node *in[1];

	in[0]     = old_sp;
	in[1]     = mem;
	irn       = new_ir_node(NULL, irg, bl, op_IncSP, sp->reg_class->mode, 2, in);
	a         = init_node_attr(irn, sp->reg_class, irg, 1);
	a->dir    = dir;
	a->offset = offset;

	be_node_set_flags(irn, -1, arch_irn_flags_ignore);

	/* Set output constraint to stack register. */
	be_set_constr_single_reg(irn, -1, sp);
	be_node_set_irn_reg(NULL, irn, sp);

	return irn;
}

ir_node *be_new_AddSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *op)
{
	be_node_attr_t *a;
	ir_node *irn;
	ir_node *in[2];

	in[0]    = old_sp;
	in[1]    = op;
	irn      = new_ir_node(NULL, irg, bl, op_AddSP, sp->reg_class->mode, 2, in);
	a        = init_node_attr(irn, sp->reg_class, irg, 1);

	be_node_set_flags(irn, -1, arch_irn_flags_ignore);

	/* Set output constraint to stack register. */
	be_set_constr_single_reg(irn, -1, sp);
	be_node_set_irn_reg(NULL, irn, sp);

	return irn;
}

ir_node *be_new_NoReg(const arch_register_t *reg, ir_graph *irg, ir_node *bl)
{
	be_node_attr_t *a;
	ir_node *irn;
	ir_node *in[1];

	irn = new_ir_node(NULL, irg, bl, op_NoReg, reg->reg_class->mode, 0, in);
	a   = init_node_attr(irn, reg->reg_class, irg, 1);
	be_node_set_flags(irn, -1, arch_irn_flags_ignore);
	be_set_constr_single_reg(irn, -1, reg);
	be_node_set_irn_reg(NULL, irn, reg);
	return irn;
}

ir_node *be_new_StackParam(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_mode *mode, ir_node *frame_pointer, unsigned offset)
{
	be_stack_attr_t *a;
	ir_node *irn;
	ir_node *in[1];

	in[0] = frame_pointer;
	irn = new_ir_node(NULL, irg, bl, op_StackParam, mode, 1, in);
	a = init_node_attr(irn, cls, irg, 1);
	a->offset = offset;
	return irn;
}

ir_node *be_new_RegParams(ir_graph *irg, ir_node *bl, int n_outs)
{
	ir_node *irn;
	ir_node *in[1];

	irn = new_ir_node(NULL, irg, bl, op_RegParams, mode_T, 0, in);
	init_node_attr(irn, NULL, irg, n_outs);
	return irn;
}

int be_is_Spill         (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Spill          ; }
int be_is_Reload        (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Reload         ; }
int be_is_Copy          (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Copy           ; }
int be_is_Perm          (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Perm           ; }
int be_is_Keep          (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Keep           ; }
int be_is_Call          (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Call           ; }
int be_is_Return        (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Return         ; }
int be_is_IncSP         (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_IncSP          ; }
int be_is_AddSP         (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_AddSP          ; }
int be_is_RegParams     (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_RegParams      ; }
int be_is_StackParam    (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_StackParam     ; }
int be_is_NoReg         (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_NoReg          ; }

static void be_limited(void *data, bitset_t *bs)
{
	be_req_t *req = data;

	switch(req->kind) {
	case be_req_kind_negate_old_limited:
	case be_req_kind_old_limited:
		req->x.old_limited.old_limited(req->x.old_limited.old_limited_env, bs);
		if(req->kind == be_req_kind_negate_old_limited)
			bitset_flip_all(bs);
		break;
	case be_req_kind_single_reg:
		bitset_clear_all(bs);
		bitset_set(bs, req->x.single_reg->index);
		break;
	}
}

static INLINE be_req_t *get_req(ir_node *irn, int pos)
{
	int idx           = pos < 0 ? -(pos + 1) : pos;
	be_node_attr_t *a = get_irn_attr(irn);
	be_reg_data_t *rd = &a->reg_data[idx];
	be_req_t       *r = pos < 0 ? &rd->req : &rd->in_req;

	assert(is_be_node(irn));
	assert(!(pos >= 0) || pos < get_irn_arity(irn));
	assert(!(pos < 0)  || -(pos + 1) <= a->max_reg_data);

	return r;
}

void be_set_constr_single_reg(ir_node *irn, int pos, const arch_register_t *reg)
{
	be_req_t *r = get_req(irn, pos);

	r->kind            = be_req_kind_single_reg;
	r->x.single_reg    = reg;
	r->req.limited     = be_limited;
	r->req.limited_env = r;
	r->req.type        = arch_register_req_type_limited;
	r->req.cls         = reg->reg_class;
}

void be_set_constr_limited(ir_node *irn, int pos, const arch_register_req_t *req)
{
	be_req_t *r = get_req(irn, pos);

	assert(arch_register_req_is(req, limited));

	r->kind            = be_req_kind_old_limited;
	r->req.limited     = be_limited;
	r->req.limited_env = r;
	r->req.type        = arch_register_req_type_limited;
	r->req.cls         = req->cls;

	r->x.old_limited.old_limited     = req->limited;
	r->x.old_limited.old_limited_env = req->limited_env;
}

void be_node_set_flags(ir_node *irn, int pos, arch_irn_flags_t flags)
{
	be_req_t *r = get_req(irn, pos);
	r->flags = flags;
}

void be_node_set_reg_class(ir_node *irn, int pos, const arch_register_class_t *cls)
{
	be_req_t *r = get_req(irn, pos);
	r->req.cls = cls;
}

void be_set_IncSP_offset(ir_node *irn, unsigned offset)
{
	be_stack_attr_t *a = get_irn_attr(irn);
	assert(be_is_IncSP(irn));
	a->offset = offset;
}

unsigned be_get_IncSP_offset(ir_node *irn)
{
	be_stack_attr_t *a = get_irn_attr(irn);
	assert(be_is_IncSP(irn));
	return a->offset;
}

void be_set_IncSP_direction(ir_node *irn, be_stack_dir_t dir)
{
	be_stack_attr_t *a = get_irn_attr(irn);
	assert(be_is_IncSP(irn));
	a->dir = dir;
}

be_stack_dir_t be_get_IncSP_direction(ir_node *irn)
{
	be_stack_attr_t *a = get_irn_attr(irn);
	assert(be_is_IncSP(irn));
	return a->dir;
}

void be_set_Spill_entity(ir_node *irn, entity *ent)
{
	be_spill_attr_t *a = get_irn_attr(irn);
	assert(be_is_Spill(irn));
	a->ent = ent;
}

static ir_node *find_a_spill_walker(ir_node *irn, unsigned visited_nr)
{
	if(get_irn_visited(irn) < visited_nr) {
		set_irn_visited(irn, visited_nr);

		if(is_Phi(irn)) {
			int i, n;
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *n = find_a_spill_walker(get_irn_n(irn, i), visited_nr);
				if(n != NULL)
					return n;
			}
		}

		else if(be_get_irn_opcode(irn) == beo_Spill)
			return irn;
	}

	return NULL;
}

ir_node *be_get_Spill_context(const ir_node *irn) {
	const be_spill_attr_t *a = get_irn_attr(irn);
	assert(be_is_Spill(irn));
	return a->spill_ctx;
}

/**
 * Finds a spill for a reload.
 * If the reload is directly using the spill, this is simple,
 * else we perform DFS from the reload (over all PhiMs) and return
 * the first spill node we find.
 */
static INLINE ir_node *find_a_spill(ir_node *irn)
{
	ir_graph *irg       = get_irn_irg(irn);
	unsigned visited_nr = get_irg_visited(irg) + 1;

	assert(be_is_Reload(irn));
	set_irg_visited(irg, visited_nr);
	return find_a_spill_walker(irn, visited_nr);
}

entity *be_get_spill_entity(ir_node *irn)
{
	int opc           = get_irn_opcode(irn);

	switch(be_get_irn_opcode(irn)) {
	case beo_Reload:
		return be_get_spill_entity(find_a_spill(irn));
	case beo_Spill:
		{
			be_spill_attr_t *a = get_irn_attr(irn);
			return a->ent;
		}
	default:
		assert(0 && "Must give spill/reload node");
	}

	return NULL;
}

ir_node *be_spill(const arch_env_t *arch_env, ir_node *irn, ir_node *ctx)
{
	const arch_register_class_t *cls = arch_get_irn_reg_class(arch_env, irn, -1);

	ir_node *bl    = get_nodes_block(irn);
	ir_graph *irg  = get_irn_irg(bl);
	ir_node *spill = be_new_Spill(cls, irg, bl, irn, ctx);
	ir_node *insert;

	/*
	 * search the right insertion point. a spill of a phi cannot be put
	 * directly after the phi, if there are some phis behind the one which
	 * is spilled.
	 */
	insert = sched_next(irn);
	while(is_Phi(insert) && !sched_is_end(insert))
		insert = sched_next(insert);

	sched_add_before(insert, spill);
	return spill;
}

ir_node *be_reload(const arch_env_t *arch_env,
				   const arch_register_class_t *cls,
				   ir_node *irn, int pos, ir_mode *mode, ir_node *spill)
{
	ir_node *reload;

	ir_node *bl   = get_nodes_block(irn);
	ir_graph *irg = get_irn_irg(bl);

	assert(be_is_Spill(spill) || (is_Phi(spill) && get_irn_mode(spill) == mode_M));

	reload = be_new_Reload(cls, irg, bl, mode, spill);

	set_irn_n(irn, pos, reload);
	sched_add_before(irn, reload);
	return reload;
}

static void *put_out_reg_req(arch_register_req_t *req, const ir_node *irn, int out_pos)
{
	const be_node_attr_t *a = get_irn_attr(irn);

	if(out_pos < a->max_reg_data)
		memcpy(req, &a->reg_data[out_pos].req, sizeof(req[0]));
	else {
		req->type = arch_register_req_type_none;
		req->cls  = NULL;
	}

	return req;
}

static void *put_in_reg_req(arch_register_req_t *req, const ir_node *irn, int pos)
{
	const be_node_attr_t *a = get_irn_attr(irn);
	int n                   = get_irn_arity(irn);

	if(pos < get_irn_arity(irn) && pos < a->max_reg_data)
		memcpy(req, &a->reg_data[pos].in_req, sizeof(req[0]));
	else {
		req->type = arch_register_req_type_none;
		req->cls  = NULL;
	}

	return req;
}

static const arch_register_req_t *
be_node_get_irn_reg_req(const void *self, arch_register_req_t *req, const ir_node *irn, int pos)
{
	int out_pos = pos;

	if(pos < 0) {
		if(get_irn_mode(irn) == mode_T)
			return NULL;

		out_pos = redir_proj((const ir_node **) &irn, pos);
		assert(is_be_node(irn));
		return put_out_reg_req(req, irn, out_pos);
	}

	else {
		return is_be_node(irn) ? put_in_reg_req(req, irn, pos) : NULL;
	}

	return req;
}

const arch_register_t *
be_node_get_irn_reg(const void *_self, const ir_node *irn)
{
	int out_pos;
	be_node_attr_t *a;

	out_pos = redir_proj((const ir_node **) &irn, -1);
	a       = get_irn_attr(irn);

	assert(is_be_node(irn));
	assert(out_pos < a->max_reg_data && "position too high");

	return a->reg_data[out_pos].reg;
}

static arch_irn_class_t be_node_classify(const void *_self, const ir_node *irn)
{
	redir_proj((const ir_node **) &irn, -1);

	switch(be_get_irn_opcode(irn)) {
#define XXX(a,b) case beo_ ## a: return arch_irn_class_ ## b;
		XXX(Spill, spill)
		XXX(Reload, reload)
		XXX(Perm, perm)
		XXX(Copy, copy)
#undef XXX
		default:
		return 0;
	}

	return 0;
}

static arch_irn_flags_t be_node_get_flags(const void *_self, const ir_node *irn)
{
	be_node_attr_t *a;

	redir_proj((const ir_node **) &irn, -1);
	assert(is_be_node(irn));
	a = get_irn_attr(irn);
	return a->max_reg_data > 0 ? a->reg_data[0].req.flags : arch_irn_flags_none;
}

static const arch_irn_ops_if_t be_node_irn_ops_if = {
	be_node_get_irn_reg_req,
	be_node_set_irn_reg,
	be_node_get_irn_reg,
	be_node_classify,
	be_node_get_flags,
};

static const arch_irn_ops_t be_node_irn_ops = {
	&be_node_irn_ops_if
};

const void *be_node_get_arch_ops(const arch_irn_handler_t *self, const ir_node *irn)
{
	redir_proj((const ir_node **) &irn, -1);
	return is_be_node(irn) ? &be_node_irn_ops : NULL;
}

const arch_irn_handler_t be_node_irn_handler = {
	be_node_get_arch_ops
};

static int dump_node(ir_node *irn, FILE *f, dump_reason_t reason)
{
	be_node_attr_t *at = get_irn_attr(irn);
	int i;

	assert(is_be_node(irn));

	switch(reason) {
		case dump_node_opcode_txt:
			fprintf(f, get_op_name(get_irn_op(irn)));
			break;
		case dump_node_mode_txt:
			fprintf(f, get_mode_name(get_irn_mode(irn)));
			break;
		case dump_node_nodeattr_txt:
			break;
		case dump_node_info_txt:
			fprintf(f, "reg class: %s\n", at->cls ? at->cls->name : "n/a");
			for(i = 0; i < at->max_reg_data; ++i) {
				const arch_register_t *reg = at->reg_data[i].reg;
				fprintf(f, "reg #%d: %s\n", i, reg ? reg->name : "n/a");
			}

			switch(be_get_irn_opcode(irn)) {
			case beo_Spill:
				{
					be_spill_attr_t *a = (be_spill_attr_t *) at;

					ir_fprintf(f, "spill context: %+F\n", a->spill_ctx);
					if (a->ent) {
						unsigned ofs = get_entity_offset_bytes(a->ent);
						ir_fprintf(f, "spill entity: %+F offset %x (%d)\n", a->ent, ofs, ofs);
					}
					else {
						ir_fprintf(f, "spill entity: n/a\n");
					}
				}
				break;

			case beo_IncSP:
				{
					be_stack_attr_t *a = (be_stack_attr_t *) at;
					fprintf(f, "offset: %u\n", a->offset);
					fprintf(f, "direction: %s\n", a->dir == be_stack_dir_along ? "along" : "against");
				}
				break;
			}

	}

	return 0;
}

/**
 * Copies the backend specific attributes from old node to new node.
 */
static void copy_attr(const ir_node *old_node, ir_node *new_node)
{
	be_node_attr_t *old_attr = get_irn_attr(old_node);
	be_node_attr_t *new_attr = get_irn_attr(new_node);
	int i;

	assert(is_be_node(old_node));
	assert(is_be_node(new_node));

	memcpy(new_attr, old_attr, get_op_attr_size(get_irn_op(old_node)));
	new_attr->reg_data = NULL;

	if(new_attr->max_reg_data > 0) {
		new_attr->reg_data = NEW_ARR_D(be_reg_data_t, get_irg_obstack(get_irn_irg(new_node)), new_attr->max_reg_data);
		memcpy(new_attr->reg_data, old_attr->reg_data, new_attr->max_reg_data * sizeof(be_reg_data_t));

		for(i = 0; i < old_attr->max_reg_data; ++i) {
			be_req_t *r;

			r = &new_attr->reg_data[i].req;
			r->req.limited_env = r;

			r = &new_attr->reg_data[i].in_req;
			r->req.limited_env = r;
		}
	}
}

static const ir_op_ops be_node_op_ops = {
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	copy_attr,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	dump_node,
	NULL
};

pset *nodes_live_at(const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live)
{
	firm_dbg_module_t *dbg = firm_dbg_register("firm.be.node");
	const ir_node *bl      = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_node *irn;
	irn_live_t *li;

	live_foreach(bl, li) {
		ir_node *irn = (ir_node *) li->irn;
		if(live_is_end(li) && arch_irn_consider_in_reg_alloc(arch_env, cls, irn))
			pset_insert_ptr(live, irn);
	}

	sched_foreach_reverse(bl, irn) {
		int i, n;
		ir_node *x;

		/*
		 * If we encounter the node we want to insert the Perm after,
		* exit immediately, so that this node is still live
		*/
		if(irn == pos)
			return live;

		DBG((dbg, LEVEL_1, "%+F\n", irn));
		for(x = pset_first(live); x; x = pset_next(live))
			DBG((dbg, LEVEL_1, "\tlive: %+F\n", x));

		if(arch_irn_consider_in_reg_alloc(arch_env, cls, irn))
			pset_remove_ptr(live, irn);

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);

			if(arch_irn_consider_in_reg_alloc(arch_env, cls, op))
				pset_insert_ptr(live, op);
		}
	}

	return live;
}

ir_node *insert_Perm_after(const arch_env_t *arch_env,
						   const arch_register_class_t *cls,
						   dom_front_info_t *dom_front,
						   ir_node *pos)
{
	ir_node *bl                 = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_graph *irg               = get_irn_irg(bl);
	pset *live                  = pset_new_ptr_default();
	firm_dbg_module_t *dbg      = firm_dbg_register("be.node");

	ir_node *curr, *irn, *perm, **nodes;
	int i, n;

	DBG((dbg, LEVEL_1, "Insert Perm after: %+F\n", pos));

	if(!nodes_live_at(arch_env, cls, pos, live));

	n = pset_count(live);

	if(n == 0)
		return NULL;

	nodes = malloc(n * sizeof(nodes[0]));

	DBG((dbg, LEVEL_1, "live:\n"));
	for(irn = pset_first(live), i = 0; irn; irn = pset_next(live), i++) {
		DBG((dbg, LEVEL_1, "\t%+F\n", irn));
		nodes[i] = irn;
	}

	perm = be_new_Perm(cls, irg, bl, n, nodes);
	sched_add_after(pos, perm);
	free(nodes);

	curr = perm;
	for(i = 0; i < n; ++i) {
		ir_node *copies[2];
		ir_node *perm_op = get_irn_n(perm, i);
		const arch_register_t *reg = arch_get_irn_register(arch_env, perm_op);

		ir_mode *mode = get_irn_mode(perm_op);
		ir_node *proj = new_r_Proj(irg, bl, perm, mode, i);
		arch_set_irn_register(arch_env, proj, reg);

		sched_add_after(curr, proj);
		curr = proj;

		copies[0] = perm_op;
		copies[1] = proj;
		be_ssa_constr(dom_front, 2, copies);
	}
	return perm;
}
