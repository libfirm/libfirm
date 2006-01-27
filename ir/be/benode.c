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

typedef struct {
	arch_register_req_t req;
	unsigned            negate_limited : 1;
	void                (*old_limited)(void *ptr, bitset_t *bs);
	void                *old_limited_env;
} be_req_t;

typedef struct {
	const arch_register_t *reg;
	be_req_t              req;
} be_reg_data_t;

typedef struct {
	int                         n_outs;
	const arch_register_class_t *cls;
	be_reg_data_t               *reg_data;
} be_node_attr_t;

typedef struct {
	be_node_attr_t node_attr;
	ir_node *spill_ctx;  /**< The node in whose context this spill was introduced. */
	entity *ent;     /**< The entity in the stack frame the spill writes to. */
} be_spill_attr_t;

static ir_op *op_Spill;
static ir_op *op_Reload;
static ir_op *op_Perm;
static ir_op *op_Copy;
static ir_op *op_Keep;

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

	op_Spill  = new_ir_op(beo_base + beo_Spill,  "Spill",  op_pin_state_mem_pinned, N, oparity_unary,    0, sizeof(be_spill_attr_t), &be_node_op_ops);
	op_Reload = new_ir_op(beo_base + beo_Reload, "Reload", op_pin_state_mem_pinned, N, oparity_zero,     0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Perm   = new_ir_op(beo_base + beo_Perm,   "Perm",   op_pin_state_pinned,     N, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Copy   = new_ir_op(beo_base + beo_Copy,   "Copy",   op_pin_state_pinned,     N, oparity_unary,    0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_Keep   = new_ir_op(beo_base + beo_Keep,   "Keep",   op_pin_state_pinned,     K, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);

	set_op_tag(op_Spill,  &be_node_tag);
	set_op_tag(op_Reload, &be_node_tag);
	set_op_tag(op_Perm,   &be_node_tag);
	set_op_tag(op_Copy,   &be_node_tag);
	set_op_tag(op_Keep,   &be_node_tag);
}

static void *init_node_attr(ir_node* irn, const arch_register_class_t *cls, ir_graph *irg, int n_outs)
{
	be_node_attr_t *a = get_irn_attr(irn);

	a->n_outs   = n_outs;
	a->cls      = cls;
	a->reg_data = NULL;

	if(n_outs > 0) {
		int i;

		a->reg_data = NEW_ARR_D(be_reg_data_t, get_irg_obstack(irg), n_outs);
		memset(a->reg_data, 0, n_outs * sizeof(a->reg_data[0]));
		for(i = 0; i < n_outs; ++i) {
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

be_opcode_t get_irn_be_opcode(const ir_node *irn)
{
	return is_be_node(irn) ? get_irn_opcode(irn) - beo_base : beo_NoBeOp;
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

int be_is_Spill(const ir_node *irn)
{
	return get_irn_be_opcode(irn) == beo_Spill;
}

int be_is_Reload(const ir_node *irn)
{
	return get_irn_be_opcode(irn) == beo_Reload;
}

int be_is_Copy(const ir_node *irn)
{
	return get_irn_be_opcode(irn) == beo_Copy;
}

int be_is_Perm(const ir_node *irn)
{
	return get_irn_be_opcode(irn) == beo_Perm;
}

int be_is_Keep(const ir_node *irn)
{
	return get_irn_be_opcode(irn) == beo_Keep;
}

static void be_limited(void *data, bitset_t *bs)
{
	be_req_t *req = data;

	req->old_limited(req->old_limited_env, bs);
	if(req->negate_limited)
		bitset_flip_all(bs);
}

void be_set_Perm_out_req(ir_node *irn, int pos, const arch_register_req_t *req, unsigned negate_limited)
{
	be_node_attr_t *a   = get_irn_attr(irn);
	be_req_t       *r = &a->reg_data[pos].req;

	assert(be_is_Perm(irn));
	assert(pos >= 0 && pos < get_irn_arity(irn));
	memcpy(&r->req, req, sizeof(req[0]));

	if(arch_register_req_is(req, limited)) {
		r->old_limited     = r->req.limited;
		r->old_limited_env = r->req.limited_env;
		r->req.limited     = be_limited;
		r->req.limited_env = r;
		r->negate_limited  = negate_limited;
	}
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

		else if(get_irn_be_opcode(irn) == beo_Spill)
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

	switch(get_irn_be_opcode(irn)) {
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

static void *put_out_reg_req(arch_register_req_t *req, const ir_node *irn, int out_pos)
{
	const be_node_attr_t *a = get_irn_attr(irn);


	if(out_pos < a->n_outs)
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

	req->type = arch_register_req_type_none;
	req->cls  = NULL;

	switch(get_irn_be_opcode(irn)) {
	case beo_Spill:
	case beo_Copy:
	case beo_Keep:
	case beo_Perm:
		if(pos < n) {
			req->type = arch_register_req_type_normal;
			req->cls  = a->cls;
		}
		break;
	case beo_Reload:
	default:
		req = NULL;
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

static void
be_node_set_irn_reg(const void *_self, ir_node *irn, const arch_register_t *reg)
{
	int out_pos;
	be_node_attr_t *a;

	out_pos = redir_proj((const ir_node **) &irn, -1);
	a       = get_irn_attr(irn);

	assert(is_be_node(irn));
	assert(out_pos < a->n_outs && "position too high");
	a->reg_data[out_pos].reg = reg;
}

const arch_register_t *
be_node_get_irn_reg(const void *_self, const ir_node *irn)
{
	int out_pos;
	be_node_attr_t *a;

	out_pos = redir_proj((const ir_node **) &irn, -1);
	a       = get_irn_attr(irn);

	assert(is_be_node(irn));
	assert(out_pos < a->n_outs && "position too high");

	return a->reg_data[out_pos].reg;
}

arch_irn_class_t be_node_classify(const void *_self, const ir_node *irn)
{
	redir_proj((const ir_node **) &irn, -1);

	switch(get_irn_be_opcode(irn)) {
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

arch_irn_class_t be_node_get_flags(const void *_self, const ir_node *irn)
{
	return 0;
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
			fprintf(f, "reg class: %s\n", at->cls->name);
			for(i = 0; i < at->n_outs; ++i) {
				const arch_register_t *reg = at->reg_data[i].reg;
				fprintf(f, "reg #%d: %s\n", i, reg ? reg->name : "n/a");
			}

			if(get_irn_be_opcode(irn) == beo_Spill) {
				be_spill_attr_t *a = (be_spill_attr_t *) at;
				unsigned ofs = get_entity_offset_bytes(a->ent);
				ir_fprintf(f, "spill context: %+F\n", a->spill_ctx);
				ir_fprintf(f, "spill entity: %+F offset %x (%d)\n", a->ent, ofs, ofs);
			}
			break;
	}

	return 0;
}

static const ir_op_ops be_node_op_ops = {
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
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
	ir_node *bl            = get_nodes_block(pos);
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

		if(arch_irn_has_reg_class(arch_env, irn, -1, cls))
			pset_remove_ptr(live, irn);

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);

			if(arch_irn_consider_in_reg_alloc(arch_env, cls, op))
				pset_insert_ptr(live, op);
		}
	}

	return NULL;
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

	if(!nodes_live_at(arch_env, cls, pos, live))
		assert(0 && "position not found");

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
		ir_node *copies[1];
		ir_node *perm_op = get_irn_n(perm, i);
		const arch_register_t *reg = arch_get_irn_register(arch_env, perm_op);

		ir_mode *mode = get_irn_mode(perm_op);
		ir_node *proj = new_r_Proj(irg, bl, perm, mode, i);
		arch_set_irn_register(arch_env, proj, reg);

		sched_add_after(curr, proj);
		curr = proj;

		copies[0] = proj;
		be_introduce_copies(dom_front, perm_op, 1, copies);
	}
	return perm;
}
