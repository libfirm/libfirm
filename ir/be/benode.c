/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend node support for generic backend nodes.
 * @author      Sebastian Hack
 * @date        17.05.2005
 *
 * Backend node support for generic backend nodes.
 * This file provides Perm, and Copy nodes.
 */
#include <stdlib.h>

#include "beirg.h"
#include "obst.h"
#include "set.h"
#include "pmap.h"
#include "util.h"
#include "debug.h"
#include "fourcc.h"
#include "bitfiddle.h"
#include "raw_bitset.h"
#include "panic.h"
#include "array.h"

#include "irop_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irbackedge_t.h"
#include "irverify_t.h"
#include "irgopt.h"

#include "be_t.h"
#include "belive.h"
#include "besched.h"
#include "benode.h"
#include "bearch.h"
#include "bedump.h"

typedef struct be_node_attr_t {
	except_attr exc;
} be_node_attr_t;

/** The be_Return nodes attribute type. */
typedef struct {
	be_node_attr_t base;
	int            num_ret_vals; /**< number of return values */
	unsigned       pop;          /**< number of bytes that should be popped */
	int            emit_pop;     /**< if set, emit pop bytes, even if pop = 0 */
} be_return_attr_t;

/** The be_IncSP attribute type. */
typedef struct {
	be_node_attr_t base;
	int            offset; /**< The offset by which the stack shall be
	                            expanded/shrinked. */
	unsigned       align;  /**< alignment after the IncSP (0=no alignment) */
} be_incsp_attr_t;

typedef struct {
	be_node_attr_t base;
	ir_entity    **in_entities;
	ir_entity    **out_entities;
	int            offset;
} be_memperm_attr_t;

static unsigned be_opcode_start;
ir_op *op_be_Perm;
ir_op *op_be_MemPerm;
ir_op *op_be_Copy;
ir_op *op_be_Keep;
ir_op *op_be_CopyKeep;
ir_op *op_be_IncSP;

#define be_op_tag FOURCC('B', 'E', '\0', '\0')

/**
 * Compare the attributes of two be_IncSP nodes.
 */
static int be_incsp_attrs_equal(const ir_node *a, const ir_node *b)
{
	const be_incsp_attr_t *attr_a = (const be_incsp_attr_t*)get_irn_generic_attr_const(a);
	const be_incsp_attr_t *attr_b = (const be_incsp_attr_t*)get_irn_generic_attr_const(b);
	return attr_a->offset == attr_b->offset && attrs_equal_be_node(a, b);
}

static arch_register_req_t *allocate_reg_req(ir_graph *const irg)
{
	struct obstack *obst = be_get_be_obst(irg);

	arch_register_req_t *req = OALLOCZ(obst, arch_register_req_t);
	return req;
}

void be_set_constr_in(ir_node *node, int pos, const arch_register_req_t *req)
{
	backend_info_t *info = be_get_info(node);
	assert(pos < get_irn_arity(node));
	info->in_reqs[pos] = req;
}

void be_set_constr_out(ir_node *node, int pos, const arch_register_req_t *req)
{
	backend_info_t *info = be_get_info(node);
	info->out_infos[pos].req = req;
}

/**
 * Initializes the generic attribute of all be nodes and return it.
 */
static void init_node_attr(ir_node *node, int n_inputs, int n_outputs,
                           arch_irn_flags_t flags)
{
	assert(n_outputs >= 0);
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = be_get_be_obst(irg);
	backend_info_t *info = be_get_info(node);

	const arch_register_req_t **in_reqs;
	if (n_inputs >= 0) {
		int i;
		assert(n_inputs == get_irn_arity(node));
		in_reqs = OALLOCN(obst, const arch_register_req_t*, n_inputs);
		for (i = 0; i < n_inputs; ++i) {
			in_reqs[i] = arch_no_register_req;
		}
	} else {
		in_reqs = NEW_ARR_F(const arch_register_req_t*, 0);
	}
	info->in_reqs = in_reqs;

	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_outputs);
	for (int i = 0; i < n_outputs; ++i) {
		info->out_infos[i].req = arch_no_register_req;
	}
	info->flags = flags;
}

static void add_register_req_in(ir_node *node, const arch_register_req_t *req)
{
	backend_info_t *info = be_get_info(node);
	ARR_APP1(const arch_register_req_t*, info->in_reqs, req);
}

ir_node *be_new_Perm(arch_register_class_t const *const cls, ir_node *const block, int const n, ir_node *const *const in)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *irn = new_ir_node(NULL, irg, block, op_be_Perm, mode_T, n, in);
	init_node_attr(irn, n, n, arch_irn_flags_none);
	be_node_attr_t *attr = (be_node_attr_t*)get_irn_generic_attr(irn);
	attr->exc.pin_state = op_pin_state_pinned;
	for (int i = 0; i < n; ++i) {
		const ir_node             *input = in[i];
		const arch_register_req_t *req   = arch_get_irn_register_req(input);
		if (req->width == 1) {
			be_set_constr_in(irn, i, cls->class_req);
			be_set_constr_out(irn, i, cls->class_req);
		} else {
			arch_register_req_t *const new_req = allocate_reg_req(irg);
			new_req->cls   = cls;
			new_req->type  = (req->type & arch_register_req_type_aligned);
			new_req->width = req->width;
			be_set_constr_in(irn, i, new_req);
			be_set_constr_out(irn, i, new_req);
		}
	}

	return irn;
}

void be_Perm_reduce(ir_node *perm, int new_size, int *map)
{
	assert(be_is_Perm(perm));
	int arity = get_irn_arity(perm);
	assert(new_size <= arity);
	const arch_register_req_t **old_in_reqs
		= ALLOCAN(const arch_register_req_t*, arity);
	reg_out_info_t *old_infos = ALLOCAN(reg_out_info_t, arity);
	backend_info_t *info      = be_get_info(perm);
	ir_node       **new_in    = ALLOCAN(ir_node*, new_size);

	/* save the old register data */
	MEMCPY(old_in_reqs, info->in_reqs, arity);
	MEMCPY(old_infos, info->out_infos, arity);

	/* compose the new in array and set the new register data directly */
	for (int i = 0; i < new_size; ++i) {
		int idx = map[i];
		new_in[i]          = get_irn_n(perm, idx);
		info->in_reqs[i]   = old_in_reqs[idx];
		info->out_infos[i] = old_infos[idx];
	}
	set_irn_in(perm, new_size, new_in);
}

ir_node *be_new_MemPerm(ir_node *const block, int n, ir_node *const *const in)
{
	ir_graph *const irg = get_irn_irg(block);
	ir_node  *const irn = new_ir_node(NULL, irg, block, op_be_MemPerm, mode_T, n, in);

	init_node_attr(irn, n, n, arch_irn_flags_none);

	be_memperm_attr_t *attr = (be_memperm_attr_t*)get_irn_generic_attr(irn);
	attr->in_entities  = OALLOCNZ(get_irg_obstack(irg), ir_entity*, n);
	attr->out_entities = OALLOCNZ(get_irg_obstack(irg), ir_entity*, n);
	attr->offset       = 0;
	return irn;
}

ir_node *be_new_Copy(ir_node *bl, ir_node *op)
{
	ir_graph *irg  = get_irn_irg(bl);
	ir_node  *in[] = { op };
	ir_node  *res  = new_ir_node(NULL, irg, bl, op_be_Copy, get_irn_mode(op),
	                             ARRAY_SIZE(in), in);
	init_node_attr(res, 1, 1, arch_irn_flags_none);
	be_node_attr_t *attr = (be_node_attr_t*) get_irn_generic_attr(res);
	attr->exc.pin_state = op_pin_state_floats;

	const arch_register_req_t   *in_req = arch_get_irn_register_req(op);
	const arch_register_class_t *cls    = in_req->cls;
	be_node_set_reg_class_in(res, 0, cls);
	be_node_set_reg_class_out(res, 0, cls);

	arch_register_req_t *const req = allocate_reg_req(irg);
	req->cls  = cls;
	req->type = arch_register_req_type_should_be_same
	            | (in_req->type & arch_register_req_type_aligned);
	req->other_same = 1U << 0;
	req->width      = in_req->width;
	be_set_constr_out(res, 0, req);
	return res;
}

ir_node *be_get_Copy_op(const ir_node *cpy)
{
	return get_irn_n(cpy, n_be_Copy_op);
}

ir_node *be_new_Keep(ir_node *const block, int const n, ir_node *const *const in)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *res = new_ir_node(NULL, irg, block, op_be_Keep, mode_ANY, -1, NULL);
	init_node_attr(res, -1, 1, arch_irn_flag_schedule_first);
	be_node_attr_t *attr = (be_node_attr_t*) get_irn_generic_attr(res);
	attr->exc.pin_state = op_pin_state_pinned;

	for (int i = 0; i < n; ++i) {
		ir_node *pred = in[i];
		add_irn_n(res, pred);
		const arch_register_req_t *req = arch_get_irn_register_req(pred);
		req = req->cls != NULL ? req->cls->class_req : arch_no_register_req;
		add_register_req_in(res, req);
	}
	keep_alive(res);
	return res;
}

void be_Keep_add_node(ir_node *keep, const arch_register_class_t *cls, ir_node *node)
{
	assert(be_is_Keep(keep));
	add_irn_n(keep, node);
	add_register_req_in(keep, cls->class_req);
}

ir_node *be_new_IncSP(const arch_register_t *sp, ir_node *bl,
                      ir_node *old_sp, int offset, unsigned align)
{
	ir_graph *irg = get_irn_irg(bl);
	ir_node  *in[] = { old_sp };
	ir_node  *irn  = new_ir_node(NULL, irg, bl, op_be_IncSP, sp->cls->mode, ARRAY_SIZE(in), in);
	init_node_attr(irn, 1, 1, arch_irn_flags_none);
	be_incsp_attr_t *a    = (be_incsp_attr_t*)get_irn_generic_attr(irn);
	a->offset             = offset;
	a->align              = align;
	a->base.exc.pin_state = op_pin_state_pinned;

	/* Set output constraint to stack register. */
	be_node_set_reg_class_in(irn, 0, sp->cls);
	be_set_constr_single_reg_out(irn, 0, sp, arch_register_req_type_produces_sp);
	return irn;
}

ir_node *be_new_CopyKeep(ir_node *bl, ir_node *src, int n, ir_node *in_keep[])
{
	ir_mode  *mode  = get_irn_mode(src);
	ir_graph *irg   = get_irn_irg(bl);
	int       arity = n+1;
	ir_node **in    = ALLOCAN(ir_node*, arity);
	in[0] = src;
	MEMCPY(&in[1], in_keep, n);
	ir_node *irn = new_ir_node(NULL, irg, bl, op_be_CopyKeep, mode, arity, in);
	init_node_attr(irn, arity, 1, arch_irn_flag_schedule_first);
	be_node_attr_t *attr = (be_node_attr_t*)get_irn_generic_attr(irn);
	attr->exc.pin_state = op_pin_state_floats;
	const arch_register_req_t   *req  = arch_get_irn_register_req(src);
	const arch_register_class_t *cls  = req->cls;
	be_node_set_reg_class_in(irn, 0, cls);
	be_node_set_reg_class_out(irn, 0, cls);
	for (int i = 0; i < n; ++i) {
		ir_node *pred = in_keep[i];
		const arch_register_req_t *req = arch_get_irn_register_req(pred);
		req = req->cls != NULL ? req->cls->class_req : arch_no_register_req;
		be_set_constr_in(irn, i+1, req);
	}
	return irn;
}

ir_node *be_new_CopyKeep_single(ir_node *bl, ir_node *src, ir_node *keep)
{
	return be_new_CopyKeep(bl, src, 1, &keep);
}

ir_node *be_get_CopyKeep_op(const ir_node *cpy)
{
	return get_irn_n(cpy, n_be_CopyKeep_op);
}

void be_set_CopyKeep_op(ir_node *cpy, ir_node *op)
{
	set_irn_n(cpy, n_be_CopyKeep_op, op);
}

void be_set_MemPerm_in_entity(const ir_node *irn, unsigned n, ir_entity *ent)
{
	assert(n < be_get_MemPerm_entity_arity(irn));
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);
	attr->in_entities[n] = ent;
}

ir_entity* be_get_MemPerm_in_entity(const ir_node* irn, unsigned n)
{
	assert(n < be_get_MemPerm_entity_arity(irn));
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);
	return attr->in_entities[n];
}

void be_set_MemPerm_out_entity(const ir_node *irn, unsigned n, ir_entity *ent)
{
	assert(n < be_get_MemPerm_entity_arity(irn));
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);
	attr->out_entities[n] = ent;
}

ir_entity* be_get_MemPerm_out_entity(const ir_node* irn, unsigned n)
{
	assert(n < be_get_MemPerm_entity_arity(irn));
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);
	return attr->out_entities[n];
}

void be_set_MemPerm_offset(ir_node *irn, int offset)
{
	assert(be_is_MemPerm(irn));
	be_memperm_attr_t *attr = (be_memperm_attr_t*)get_irn_generic_attr(irn);
	attr->offset = offset;
}

int be_get_MemPerm_offset(const ir_node *irn)
{
	assert(be_is_MemPerm(irn));
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);
	return attr->offset;
}

unsigned be_get_MemPerm_entity_arity(const ir_node *irn)
{
	assert(be_is_MemPerm(irn));
	return get_irn_arity(irn);
}

const arch_register_req_t *be_create_reg_req(struct obstack *obst,
		const arch_register_t *reg, arch_register_req_type_t additional_types)
{
	arch_register_class_t const *cls     = reg->cls;
	unsigned                    *limited = rbitset_obstack_alloc(obst, cls->n_regs);
	rbitset_set(limited, reg->index);
	arch_register_req_t *req = OALLOC(obst, arch_register_req_t);
	req->type    = arch_register_req_type_limited | additional_types;
	req->cls     = cls;
	req->limited = limited;
	req->width   = 1;
	return req;
}

void be_set_constr_single_reg_in(ir_node *node, int pos,
		const arch_register_t *reg, arch_register_req_type_t additional_types)
{
	const arch_register_req_t *req;
	if (additional_types == 0) {
		req = reg->single_req;
	} else {
		ir_graph       *irg  = get_irn_irg(node);
		struct obstack *obst = be_get_be_obst(irg);
		req = be_create_reg_req(obst, reg, additional_types);
	}
	be_set_constr_in(node, pos, req);
}

void be_set_constr_single_reg_out(ir_node *node, int pos,
		const arch_register_t *reg, arch_register_req_type_t additional_types)
{
	ir_graph                  *irg  = get_irn_irg(node);
	be_irg_t                  *birg = be_birg_from_irg(irg);

	/* if we have an ignore register, add ignore flag and just assign it */
	if (!rbitset_is_set(birg->allocatable_regs, reg->global_index))
		additional_types |= arch_register_req_type_ignore;

	const arch_register_req_t *req;
	if (additional_types == 0) {
		req = reg->single_req;
	} else {
		struct obstack *obst = be_get_be_obst(irg);
		req = be_create_reg_req(obst, reg, additional_types);
	}

	arch_set_irn_register_out(node, pos, reg);
	be_set_constr_out(node, pos, req);
}

void be_node_set_reg_class_in(ir_node *irn, int pos,
                              const arch_register_class_t *cls)
{
	be_set_constr_in(irn, pos, cls->class_req);
}

void be_node_set_reg_class_out(ir_node *irn, int pos,
                               const arch_register_class_t *cls)
{
	be_set_constr_out(irn, pos, cls->class_req);
}

ir_node *be_get_IncSP_pred(ir_node *irn)
{
	assert(be_is_IncSP(irn));
	return get_irn_n(irn, 0);
}

void be_set_IncSP_pred(ir_node *incsp, ir_node *pred)
{
	assert(be_is_IncSP(incsp));
	set_irn_n(incsp, 0, pred);
}

void be_set_IncSP_offset(ir_node *irn, int offset)
{
	assert(be_is_IncSP(irn));
	be_incsp_attr_t *a = (be_incsp_attr_t*)get_irn_generic_attr(irn);
	a->offset = offset;
}

int be_get_IncSP_offset(const ir_node *irn)
{
	assert(be_is_IncSP(irn));
	const be_incsp_attr_t *a = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
	return a->offset;
}

unsigned be_get_IncSP_align(const ir_node *irn)
{
	assert(be_is_IncSP(irn));
	const be_incsp_attr_t *a = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
	return a->align;
}

static ir_entity *be_node_get_frame_entity(const ir_node *irn)
{
	if (be_is_MemPerm(irn))
		return be_get_MemPerm_in_entity(irn, 0);
	return NULL;
}

static void be_node_set_frame_offset(ir_node *irn, int offset)
{
	if (be_is_MemPerm(irn))
		be_set_MemPerm_offset(irn, offset);
}

static int be_node_get_sp_bias(const ir_node *irn)
{
	if (be_is_IncSP(irn))
		return be_get_IncSP_offset(irn);
	return 0;
}

/* for be nodes */
static const arch_irn_ops_t be_node_irn_ops = {
	.get_frame_entity = be_node_get_frame_entity,
	.set_frame_offset = be_node_set_frame_offset,
	.get_sp_bias      = be_node_get_sp_bias,
};

static unsigned get_start_reg_index(ir_graph *irg, const arch_register_t *reg)
{
	/* do a naive linear search... */
	ir_node *start  = get_irg_start(irg);
	be_foreach_out(start, i) {
		arch_register_req_t const *const out_req = arch_get_irn_register_req_out(start, i);
		if (!arch_register_req_is(out_req, limited))
			continue;
		if (out_req->cls != reg->cls)
			continue;
		if (!rbitset_is_set(out_req->limited, reg->index))
			continue;
		return i;
	}
	panic("Tried querying undefined register '%s' at Start", reg->name);
}

ir_node *be_get_initial_reg_value(ir_graph *irg, const arch_register_t *reg)
{
	unsigned const i     = get_start_reg_index(irg, reg);
	ir_node *const start = get_irg_start(irg);
	ir_node *const proj  = get_Proj_for_pn(start, i);
	return proj ? proj : new_r_Proj(start, reg->cls->mode, i);
}

static ir_entity* dummy_get_frame_entity(const ir_node *node)
{
	(void)node;
	return NULL;
}

static void dummy_set_frame_offset(ir_node *node, int bias)
{
	(void)node;
	(void)bias;
	panic("should not be called");
}

static int dummy_get_sp_bias(const ir_node *node)
{
	(void)node;
	return 0;
}

/* for "middleend" nodes */
static const arch_irn_ops_t dummy_be_irn_ops = {
	.get_frame_entity = dummy_get_frame_entity,
	.set_frame_offset = dummy_set_frame_offset,
	.get_sp_bias      = dummy_get_sp_bias,
};

ir_node *be_new_Phi(ir_node *block, int n_ins, ir_node **ins, ir_mode *mode,
                    const arch_register_req_t *req)
{
	ir_graph *irg  = get_irn_irg(block);
	ir_node  *phi = new_ir_node(NULL, irg, block, op_Phi, mode, n_ins, ins);
	phi->attr.phi.u.backedge = new_backedge_arr(get_irg_obstack(irg), n_ins);
	struct obstack *obst = be_get_be_obst(irg);
	backend_info_t *info = be_get_info(phi);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, 1);
	info->in_reqs   = OALLOCN(obst, const arch_register_req_t*, n_ins);

	info->out_infos[0].req = req;
	for (int i = 0; i < n_ins; ++i) {
		info->in_reqs[i] = req;
	}
	verify_new_node(irg, phi);
	phi = optimize_node(phi);
	return phi;
}

void be_set_phi_reg_req(ir_node *node, const arch_register_req_t *req)
{
	assert(mode_is_data(get_irn_mode(node)));
	backend_info_t *info = be_get_info(node);
	info->out_infos[0].req = req;
	for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		info->in_reqs[i] = req;
	}
}

void be_dump_phi_reg_reqs(FILE *F, const ir_node *node, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fputs(get_irn_opname(node), F);
		break;
	case dump_node_mode_txt:
		fprintf(F, "%s", get_mode_name(get_irn_mode(node)));
		break;
	case dump_node_nodeattr_txt:
		break;
	case dump_node_info_txt: {
		ir_graph *irg = get_irn_irg(node);
		if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
			be_dump_reqs_and_registers(F, node);
		}
		break;
	}
	}
}

static const arch_irn_ops_t phi_irn_ops = {
	dummy_get_frame_entity,
	dummy_set_frame_offset,
	dummy_get_sp_bias,
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* perform_memory_operand  */
};

/**
 * ir_op-Operation: dump a be node to file
 */
static void dump_node(FILE *f, const ir_node *irn, dump_reason_t reason)
{
	assert(is_be_node(irn));

	switch (reason) {
	case dump_node_opcode_txt:
		fputs(get_irn_opname(irn), f);
		break;
	case dump_node_mode_txt:
		if (be_is_Copy(irn) || be_is_CopyKeep(irn))
			fprintf(f, "%s", get_mode_name(get_irn_mode(irn)));
		break;
	case dump_node_nodeattr_txt:
		if (be_is_IncSP(irn)) {
			const be_incsp_attr_t *attr = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
			fprintf(f, " [%d] ", attr->offset);
		}
		break;
	case dump_node_info_txt:
		be_dump_reqs_and_registers(f, irn);

		switch (get_be_irn_opcode(irn)) {
		case beo_IncSP: {
			const be_incsp_attr_t *a = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
			fprintf(f, "align: %u\n", a->align);
			fprintf(f, "offset: %d\n", a->offset);
			break;
		}
		case beo_MemPerm: {
			for (unsigned i = 0; i < be_get_MemPerm_entity_arity(irn); ++i) {
				ir_entity *in  = be_get_MemPerm_in_entity(irn, i);
				ir_entity *out = be_get_MemPerm_out_entity(irn, i);
				if (in != NULL)
					fprintf(f, "\nin[%u]: %s\n", i, get_entity_name(in));
				if (out != NULL)
					fprintf(f, "\nout[%u]: %s\n", i, get_entity_name(out));
			}
			break;
		}

		default:
			break;
		}
	}
}

/**
 * ir_op-Operation:
 * Copies the backend specific attributes from old node to new node.
 */
static void copy_attr(ir_graph *irg, const ir_node *old_node, ir_node *new_node)
{
	assert(is_be_node(old_node));
	assert(is_be_node(new_node));

	const void     *old_attr = get_irn_generic_attr_const(old_node);
	void           *new_attr = get_irn_generic_attr(new_node);
	struct obstack *obst     = be_get_be_obst(irg);
	backend_info_t *old_info = be_get_info(old_node);
	backend_info_t *new_info = be_get_info(new_node);
	memcpy(new_attr, old_attr, get_op_attr_size(get_irn_op(old_node)));
	new_info->flags     = old_info->flags;
	new_info->out_infos = old_info->out_infos ? DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos) : NULL;

	/* input infos */
	if (old_info->in_reqs != NULL) {
		unsigned n_ins = get_irn_arity(old_node);
		/* need dynamic in infos? */
		if (is_irn_dynamic(old_node)) {
			new_info->in_reqs = NEW_ARR_F(const arch_register_req_t*, n_ins);
		} else {
			new_info->in_reqs = OALLOCN(obst,const arch_register_req_t*, n_ins);
		}
		MEMCPY(new_info->in_reqs, old_info->in_reqs, n_ins);
	} else {
		new_info->in_reqs = NULL;
	}
}

bool is_be_node(const ir_node *irn)
{
	return get_op_tag(get_irn_op(irn)) == be_op_tag;
}

be_opcode get_be_irn_opcode(const ir_node *node)
{
	assert(is_be_node(node));
	return (be_opcode) (get_irn_opcode(node) - be_opcode_start);
}

static ir_op *new_be_op(unsigned code, const char *name, op_pin_state p,
                        irop_flags flags, op_arity opar, size_t attr_size)
{
	ir_op *res = new_ir_op(code, name, p, flags, opar, 0, attr_size);
	res->ops.dump_node = dump_node;
	res->ops.copy_attr = copy_attr;
	res->ops.be_ops    = &be_node_irn_ops;
	set_op_tag(res, be_op_tag);
	return res;
}

void be_init_op(void)
{
	assert(op_be_Perm == NULL);

	be_opcode_start = get_next_ir_opcodes(beo_last+1);

	/* Acquire all needed opcodes. */
	unsigned o = be_opcode_start;
	op_be_Perm     = new_be_op(o+beo_Perm,     "be_Perm",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_variable, sizeof(be_node_attr_t));
	op_be_MemPerm  = new_be_op(o+beo_MemPerm,  "be_MemPerm",  op_pin_state_exc_pinned, irop_flag_none,                          oparity_variable, sizeof(be_memperm_attr_t));
	op_be_Copy     = new_be_op(o+beo_Copy,     "be_Copy",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_node_attr_t));
	op_be_Keep     = new_be_op(o+beo_Keep,     "be_Keep",     op_pin_state_exc_pinned, irop_flag_keep,                          oparity_dynamic,  sizeof(be_node_attr_t));
	op_be_CopyKeep = new_be_op(o+beo_CopyKeep, "be_CopyKeep", op_pin_state_exc_pinned, irop_flag_keep,                          oparity_variable, sizeof(be_node_attr_t));
	op_be_IncSP    = new_be_op(o+beo_IncSP,    "be_IncSP",    op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_incsp_attr_t));

	set_op_attrs_equal(op_be_Perm,     attrs_equal_be_node);
	set_op_attrs_equal(op_be_MemPerm,  attrs_equal_be_node);
	set_op_attrs_equal(op_be_Copy,     attrs_equal_be_node);
	set_op_attrs_equal(op_be_Keep,     attrs_equal_be_node);
	set_op_attrs_equal(op_be_CopyKeep, attrs_equal_be_node);
	set_op_attrs_equal(op_be_IncSP,    be_incsp_attrs_equal);

	/* attach out dummy_ops to middle end nodes */
	for (unsigned opc = iro_first; opc <= iro_last; ++opc) {
		ir_op *op = ir_get_opcode(opc);
		assert(op->ops.be_ops == NULL);
		op->ops.be_ops = &dummy_be_irn_ops;
	}
	op_Phi->ops.be_ops = &phi_irn_ops;
}

void be_finish_op(void)
{
	free_ir_op(op_be_Perm);     op_be_Perm     = NULL;
	free_ir_op(op_be_MemPerm);  op_be_MemPerm  = NULL;
	free_ir_op(op_be_Copy);     op_be_Copy     = NULL;
	free_ir_op(op_be_Keep);     op_be_Keep     = NULL;
	free_ir_op(op_be_CopyKeep); op_be_CopyKeep = NULL;
	free_ir_op(op_be_IncSP);    op_be_IncSP    = NULL;
}
