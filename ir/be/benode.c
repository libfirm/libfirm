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
 * This file provides Perm, Copy, Spill and Reload nodes.
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
#include "error.h"
#include "array_t.h"

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
#include "belive_t.h"
#include "besched.h"
#include "benode.h"
#include "bearch.h"

#include "beirgmod.h"

typedef struct be_node_attr_t {
	except_attr  exc;
} be_node_attr_t;

typedef struct {
	be_node_attr_t base;
	bool           setup_stackframe; /**< set stackframe up */
} be_start_attr_t;

/** The be_Return nodes attribute type. */
typedef struct {
	be_node_attr_t base;
	int            num_ret_vals; /**< number of return values */
	unsigned       pop;          /**< number of bytes that should be popped */
	int            emit_pop;     /**< if set, emit pop bytes, even if pop = 0 */
	bool           destroy_stackframe; /**< if set destroys the stackframe */
} be_return_attr_t;

/** The be_IncSP attribute type. */
typedef struct {
	be_node_attr_t base;
	int            offset;    /**< The offset by which the stack shall be
	                               expanded/shrinked. */
	int            align;     /**< whether stack should be aligned after the
	                               IncSP */
} be_incsp_attr_t;

/** The be_Frame attribute type. */
typedef struct {
	be_node_attr_t  base;
	ir_entity      *ent;
	int             offset;
} be_frame_attr_t;

/** The be_Call attribute type. */
typedef struct {
	be_node_attr_t  base;
	ir_entity      *ent;        /**< called entity if this is a static call. */
	unsigned        pop;
	ir_type        *call_tp;    /**< call type, copied from the original Call */
} be_call_attr_t;

typedef struct {
	be_node_attr_t base;
	ir_entity    **in_entities;
	ir_entity    **out_entities;
} be_memperm_attr_t;

static unsigned be_opcode_start;
ir_op *op_be_Spill;
ir_op *op_be_Reload;
ir_op *op_be_Perm;
ir_op *op_be_MemPerm;
ir_op *op_be_Copy;
ir_op *op_be_Keep;
ir_op *op_be_CopyKeep;
ir_op *op_be_Call;
ir_op *op_be_Return;
ir_op *op_be_IncSP;
ir_op *op_be_AddSP;
ir_op *op_be_SubSP;
ir_op *op_be_Start;
ir_op *op_be_FrameAddr;

#define be_op_tag FOURCC('B', 'E', '\0', '\0')

/**
 * Compare the attributes of two be_FrameAddr nodes.
 *
 * @return zero if both nodes have identically attributes
 */
static int FrameAddr_cmp_attr(const ir_node *a, const ir_node *b)
{
	const be_frame_attr_t *a_attr = (const be_frame_attr_t*)get_irn_generic_attr_const(a);
	const be_frame_attr_t *b_attr = (const be_frame_attr_t*)get_irn_generic_attr_const(b);

	if (a_attr->ent != b_attr->ent || a_attr->offset != b_attr->offset)
		return 1;

	return be_nodes_equal(a, b);
}

/**
 * Compare the attributes of two be_Return nodes.
 *
 * @return zero if both nodes have identically attributes
 */
static int Return_cmp_attr(const ir_node *a, const ir_node *b)
{
	const be_return_attr_t *a_attr = (const be_return_attr_t*)get_irn_generic_attr_const(a);
	const be_return_attr_t *b_attr = (const be_return_attr_t*)get_irn_generic_attr_const(b);

	if (a_attr->num_ret_vals != b_attr->num_ret_vals)
		return 1;
	if (a_attr->pop != b_attr->pop)
		return 1;
	if (a_attr->emit_pop != b_attr->emit_pop)
		return 1;

	return be_nodes_equal(a, b);
}

/**
 * Compare the attributes of two be_IncSP nodes.
 *
 * @return zero if both nodes have identically attributes
 */
static int IncSP_cmp_attr(const ir_node *a, const ir_node *b)
{
	const be_incsp_attr_t *a_attr = (const be_incsp_attr_t*)get_irn_generic_attr_const(a);
	const be_incsp_attr_t *b_attr = (const be_incsp_attr_t*)get_irn_generic_attr_const(b);

	if (a_attr->offset != b_attr->offset)
		return 1;

	return be_nodes_equal(a, b);
}

/**
 * Compare the attributes of two be_Call nodes.
 *
 * @return zero if both nodes have identically attributes
 */
static int Call_cmp_attr(const ir_node *a, const ir_node *b)
{
	const be_call_attr_t *a_attr = (const be_call_attr_t*)get_irn_generic_attr_const(a);
	const be_call_attr_t *b_attr = (const be_call_attr_t*)get_irn_generic_attr_const(b);

	if (a_attr->ent != b_attr->ent ||
		a_attr->call_tp != b_attr->call_tp)
		return 1;

	return be_nodes_equal(a, b);
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
static void init_node_attr(ir_node *node, int n_inputs, int n_outputs)
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
}

static void add_register_req_in(ir_node *node, const arch_register_req_t *req)
{
	backend_info_t *info = be_get_info(node);
	ARR_APP1(const arch_register_req_t*, info->in_reqs, req);
}

ir_node *be_new_Spill(const arch_register_class_t *cls,
		const arch_register_class_t *cls_frame, ir_node *bl,
		ir_node *frame, ir_node *to_spill)
{
	be_frame_attr_t *a;
	ir_node         *in[2];
	ir_node         *res;
	ir_graph        *irg = get_Block_irg(bl);

	in[0]     = frame;
	in[1]     = to_spill;
	res       = new_ir_node(NULL, irg, bl, op_be_Spill, mode_M, 2, in);
	init_node_attr(res, 2, 1);
	a         = (be_frame_attr_t*) get_irn_generic_attr(res);
	a->ent    = NULL;
	a->offset = 0;
	a->base.exc.pin_state = op_pin_state_pinned;

	be_node_set_reg_class_in(res, n_be_Spill_frame, cls_frame);
	be_node_set_reg_class_in(res, n_be_Spill_val, cls);
	arch_set_irn_register_req_out(res, 0, arch_no_register_req);
	arch_add_irn_flags(res, arch_irn_flags_spill);

	return res;
}

ir_node *be_new_Reload(const arch_register_class_t *cls,
		const arch_register_class_t *cls_frame, ir_node *block,
		ir_node *frame, ir_node *mem, ir_mode *mode)
{
	ir_node  *in[2];
	ir_node  *res;
	ir_graph *irg = get_Block_irg(block);
	be_frame_attr_t *a;

	in[0] = frame;
	in[1] = mem;
	res   = new_ir_node(NULL, irg, block, op_be_Reload, mode, 2, in);

	init_node_attr(res, 2, 1);
	be_node_set_reg_class_out(res, 0, cls);

	be_node_set_reg_class_in(res, n_be_Reload_frame, cls_frame);
	arch_set_irn_flags(res, arch_irn_flags_rematerializable);

	a         = (be_frame_attr_t*) get_irn_generic_attr(res);
	a->ent    = NULL;
	a->offset = 0;
	a->base.exc.pin_state = op_pin_state_pinned;

	return res;
}

ir_node *be_get_Reload_mem(const ir_node *irn)
{
	assert(be_is_Reload(irn));
	return get_irn_n(irn, n_be_Reload_mem);
}

ir_node *be_get_Reload_frame(const ir_node *irn)
{
	assert(be_is_Reload(irn));
	return get_irn_n(irn, n_be_Reload_frame);
}

ir_node *be_get_Spill_val(const ir_node *irn)
{
	assert(be_is_Spill(irn));
	return get_irn_n(irn, n_be_Spill_val);
}

ir_node *be_get_Spill_frame(const ir_node *irn)
{
	assert(be_is_Spill(irn));
	return get_irn_n(irn, n_be_Spill_frame);
}

ir_node *be_new_Perm(arch_register_class_t const *const cls, ir_node *const block, int const n, ir_node *const *const in)
{
	int            i;
	ir_graph       *irg = get_Block_irg(block);
	be_node_attr_t *attr;

	ir_node *irn = new_ir_node(NULL, irg, block, op_be_Perm, mode_T, n, in);
	init_node_attr(irn, n, n);
	attr                = (be_node_attr_t*) get_irn_generic_attr(irn);
	attr->exc.pin_state = op_pin_state_pinned;
	for (i = 0; i < n; ++i) {
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
	int             arity      = get_irn_arity(perm);
	const arch_register_req_t **old_in_reqs
		= ALLOCAN(const arch_register_req_t*, arity);
	reg_out_info_t  *old_infos = ALLOCAN(reg_out_info_t, arity);
	backend_info_t  *info      = be_get_info(perm);
	ir_node        **new_in;
	int              i;

	assert(be_is_Perm(perm));
	assert(new_size <= arity);

	new_in = ALLOCAN(ir_node*, new_size);

	/* save the old register data */
	memcpy(old_in_reqs, info->in_reqs, arity * sizeof(old_in_reqs[0]));
	memcpy(old_infos, info->out_infos, arity * sizeof(old_infos[0]));

	/* compose the new in array and set the new register data directly */
	for (i = 0; i < new_size; ++i) {
		int idx = map[i];
		new_in[i]          = get_irn_n(perm, idx);
		info->in_reqs[i]   = old_in_reqs[idx];
		info->out_infos[i] = old_infos[idx];
	}

	set_irn_in(perm, new_size, new_in);
}

ir_node *be_new_MemPerm(ir_node *const block, int n, ir_node *const *const in)
{
	ir_graph                     *irg       = get_Block_irg(block);
	const arch_env_t             *arch_env  = be_get_irg_arch_env(irg);
	ir_node                      *frame     = get_irg_frame(irg);
	const arch_register_t        *sp        = arch_env->sp;
	ir_node                      *irn;
	be_memperm_attr_t            *attr;
	ir_node                     **real_in;

	real_in = ALLOCAN(ir_node*, n + 1);
	real_in[0] = frame;
	memcpy(&real_in[1], in, n * sizeof(real_in[0]));

	irn = new_ir_node(NULL, irg, block, op_be_MemPerm, mode_T, n+1, real_in);

	init_node_attr(irn, n + 1, n);
	be_node_set_reg_class_in(irn, 0, sp->reg_class);

	attr               = (be_memperm_attr_t*)get_irn_generic_attr(irn);
	attr->in_entities  = OALLOCNZ(get_irg_obstack(irg), ir_entity*, n);
	attr->out_entities = OALLOCNZ(get_irg_obstack(irg), ir_entity*, n);

	return irn;
}

ir_node *be_new_Copy(ir_node *bl, ir_node *op)
{
	ir_node *in[1];
	ir_node *res;
	be_node_attr_t *attr;
	ir_graph *irg = get_Block_irg(bl);
	const arch_register_req_t   *in_req = arch_get_irn_register_req(op);
	const arch_register_class_t *cls    = in_req->cls;

	in[0] = op;
	res   = new_ir_node(NULL, irg, bl, op_be_Copy, get_irn_mode(op), 1, in);
	init_node_attr(res, 1, 1);
	attr = (be_node_attr_t*) get_irn_generic_attr(res);
	attr->exc.pin_state = op_pin_state_floats;
	be_node_set_reg_class_in(res, 0, cls);
	be_node_set_reg_class_out(res, 0, cls);

	arch_register_req_t *const req = allocate_reg_req(irg);
	req->cls        = cls;
	req->type       = arch_register_req_type_should_be_same
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
	int i;
	ir_node *res;
	ir_graph *irg = get_Block_irg(block);
	be_node_attr_t *attr;

	res = new_ir_node(NULL, irg, block, op_be_Keep, mode_ANY, -1, NULL);
	init_node_attr(res, -1, 1);
	attr = (be_node_attr_t*) get_irn_generic_attr(res);
	attr->exc.pin_state = op_pin_state_pinned;

	for (i = 0; i < n; ++i) {
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

ir_node *be_new_Call(dbg_info *const dbg, ir_node *const bl, ir_node *const mem, arch_register_req_t const *const sp_req, ir_node *const sp, arch_register_req_t const *const ptr_req, ir_node *const ptr, int const n_outs, int const n, ir_node *const *const in, ir_type *const call_tp)
{
	be_call_attr_t *a;
	int real_n = n_be_Call_first_arg + n;
	ir_node **real_in;

	NEW_ARR_A(ir_node *, real_in, real_n);
	real_in[n_be_Call_mem] = mem;
	real_in[n_be_Call_sp]  = sp;
	real_in[n_be_Call_ptr] = ptr;
	memcpy(&real_in[n_be_Call_first_arg], in, n * sizeof(in[0]));

	ir_graph *const irg = get_Block_irg(bl);
	ir_node  *const irn = new_ir_node(dbg, irg, bl, op_be_Call, mode_T, real_n, real_in);
	init_node_attr(irn, real_n, n_outs);
	a                     = (be_call_attr_t*)get_irn_generic_attr(irn);
	a->ent                = NULL;
	a->call_tp            = call_tp;
	a->pop                = 0;
	a->base.exc.pin_state = op_pin_state_pinned;
	be_set_constr_in(irn, n_be_Call_sp, sp_req);
	be_set_constr_in(irn, n_be_Call_ptr, ptr_req);
	return irn;
}

ir_entity *be_Call_get_entity(const ir_node *call)
{
	const be_call_attr_t *a = (const be_call_attr_t*)get_irn_generic_attr_const(call);
	assert(be_is_Call(call));
	return a->ent;
}

void be_Call_set_entity(ir_node *call, ir_entity *ent)
{
	be_call_attr_t *a = (be_call_attr_t*)get_irn_generic_attr(call);
	assert(be_is_Call(call));
	a->ent = ent;
}

ir_type *be_Call_get_type(ir_node *call)
{
	const be_call_attr_t *a = (const be_call_attr_t*)get_irn_generic_attr_const(call);
	assert(be_is_Call(call));
	return a->call_tp;
}

void be_Call_set_type(ir_node *call, ir_type *call_tp)
{
	be_call_attr_t *a = (be_call_attr_t*)get_irn_generic_attr(call);
	assert(be_is_Call(call));
	a->call_tp = call_tp;
}

void be_Call_set_pop(ir_node *call, unsigned pop)
{
	be_call_attr_t *a = (be_call_attr_t*)get_irn_generic_attr(call);
	a->pop = pop;
}

unsigned be_Call_get_pop(const ir_node *call)
{
	const be_call_attr_t *a = (const be_call_attr_t*)get_irn_generic_attr_const(call);
	return a->pop;
}

ir_node *be_new_Return(dbg_info *const dbg, ir_node *const block, int const n_res, unsigned const pop, int const n, ir_node *const *const in)
{
	ir_graph *const irg = get_Block_irg(block);
	ir_node  *const res = new_ir_node(dbg, irg, block, op_be_Return, mode_X, n, in);
	init_node_attr(res, n, 1);
	be_set_constr_out(res, 0, arch_no_register_req);

	be_return_attr_t *const a = (be_return_attr_t*)get_irn_generic_attr(res);
	a->num_ret_vals       = n_res;
	a->pop                = pop;
	a->emit_pop           = 0;
	a->base.exc.pin_state = op_pin_state_pinned;

	return res;
}

void be_return_set_destroy_stackframe(ir_node *node, bool value)
{
	be_return_attr_t *attr = (be_return_attr_t*) get_irn_generic_attr(node);
	assert(be_is_Return(node));
	attr->destroy_stackframe = value;
}

int be_Return_get_n_rets(const ir_node *ret)
{
	const be_return_attr_t *a = (const be_return_attr_t*)get_irn_generic_attr_const(ret);
	return a->num_ret_vals;
}

unsigned be_Return_get_pop(const ir_node *ret)
{
	const be_return_attr_t *a = (const be_return_attr_t*)get_irn_generic_attr_const(ret);
	return a->pop;
}

int be_Return_get_emit_pop(const ir_node *ret)
{
	const be_return_attr_t *a = (const be_return_attr_t*)get_irn_generic_attr_const(ret);
	return a->emit_pop;
}

void be_Return_set_emit_pop(ir_node *ret, int emit_pop)
{
	be_return_attr_t *a = (be_return_attr_t*)get_irn_generic_attr(ret);
	a->emit_pop = emit_pop;
}

ir_node *be_new_IncSP(const arch_register_t *sp, ir_node *bl,
                      ir_node *old_sp, int offset, int align)
{
	be_incsp_attr_t *a;
	ir_node *irn;
	ir_node *in[1];
	ir_graph *irg = get_Block_irg(bl);

	in[0]     = old_sp;
	irn       = new_ir_node(NULL, irg, bl, op_be_IncSP, sp->reg_class->mode,
	                        ARRAY_SIZE(in), in);
	init_node_attr(irn, 1, 1);
	a                     = (be_incsp_attr_t*)get_irn_generic_attr(irn);
	a->offset             = offset;
	a->align              = align;
	a->base.exc.pin_state = op_pin_state_pinned;

	/* Set output constraint to stack register. */
	be_node_set_reg_class_in(irn, 0, sp->reg_class);
	be_set_constr_single_reg_out(irn, 0, sp, arch_register_req_type_produces_sp);

	return irn;
}

ir_node *be_new_AddSP(const arch_register_t *sp, ir_node *bl, ir_node *old_sp,
		ir_node *sz)
{
	ir_node *irn;
	ir_node *in[n_be_AddSP_last];
	ir_graph *irg;
	be_node_attr_t *attr;

	in[n_be_AddSP_old_sp] = old_sp;
	in[n_be_AddSP_size]   = sz;

	irg = get_Block_irg(bl);
	irn = new_ir_node(NULL, irg, bl, op_be_AddSP, mode_T, n_be_AddSP_last, in);
	init_node_attr(irn, n_be_AddSP_last, pn_be_AddSP_last);
	attr = (be_node_attr_t*) get_irn_generic_attr(irn);
	attr->exc.pin_state = op_pin_state_pinned;

	/* Set output constraint to stack register. */
	be_set_constr_single_reg_in(irn, n_be_AddSP_old_sp, sp,
	                            arch_register_req_type_none);
	be_node_set_reg_class_in(irn, n_be_AddSP_size, sp->reg_class);
	be_set_constr_single_reg_out(irn, pn_be_AddSP_sp, sp,
	                             arch_register_req_type_produces_sp);

	return irn;
}

ir_node *be_new_SubSP(const arch_register_t *sp, ir_node *bl, ir_node *old_sp, ir_node *sz)
{
	ir_node *irn;
	ir_node *in[n_be_SubSP_last];
	ir_graph *irg;
	be_node_attr_t *attr;

	in[n_be_SubSP_old_sp] = old_sp;
	in[n_be_SubSP_size]   = sz;

	irg = get_Block_irg(bl);
	irn = new_ir_node(NULL, irg, bl, op_be_SubSP, mode_T, n_be_SubSP_last, in);
	init_node_attr(irn, n_be_SubSP_last, pn_be_SubSP_last);
	attr = (be_node_attr_t*) get_irn_generic_attr(irn);
	attr->exc.pin_state = op_pin_state_pinned;

	/* Set output constraint to stack register. */
	be_set_constr_single_reg_in(irn, n_be_SubSP_old_sp, sp,
	                            arch_register_req_type_none);
	be_node_set_reg_class_in(irn, n_be_SubSP_size, sp->reg_class);
	be_set_constr_single_reg_out(irn, pn_be_SubSP_sp, sp, arch_register_req_type_produces_sp);

	return irn;
}

ir_node *be_new_Start(dbg_info *dbgi, ir_node *bl, int n_outs)
{
	ir_node *res;
	ir_graph *irg = get_Block_irg(bl);
	be_node_attr_t *attr;

	res = new_ir_node(dbgi, irg, bl, op_be_Start, mode_T, 0, NULL);
	init_node_attr(res, 0, n_outs);
	attr = (be_node_attr_t*) get_irn_generic_attr(res);
	attr->exc.pin_state = op_pin_state_pinned;

	return res;
}

void be_start_set_setup_stackframe(ir_node *node, bool value)
{
	be_start_attr_t *attr = (be_start_attr_t*) get_irn_generic_attr(node);
	assert(be_is_Start(node));
	attr->setup_stackframe = value;
}

ir_node *be_new_FrameAddr(const arch_register_class_t *cls_frame, ir_node *bl, ir_node *frame, ir_entity *ent)
{
	be_frame_attr_t *a;
	ir_node *irn;
	ir_node *in[1];
	ir_graph *irg = get_Block_irg(bl);

	in[0]  = frame;
	irn    = new_ir_node(NULL, irg, bl, op_be_FrameAddr, get_irn_mode(frame), 1, in);
	init_node_attr(irn, 1, 1);
	a                     = (be_frame_attr_t*)get_irn_generic_attr(irn);
	a->ent                = ent;
	a->offset             = 0;
	a->base.exc.pin_state = op_pin_state_floats;
	be_node_set_reg_class_in(irn, 0, cls_frame);
	be_node_set_reg_class_out(irn, 0, cls_frame);

	return optimize_node(irn);
}

ir_node *be_get_FrameAddr_frame(const ir_node *node)
{
	assert(be_is_FrameAddr(node));
	return get_irn_n(node, n_be_FrameAddr_ptr);
}

ir_entity *be_get_FrameAddr_entity(const ir_node *node)
{
	const be_frame_attr_t *attr = (const be_frame_attr_t*)get_irn_generic_attr_const(node);
	return attr->ent;
}

ir_node *be_new_CopyKeep(ir_node *bl, ir_node *src, int n, ir_node *in_keep[])
{
	ir_node  *irn;
	ir_node **in = ALLOCAN(ir_node*, n + 1);
	ir_graph *irg = get_Block_irg(bl);
	const arch_register_req_t   *req  = arch_get_irn_register_req(src);
	const arch_register_class_t *cls  = req->cls;
	ir_mode                     *mode = get_irn_mode(src);
	be_node_attr_t *attr;

	in[0] = src;
	memcpy(&in[1], in_keep, n * sizeof(in[0]));
	irn   = new_ir_node(NULL, irg, bl, op_be_CopyKeep, mode, n + 1, in);
	init_node_attr(irn, n + 1, 1);
	attr = (be_node_attr_t*) get_irn_generic_attr(irn);
	attr->exc.pin_state = op_pin_state_floats;
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

static bool be_has_frame_entity(const ir_node *irn)
{
	return be_is_Spill(irn) || be_is_Reload(irn) || be_is_FrameAddr(irn);
}

ir_entity *be_get_frame_entity(const ir_node *irn)
{
	if (be_has_frame_entity(irn)) {
		const be_frame_attr_t *a = (const be_frame_attr_t*)get_irn_generic_attr_const(irn);
		return a->ent;
	}
	return NULL;
}

int be_get_frame_offset(const ir_node *irn)
{
	assert(is_be_node(irn));
	if (be_has_frame_entity(irn)) {
		const be_frame_attr_t *a = (const be_frame_attr_t*)get_irn_generic_attr_const(irn);
		return a->offset;
	}
	return 0;
}

void be_set_MemPerm_in_entity(const ir_node *irn, int n, ir_entity *ent)
{
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);

	assert(be_is_MemPerm(irn));
	assert(n < be_get_MemPerm_entity_arity(irn));

	attr->in_entities[n] = ent;
}

ir_entity* be_get_MemPerm_in_entity(const ir_node* irn, int n)
{
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);

	assert(be_is_MemPerm(irn));
	assert(n < be_get_MemPerm_entity_arity(irn));

	return attr->in_entities[n];
}

void be_set_MemPerm_out_entity(const ir_node *irn, int n, ir_entity *ent)
{
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);

	assert(be_is_MemPerm(irn));
	assert(n < be_get_MemPerm_entity_arity(irn));

	attr->out_entities[n] = ent;
}

ir_entity* be_get_MemPerm_out_entity(const ir_node* irn, int n)
{
	const be_memperm_attr_t *attr = (const be_memperm_attr_t*)get_irn_generic_attr_const(irn);

	assert(be_is_MemPerm(irn));
	assert(n < be_get_MemPerm_entity_arity(irn));

	return attr->out_entities[n];
}

int be_get_MemPerm_entity_arity(const ir_node *irn)
{
	return get_irn_arity(irn) - 1;
}

const arch_register_req_t *be_create_reg_req(struct obstack *obst,
		const arch_register_t *reg, arch_register_req_type_t additional_types)
{
	arch_register_req_t         *req = OALLOC(obst, arch_register_req_t);
	const arch_register_class_t *cls = reg->reg_class;
	unsigned                    *limited_bitset;

	limited_bitset = rbitset_obstack_alloc(obst, arch_register_class_n_regs(cls));
	rbitset_set(limited_bitset, reg->index);

	req->type    = arch_register_req_type_limited | additional_types;
	req->cls     = cls;
	req->limited = limited_bitset;
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
	const arch_register_req_t *req;

	/* if we have an ignore register, add ignore flag and just assign it */
	if (!rbitset_is_set(birg->allocatable_regs, reg->global_index)) {
		additional_types |= arch_register_req_type_ignore;
	}

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
	be_incsp_attr_t *a = (be_incsp_attr_t*)get_irn_generic_attr(irn);
	assert(be_is_IncSP(irn));
	a->offset = offset;
}

int be_get_IncSP_offset(const ir_node *irn)
{
	const be_incsp_attr_t *a = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
	assert(be_is_IncSP(irn));
	return a->offset;
}

int be_get_IncSP_align(const ir_node *irn)
{
	const be_incsp_attr_t *a = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
	assert(be_is_IncSP(irn));
	return a->align;
}

static ir_entity *be_node_get_frame_entity(const ir_node *irn)
{
	return be_get_frame_entity(irn);
}

void be_node_set_frame_entity(ir_node *irn, ir_entity *ent)
{
	be_frame_attr_t *a;

	assert(be_has_frame_entity(irn));

	a = (be_frame_attr_t*)get_irn_generic_attr(irn);
	a->ent = ent;
}

static void be_node_set_frame_offset(ir_node *irn, int offset)
{
	be_frame_attr_t *a;

	if (!be_has_frame_entity(irn))
		return;

	a = (be_frame_attr_t*)get_irn_generic_attr(irn);
	a->offset = offset;
}

static int be_node_get_sp_bias(const ir_node *irn)
{
	if (be_is_IncSP(irn))
		return be_get_IncSP_offset(irn);
	if (be_is_Call(irn))
		return -(int)be_Call_get_pop(irn);
	if (be_is_Start(irn)) {
		const be_start_attr_t *attr
			= (const be_start_attr_t*) get_irn_generic_attr_const(irn);
		if (attr->setup_stackframe) {
			ir_graph *irg = get_irn_irg(irn);
			ir_type  *frame = get_irg_frame_type(irg);
			return get_type_size_bytes(frame);
		}
	}
	if (be_is_Return(irn)) {
		const be_return_attr_t *attr
			= (const be_return_attr_t*) get_irn_generic_attr_const(irn);
		if (attr->destroy_stackframe) {
			ir_graph *irg = get_irn_irg(irn);
			ir_type  *frame = get_irg_frame_type(irg);
			return -(int)get_type_size_bytes(frame);
		}
	}

	return 0;
}



/* for be nodes */
static const arch_irn_ops_t be_node_irn_ops = {
	be_node_get_frame_entity,
	be_node_set_frame_offset,
	be_node_get_sp_bias,
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};

static int get_start_reg_index(ir_graph *irg, const arch_register_t *reg)
{
	ir_node *start  = get_irg_start(irg);

	/* do a naive linear search... */
	be_foreach_out(start, i) {
		arch_register_req_t const *const out_req = arch_get_irn_register_req_out(start, i);
		if (!arch_register_req_is(out_req, limited))
			continue;
		if (out_req->cls != reg->reg_class)
			continue;
		if (!rbitset_is_set(out_req->limited, reg->index))
			continue;
		return i;
	}
	panic("Tried querying undefined register '%s' at Start", reg->name);
}

ir_node *be_get_initial_reg_value(ir_graph *irg, const arch_register_t *reg)
{
	int      i     = get_start_reg_index(irg, reg);
	ir_node *start = get_irg_start(irg);
	ir_mode *mode  = arch_register_class_mode(reg->reg_class);

	foreach_out_edge(start, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj)) // maybe End/Anchor
			continue;
		if (get_Proj_proj(proj) == i) {
			return proj;
		}
	}
	return new_r_Proj(start, mode, i);
}

static ir_entity* dummy_get_frame_entity(const ir_node *node)
{
	(void) node;
	return NULL;
}

static void dummy_set_frame_offset(ir_node *node, int bias)
{
	(void) node;
	(void) bias;
	panic("should not be called");
}

static int dummy_get_sp_bias(const ir_node *node)
{
	(void) node;
	return 0;
}

/* for "middleend" nodes */
static const arch_irn_ops_t dummy_be_irn_ops = {
	dummy_get_frame_entity,
	dummy_set_frame_offset,
	dummy_get_sp_bias,
	NULL,      /* get_op_estimated_cost */
	NULL,      /* possible_memory_operand */
	NULL,      /* perform_memory_operand */
};



ir_node *be_new_Phi(ir_node *block, int n_ins, ir_node **ins, ir_mode *mode,
                    const arch_register_req_t *req)
{
	ir_graph       *irg  = get_irn_irg(block);
	struct obstack *obst = be_get_be_obst(irg);
	backend_info_t *info;
	int             i;

	ir_node *phi = new_ir_node(NULL, irg, block, op_Phi, mode, n_ins, ins);
	phi->attr.phi.u.backedge = new_backedge_arr(get_irg_obstack(irg), n_ins);
	info = be_get_info(phi);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, 1);
	info->in_reqs   = OALLOCN(obst, const arch_register_req_t*, n_ins);

	info->out_infos[0].req = req;
	for (i = 0; i < n_ins; ++i) {
		info->in_reqs[i] = req;
	}
	irn_verify_irg(phi, irg);
	phi = optimize_node(phi);
	return phi;
}

void be_set_phi_reg_req(ir_node *node, const arch_register_req_t *req)
{
	int arity = get_irn_arity(node);
	int i;

	backend_info_t *info = be_get_info(node);
	info->out_infos[0].req = req;
	for (i = 0; i < arity; ++i) {
		info->in_reqs[i] = req;
	}

	assert(mode_is_datab(get_irn_mode(node)));
}

void be_dump_phi_reg_reqs(FILE *F, const ir_node *node, dump_reason_t reason)
{
	ir_graph *irg = get_irn_irg(node);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND))
		return;

	switch (reason) {
	case dump_node_opcode_txt:
		fputs(get_op_name(get_irn_op(node)), F);
		break;
	case dump_node_mode_txt:
		fprintf(F, "%s", get_mode_name(get_irn_mode(node)));
		break;
	case dump_node_nodeattr_txt:
		break;
	case dump_node_info_txt:
		arch_dump_reqs_and_registers(F, node);
		break;

	default:
		break;
	}
}

static const arch_irn_ops_t phi_irn_ops = {
	dummy_get_frame_entity,
	dummy_set_frame_offset,
	dummy_get_sp_bias,
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
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
			fputs(get_op_name(get_irn_op(irn)), f);
			break;
		case dump_node_mode_txt:
			if (be_is_Copy(irn) || be_is_CopyKeep(irn)) {
				fprintf(f, "%s", get_mode_name(get_irn_mode(irn)));
			}
			break;
		case dump_node_nodeattr_txt:
			if (be_is_Call(irn)) {
				const be_call_attr_t *a = (const be_call_attr_t*)get_irn_generic_attr_const(irn);
				if (a->ent)
					fprintf(f, " [%s] ", get_entity_name(a->ent));
			}
			if (be_is_IncSP(irn)) {
				const be_incsp_attr_t *attr = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
				fprintf(f, " [%d] ", attr->offset);
			}
			break;
		case dump_node_info_txt:
			arch_dump_reqs_and_registers(f, irn);

			if (be_has_frame_entity(irn)) {
				const be_frame_attr_t *a = (const be_frame_attr_t*)get_irn_generic_attr_const(irn);
				if (a->ent) {
					unsigned size = get_type_size_bytes(get_entity_type(a->ent));
					ir_fprintf(f, "frame entity: %+F, offset 0x%x (%d), size 0x%x (%d) bytes\n",
					  a->ent, a->offset, a->offset, size, size);
				}

			}

			switch (get_be_irn_opcode(irn)) {
			case beo_IncSP: {
				const be_incsp_attr_t *a = (const be_incsp_attr_t*)get_irn_generic_attr_const(irn);
				fprintf(f, "align: %d\n", a->align);
				fprintf(f, "offset: %d\n", a->offset);
				break;
			}
			case beo_Call: {
				const be_call_attr_t *a = (const be_call_attr_t*)get_irn_generic_attr_const(irn);

				if (a->ent)
					fprintf(f, "\ncalling: %s\n", get_entity_name(a->ent));
				break;
			}
			case beo_MemPerm: {
				int i;
				for (i = 0; i < be_get_MemPerm_entity_arity(irn); ++i) {
					ir_entity *in, *out;
					in = be_get_MemPerm_in_entity(irn, i);
					out = be_get_MemPerm_out_entity(irn, i);
					if (in) {
						fprintf(f, "\nin[%d]: %s\n", i, get_entity_name(in));
					}
					if (out) {
						fprintf(f, "\nout[%d]: %s\n", i, get_entity_name(out));
					}
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
	const void     *old_attr = get_irn_generic_attr_const(old_node);
	void           *new_attr = get_irn_generic_attr(new_node);
	struct obstack *obst     = be_get_be_obst(irg);
	backend_info_t *old_info = be_get_info(old_node);
	backend_info_t *new_info = be_get_info(new_node);

	assert(is_be_node(old_node));
	assert(is_be_node(new_node));

	memcpy(new_attr, old_attr, get_op_attr_size(get_irn_op(old_node)));

	new_info->flags     = old_info->flags;
	new_info->out_infos = old_info->out_infos ? DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos) : NULL;

	/* input infos */
	if (old_info->in_reqs != NULL) {
		unsigned n_ins = get_irn_arity(old_node);
		/* need dynamic in infos? */
		if (get_irn_op(old_node)->opar == oparity_dynamic) {
			new_info->in_reqs = NEW_ARR_F(const arch_register_req_t*, n_ins);
		} else {
			new_info->in_reqs = OALLOCN(obst,const arch_register_req_t*, n_ins);
		}
		memcpy(new_info->in_reqs, old_info->in_reqs,
		       n_ins * sizeof(new_info->in_reqs[0]));
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
	assert(op_be_Spill == NULL);

	be_opcode_start = get_next_ir_opcodes(beo_last+1);

	/* Acquire all needed opcodes. */
	unsigned o = be_opcode_start;
	op_be_Spill     = new_be_op(o+beo_Spill,     "be_Spill",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_frame_attr_t));
	op_be_Reload    = new_be_op(o+beo_Reload,    "be_Reload",    op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_frame_attr_t));
	op_be_Perm      = new_be_op(o+beo_Perm,      "be_Perm",      op_pin_state_exc_pinned, irop_flag_none,                          oparity_variable, sizeof(be_node_attr_t));
	op_be_MemPerm   = new_be_op(o+beo_MemPerm,   "be_MemPerm",   op_pin_state_exc_pinned, irop_flag_none,                          oparity_variable, sizeof(be_memperm_attr_t));
	op_be_Copy      = new_be_op(o+beo_Copy,      "be_Copy",      op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_node_attr_t));
	op_be_Keep      = new_be_op(o+beo_Keep,      "be_Keep",      op_pin_state_exc_pinned, irop_flag_keep,                          oparity_dynamic,  sizeof(be_node_attr_t));
	op_be_CopyKeep  = new_be_op(o+beo_CopyKeep,  "be_CopyKeep",  op_pin_state_exc_pinned, irop_flag_keep,                          oparity_variable, sizeof(be_node_attr_t));
	op_be_Call      = new_be_op(o+beo_Call,      "be_Call",      op_pin_state_exc_pinned, irop_flag_fragile|irop_flag_uses_memory, oparity_variable, sizeof(be_call_attr_t));
	op_be_Return    = new_be_op(o+beo_Return,    "be_Return",    op_pin_state_exc_pinned, irop_flag_cfopcode,                      oparity_variable, sizeof(be_return_attr_t));
	op_be_AddSP     = new_be_op(o+beo_AddSP,     "be_AddSP",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_node_attr_t));
	op_be_SubSP     = new_be_op(o+beo_SubSP,     "be_SubSP",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_node_attr_t));
	op_be_IncSP     = new_be_op(o+beo_IncSP,     "be_IncSP",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_incsp_attr_t));
	op_be_Start     = new_be_op(o+beo_Start,     "be_Start",     op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_start_attr_t));
	op_be_FrameAddr = new_be_op(o+beo_FrameAddr, "be_FrameAddr", op_pin_state_exc_pinned, irop_flag_none,                          oparity_any,      sizeof(be_frame_attr_t));

	ir_op_set_memory_index(op_be_Call, n_be_Call_mem);
	ir_op_set_fragile_indices(op_be_Call, pn_be_Call_X_regular, pn_be_Call_X_except);

	op_be_Spill->ops.node_cmp_attr     = FrameAddr_cmp_attr;
	op_be_Reload->ops.node_cmp_attr    = FrameAddr_cmp_attr;
	op_be_Perm->ops.node_cmp_attr      = be_nodes_equal;
	op_be_MemPerm->ops.node_cmp_attr   = be_nodes_equal;
	op_be_Copy->ops.node_cmp_attr      = be_nodes_equal;
	op_be_Keep->ops.node_cmp_attr      = be_nodes_equal;
	op_be_CopyKeep->ops.node_cmp_attr  = be_nodes_equal;
	op_be_Call->ops.node_cmp_attr      = Call_cmp_attr;
	op_be_Return->ops.node_cmp_attr    = Return_cmp_attr;
	op_be_AddSP->ops.node_cmp_attr     = be_nodes_equal;
	op_be_SubSP->ops.node_cmp_attr     = be_nodes_equal;
	op_be_IncSP->ops.node_cmp_attr     = IncSP_cmp_attr;
	op_be_Start->ops.node_cmp_attr     = be_nodes_equal;
	op_be_FrameAddr->ops.node_cmp_attr = FrameAddr_cmp_attr;

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
	free_ir_op(op_be_Spill);     op_be_Spill     = NULL;
	free_ir_op(op_be_Reload);    op_be_Reload    = NULL;
	free_ir_op(op_be_Perm);      op_be_Perm      = NULL;
	free_ir_op(op_be_MemPerm);   op_be_MemPerm   = NULL;
	free_ir_op(op_be_Copy);      op_be_Copy      = NULL;
	free_ir_op(op_be_Keep);      op_be_Keep      = NULL;
	free_ir_op(op_be_CopyKeep);  op_be_CopyKeep  = NULL;
	free_ir_op(op_be_Call);      op_be_Call      = NULL;
	free_ir_op(op_be_Return);    op_be_Return    = NULL;
	free_ir_op(op_be_IncSP);     op_be_IncSP     = NULL;
	free_ir_op(op_be_AddSP);     op_be_AddSP     = NULL;
	free_ir_op(op_be_SubSP);     op_be_SubSP     = NULL;
	free_ir_op(op_be_Start);     op_be_Start     = NULL;
	free_ir_op(op_be_FrameAddr); op_be_FrameAddr = NULL;
}
