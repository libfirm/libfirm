/**
 * @file   benode.c
 * @date   17.05.2005
 * @author Sebastian Hack
 *
 * Backend node support.
 *
 * This file provides Perm, Copy, Spill and Reload nodes.
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
#include "offset.h"
#include "bitfiddle.h"

#include "irop_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irprintf.h"
#include "irgwalk.h"

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "benode_t.h"

#include "beirgmod.h"

#define OUT_POS(x) (-((x) + 1))

/* Sometimes we want to put const nodes into get_irn_generic_attr ... */
#define get_irn_attr(irn) get_irn_generic_attr((ir_node *) (irn))

static unsigned be_node_tag = FOURCC('B', 'E', 'N', 'O');

#if 0
typedef enum _node_kind_t {
	node_kind_spill,
	node_kind_reload,
	node_kind_perm,
	node_kind_copy,
	node_kind_kill,
	node_kind_last
} node_kind_t;
#endif

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

/** The generic be nodes attribute type. */
typedef struct {
	int                   max_reg_data;
	be_reg_data_t         *reg_data;
} be_node_attr_t;

/** The be_Stack attribute type. */
typedef struct {
	be_node_attr_t node_attr;
	int offset;           /**< The offset by which the stack shall be increased/decreased. */
	be_stack_dir_t dir;   /**< The direction in which the stack shall be modified (expand or shrink). */
} be_stack_attr_t;

/** The be_Frame attribute type. */
typedef struct {
	be_node_attr_t node_attr;
	entity *ent;
	int offset;
} be_frame_attr_t;

/** The be_Call attribute type. */
typedef struct {
	be_node_attr_t node_attr;
	entity *ent;         /**< The called entity if this is a static call. */
	ir_type *call_tp;    /**< The call type, copied from the original Call node. */
} be_call_attr_t;

/** The be_Spill attribute type. */
typedef struct {
	be_frame_attr_t frame_attr;
	ir_node *spill_ctx;  /**< The node in whose context this spill was introduced. */
} be_spill_attr_t;

ir_op *op_be_Spill;
ir_op *op_be_Reload;
ir_op *op_be_Perm;
ir_op *op_be_Copy;
ir_op *op_be_Keep;
ir_op *op_be_CopyKeep;
ir_op *op_be_Call;
ir_op *op_be_Return;
ir_op *op_be_IncSP;
ir_op *op_be_AddSP;
ir_op *op_be_SetSP;
ir_op *op_be_RegParams;
ir_op *op_be_StackParam;
ir_op *op_be_FrameAddr;
ir_op *op_be_FrameLoad;
ir_op *op_be_FrameStore;
ir_op *op_be_Barrier;

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
#define M   irop_flag_machine


void be_node_init(void) {
	static int inited = 0;

	if(inited)
		return;

	inited = 1;

	/* Acquire all needed opcodes. */
	beo_base = get_next_ir_opcodes(beo_Last - 1);

	op_be_Spill      = new_ir_op(beo_base + beo_Spill,      "Spill",      op_pin_state_mem_pinned, N, oparity_unary,    0, sizeof(be_spill_attr_t), &be_node_op_ops);
	op_be_Reload     = new_ir_op(beo_base + beo_Reload,     "Reload",     op_pin_state_mem_pinned, N, oparity_zero,     0, sizeof(be_frame_attr_t), &be_node_op_ops);
	op_be_Perm       = new_ir_op(beo_base + beo_Perm,       "Perm",       op_pin_state_pinned,     N, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_Copy       = new_ir_op(beo_base + beo_Copy,       "Copy",       op_pin_state_floats,     N, oparity_unary,    0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_Keep       = new_ir_op(beo_base + beo_Keep,       "Keep",       op_pin_state_pinned,     K, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_CopyKeep   = new_ir_op(beo_base + beo_CopyKeep,   "CopyKeep",   op_pin_state_pinned,     K, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_Call       = new_ir_op(beo_base + beo_Call,       "Call",       op_pin_state_pinned,     N, oparity_variable, 0, sizeof(be_call_attr_t),  &be_node_op_ops);
	op_be_Return     = new_ir_op(beo_base + beo_Return,     "Return",     op_pin_state_pinned,     X, oparity_variable, 0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_AddSP      = new_ir_op(beo_base + beo_AddSP,      "AddSP",      op_pin_state_pinned,     N, oparity_unary,    0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_SetSP      = new_ir_op(beo_base + beo_SetSP,      "SetSP",      op_pin_state_pinned,     N, oparity_binary,   0, sizeof(be_stack_attr_t), &be_node_op_ops);
	op_be_IncSP      = new_ir_op(beo_base + beo_IncSP,      "IncSP",      op_pin_state_pinned,     N, oparity_binary,   0, sizeof(be_stack_attr_t), &be_node_op_ops);
	op_be_RegParams  = new_ir_op(beo_base + beo_RegParams,  "RegParams",  op_pin_state_pinned,     N, oparity_zero,     0, sizeof(be_node_attr_t),  &be_node_op_ops);
	op_be_StackParam = new_ir_op(beo_base + beo_StackParam, "StackParam", op_pin_state_pinned,     N, oparity_unary,    0, sizeof(be_frame_attr_t), &be_node_op_ops);
	op_be_FrameAddr  = new_ir_op(beo_base + beo_FrameAddr,  "FrameAddr",  op_pin_state_pinned,     N, oparity_binary,   0, sizeof(be_frame_attr_t), &be_node_op_ops);
	op_be_FrameLoad  = new_ir_op(beo_base + beo_FrameLoad,  "FrameLoad",  op_pin_state_pinned,     N, oparity_any,      0, sizeof(be_frame_attr_t), &be_node_op_ops);
	op_be_FrameStore = new_ir_op(beo_base + beo_FrameStore, "FrameStore", op_pin_state_pinned,     N, oparity_any,      0, sizeof(be_frame_attr_t), &be_node_op_ops);
	op_be_Barrier    = new_ir_op(beo_base + beo_Barrier,    "Barrier",    op_pin_state_pinned,     N, oparity_any,      0, sizeof(be_node_attr_t),  &be_node_op_ops);

	set_op_tag(op_be_Spill,      &be_node_tag);
	set_op_tag(op_be_Reload,     &be_node_tag);
	set_op_tag(op_be_Perm,       &be_node_tag);
	set_op_tag(op_be_Copy,       &be_node_tag);
	set_op_tag(op_be_Keep,       &be_node_tag);
	set_op_tag(op_be_CopyKeep,   &be_node_tag);
	set_op_tag(op_be_Call,       &be_node_tag);
	set_op_tag(op_be_Return,     &be_node_tag);
	set_op_tag(op_be_AddSP,      &be_node_tag);
	set_op_tag(op_be_SetSP,      &be_node_tag);
	set_op_tag(op_be_IncSP,      &be_node_tag);
	set_op_tag(op_be_RegParams,  &be_node_tag);
	set_op_tag(op_be_StackParam, &be_node_tag);
	set_op_tag(op_be_FrameLoad,  &be_node_tag);
	set_op_tag(op_be_FrameStore, &be_node_tag);
	set_op_tag(op_be_FrameAddr,  &be_node_tag);
	set_op_tag(op_be_Barrier,    &be_node_tag);
}

/**
 * Initializes the generic attribute of all be nodes and return ir.
 */
static void *init_node_attr(ir_node* irn, int max_reg_data)
{
	ir_graph *irg     = get_irn_irg(irn);
	be_node_attr_t *a = get_irn_attr(irn);

	memset(a, 0, sizeof(get_op_attr_size(get_irn_op(irn))));
	a->max_reg_data = max_reg_data;
	a->reg_data     = NULL;

	if(max_reg_data > 0) {
		int i;

		a->reg_data = NEW_ARR_D(be_reg_data_t, get_irg_obstack(irg), max_reg_data);
		memset(a->reg_data, 0, max_reg_data * sizeof(a->reg_data[0]));
		for(i = 0; i < max_reg_data; ++i) {
			a->reg_data[i].req.req.cls  = NULL;
			a->reg_data[i].req.req.type = arch_register_req_type_none;
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
		ir_node *irn;

		assert(pos == -1 && "Illegal pos for a Proj");
		*node = irn = get_Proj_pred(n);
		if(is_Proj(irn)) {
			assert(get_irn_mode(irn) == mode_T);
			*node = get_Proj_pred(irn);
		}
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


ir_node *be_new_Spill(const arch_register_class_t *cls, const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, ir_node *to_spill, ir_node *ctx)
{
	be_spill_attr_t *a;
	ir_node *in[2];
	ir_node *res;

	in[0] = frame;
	in[1] = to_spill;
	res   = new_ir_node(NULL, irg, bl, op_be_Spill, mode_M, 2, in);
	a     = init_node_attr(res, 2);
	a->frame_attr.ent = NULL;
	a->frame_attr.offset = 0;
	a->spill_ctx = ctx;

	be_node_set_reg_class(res, 0, cls_frame);
	be_node_set_reg_class(res, 1, cls);
	return res;
}

ir_node *be_new_Reload(const arch_register_class_t *cls, const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, ir_node *mem, ir_mode *mode)
{
	ir_node *in[2];
	ir_node *res;

	in[0] = frame;
	in[1] = mem;
	res   = new_ir_node(NULL, irg, bl, op_be_Reload, mode, 2, in);
	init_node_attr(res, 2);
	be_node_set_reg_class(res, 0, cls_frame);
	be_node_set_reg_class(res, -1, cls);
	return res;
}

ir_node *(be_get_Reload_mem)(const ir_node *irn)
{
	assert(be_is_Reload(irn));
	return get_irn_n(irn, be_pos_Reload_mem);
}

ir_node *be_new_Perm(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	int i;
	ir_node *irn = new_ir_node(NULL, irg, bl, op_be_Perm, mode_T, n, in);
	init_node_attr(irn, n);
	for(i = 0; i < n; ++i) {
		be_node_set_reg_class(irn, i, cls);
		be_node_set_reg_class(irn, OUT_POS(i), cls);
	}

	return irn;
}

ir_node *be_new_Copy(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *op)
{
	ir_node *in[1];
	ir_node *res;

	in[0] = op;
	res   = new_ir_node(NULL, irg, bl, op_be_Copy, get_irn_mode(op), 1, in);
	init_node_attr(res, 1);
	be_node_set_reg_class(res, 0, cls);
	be_node_set_reg_class(res, OUT_POS(0), cls);
	return res;
}

ir_node *be_new_Keep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	int i;
	ir_node *irn;

	irn = new_ir_node(NULL, irg, bl, op_be_Keep, mode_ANY, n, in);
	init_node_attr(irn, n);
	for(i = 0; i < n; ++i) {
		be_node_set_reg_class(irn, i, cls);
	}
	keep_alive(irn);
	return irn;
}

ir_node *be_new_Call(dbg_info *dbg, ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *sp, ir_node *ptr,
                     int n_outs, int n, ir_node *in[], ir_type *call_tp)
{
	be_call_attr_t *a;
	int real_n = be_pos_Call_first_arg + n;
	ir_node *irn;
	ir_node **real_in;

	NEW_ARR_A(ir_node *, real_in, real_n);
	real_in[be_pos_Call_mem] = mem;
	real_in[be_pos_Call_sp]  = sp;
	real_in[be_pos_Call_ptr] = ptr;
	memcpy(&real_in[be_pos_Call_first_arg], in, n * sizeof(in[0]));

	irn = new_ir_node(NULL, irg, bl, op_be_Call, mode_T, real_n, real_in);
	a = init_node_attr(irn, (n_outs > real_n ? n_outs : real_n));
	a->ent     = NULL;
	a->call_tp = call_tp;
	return irn;
}

/* Gets the call entity or NULL if this is no static call. */
entity *be_Call_get_entity(const ir_node *call) {
	be_call_attr_t *a = get_irn_attr(call);
	assert(be_is_Call(call));
	return a->ent;
}

/* Sets the call entity. */
void be_Call_set_entity(ir_node *call, entity *ent) {
	be_call_attr_t *a = get_irn_attr(call);
	assert(be_is_Call(call));
	a->ent = ent;
}

/* Gets the call type. */
ir_type *be_Call_get_type(ir_node *call) {
	be_call_attr_t *a = get_irn_attr(call);
	assert(be_is_Call(call));
	return a->call_tp;
}

/* Sets the call type. */
void be_Call_set_type(ir_node *call, ir_type *call_tp) {
	be_call_attr_t *a = get_irn_attr(call);
	assert(be_is_Call(call));
	a->call_tp = call_tp;
}


ir_node *be_new_Return(dbg_info *dbg, ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	ir_node *irn = new_ir_node(dbg, irg, bl, op_be_Return, mode_X, n, in);
	init_node_attr(irn, n);

	return irn;
}

ir_node *be_new_IncSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *mem, unsigned offset, be_stack_dir_t dir)
{
	be_stack_attr_t *a;
	ir_node *irn;
	ir_node *in[1];

	in[0]     = old_sp;
	in[1]     = mem;
	irn       = new_ir_node(NULL, irg, bl, op_be_IncSP, sp->reg_class->mode, 2, in);
	a         = init_node_attr(irn, 1);
	a->dir    = dir;
	a->offset = offset;

	be_node_set_flags(irn, -1, arch_irn_flags_ignore);

	/* Set output constraint to stack register. */
	be_node_set_reg_class(irn, 0, sp->reg_class);
	be_set_constr_single_reg(irn, -1, sp);
	be_node_set_irn_reg(NULL, irn, sp);

	return irn;
}

ir_node *be_new_AddSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *sz)
{
	be_node_attr_t *a;
	ir_node *irn;
	ir_node *in[be_pos_AddSP_last];

	in[be_pos_AddSP_old_sp] = old_sp;
	in[be_pos_AddSP_size]   = sz;

	irn      = new_ir_node(NULL, irg, bl, op_be_AddSP, mode_T, be_pos_AddSP_last, in);
	a        = init_node_attr(irn, be_pos_AddSP_last);

	be_node_set_flags(irn, OUT_POS(0), arch_irn_flags_ignore);

	/* Set output constraint to stack register. */
	be_set_constr_single_reg(irn, OUT_POS(0), sp);

	return irn;
}

ir_node *be_new_SetSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *op, ir_node *mem)
{
	be_node_attr_t *a;
	ir_node *irn;
	ir_node *in[3];

	in[0]    = mem;
	in[1]    = old_sp;
	in[2]    = op;
	irn      = new_ir_node(NULL, irg, bl, op_be_SetSP, get_irn_mode(old_sp), 3, in);
	a        = init_node_attr(irn, 3);

	be_node_set_flags(irn, OUT_POS(0), arch_irn_flags_ignore);

	/* Set output constraint to stack register. */
	be_set_constr_single_reg(irn, OUT_POS(0), sp);
	be_node_set_reg_class(irn, 1, sp->reg_class);
	be_node_set_reg_class(irn, 2, sp->reg_class);
	be_node_set_irn_reg(NULL, irn, sp);

	return irn;
}

ir_node *be_new_StackParam(const arch_register_class_t *cls, const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_mode *mode, ir_node *frame_pointer, entity *ent)
{
	be_frame_attr_t *a;
	ir_node *irn;
	ir_node *in[1];

	in[0] = frame_pointer;
	irn = new_ir_node(NULL, irg, bl, op_be_StackParam, mode, 1, in);
	a = init_node_attr(irn, 1);
	a->ent = ent;

	be_node_set_reg_class(irn, 0, cls_frame);
	be_node_set_reg_class(irn, OUT_POS(0), cls);
	return irn;
}

ir_node *be_new_RegParams(ir_graph *irg, ir_node *bl, int n_outs)
{
	ir_node *irn;
	ir_node *in[1];

	irn = new_ir_node(NULL, irg, bl, op_be_RegParams, mode_T, 0, in);
	init_node_attr(irn, n_outs);
	return irn;
}

ir_node *be_new_FrameLoad(const arch_register_class_t *cls_frame, const arch_register_class_t *cls_data,
						  ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *frame, entity *ent)
{
	be_frame_attr_t *a;
	ir_node *irn;
	ir_node *in[2];

	in[0]  = mem;
	in[1]  = frame;
	irn    = new_ir_node(NULL, irg, bl, op_be_FrameLoad, mode_T, 2, in);
	a      = init_node_attr(irn, 3);
	a->ent = ent;
	a->offset = 0;
	be_node_set_reg_class(irn, 1, cls_frame);
	be_node_set_reg_class(irn, OUT_POS(pn_Load_res), cls_data);
	return irn;
}

ir_node *be_new_FrameStore(const arch_register_class_t *cls_frame, const arch_register_class_t *cls_data,
						   ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *frame, ir_node *data, entity *ent)
{
	be_frame_attr_t *a;
	ir_node *irn;
	ir_node *in[3];

	in[0]  = mem;
	in[1]  = frame;
	in[2]  = data;
	irn    = new_ir_node(NULL, irg, bl, op_be_FrameStore, mode_T, 3, in);
	a      = init_node_attr(irn, 3);
	a->ent = ent;
	a->offset = 0;
	be_node_set_reg_class(irn, 1, cls_frame);
	be_node_set_reg_class(irn, 2, cls_data);
	return irn;
}

ir_node *be_new_FrameAddr(const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, entity *ent)
{
	be_frame_attr_t *a;
	ir_node *irn;
	ir_node *in[1];

	in[0]  = frame;
	irn    = new_ir_node(NULL, irg, bl, op_be_FrameAddr, get_irn_mode(frame), 1, in);
	a      = init_node_attr(irn, 1);
	a->ent = ent;
	a->offset = 0;
	be_node_set_reg_class(irn, 0, cls_frame);
	be_node_set_reg_class(irn, OUT_POS(0), cls_frame);
	return irn;
}

ir_node *be_new_CopyKeep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *src, int n, ir_node *in_keep[], ir_mode *mode)
{
	ir_node *irn;
	ir_node **in = (ir_node **) alloca((n + 1) * sizeof(in[0]));

	in[0] = src;
	memcpy(&in[1], in_keep, n * sizeof(in[0]));
	irn   = new_ir_node(NULL, irg, bl, op_be_CopyKeep, mode, n + 1, in);
	init_node_attr(irn, n + 1);
	be_node_set_reg_class(irn, OUT_POS(0), cls);
	be_node_set_reg_class(irn, 0, cls);

	return irn;
}

ir_node *be_new_CopyKeep_single(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *src, ir_node *keep, ir_mode *mode)
{
	ir_node *in[1];

	in[0] = keep;
	return be_new_CopyKeep(cls, irg, bl, src, 1, in, mode);
}

ir_node *be_new_Barrier(ir_graph *irg, ir_node *bl, int n, ir_node *in[])
{
	ir_node *irn;

	irn = new_ir_node(NULL, irg, bl, op_be_Barrier, mode_T, n, in);
	init_node_attr(irn, n);
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
int be_is_SetSP         (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_SetSP          ; }
int be_is_AddSP         (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_AddSP          ; }
int be_is_RegParams     (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_RegParams      ; }
int be_is_StackParam    (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_StackParam     ; }
int be_is_FrameAddr     (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_FrameAddr      ; }
int be_is_FrameLoad     (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_FrameLoad      ; }
int be_is_FrameStore    (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_FrameStore     ; }
int be_is_Barrier       (const ir_node *irn) { return be_get_irn_opcode(irn) == beo_Barrier        ; }

int be_has_frame_entity(const ir_node *irn)
{
	switch(be_get_irn_opcode(irn)) {
	case beo_StackParam:
	case beo_Spill:
	case beo_Reload:
	case beo_FrameStore:
	case beo_FrameLoad:
	case beo_FrameAddr:
		return 1;
	}

	return 0;
}

entity *be_get_frame_entity(const ir_node *irn)
{
	if(be_has_frame_entity(irn)) {
		be_frame_attr_t *a = get_irn_attr(irn);
		return a->ent;
	}
	return NULL;
}

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
	if(r->req.type == arch_register_req_type_none)
		r->req.type = arch_register_req_type_normal;
}

void be_node_set_req_type(ir_node *irn, int pos, arch_register_req_type_t type)
{
	be_req_t *r = get_req(irn, pos);
	r->req.type = type;
}

void be_set_IncSP_offset(ir_node *irn, unsigned offset)
{
	be_stack_attr_t *a = get_irn_attr(irn);
	assert(be_is_IncSP(irn));
	a->offset = offset;
}

unsigned be_get_IncSP_offset(const ir_node *irn)
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

be_stack_dir_t be_get_IncSP_direction(const ir_node *irn)
{
	be_stack_attr_t *a = get_irn_attr(irn);
	assert(be_is_IncSP(irn));
	return a->dir;
}

void be_set_Spill_entity(ir_node *irn, entity *ent)
{
	be_spill_attr_t *a = get_irn_attr(irn);
	assert(be_is_Spill(irn));
	a->frame_attr.ent = ent;
}

static ir_node *find_a_spill_walker(ir_node *irn, unsigned visited_nr)
{
	unsigned nr = get_irn_visited(irn);

	set_irn_visited(irn, visited_nr);

	if(is_Phi(irn)) {
		int i, n;
		if(nr < visited_nr) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *n = find_a_spill_walker(get_irn_n(irn, i), visited_nr);
				if(n != NULL)
					return n;
			}
		}
	}

	else if(be_get_irn_opcode(irn) == beo_Spill)
		return irn;

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
static INLINE ir_node *find_a_spill(const ir_node *irn)
{
	ir_graph *irg       = get_irn_irg(irn);
	unsigned visited_nr = get_irg_visited(irg) + 1;

	assert(be_is_Reload(irn));
	set_irg_visited(irg, visited_nr);
	return find_a_spill_walker(be_get_Reload_mem(irn), visited_nr);
}

entity *be_get_spill_entity(const ir_node *irn)
{
	int opc           = get_irn_opcode(irn);

	switch(be_get_irn_opcode(irn)) {
	case beo_Reload:
		{
			ir_node *spill = find_a_spill(irn);
			return be_get_spill_entity(spill);
		}
	case beo_Spill:
		{
			be_spill_attr_t *a = get_irn_attr(irn);
			return a->frame_attr.ent;
		}
	default:
		assert(0 && "Must give spill/reload node");
		break;
	}

	return NULL;
}

static void link_reload_walker(ir_node *irn, void *data)
{
	ir_node **root = (ir_node **) data;
	if(be_is_Reload(irn)) {
		set_irn_link(irn, *root);
		*root = irn;
	}
}

void be_copy_entities_to_reloads(ir_graph *irg)
{
	ir_node *irn = NULL;
	irg_walk_graph(irg, link_reload_walker, NULL, (void *) &irn);

	while(irn) {
		be_frame_attr_t *a = get_irn_attr(irn);
		entity *ent        = be_get_spill_entity(irn);
		a->ent = ent;
		irn    = get_irn_link(irn);
	}
}

ir_node *be_spill(const arch_env_t *arch_env, ir_node *irn, ir_node *ctx)
{
	ir_node *bl     = get_nodes_block(irn);
	ir_graph *irg   = get_irn_irg(bl);
	ir_node *frame  = get_irg_frame(irg);
	ir_node *insert = bl;
	ir_node *spill;

	const arch_register_class_t *cls       = arch_get_irn_reg_class(arch_env, irn, -1);
	const arch_register_class_t *cls_frame = arch_get_irn_reg_class(arch_env, frame, -1);

	spill = be_new_Spill(cls, cls_frame, irg, bl, frame, irn, ctx);

	/*
	 * search the right insertion point. a spill of a phi cannot be put
	 * directly after the phi, if there are some phis behind the one which
	 * is spilled. Also, a spill of a Proj must be after all Projs of the
	 * same tuple node.
	 *
	 * Here's one special case:
	 * If the spill is in the start block, the spill must be after the frame
	 * pointer is set up. This is done by setting insert to the end of the block
	 * which is its default initialization (see above).
	 */

	insert = sched_next(irn);
	if(bl == get_irg_start_block(irg) && insert != bl && sched_get_time_step(frame) >= sched_get_time_step(insert))
		insert = sched_next(frame);

	while((is_Phi(insert) || is_Proj(insert)) && !sched_is_end(insert))
		insert = sched_next(insert);

	sched_add_before(insert, spill);
	return spill;
}

ir_node *be_reload(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *insert, ir_mode *mode, ir_node *spill)
{
	ir_node *reload;

	ir_node *bl    = is_Block(insert) ? insert : get_nodes_block(insert);
	ir_graph *irg  = get_irn_irg(bl);
	ir_node *frame = get_irg_frame(irg);
	const arch_register_class_t *cls_frame = arch_get_irn_reg_class(arch_env, frame, -1);

	assert(be_is_Spill(spill) || (is_Phi(spill) && get_irn_mode(spill) == mode_M));

	reload = be_new_Reload(cls, cls_frame, irg, bl, frame, spill, mode);

	if(is_Block(insert)) {
		insert = sched_skip(insert, 0, sched_skip_cf_predicator, (void *) arch_env);
		sched_add_after(insert, reload);
	}

	else
		sched_add_before(insert, reload);

	return reload;
}

/*
  ____              ____
 |  _ \ ___  __ _  |  _ \ ___  __ _ ___
 | |_) / _ \/ _` | | |_) / _ \/ _` / __|
 |  _ <  __/ (_| | |  _ <  __/ (_| \__ \
 |_| \_\___|\__, | |_| \_\___|\__, |___/
            |___/                |_|

*/


static void *put_out_reg_req(arch_register_req_t *req, const ir_node *irn, int out_pos)
{
	const be_node_attr_t *a = get_irn_attr(irn);

	if(out_pos < a->max_reg_data) {
		memcpy(req, &a->reg_data[out_pos].req, sizeof(req[0]));

		if(be_is_Copy(irn)) {
			req->type |= arch_register_req_type_should_be_same;
			req->other_same = get_irn_n(irn, be_pos_Copy_orig);
		}
	}
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
	int out_pos;
	be_node_attr_t *a;

	out_pos = redir_proj((const ir_node **) &irn, -1);
	a       = get_irn_attr(irn);

	assert(is_be_node(irn));
	assert(out_pos < a->max_reg_data && "position too high");

	return a->reg_data[out_pos].req.flags;
}

static entity *be_node_get_frame_entity(const void *self, const ir_node *irn)
{
	return be_get_frame_entity(irn);
}

static void be_node_set_frame_offset(const void *self, ir_node *irn, int offset)
{
	if(be_has_frame_entity(irn)) {
		be_frame_attr_t *a = get_irn_attr(irn);
		a->offset = offset;
	}
}

/*
  ___ ____  _   _   _   _                 _ _
 |_ _|  _ \| \ | | | | | | __ _ _ __   __| | | ___ _ __
  | || |_) |  \| | | |_| |/ _` | '_ \ / _` | |/ _ \ '__|
  | ||  _ <| |\  | |  _  | (_| | | | | (_| | |  __/ |
 |___|_| \_\_| \_| |_| |_|\__,_|_| |_|\__,_|_|\___|_|

*/

static const arch_irn_ops_if_t be_node_irn_ops_if = {
	be_node_get_irn_reg_req,
	be_node_set_irn_reg,
	be_node_get_irn_reg,
	be_node_classify,
	be_node_get_flags,
	be_node_get_frame_entity,
	be_node_set_frame_offset
};

static const arch_irn_ops_t be_node_irn_ops = {
	&be_node_irn_ops_if
};

const void *be_node_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn)
{
	redir_proj((const ir_node **) &irn, -1);
	return is_be_node(irn) ? &be_node_irn_ops : NULL;
}

const arch_irn_handler_t be_node_irn_handler = {
	be_node_get_irn_ops
};

/*
  ____  _     _   ___ ____  _   _   _   _                 _ _
 |  _ \| |__ (_) |_ _|  _ \| \ | | | | | | __ _ _ __   __| | | ___ _ __
 | |_) | '_ \| |  | || |_) |  \| | | |_| |/ _` | '_ \ / _` | |/ _ \ '__|
 |  __/| | | | |  | ||  _ <| |\  | |  _  | (_| | | | | (_| | |  __/ |
 |_|   |_| |_|_| |___|_| \_\_| \_| |_| |_|\__,_|_| |_|\__,_|_|\___|_|

*/

typedef struct {
	arch_irn_handler_t irn_handler;
	arch_irn_ops_t     irn_ops;
	const arch_env_t   *arch_env;
	pmap               *regs;
} phi_handler_t;

#define get_phi_handler_from_handler(h)  container_of(h, phi_handler_t, irn_handler)
#define get_phi_handler_from_ops(h)      container_of(h, phi_handler_t, irn_ops)

static const void *phi_get_irn_ops(const arch_irn_handler_t *handler, const ir_node *irn)
{
	const phi_handler_t *h = get_phi_handler_from_handler(handler);
	return is_Phi(irn) && mode_is_datab(get_irn_mode(irn)) ? &h->irn_ops : NULL;
}

/**
 * Get register class of a Phi.
 *
 */
static const arch_register_req_t *get_Phi_reg_req_recursive(const phi_handler_t *h, arch_register_req_t *req, const ir_node *phi, pset **visited)
{
	int n = get_irn_arity(phi);
	ir_node *op;
	int done = 0;
	int i;

	if(*visited && pset_find_ptr(*visited, phi))
		return NULL;

	for(i = 0; i < n; ++i) {
		op = get_irn_n(phi, i);
		if(!is_Phi(op))
			return arch_get_register_req(h->arch_env, req, op, BE_OUT_POS(0));
	}

	/*
	The operands of that Phi were all Phis themselves.
	We have to start a DFS for a non-Phi argument now.
	*/
	if(!*visited)
		*visited = pset_new_ptr(16);

	pset_insert_ptr(*visited, phi);

	for(i = 0; i < n; ++i) {
		op = get_irn_n(phi, i);
		if(get_Phi_reg_req_recursive(h, req, op, visited))
			return req;
	}

	return NULL;
}

static const arch_register_req_t *phi_get_irn_reg_req(const void *self, arch_register_req_t *req, const ir_node *irn, int pos)
{
	phi_handler_t *phi_handler = get_phi_handler_from_ops(self);
	pset *visited              = NULL;

	get_Phi_reg_req_recursive(phi_handler, req, irn, &visited);
	/* Set the requirements type to normal, since an operand of the Phi could have had constraints. */
	req->type = arch_register_req_type_normal;
	if(visited)
		del_pset(visited);

	return req;
}

static void phi_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg)
{
	phi_handler_t *h = get_phi_handler_from_ops(self);
	pmap_insert(h->regs, irn, (void *) reg);
}

static const arch_register_t *phi_get_irn_reg(const void *self, const ir_node *irn)
{
	phi_handler_t *h = get_phi_handler_from_ops(self);
	return pmap_get(h->regs, (void *) irn);
}

static arch_irn_class_t phi_classify(const void *_self, const ir_node *irn)
{
	return arch_irn_class_normal;
}

static arch_irn_flags_t phi_get_flags(const void *_self, const ir_node *irn)
{
	return arch_irn_flags_none;
}

static entity *phi_get_frame_entity(const void *_self, const ir_node *irn)
{
	return NULL;
}

static void phi_set_frame_offset(const void *_self, ir_node *irn, int bias)
{
}

static const arch_irn_ops_if_t phi_irn_ops = {
	phi_get_irn_reg_req,
	phi_set_irn_reg,
	phi_get_irn_reg,
	phi_classify,
	phi_get_flags,
	phi_get_frame_entity,
	phi_set_frame_offset
};

static const arch_irn_handler_t phi_irn_handler = {
	phi_get_irn_ops
};

arch_irn_handler_t *be_phi_handler_new(const arch_env_t *arch_env)
{
	phi_handler_t *h           = xmalloc(sizeof(h[0]));
	h->irn_handler.get_irn_ops = phi_get_irn_ops;
	h->irn_ops.impl            = &phi_irn_ops;
	h->arch_env                = arch_env;
	h->regs                    = pmap_create();
	return (arch_irn_handler_t *) h;
}

void be_phi_handler_free(arch_irn_handler_t *handler)
{
	phi_handler_t *h = (void *) handler;
	pmap_destroy(h->regs);
	free(handler);
}

const void *be_phi_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn)
{
	phi_handler_t *phi_handler = get_phi_handler_from_handler(self);
	return is_Phi(irn) ? &phi_handler->irn_ops : NULL;
}

void be_phi_handler_reset(arch_irn_handler_t *handler)
{
	phi_handler_t *h = get_phi_handler_from_handler(handler);
	if(h->regs)
		pmap_destroy(h->regs);
	h->regs = pmap_create();
}


/*
  _   _           _        ____                        _
 | \ | | ___   __| | ___  |  _ \ _   _ _ __ ___  _ __ (_)_ __   __ _
 |  \| |/ _ \ / _` |/ _ \ | | | | | | | '_ ` _ \| '_ \| | '_ \ / _` |
 | |\  | (_) | (_| |  __/ | |_| | |_| | | | | | | |_) | | | | | (_| |
 |_| \_|\___/ \__,_|\___| |____/ \__,_|_| |_| |_| .__/|_|_| |_|\__, |
                                                |_|            |___/
*/

/**
 * Dumps a register requirement to a file.
 */
static void dump_node_req(FILE *f, int idx, be_req_t *req)
{
	unsigned i;
	int did_something = 0;
	char buf[16];
	const char *prefix = buf;

	snprintf(buf, sizeof(buf), "#%d ", idx);

	if(req->flags != arch_irn_flags_none) {
		fprintf(f, "%sflags: ", prefix);
		prefix = "";
		for(i = arch_irn_flags_none; i <= log2_ceil(arch_irn_flags_last); ++i) {
			if(req->flags & (1 << i)) {
				fprintf(f, "%s%s", prefix, arch_irn_flag_str(1 << i));
				prefix = "|";
			}
		}
		prefix = ", ";
		did_something = 1;
	}

	if(req->req.cls != 0) {
		char tmp[256];
		fprintf(f, prefix);
		arch_register_req_format(tmp, sizeof(tmp), &req->req);
		fprintf(f, "%s", tmp);
		did_something = 1;
	}

	if(did_something)
		fprintf(f, "\n");
}

/**
 * Dumps node register requirements to a file.
 */
static void dump_node_reqs(FILE *f, ir_node *irn)
{
	int i;
	be_node_attr_t *a = get_irn_attr(irn);

	fprintf(f, "registers: \n");
	for(i = 0; i < a->max_reg_data; ++i) {
		be_reg_data_t *rd = &a->reg_data[i];
		if(rd->reg)
			fprintf(f, "#%d: %s\n", i, rd->reg->name);
	}

	fprintf(f, "in requirements\n");
	for(i = 0; i < a->max_reg_data; ++i) {
		dump_node_req(f, i, &a->reg_data[i].in_req);
	}

	fprintf(f, "\nout requirements\n");
	for(i = 0; i < a->max_reg_data; ++i) {
		dump_node_req(f, i, &a->reg_data[i].req);
	}
}

/**
 * ir_op-Operation: dump a be node to file
 */
static int dump_node(ir_node *irn, FILE *f, dump_reason_t reason)
{
	be_node_attr_t *at = get_irn_attr(irn);

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
			dump_node_reqs(f, irn);

			if(be_has_frame_entity(irn)) {
				be_frame_attr_t *a = (be_frame_attr_t *) at;
				if (a->ent)
					ir_fprintf(f, "frame entity: %+F offset %x (%d)\n", a->ent, a->offset, a->offset);

			}

			switch(be_get_irn_opcode(irn)) {
			case beo_Spill:
				{
					be_spill_attr_t *a = (be_spill_attr_t *) at;
					ir_fprintf(f, "spill context: %+F\n", a->spill_ctx);
				}
				break;

			case beo_IncSP:
				{
					be_stack_attr_t *a = (be_stack_attr_t *) at;
					fprintf(f, "offset: %u\n", a->offset);
					fprintf(f, "direction: %s\n", a->dir == be_stack_dir_expand ? "expand" : "shrink");
				}
				break;
			}
	}

	return 0;
}

/**
 * ir_op-Operation:
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
