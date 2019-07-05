/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#include "vhdl_transform.h"

#include "betranshlp.h"
#include "irnode_t.h"
#include "panic.h"


static ir_node *gen_Add(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_And(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Cmp(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Cond(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Conv(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Eor(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Jmp(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Mul(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Minus(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Mux(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Not(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Or(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Phi(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_PinnedConst(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Proj_Proj_Start(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Proj_Proj(ir_node *const node)
{
	ir_node *const pred      = get_Proj_pred(node);
	ir_node *const pred_pred = get_Proj_pred(pred);
	switch (get_irn_opcode(pred_pred)) {
	case iro_Start: return gen_Proj_Proj_Start(node);
	default:        panic("unexpected Proj-Proj");
	}
}

static ir_node *gen_Proj_Start(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Return(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Shl(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Shr(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Shrs(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Start(ir_node *const node)
{
	TODO(node);
}

static ir_node *gen_Sub(ir_node *const node)
{
	TODO(node);
}

static void vhdl_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,         gen_Add);
	be_set_transform_function(op_And,         gen_And);
	be_set_transform_function(op_Cmp,         gen_Cmp);
	be_set_transform_function(op_Cond,        gen_Cond);
	be_set_transform_function(op_Conv,        gen_Conv);
	be_set_transform_function(op_Eor,         gen_Eor);
	be_set_transform_function(op_Jmp,         gen_Jmp);
	be_set_transform_function(op_Mul,         gen_Mul);
	be_set_transform_function(op_Minus,       gen_Minus);
	be_set_transform_function(op_Mux,         gen_Mux);
	be_set_transform_function(op_Not,         gen_Not);
	be_set_transform_function(op_Or,          gen_Or);
	be_set_transform_function(op_Phi,         gen_Phi);
	be_set_transform_function(op_PinnedConst, gen_PinnedConst);
	be_set_transform_function(op_Return,      gen_Return);
	be_set_transform_function(op_Shl,         gen_Shl);
	be_set_transform_function(op_Shr,         gen_Shr);
	be_set_transform_function(op_Shrs,        gen_Shrs);
	be_set_transform_function(op_Start,       gen_Start);
	be_set_transform_function(op_Sub,         gen_Sub);

	be_set_transform_proj_function(op_Proj,   gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,  gen_Proj_Start);
}

void vhdl_transform_graph(ir_graph *const irg)
{
	vhdl_register_transformers();
	be_transform_graph(irg, NULL);
}
