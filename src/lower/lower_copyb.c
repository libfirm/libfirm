/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower small CopyB nodes into a series of Load/Store nodes
 * @author  Michael Beck, Matthias Braun, Manuel Mohr
 */
#include "adt/list.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "lowering.h"
#include "panic.h"
#include "target_t.h"
#include "type_t.h"
#include "util.h"

static unsigned max_small_size; /**< The maximum size of a CopyB node
                                     so that it is regarded as 'small'. */
static unsigned min_large_size; /**< The minimum size of a CopyB node
                                     so that it is regarded as 'large'. */
static unsigned native_mode_bytes; /**< The size of the native mode in bytes. */
static bool allow_misalignments; /**< Whether backend can handle misaligned
                                      loads and stores. */

typedef struct walk_env {
	ir_node **copybs; /**< The list of CopyB nodes. */
} walk_env_t;

static ir_mode *get_ir_mode(unsigned mode_bytes)
{
	switch (mode_bytes) {
	case 1:  return mode_Bu;
	case 2:  return mode_Hu;
	case 4:  return mode_Iu;
	case 8:  return mode_Lu;
	default:
		panic("unexpected mode size requested in copyb lowering");
	}
}

/**
 * Turn a small CopyB node into a series of Load/Store nodes.
 */
static void lower_small_copyb_node(ir_node *irn)
{
	ir_graph      *irg         = get_irn_irg(irn);
	dbg_info      *dbgi        = get_irn_dbg_info(irn);
	ir_node       *block       = get_nodes_block(irn);
	ir_type       *tp          = get_CopyB_type(irn);
	ir_node       *addr_src    = get_CopyB_src(irn);
	ir_node       *addr_dst    = get_CopyB_dst(irn);
	ir_node       *mem         = get_CopyB_mem(irn);
	ir_mode       *mode_ref    = get_irn_mode(addr_src);
	unsigned       mode_bytes  = allow_misalignments ? native_mode_bytes
	                                                 : get_type_alignment(tp);
	unsigned       size        = get_type_size(tp);
	unsigned       offset      = 0;
	bool           is_volatile = get_CopyB_volatility(irn) == volatility_is_volatile;
	ir_cons_flags  flags       = is_volatile ? cons_volatile : cons_none;

	while (offset < size) {
		ir_mode *mode = get_ir_mode(mode_bytes);
		for (; offset + mode_bytes <= size; offset += mode_bytes) {
			ir_mode *mode_ref_int = get_reference_offset_mode(mode_ref);

			/* construct offset */
			ir_node *addr_const = new_r_Const_long(irg, mode_ref_int, offset);
			ir_node *add        = new_r_Add(block, addr_src, addr_const);

			ir_node *load     = new_rd_Load(dbgi, block, mem, add, mode, tp, flags);
			ir_node *load_res = new_r_Proj(load, mode, pn_Load_res);
			ir_node *load_mem = new_r_Proj(load, mode_M, pn_Load_M);

			ir_node *addr_const2 = new_r_Const_long(irg, mode_ref_int, offset);
			ir_node *add2        = new_r_Add(block, addr_dst, addr_const2);

			ir_node *store     = new_rd_Store(dbgi, block, load_mem, add2, load_res, tp, flags);
			ir_node *store_mem = new_r_Proj(store, mode_M, pn_Store_M);

			mem = store_mem;
		}

		mode_bytes /= 2;
	}

	exchange(irn, mem);
}

static ir_type *get_memcpy_methodtype(void)
{
	ir_type *tp          = new_type_method(3, 1, false, cc_cdecl_set, mtp_no_property);
	ir_mode *size_t_mode = get_ir_mode(native_mode_bytes);

	set_method_param_type(tp, 0, get_type_for_mode(mode_P));
	set_method_param_type(tp, 1, get_type_for_mode(mode_P));
	set_method_param_type(tp, 2, get_type_for_mode(size_t_mode));
	set_method_res_type  (tp, 0, get_type_for_mode(mode_P));

	return tp;
}

static ir_node *get_memcpy_address(ir_graph *irg)
{
	ir_type   *mt  = get_memcpy_methodtype();
	ir_entity *ent = create_compilerlib_entity("memcpy", mt);

	return new_r_Address(irg, ent);
}

/**
 * Turn a large CopyB node into a memcpy call.
 */
static void lower_large_copyb_node(ir_node *irn)
{
	ir_graph *irg      = get_irn_irg(irn);
	ir_node  *block    = get_nodes_block(irn);
	dbg_info *dbgi     = get_irn_dbg_info(irn);
	ir_node  *mem      = get_CopyB_mem(irn);
	ir_node  *addr_src = get_CopyB_src(irn);
	ir_node  *addr_dst = get_CopyB_dst(irn);
	ir_type  *copyb_tp = get_CopyB_type(irn);
	unsigned  size     = get_type_size(copyb_tp);

	ir_node  *callee      = get_memcpy_address(irg);
	ir_type  *call_tp     = get_memcpy_methodtype();
	ir_mode  *mode_size_t = get_ir_mode(native_mode_bytes);
	ir_node  *size_cnst   = new_r_Const_long(irg, mode_size_t, size);
	ir_node  *in[]        = { addr_dst, addr_src, size_cnst };
	ir_node  *call        = new_rd_Call(dbgi, block, mem, callee, ARRAY_SIZE(in), in, call_tp);
	ir_node  *call_mem    = new_r_Proj(call, mode_M, pn_Call_M);

	exchange(irn, call_mem);
}

static void lower_copyb_node(ir_node *irn)
{
	ir_type *tp   = get_CopyB_type(irn);
	unsigned size = get_type_size(tp);

	if (size <= max_small_size)
		lower_small_copyb_node(irn);
	else if (size >= min_large_size)
		lower_large_copyb_node(irn);
	else
		panic("CopyB of invalid size");
}

/**
 * Post-Walker: find CopyB nodes.
 */
static void find_copyb_nodes(ir_node *irn, void *ctx)
{
	if (!is_CopyB(irn))
		return;

	ir_type *tp = get_CopyB_type(irn);
	if (get_type_state(tp) != layout_fixed)
		return;

	unsigned size         = get_type_size(tp);
	bool     medium_sized = max_small_size < size && size < min_large_size;
	if (medium_sized)
		return; /* Nothing to do for medium-sized CopyBs. */

	/* Okay, either small or large CopyB, so link it in and lower it later. */
	walk_env_t *env = (walk_env_t*)ctx;
	ARR_APP1(ir_node*, env->copybs, irn);
}

void lower_CopyB(ir_graph *irg, unsigned max_small_sz, unsigned min_large_sz,
                 int allow_misaligns)
{
	assert(max_small_sz < min_large_sz && "CopyB size ranges must not overlap");

	max_small_size      = max_small_sz;
	min_large_size      = min_large_sz;
	native_mode_bytes   = ir_target_pointer_size();
	allow_misalignments = allow_misaligns;

	walk_env_t env = { .copybs = NEW_ARR_F(ir_node*, 0) };
	irg_walk_graph(irg, NULL, find_copyb_nodes, &env);

	bool changed = false;
	for (size_t i = 0, n = ARR_LEN(env.copybs); i != n; ++i) {
		lower_copyb_node(env.copybs[i]);
		changed = true;
	}
	confirm_irg_properties(irg, changed ? IR_GRAPH_PROPERTIES_CONTROL_FLOW
	                                    : IR_GRAPH_PROPERTIES_ALL);

	DEL_ARR_F(env.copybs);
}
