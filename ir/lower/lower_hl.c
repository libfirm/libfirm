/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower some High-level constructs, moved from the firmlower.
 * @author  Boris Boesler, Goetz Lindenmaier, Michael Beck
 */
#include "error.h"
#include "lowering.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "typerep.h"
#include "irprog_t.h"
#include "ircons.h"
#include "irhooks.h"
#include "irgmod.h"
#include "irgwalk.h"

/**
 * Lower a Sel node. Do not touch Sels accessing entities on the frame type.
 */
static void lower_sel(ir_node *sel)
{
	ir_graph  *irg   = get_irn_irg(sel);
	ir_entity *ent   = get_Sel_entity(sel);
	ir_type   *owner = get_entity_owner(ent);
	dbg_info  *dbg   = get_irn_dbg_info(sel);
	ir_mode   *mode  = get_irn_mode(sel);
	ir_node   *bl    = get_nodes_block(sel);
	ir_node   *newn;

	/* we can only replace Sels when the layout of the owner type is decided. */
	if (get_type_state(owner) != layout_fixed)
		return;

	if (0 < get_Sel_n_indexs(sel)) {
		/* an Array access */
		ir_type *basetyp = get_entity_type(ent);
		ir_mode *basemode;
		ir_node *index;
		if (is_Primitive_type(basetyp))
			basemode = get_type_mode(basetyp);
		else
			basemode = mode_P_data;

		assert(basemode && "no mode for lowering Sel");
		assert((get_mode_size_bits(basemode) % 8 == 0) && "can not deal with unorthodox modes");
		index = get_Sel_index(sel, 0);

		if (is_Array_type(owner)) {
			ir_type *arr_ty = owner;
			size_t   dims   = get_array_n_dimensions(arr_ty);
			size_t  *map    = ALLOCAN(size_t, dims);
			ir_mode *mode_Int = get_reference_mode_signed_eq(mode);
			ir_tarval *tv;
			ir_node *last_size;
			size_t   i;

			assert(dims == (size_t)get_Sel_n_indexs(sel)
				&& "array dimension must match number of indices of Sel node");

			for (i = 0; i < dims; i++) {
				size_t order = get_array_order(arr_ty, i);

				assert(order < dims &&
					"order of a dimension must be smaller than the arrays dim");
				map[order] = i;
			}
			newn = get_Sel_ptr(sel);

			/* Size of the array element */
			tv = new_tarval_from_long(get_type_size_bytes(basetyp), mode_Int);
			last_size = new_rd_Const(dbg, irg, tv);

			/*
			 * We compute the offset part of dimension d_i recursively
			 * with the the offset part of dimension d_{i-1}
			 *
			 *     off_0 = sizeof(array_element_type);
			 *     off_i = (u_i - l_i) * off_{i-1}  ; i >= 1
			 *
			 * whereas u_i is the upper bound of the current dimension
			 * and l_i the lower bound of the current dimension.
			 */
			for (i = dims; i > 0;) {
				size_t dim = map[--i];
				ir_node *lb, *ub, *elms, *n, *ind;

				elms = NULL;
				lb = get_array_lower_bound(arr_ty, dim);
				ub = get_array_upper_bound(arr_ty, dim);

				if (! is_Unknown(lb))
					lb = new_rd_Conv(dbg, bl, duplicate_subgraph(get_irn_dbg_info(sel), lb, bl), mode_Int);
				else
					lb = NULL;

				if (! is_Unknown(ub))
					ub = new_rd_Conv(dbg, bl, duplicate_subgraph(get_irn_dbg_info(sel), ub, bl), mode_Int);
				else
					ub = NULL;

				/*
				 * If the array has more than one dimension, lower and upper
				 * bounds have to be set in the non-last dimension.
				 */
				if (i > 0) {
					assert(lb != NULL && "lower bound has to be set in multi-dim array");
					assert(ub != NULL && "upper bound has to be set in multi-dim array");

					/* Elements in one Dimension */
					elms = new_rd_Sub(dbg, bl, ub, lb, mode_Int);
				}

				ind = new_rd_Conv(dbg, bl, get_Sel_index(sel, dim), mode_Int);

				/*
				 * Normalize index, id lower bound is set, also assume
				 * lower bound == 0
			 */
				if (lb != NULL)
					ind = new_rd_Sub(dbg, bl, ind, lb, mode_Int);

				n = new_rd_Mul(dbg, bl, ind, last_size, mode_Int);

				/*
				 * see comment above.
				 */
				if (i > 0)
					last_size = new_rd_Mul(dbg, bl, last_size, elms, mode_Int);

				newn = new_rd_Add(dbg, bl, newn, n, mode);
			}
		} else {
			/* no array type */
			ir_mode   *idx_mode = get_irn_mode(index);
			ir_tarval *tv       = new_tarval_from_long(get_mode_size_bytes(basemode), idx_mode);

			newn = new_rd_Add(dbg, bl, get_Sel_ptr(sel),
				new_rd_Mul(dbg, bl, index,
				new_r_Const(irg, tv),
				idx_mode),
				mode);
		}
	} else if (is_Method_type(get_entity_type(ent)) && is_Class_type(owner)) {
		/* We need an additional load when accessing methods from a dispatch
		 * table.
		 * Matze TODO: Is this really still used? At least liboo does its own
		 * lowering of Method-Sels...
		 */
		ir_mode   *ent_mode = get_type_mode(get_entity_type(ent));
		int        offset   = get_entity_offset(ent);
		ir_mode   *mode_Int = get_reference_mode_signed_eq(mode);
		ir_tarval *tv       = new_tarval_from_long(offset, mode_Int);
		ir_node   *cnst     = new_rd_Const(dbg, irg, tv);
		ir_node   *add      = new_rd_Add(dbg, bl, get_Sel_ptr(sel), cnst, mode);
		ir_node   *mem      = get_Sel_mem(sel);
		newn = new_rd_Load(dbg, bl, mem, add, ent_mode, cons_none);
		newn = new_r_Proj(newn, ent_mode, pn_Load_res);
	} else {
		int offset = get_entity_offset(ent);

		/* replace Sel by add(obj, const(ent.offset)) */
		newn = get_Sel_ptr(sel);
		if (offset != 0) {
			ir_mode   *mode_UInt = get_reference_mode_unsigned_eq(mode);
			ir_tarval *tv        = new_tarval_from_long(offset, mode_UInt);
			ir_node   *cnst      = new_r_Const(irg, tv);
			newn = new_rd_Add(dbg, bl, newn, cnst, mode);
		}
	}

	/* run the hooks */
	hook_lower(sel);

	exchange(sel, newn);
}

/**
 * Lower a all possible SymConst nodes.
 */
static void lower_symconst(ir_node *symc)
{
	ir_node   *newn;
	ir_type   *tp;
	ir_entity *ent;
	ir_mode   *mode;
	ir_graph  *irg;

	switch (get_SymConst_kind(symc)) {
	case symconst_type_size:
		/* rewrite the SymConst node by a Const node */
		irg  = get_irn_irg(symc);
		tp   = get_SymConst_type(symc);
		assert(get_type_state(tp) == layout_fixed);
		mode = get_irn_mode(symc);
		newn = new_r_Const_long(irg, mode, get_type_size_bytes(tp));
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		return;

	case symconst_type_align:
		/* rewrite the SymConst node by a Const node */
		irg  = get_irn_irg(symc);
		tp   = get_SymConst_type(symc);
		assert(get_type_state(tp) == layout_fixed);
		mode = get_irn_mode(symc);
		newn = new_r_Const_long(irg, mode, get_type_alignment_bytes(tp));
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		return;

	case symconst_addr_ent:
		/* leave */
		return;

	case symconst_ofs_ent:
		/* rewrite the SymConst node by a Const node */
		irg  = get_irn_irg(symc);
		ent  = get_SymConst_entity(symc);
		assert(get_type_state(get_entity_type(ent)) == layout_fixed);
		mode = get_irn_mode(symc);
		newn = new_r_Const_long(irg, mode, get_entity_offset(ent));
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		return;
	}
	panic("invalid SymConst kind");
}

/**
 * lowers IR-nodes, called from walker
 */
static void lower_irnode(ir_node *irn, void *env)
{
	(void) env;
	switch (get_irn_opcode(irn)) {
	case iro_Sel:
		lower_sel(irn);
		break;
	case iro_SymConst:
		lower_symconst(irn);
		break;
	default:
		break;
	}
}

/*
 * Replaces SymConsts by a real constant if possible.
 * Replace Sel nodes by address computation.  Also resolves array access.
 * Handle Bitfields by added And/Or calculations.
 */
void lower_highlevel_graph(ir_graph *irg)
{
	/* Finally: lower SymConst-Size and Sel nodes, unaligned Load/Stores. */
	irg_walk_graph(irg, NULL, lower_irnode, NULL);
}

/*
 * does the same as lower_highlevel() for all nodes on the const code irg
 */
void lower_const_code(void)
{
	walk_const_code(NULL, lower_irnode, NULL);
}

/*
 * Replaces SymConsts by a real constant if possible.
 * Replace Sel nodes by address computation.  Also resolves array access.
 * Handle Bitfields by added And/Or calculations.
 */
void lower_highlevel()
{
	size_t i, n;

	n = get_irp_n_irgs();
	for (i = 0; i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);
		lower_highlevel_graph(irg);
	}
	lower_const_code();
}
