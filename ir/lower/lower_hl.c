/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Lower some High-level constructs, moved from the firmlower.
 * @author  Boris Boesler, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

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
static void lower_sel(ir_node *sel) {
	ir_graph *irg = current_ir_graph;
	ir_entity   *ent;
	ir_node  *newn, *cnst, *index, *ptr, *bl;
	tarval   *tv;
	ir_mode  *basemode, *mode, *mode_Int;
	ir_type  *basetyp, *owner;
	dbg_info *dbg;

	assert(is_Sel(sel));

	ent   = get_Sel_entity(sel);
	owner = get_entity_owner(ent);

	/* Do not lower frame type access: must be lowered by the backend. */
	if (is_frame_type(owner))
		return;

	/*
	 * Cannot handle value param entities here.
	 * Must be lowered by the backend.
	 */
	if (is_value_param_type(owner))
		return;

	ptr  = get_Sel_ptr(sel);
	dbg  = get_irn_dbg_info(sel);
	mode = get_irn_mode(sel);

	mode_Int = get_reference_mode_signed_eq(mode);

	/* TLS access, must be handled by the linker */
	if (get_tls_type() == owner) {
		symconst_symbol sym;

		sym.entity_p = ent;
		bl = get_nodes_block(sel);

		cnst = new_rd_SymConst(dbg, irg, bl, sym, symconst_addr_ent);
		newn = new_rd_Add(dbg, irg, bl, ptr, cnst, mode);
	} else {
		/* not TLS */

		assert(get_type_state(get_entity_owner(ent)) == layout_fixed);
		assert(get_type_state(get_entity_type(ent)) == layout_fixed);

		bl = get_nodes_block(sel);
		if (0 < get_Sel_n_indexs(sel)) {
			/* an Array access */
			basetyp = get_entity_type(ent);
			if (is_Primitive_type(basetyp))
				basemode = get_type_mode(basetyp);
			else
				basemode = mode_P_data;

			assert(basemode && "no mode for lowering Sel");
			assert((get_mode_size_bytes(basemode) != -1) && "can not deal with unorthodox modes");
			index = get_Sel_index(sel, 0);

			if (is_Array_type(owner)) {
				ir_node *last_size;
				ir_type *arr_ty = owner;
				int dims = get_array_n_dimensions(arr_ty);
				int *map = alloca(sizeof(int) * dims);
				int i;

				assert(dims == get_Sel_n_indexs(sel)
					&& "array dimension must match number of indices of Sel node");

				for (i = 0; i < dims; i++) {
					int order = get_array_order(arr_ty, i);

					assert(order < dims &&
						"order of a dimension must be smaller than the arrays dim");
					map[order] = i;
				}
				newn = get_Sel_ptr(sel);

				/* Size of the array element */
				tv = new_tarval_from_long(get_type_size_bytes(basetyp), mode_Int);
				last_size = new_rd_Const(dbg, irg, get_irg_start_block(irg), mode_Int, tv);

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
				for (i = dims - 1; i >= 0; i--) {
					int dim = map[i];
					ir_node *lb, *ub, *elms, *n, *ind;

					elms = NULL;
					lb = get_array_lower_bound(arr_ty, dim);
					ub = get_array_upper_bound(arr_ty, dim);

					assert(irg == current_ir_graph);
					if (get_irn_op(lb) != op_Unknown)
						lb = new_rd_Conv(dbg, irg, bl, copy_const_value(get_irn_dbg_info(sel), lb), mode_Int);
					else
						lb = NULL;

					if (get_irn_op(ub) != op_Unknown)
						ub = new_rd_Conv(dbg, irg, bl, copy_const_value(get_irn_dbg_info(sel), ub), mode_Int);
					else
						ub = NULL;

					/*
					 * If the array has more than one dimension, lower and upper
					 * bounds have to be set in the non-last dimension.
					 */
					if (i > 0) {
						assert(lb && "lower bound has to be set in multi-dim array");
						assert(lb && "upper bound has to be set in multi-dim array");

						/* Elements in one Dimension */
						elms = new_rd_Sub(dbg, irg, bl, ub, lb, mode_Int);
					}

					ind = new_rd_Conv(dbg, irg, bl, get_Sel_index(sel, dim), mode_Int);

					/*
					 * Normalize index, id lower bound is set, also assume
					 * lower bound == 0
					 */
					if (lb)
						ind = new_rd_Sub(dbg, irg, bl, ind, lb, mode_Int);

					n = new_rd_Mul(dbg, irg, bl, ind, last_size, mode_Int);

					/*
					 * see comment above.
					 */
					if (i > 0)
						last_size = new_rd_Mul(dbg, irg, bl, last_size, elms, mode_Int);

					newn = new_rd_Add(dbg, irg, bl, newn, n, mode);
				}
			} else {
				/* no array type */
				ir_mode *idx_mode = get_irn_mode(index);
				tarval *tv = new_tarval_from_long(get_mode_size_bytes(basemode), idx_mode);

				newn = new_rd_Add(dbg, irg, bl, get_Sel_ptr(sel),
					new_rd_Mul(dbg, irg, bl, index,
					new_r_Const(irg, get_irg_start_block(irg), idx_mode, tv),
					idx_mode),
					mode);
			}
		} else if (is_Method_type(get_entity_type(ent)) &&
	               is_Class_type(owner) &&
	               (owner != get_glob_type()) &&
	               (!is_frame_type(owner)))  {
			ir_node *add;
			ir_mode *ent_mode = get_type_mode(get_entity_type(ent));

			/* We need an additional load when accessing methods from a dispatch table. */
			tv   = new_tarval_from_long(get_entity_offset(ent), mode_Int);
			cnst = new_rd_Const(dbg, irg, get_irg_start_block(irg), mode_Int, tv);
			add  = new_rd_Add(dbg, irg, bl, get_Sel_ptr(sel), cnst, mode);
#ifdef DO_CACHEOPT  /* cacheopt version */
			newn = new_rd_Load(dbg, irg, bl, get_Sel_mem(sel), sel, ent_mode);
			cacheopt_map_addrs_register_node(newn);
			set_Load_ptr(newn, add);
#else /* normal code */
			newn = new_rd_Load(dbg, irg, bl, get_Sel_mem(sel), add, ent_mode);
#endif
			newn = new_r_Proj(irg, bl, newn, ent_mode, pn_Load_res);

		} else if (get_entity_owner(ent) != get_glob_type())  {
			/* replace Sel by add(obj, const(ent.offset)) */
			assert(!(get_entity_allocation(ent) == allocation_static &&
				(get_entity_n_overwrites(ent) == 0 && get_entity_n_overwrittenby(ent) == 0)));
			tv = new_tarval_from_long(get_entity_offset(ent), mode_Int);
			cnst = new_r_Const(irg, get_irg_start_block(irg), mode_Int, tv);
			newn = new_rd_Add(dbg, irg, bl, get_Sel_ptr(sel), cnst, mode);
		} else {
			/* global_type */
			newn = new_rd_SymConst_addr_ent(NULL, current_ir_graph, ent, firm_unknown_type);
		}
	}
	/* run the hooks */
	hook_lower(sel);

	exchange(sel, newn);
}  /* lower_sel */

/**
 * Lower a all possible SymConst nodes.
 */
static void lower_symconst(ir_node *symc) {
	ir_node       *newn;
	ir_type       *tp;
	ir_entity     *ent;
	tarval        *tv;
	ir_enum_const *ec;
	ir_mode       *mode;

	switch (get_SymConst_kind(symc)) {
	case symconst_type_tag:
		assert(!"SymConst kind symconst_type_tag not implemented");
		break;
	case symconst_type_size:
		/* rewrite the SymConst node by a Const node */
		tp   = get_SymConst_type(symc);
		assert(get_type_state(tp) == layout_fixed);
		mode = get_irn_mode(symc);
		tv   = new_tarval_from_long(get_type_size_bytes(tp), mode);
		newn = new_r_Const(current_ir_graph,
		                   get_irg_start_block(current_ir_graph),
		                   get_irn_mode(symc), tv);
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		break;
	case symconst_type_align:
		/* rewrite the SymConst node by a Const node */
		tp   = get_SymConst_type(symc);
		assert(get_type_state(tp) == layout_fixed);
		mode = get_irn_mode(symc);
		tv   = new_tarval_from_long(get_type_alignment_bytes(tp), mode);
		newn = new_r_Const(current_ir_graph,
		                   get_irg_start_block(current_ir_graph),
		                   mode, tv);
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		break;
	case symconst_addr_name:
		/* do not rewrite - pass info to back end */
		break;
	case symconst_addr_ent:
		/* leave */
		break;
	case symconst_ofs_ent:
		/* rewrite the SymConst node by a Const node */
		ent  = get_SymConst_entity(symc);
		assert(get_type_state(get_entity_type(ent)) == layout_fixed);
		mode = get_irn_mode(symc);
		tv   = new_tarval_from_long(get_entity_offset(ent), mode);
		newn = new_r_Const(current_ir_graph,
		                   get_irg_start_block(current_ir_graph),
		                   mode, tv);
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		break;
	case symconst_enum_const:
		/* rewrite the SymConst node by a Const node */
		ec   = get_SymConst_enum(symc);
		assert(get_type_state(get_enumeration_owner(ec)) == layout_fixed);
		tv   = get_enumeration_value(ec);
		newn = new_r_Const(current_ir_graph,
				  get_irg_start_block(current_ir_graph),
				  get_irn_mode(symc), tv);
		assert(newn);
		/* run the hooks */
		hook_lower(symc);
		exchange(symc, newn);
		break;
	case symconst_label:
		/* leave */
		break;

	default:
		assert(!"unknown SymConst kind");
		break;
	}
}  /* lower_symconst */

/**
 * Checks, whether a size is an integral size
 *
 * @param size  the size on bits
 */
static int is_integral_size(int size) {
	/* must be a 2^n */
	if (size & (size-1))
		return 0;
	/* must be at least byte size */
	return size >= 8;
}  /* is_integral_size */

/**
 * lower bitfield load access.
 *
 * @param proj  the Proj(result) node
 * @param load  the Load node
 */
static void lower_bitfields_loads(ir_node *proj, ir_node *load) {
	ir_node *sel = get_Load_ptr(load);
	ir_node *block, *n_proj, *res, *ptr;
	ir_entity *ent;
	ir_type *bf_type;
	ir_mode *bf_mode, *mode;
	int offset, bit_offset, bits, bf_bits, old_cse;
	dbg_info *db;

	if (get_irn_op(sel) != op_Sel)
		return;

	ent     = get_Sel_entity(sel);
	bf_type = get_entity_type(ent);

	/* must be a bitfield type */
	if (!is_Primitive_type(bf_type) || get_primitive_base_type(bf_type) == NULL)
		return;

	/* We have a bitfield access, if either a bit offset is given, or
	   the size is not integral. */
	bf_mode = get_type_mode(bf_type);
	if (! bf_mode)
		return;

	mode       = get_irn_mode(proj);
	block      = get_nodes_block(proj);
	bf_bits    = get_mode_size_bits(bf_mode);
	bit_offset = get_entity_offset_bits_remainder(ent);

	if (bit_offset == 0 && is_integral_size(bf_bits) && bf_mode == get_Load_mode(load))
		return;

	bits   = get_mode_size_bits(mode);
	offset = get_entity_offset(ent);

	/*
	 * ok, here we are: now convert the Proj_mode_bf(Load) into And(Shr(Proj_mode(Load)) for unsigned
	 * and Shr(Shl(Proj_mode(load)) for signed
	 */

	/* abandon bitfield sel */
	ptr = get_Sel_ptr(sel);
	db  = get_irn_dbg_info(sel);
	ptr = new_rd_Add(db, current_ir_graph, block, ptr, new_Const_long(mode_Is, offset), get_irn_mode(ptr));

	set_Load_ptr(load, ptr);
	set_Load_mode(load, mode);


	/* create new proj, switch off CSE or we may get the old one back */
	old_cse = get_opt_cse();
	set_opt_cse(0);
	res = n_proj = new_r_Proj(current_ir_graph, block, load, mode, pn_Load_res);
	set_opt_cse(old_cse);

	if (mode_is_signed(mode)) { /* signed */
		int shift_count_up    = bits - (bf_bits + bit_offset);
		int shift_count_down  = bits - bf_bits;

		if (shift_count_up) {
			res = new_r_Shl(current_ir_graph, block, res,
				new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(shift_count_up, mode_Iu)), mode);
		}
		if (shift_count_down) {
			res = new_r_Shrs(current_ir_graph, block, res,
				new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(shift_count_down, mode_Iu)), mode);
		}
	} else { /* unsigned */
		int shift_count_down  = bit_offset;
		unsigned mask = ((unsigned)-1) >> (bits - bf_bits);

		if (shift_count_down) {
			res = new_r_Shr(current_ir_graph, block, res,
				new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(shift_count_down, mode_Iu)), mode);
		}
		if (bits != bf_bits) {
			res = new_r_And(current_ir_graph, block, res,
				new_r_Const(current_ir_graph, block, mode, new_tarval_from_long(mask, mode)), mode);
		}
	}

	exchange(proj, res);
}  /* lower_bitfields_loads */

/**
 * lower bitfield store access.
 *
 * @todo: It adds a load which may produce an exception!
 */
static void lower_bitfields_stores(ir_node *store) {
	ir_node   *sel = get_Store_ptr(store);
	ir_node   *ptr, *value;
	ir_entity *ent;
	ir_type *bf_type;
	ir_mode *bf_mode, *mode;
	ir_node *mem, *irn, *block;
	unsigned mask, neg_mask;
	int bf_bits, bits_mask, offset, bit_offset;
	dbg_info *db;

	/* check bitfield access */
	if (get_irn_op(sel) != op_Sel)
		return;

	ent     = get_Sel_entity(sel);
	bf_type = get_entity_type(ent);

	/* must be a bitfield type */
	if (!is_Primitive_type(bf_type) || get_primitive_base_type(bf_type) == NULL)
		return;

	/* We have a bitfield access, if either a bit offset is given, or
	   the size is not integral. */
	bf_mode = get_type_mode(bf_type);
	if (! bf_mode)
		return;

	value      = get_Store_value(store);
	mode       = get_irn_mode(value);
	block      = get_nodes_block(store);

	bf_bits    = get_mode_size_bits(bf_mode);
	bit_offset = get_entity_offset_bits_remainder(ent);

	if (bit_offset == 0 && is_integral_size(bf_bits) && bf_mode == get_irn_mode(value))
		return;

	/*
	 * ok, here we are: now convert the Store(Sel(), value) into Or(And(Load(Sel),c), And(Value,c))
	 */
	mem        = get_Store_mem(store);
	offset     = get_entity_offset(ent);

	bits_mask = get_mode_size_bits(mode) - bf_bits;
	mask = ((unsigned)-1) >> bits_mask;
	mask <<= bit_offset;
	neg_mask = ~mask;

	/* abandon bitfield sel */
	ptr = get_Sel_ptr(sel);
	db  = get_irn_dbg_info(sel);
	ptr = new_rd_Add(db, current_ir_graph, block, ptr, new_Const_long(mode_Is, offset), get_irn_mode(ptr));

	if (neg_mask) {
		/* there are some bits, normal case */
		irn  = new_r_Load(current_ir_graph, block, mem, ptr, mode);
		mem  = new_r_Proj(current_ir_graph, block, irn, mode_M, pn_Load_M);
		irn  = new_r_Proj(current_ir_graph, block, irn, mode, pn_Load_res);

		irn = new_r_And(current_ir_graph, block, irn,
			new_r_Const(current_ir_graph, block, mode, new_tarval_from_long(neg_mask, mode)), mode);

		if (bit_offset > 0) {
			value = new_r_Shl(current_ir_graph, block, value,
				new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(bit_offset, mode_Iu)), mode);
		}

		value = new_r_And(current_ir_graph, block, value,
			new_r_Const(current_ir_graph, block, mode, new_tarval_from_long(mask, mode)), mode);

		value = new_r_Or(current_ir_graph, block, value, irn, mode);
	}

	set_Store_mem(store, mem);
	set_Store_value(store, value);
	set_Store_ptr(store, ptr);
}  /* lower_bitfields_stores */

/**
 * lowers IR-nodes, called from walker
 */
static void lower_irnode(ir_node *irn, void *env) {
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
}  /* lower_irnode */

/**
 * Walker: lowers IR-nodes for bitfield access
 */
static void lower_bf_access(ir_node *irn, void *env) {
	(void) env;
	switch (get_irn_opcode(irn)) {
	case iro_Proj:
	{
		long proj     = get_Proj_proj(irn);
		ir_node *pred = get_Proj_pred(irn);
		ir_op *op     = get_irn_op(pred);

		if ((proj == pn_Load_res) && (op == op_Load))
			lower_bitfields_loads(irn, pred);
		break;
	}
	case iro_Store:
		lower_bitfields_stores(irn);
		break;

	default:
		break;
	}
}  /* lower_bf_access */

/*
 * Replaces SymConsts by a real constant if possible.
 * Replace Sel nodes by address computation.  Also resolves array access.
 * Handle Bitfields by added And/Or calculations.
 */
void lower_highlevel(void) {
	int i, n;

	n = get_irp_n_irgs();
	for (i = 0; i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);

		/* First step: lower bitfield access: must be run as long as Sels still exists. */
		irg_walk_graph(irg, NULL, lower_bf_access, NULL);

		/* Finally: lower SymConst-Size and Sel nodes. */
		irg_walk_graph(irg, NULL, lower_irnode, NULL);

		set_irg_phase_low(irg);
	}
}  /* lower_highlevel */
