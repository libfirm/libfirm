/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief      read/write analyze of graph argument, which have mode reference.
 * @author     Beyhan Veliev
 */
#include "analyze_irg_args.h"

#include "array.h"
#include "cgana.h"
#include "entity_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irouts_t.h"
#include "irprog.h"
#include "util.h"
#include <stdlib.h>

/**
 * Walk recursive the successors of a graph argument
 * with mode reference and mark if it will be read,
 * written or stored.
 *
 * @param arg   The graph argument with mode reference,
 *             that must be checked.
 */
static ptr_access_kind analyze_arg(ir_node *arg, ptr_access_kind bits)
{
	/* We must visit a node once to avoid endless recursion.*/
	mark_irn_visited(arg);

	foreach_irn_out_r(arg, i, succ) {
		if (irn_visited(succ))
			continue;

		/* We should not walk over the memory edge.*/
		if (get_irn_mode(succ) == mode_M)
			continue;

		/* If we reach with the recursion a Call node and our reference
		   isn't the address of this Call we accept that the reference will
		   be read and written if the graph of the method represented by
		   "Call" isn't computed else we analyze that graph. If our
		   reference is the address of this
		   Call node that mean the reference will be read.*/
		switch (get_irn_opcode(succ)) {

		case iro_Call: {
			ir_node *ptr = get_Call_ptr(succ);

			if (ptr == arg) {
				/* Hmm: not sure what this is, most likely a read */
				bits |= ptr_access_read;
			} else {
				ir_entity *callee = get_Call_callee(succ);

				if (callee != NULL) {
					for (int p = get_Call_n_params(succ); p-- > 0; ) {
						if (get_Call_param(succ, p) == arg) {
							/* an arg can be used more than once ! */
							bits |= get_method_param_access(callee, p);
						}
					}
				} else if (is_Member(ptr) && get_irp_callee_info_state() == irg_callee_info_consistent) {
					/* is be a polymorphic call but callee information is available */
					size_t n_params = get_Call_n_params(succ);

					/* simply look into ALL possible callees */
					for (int c = cg_get_call_n_callees(succ); c-- > 0; ) {
						ir_entity *meth_ent = cg_get_call_callee(succ, c);

						/* unknown_entity is used to signal that we don't know what is called */
						if (is_unknown_entity(meth_ent)) {
							bits |= ptr_access_all;
							break;
						}

						for (size_t p = n_params; p-- > 0; ) {
							if (get_Call_param(succ, p) == arg) {
								/* an arg can be used more than once ! */
								bits |= get_method_param_access(meth_ent, p);
							}
						}
					}
				} else /* can do anything */
					bits |= ptr_access_all;
			}

			/* search stops here anyway */
			continue;
		}
		case iro_Store:
			/* We have reached a Store node => the reference is written or stored. */
			if (get_Store_ptr(succ) == arg) {
				/* written to */
				bits |= ptr_access_write;
			} else {
				/* stored itself */
				bits |= ptr_access_store;
			}

			/* search stops here anyway */
			continue;

		case iro_Load:
			/* We have reached a Load node => the reference is read. */
			bits |= ptr_access_read;

			/* search stops here anyway */
			continue;

		case iro_Conv:
			/* our address is casted into something unknown. Break our search. */
			bits = ptr_access_all;
			break;

		default:
			break;
		}

		/* If we know that, the argument will be read, write and stored, we
		   can break the recursion.*/
		if (bits == ptr_access_all) {
			bits = ptr_access_all;
			break;
		}

		/*
		 * A calculation that do not lead to a reference mode ends our search.
		 * This is dangerous: It would allow to cast into integer and that cast back ...
		 * so, when we detect a Conv we go mad, see the Conv case above.
		 */
		if (!mode_is_reference(get_irn_mode(succ)))
			continue;

		/* follow further the address calculation */
		bits = analyze_arg(succ, bits);
	}
	return bits;
}

/**
 * Check if a argument of the ir graph with mode
 * reference is read, write or both.
 *
 * @param irg   The ir graph to analyze.
 */
static void analyze_ent_args(ir_entity *ent)
{
	ir_type *mtp     = get_entity_type(ent);
	size_t   nparams = get_method_n_params(mtp);

	ent->attr.mtd_attr.param_access = NEW_ARR_F(ptr_access_kind, nparams);

	/* If the method haven't parameters we have
	 * nothing to do.
	 */
	if (nparams <= 0)
		return;

  /* we have not yet analyzed the graph, set ALL access for pointer args */
	for (size_t i = nparams; i-- > 0; ) {
		ir_type *type = get_method_param_type(mtp, i);
		ent->attr.mtd_attr.param_access[i] = is_Pointer_type(type) ? ptr_access_all : ptr_access_none;
	}

	ir_graph *irg = get_entity_irg(ent);
	if (irg == NULL) {
		/* no graph, no better info */
		return;
	}

	assure_irg_outs(irg);
	ir_node *irg_args = get_irg_args(irg);

	/* A array to save the information for each argument with
	   mode reference.*/
	ptr_access_kind *rw_info = ALLOCAN(ptr_access_kind, nparams);

	/* We initialize the element with none state. */
	for (size_t i = nparams; i-- > 0; )
		rw_info[i] = ptr_access_none;

	/* search for arguments with mode reference
	   to analyze them.*/
	foreach_irn_out_r(irg_args, i, arg) {
		ir_mode *arg_mode = get_irn_mode(arg);
		unsigned proj_nr  = get_Proj_num(arg);

		if (mode_is_reference(arg_mode))
			rw_info[proj_nr] |= analyze_arg(arg, rw_info[proj_nr]);
	}

	/* copy the temporary info */
	MEMCPY(ent->attr.mtd_attr.param_access, rw_info, nparams);
}

void analyze_irg_args(ir_graph *irg)
{
	if (irg == get_const_code_irg())
		return;

	ir_entity *entity = get_irg_entity(irg);
	if (entity == NULL)
		return;

	if (!entity->attr.mtd_attr.param_access)
		analyze_ent_args(entity);
}

ptr_access_kind get_method_param_access(ir_entity *ent, size_t pos)
{
#ifndef NDEBUG
	ir_type *mtp = get_entity_type(ent);
	assert(is_method_variadic(mtp) || pos < get_method_n_params(mtp));
#endif

	if (ent->attr.mtd_attr.param_access) {
		if (pos < ARR_LEN(ent->attr.mtd_attr.param_access))
			return ent->attr.mtd_attr.param_access[pos];
		else
			return ptr_access_all;
	}

	analyze_ent_args(ent);

	if (pos < ARR_LEN(ent->attr.mtd_attr.param_access))
		return ent->attr.mtd_attr.param_access[pos];
	else
		return ptr_access_all;
}

/* Weights for parameters */
enum args_weight {
	null_weight          = 0,   /**< If can't be anything optimized. */
	binop_weight         = 1,   /**< If the argument have mode_weight and take part in binop. */
	const_binop_weight   = 1,   /**< If the argument have mode_weight and take part in binop with a constant.*/
	cmp_weight           = 4,   /**< If the argument take part in cmp. */
	const_cmp_weight     = 10,  /**< If the argument take part in cmp with a constant. */
	indirect_call_weight = 125, /**< If the argument is the address of an indirect Call. */
};

/**
 * Computes the weight of a method parameter
 *
 * @param arg  The parameter whose weight is to be computed.
 */
static unsigned calc_method_param_weight(ir_node *arg)
{
	/* We mark the nodes to avoid endless recursion */
	mark_irn_visited(arg);

	unsigned weight = null_weight;
	foreach_irn_out_r(arg, i, succ) {
		if (irn_visited(succ))
			continue;

		/* We should not walk over the memory edge.*/
		if (get_irn_mode(succ) == mode_M)
			continue;

		switch (get_irn_opcode(succ)) {
		case iro_Call:
			if (get_Call_ptr(succ) == arg) {
				/* the arguments is used as an pointer input for a call,
				   we can probably change an indirect Call into a direct one. */
				weight += indirect_call_weight;
			}
			break;
		case iro_Cmp: {
			/* We have reached a cmp and we must increase the
			   weight with the cmp_weight. */
			ir_node *op;
			if (get_Cmp_left(succ) == arg)
				op = get_Cmp_right(succ);
			else
				op = get_Cmp_left(succ);

			if (is_irn_constlike(op)) {
				weight += const_cmp_weight;
			} else
				weight += cmp_weight;
			break;
		}
		case iro_Cond:
			/* the argument is used for a SwitchCond, a big win */
			weight += const_cmp_weight * get_irn_n_outs(succ);
			break;
		case iro_Id:
			/* when looking backward we might find Id nodes */
			weight += calc_method_param_weight(succ);
			break;
		case iro_Tuple:
			/* unoptimized tuple */
			for (unsigned j = get_Tuple_n_preds(succ); j-- > 0; ) {
				ir_node *pred = get_Tuple_pred(succ, j);
				if (pred == arg) {
					/* look for Proj(j) */
					foreach_irn_out_r(succ, k, succ_succ) {
						if (get_Proj_num(succ_succ) == j) {
							/* found */
							weight += calc_method_param_weight(succ_succ);
						}
					}
				}
			}
			break;
		default:
			if (is_binop(succ)) {
				/* We have reached a BinOp and we must increase the
				   weight with the binop_weight. If the other operand of the
				   BinOp is a constant we increase the weight with const_binop_weight
				   and call the function recursive.
				 */
				ir_node *op;
				if (get_binop_left(succ) == arg)
					op = get_binop_right(succ);
				else
					op = get_binop_left(succ);

				if (is_irn_constlike(op)) {
					weight += const_binop_weight;
					weight += calc_method_param_weight(succ);
				} else
					weight += binop_weight;
			} else if (get_irn_arity(succ) == 1) {
				/* We have reached a binop and we must increase the
				   weight with the const_binop_weight and call the function
				   recursive.*/
				weight += const_binop_weight;
				weight += calc_method_param_weight(succ);
			}
			break;
		}
	}
	return weight;
}

/**
 * Calculate a weight for each argument of an entity.
 *
 * @param ent  The entity of the ir_graph.
 */
static void analyze_method_params_weight(ir_entity *ent)
{
	/* allocate a new array. currently used as 'analysed' flag */
	ir_type *mtp      = get_entity_type(ent);
	size_t   nparams  = get_method_n_params(mtp);
	ent->attr.mtd_attr.param_weight = NEW_ARR_F(unsigned, nparams);

	/* If the method has no parameters, we have nothing to do */
	if (nparams <= 0)
	  return;

	/* First we initialize the parameter weights with 0. */
	for (size_t i = nparams; i-- > 0; )
		ent->attr.mtd_attr.param_weight[i] = null_weight;

	ir_graph *irg = get_entity_irg(ent);
	if (irg == NULL) {
		/* no graph, no better info */
		return;
	}

	/* Call algorithm that computes the out edges */
	assure_irg_outs(irg);

	ir_node *irg_args = get_irg_args(irg);
	foreach_irn_out_r(irg_args, i, arg) {
		unsigned const proj_nr = get_Proj_num(arg);
		ent->attr.mtd_attr.param_weight[proj_nr] += calc_method_param_weight(arg);
	}
}

unsigned get_method_param_weight(ir_entity *ent, size_t pos)
{
	if (!ent->attr.mtd_attr.param_weight)
		analyze_method_params_weight(ent);

	if (pos < ARR_LEN(ent->attr.mtd_attr.param_weight))
		return ent->attr.mtd_attr.param_weight[pos];
	else
		return null_weight;
}

void analyze_irg_args_weight(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	if (entity == NULL)
		return;

	assert(is_method_entity(entity));
	if (entity->attr.mtd_attr.param_weight != NULL)
		return;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	analyze_method_params_weight(entity);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}
