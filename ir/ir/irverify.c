/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Check irnodes for correctness.
 * @author   Christian Schaefer, Goetz Lindenmaier, Till Riedel, Michael Beck,
 *           Matthias Braun
 */
#include "irverify_t.h"

#include "ircons.h"
#include "irdom_t.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodeset.h"
#include "irop_t.h"
#include "irouts.h"
#include "irprintf.h"
#include "irprog.h"

static void warn(const ir_node *n, const char *format, ...)
{
	FILE *out = stderr;
	fputs("Verify warning: ", out);
	if (n != NULL) {
		ir_fprintf(out, "%+F(%+F): ", n, get_irn_irg(n));
	}
	va_list ap;
	va_start(ap, format);
	ir_vfprintf(out, format, ap);
	va_end(ap);
	fputc('\n', out);
}

/**
 * Checks if node @p n has mode @p expected. Displays a message and returns
 * false in case of mismatch.
 */
static bool check_mode(const ir_node *n, const ir_mode *expected)
{
	ir_mode *mode = get_irn_mode(n);
	if (mode != expected) {
		warn(n, "expected mode %+F but found %+F", expected, mode);
		return false;
	}
	return true;
}

typedef int (*check_mode_func_ptr)(const ir_mode *mode);

/**
 * Checks if the mode of node @p fulfills the predicate function @p func.
 * Displays a message and returns false if predicate is not fulfilled.
 */
static bool check_mode_func(const ir_node *n, check_mode_func_ptr func,
                            const char *name)
{
	ir_mode *mode = get_irn_mode(n);
	if (!func(mode)) {
		warn(n, "expected %s mode but found %+F", name, mode);
		return false;
	}
	return true;
}

/**
 * Checks if input @p input of node @p n has mode @p expected. Displays a
 * message and returns false in case of mismatch.
 */
static bool check_input_mode(const ir_node *n, int input, const char *inputname,
                             const ir_mode *mode)
{
	ir_node *in      = get_irn_n(n, input);
	ir_mode *in_mode = get_irn_mode(in);
	if (in_mode != mode) {
		char num[17];
		if (inputname == NULL) {
			snprintf(num, sizeof(num), "input %d", input);
			inputname = num;
		}
		warn(n, "expected mode %+F for input '%s' but found %+F (%+F)",
		     mode, inputname, in_mode, in);
		return false;
	}
	return true;
}

/**
 * Checks if mode of input @p input of node @p n fulfills the predicate
 * functions @p func. Displays a message and returns false if unfulfilled.
 */
static bool check_input_func(const ir_node *n, int input, const char *inputname,
                             check_mode_func_ptr func, const char *modedescr)
{
	ir_node *in   = get_irn_n(n, input);
	ir_mode *mode = get_irn_mode(in);
	if (!func(mode)) {
		char num[16];
		if (inputname == NULL) {
			snprintf(num, sizeof(num), "input %d", input);
			inputname = num;
		}
		warn(n, "expected %s mode for input '%s' but found %+F (%+F)",
		     modedescr, inputname, mode, in);
		return false;
	}
	return true;
}

static bool check_mode_same_input(const ir_node *n, int input,
                                  const char *inputname)
{
	ir_mode *mode    = get_irn_mode(n);
	ir_node *in      = get_irn_n(n, input);
	ir_mode *in_mode = get_irn_mode(in);
	if (mode != in_mode) {
		char num[17];
		if (inputname == NULL) {
			snprintf(num, sizeof(num), "input %d", input);
			inputname = num;
		}
		warn(n, "mode of input '%s' different from output mode %+F",
		     inputname, mode);
		return false;
	}
	return true;
}

/**
 * Displays error message that a wrong proj number was found and returns false.
 */
static bool invalid_proj(const ir_node *proj)
{
	warn(proj, "invalid proj number %u for predecessor %+F",
	     get_Proj_num(proj), get_Proj_pred(proj));
	return false;
}

static int verify_node_Proj_Start(const ir_node *p)
{
	switch ((pn_Start)get_Proj_num(p)) {
	case pn_Start_M:            return check_mode(p, mode_M);
	case pn_Start_P_frame_base: return check_mode_func(p, mode_is_reference, "reference");
	case pn_Start_T_args:       return check_mode(p, mode_T);
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Cond(const ir_node *p)
{
	switch ((pn_Cond)get_Proj_num(p)) {
	case pn_Cond_true:
	case pn_Cond_false: return check_mode(p, mode_X);
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Switch(const ir_node *p)
{
	unsigned pn   = get_Proj_num(p);
	ir_node *pred = get_Proj_pred(p);
	bool     fine = check_mode(p, mode_X);
	if (pn >= get_Switch_n_outs(pred)) {
		warn(p, "invalid proj number %u for predecessor %+F", pn, pred);
		fine = false;
	}
	return fine;
}

static int verify_node_Proj_Raise(const ir_node *p)
{
	switch ((pn_Raise)get_Proj_num(p)) {
	case pn_Raise_X: return check_mode(p, mode_X);
	case pn_Raise_M: return check_mode(p, mode_M);
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Call(const ir_node *p)
{
	switch ((pn_Call)get_Proj_num(p)) {
	case pn_Call_M:         return check_mode(p, mode_M);
	case pn_Call_X_except:  return check_mode(p, mode_X);
	case pn_Call_X_regular: return check_mode(p, mode_X);
	case pn_Call_T_result:  return check_mode(p, mode_T);
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Div(const ir_node *p)
{
	switch ((pn_Div)get_Proj_num(p)) {
	case pn_Div_M:         return check_mode(p, mode_M);
	case pn_Div_X_regular: return check_mode(p, mode_X);
	case pn_Div_X_except:  return check_mode(p, mode_X);
	case pn_Div_res:       return check_mode(p, get_Div_resmode(get_Proj_pred(p)));
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Mod(const ir_node *p)
{
	switch ((pn_Mod)get_Proj_num(p)) {
	case pn_Mod_M:         return check_mode(p, mode_M);
	case pn_Mod_X_regular: return check_mode(p, mode_X);
	case pn_Mod_X_except:  return check_mode(p, mode_X);
	case pn_Mod_res:       return check_mode(p, get_Mod_resmode(get_Proj_pred(p)));
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Load(const ir_node *p)
{
	switch ((pn_Load)get_Proj_num(p)) {
	case pn_Load_M:         return check_mode(p, mode_M);
	case pn_Load_X_regular: return check_mode(p, mode_X);
	case pn_Load_X_except:  return check_mode(p, mode_X);
	case pn_Load_res:       return check_mode(p, get_Load_mode(get_Proj_pred(p)));
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Store(const ir_node *p)
{
	switch ((pn_Store)get_Proj_num(p)) {
	case pn_Store_M:         return check_mode(p, mode_M);
	case pn_Store_X_regular: return check_mode(p, mode_X);
	case pn_Store_X_except:  return check_mode(p, mode_X);
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Alloc(const ir_node *p)
{
	switch ((pn_Alloc)get_Proj_num(p)) {
	case pn_Alloc_M:   return check_mode(p, mode_M);
	case pn_Alloc_res: return check_mode_func(p, mode_is_reference, "reference");
	}
	return invalid_proj(p);
}

static int verify_node_Proj_Proj_Start(const ir_node *p)
{
	ir_graph *irg = get_irn_irg(p);
	ir_type  *mt  = get_entity_type(get_irg_entity(irg));
	if (!is_Method_type(mt))
		return true;
	unsigned pn = get_Proj_num(p);
	if (pn >= get_method_n_params(mt)) {
		warn(p, "invalid proj number %u after Proj(%+F)", pn,
		     get_Proj_pred(get_Proj_pred(p)));
		return false;
	}
	ir_type *param_type = get_method_param_type(mt, pn);
	return check_mode(p, get_type_mode(param_type));
}

static int verify_node_Proj_Proj_Call(const ir_node *p)
{
	ir_node *pred = get_Proj_pred(p);
	ir_node *call = get_Proj_pred(pred);
	ir_type *mt   = get_Call_type(call);
	if (!is_Method_type(mt))
		return true;
	unsigned pn = get_Proj_num(p);
	if (pn >= get_method_n_ress(mt)) {
		warn(p, "invalid proj number %u after %+F", pn, call);
		return false;
	}
	ir_type *type = get_method_res_type(mt, pn);
	ir_mode *mode = is_aggregate_type(type) ? mode_P : get_type_mode(type);
	return check_mode(p, mode);
}

static int verify_node_Proj_Proj(const ir_node *p)
{
	ir_node *pred     = get_Proj_pred(p);
	ir_node *predpred = get_Proj_pred(pred);
	switch (get_irn_opcode(predpred)) {
	case iro_Start: return verify_node_Proj_Proj_Start(p);
	case iro_Call:  return verify_node_Proj_Proj_Call(p);
	default:        return true;
	}
}

static int verify_node_Proj_Tuple(const ir_node *p)
{
	ir_node *tuple = get_Proj_pred(p);
	unsigned pn    = get_Proj_num(p);
	if (pn >= (unsigned)get_irn_arity(tuple)) {
		warn(p, "invalid proj number on %+F", tuple);
		return false;
	}
	ir_node *in = get_irn_n(tuple, pn);
	return check_mode(p, get_irn_mode(in));
}

static bool verify_node_Proj_fragile(const ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	if (!is_fragile_op(pred))
		return true;

	if (!is_x_except_Proj(node) && !is_x_regular_Proj(node))
		return true;
	int throws_exception = ir_throws_exception(pred);
	if (!throws_exception) {
		warn(node, "exception Proj on %+F which is not marked as throwing exceptions", pred);
		return false;
	}
	return true;
}

static int verify_node_Proj(const ir_node *p)
{
	ir_graph *irg  = get_irn_irg(p);
	ir_node  *pred = get_Proj_pred(p);
	if (get_irn_mode(pred) != mode_T) {
		warn(p, "predecessor %+F does not have mode_T", pred);
		return false;
	}
	bool fine = true;
	if (get_irg_pinned(irg) != op_pin_state_floats
	    && get_nodes_block(pred) != get_nodes_block(p)) {
	    warn(p, "different block than predecessor %+F", pred);
	    fine = false;
	}
	fine &= verify_node_Proj_fragile(p);

	ir_op *op = get_irn_op(pred);
	if (op->ops.verify_proj_node)
		fine &= op->ops.verify_proj_node(p);

	return fine;
}

static int verify_node_Block(const ir_node *n)
{
	bool fine = check_mode(n, mode_BB);
	for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(n); i < n_cfgpreds; ++i) {
		fine &= check_input_mode(n, i, NULL, mode_X);
		ir_node *pred         = get_Block_cfgpred(n, i);
		ir_node *skipped_pred = skip_Proj(skip_Tuple(pred));
		if (!is_cfop(skipped_pred) && !is_Bad(skipped_pred)) {
			warn(n, "predecessor %d of block is not a cfop, but %+F", i,
			     skipped_pred);
			fine = false;
		}

		if (is_IJmp(skipped_pred) && get_Block_entity(n) == NULL) {
			warn(n, "succesor block of IJmp %+F has no entity assigned",
			     skipped_pred);
			fine = false;
		}
	}

	ir_graph *irg = get_irn_irg(n);
	if (n == get_irg_start_block(irg) && get_Block_n_cfgpreds(n) != 0) {
		warn(n, "start block mustn't have inputs");
		fine = false;
	} else if (n == get_irg_end_block(irg)
	           && !irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
		/* End block may only have Return, Raise or fragile ops as preds. */
		for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(n); i < n_cfgpreds;
		     ++i) {
		    ir_node *pred         = get_Block_cfgpred(n, i);
			ir_node *skipped_pred = skip_Proj(skip_Tuple(pred));
			if (!is_Return(skipped_pred) && !is_Bad(skipped_pred)
			    && !is_Raise(skipped_pred)) {
			    warn(n, "end block must have Return or Raise predecessor, found %+F",
			         skipped_pred);
			    fine = false;
			}
		}
	}
	return fine;
}

static int verify_node_Deleted(const ir_node *n)
{
	warn(n, "Deleted node %+F appears to be reachable");
	return false;
}

static int verify_node_Start(const ir_node *n)
{
	return check_mode(n, mode_T);
}

static int verify_node_End(const ir_node *n)
{
	bool fine = check_mode(n, mode_X);
	/* check that only blocks, PhiM and (noreturn) Call nodes are connected
	 * by keep-alive edges */
	ir_graph *irg = get_irn_irg(n);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
		foreach_irn_in(n, i, kept) {
			/* endless loop handling may keep PhiM and Block nodes */
			if (is_Block(kept))
				continue;
			if (is_Phi(kept) && get_Phi_loop(kept))
				continue;
			/* noreturn calls are currently kept with keep-alive edges */
			if (is_Call(kept))
				continue;
			if (is_Bad(kept))
				continue;
			warn(n, "keep-alive edge only allowed on Block, PhiLoop and Call node, found %+F",
			     kept);
			fine = false;
		}
	}
	return fine;
}

static int verify_node_Jmp(const ir_node *n)
{
	return check_mode(n, mode_X);
}

static int verify_node_IJmp(const ir_node *n)
{
	bool fine = check_input_func(n, n_IJmp_target, "target", mode_is_reference, "reference");
	fine &= check_mode(n, mode_X);
	return fine;
}

static int verify_node_Cond(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	fine &= check_input_mode(n, n_Cond_selector, "selector", mode_b);
	return fine;
}

static bool verify_switch_table(const ir_node *n)
{
	const ir_switch_table *table = get_Switch_table(n);
	if (table == NULL) {
		warn(n, "switch table is NULL");
		return false;
	}

	unsigned  n_outs   = get_Switch_n_outs(n);
	ir_node  *selector = get_Switch_selector(n);
	ir_mode  *mode     = get_irn_mode(selector);
	bool      fine     = true;
	for (size_t e = 0, n_entries = ir_switch_table_get_n_entries(table);
	     e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		if (entry->pn == 0)
			continue;
		if (entry->min == NULL || entry->max == NULL) {
			warn(n, "switch table entry %zu without min+max value", e);
			fine = false;
		}
		if (mode_is_int(mode) && (get_tarval_mode(entry->min) != mode
		                       || get_tarval_mode(entry->max) != mode)) {
			warn(n, "switch table entry %zu has wrong mode for min or max value",
				 e);
			fine = false;
		} else if (tarval_cmp(entry->min, entry->max) == ir_relation_greater) {
			warn(n, "switch table entry %zu min is not less or equal than max", e);
			fine = false;
		}
		if (entry->pn >= n_outs) {
			warn(n, "switch table entry %zu has invalid proj number", e);
			fine = false;
		}
	}
	return fine;
}

static int verify_node_Switch(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	fine &= check_input_func(n, n_Switch_selector, "selector", mode_is_int, "int");
	fine &= verify_switch_table(n);
	return fine;
}

static int verify_node_Return(const ir_node *n)
{
	bool fine = check_mode(n, mode_X);
	fine &= check_input_mode(n, n_Return_mem, "mem", mode_M);

	ir_graph *irg = get_irn_irg(n);
	ir_type  *mt  = get_entity_type(get_irg_entity(irg));
	if (!is_Method_type(mt)) /* someone else should report that problem */
		return true;
	if ((size_t)get_Return_n_ress(n) != get_method_n_ress(mt)) {
		warn(n, "number of inputs does not match method type (%zu inputs, %zu declared)",
		     get_Return_n_ress(n), get_method_n_ress(mt));
		fine = false;
	} else {
		for (int i = get_Return_n_ress(n); i-- > 0; ) {
			ir_type *expected = get_method_res_type(mt, i);
			if (is_atomic_type(expected)) {
				ir_mode *mode = get_type_mode(expected);
				fine &= check_input_mode(n, n_Return_max+1+i, NULL, mode);
			} else {
				fine &= check_input_func(n, n_Return_max+1+i, NULL, mode_is_reference, "reference");
			}
		}
	}
	return fine;
}

static int verify_node_Raise(const ir_node *n)
{
	bool fine = check_mode(n, mode_X);
	fine &= check_input_func(n, n_Raise_exo_ptr, "exo_ptr", mode_is_reference, "reference");
	fine &= check_input_mode(n, n_Raise_mem, "mem", mode_M);
	return fine;
}

static int verify_node_Address(const ir_node *n)
{
	ir_entity *ent  = get_Address_entity(n);
	bool       fine = check_mode_func(n, mode_is_reference, "reference");
	if (!is_segment_type(get_entity_owner(ent)) && !is_method_entity(ent)) {
		warn(n, "entity of %+F is not in a segment type but %+F", ent,
		     get_entity_owner(ent));
		fine = false;
	}
	return fine;
}

static int verify_node_Const(const ir_node *n)
{
	bool     fine    = check_mode_func(n, mode_is_data, "data");
	ir_mode *mode    = get_irn_mode(n);
	ir_mode *tv_mode = get_tarval_mode(get_Const_tarval(n));
	if (fine && tv_mode != mode) {
		warn(n, "tarval mode (%+F) different from Const mode (%+F)", tv_mode,
		     mode);
		fine = false;
	}
	return fine;
}

static int verify_node_int(const ir_node *n)
{
	return check_mode_func(n, mode_is_int, "int");
}

static int verify_node_Sel(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_reference, "reference");
	fine &= check_input_func(n, n_Sel_ptr, "ptr", mode_is_reference, "reference");
	fine &= check_input_func(n, n_Sel_index, "index", mode_is_int, "int");
	ir_type *type = get_Sel_type(n);
	if (type == NULL) {
		warn(n, "type is NULL");
		fine = false;
	} else if (!is_Array_type(type)) {
		warn(n, "type %+F is not an array type", type);
		fine = false;
	}
	return fine;
}

static int verify_node_Member(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_reference, "reference");
	/* do not check in backend until beabi.c is gone */
	fine &= check_input_func(n, n_Member_ptr, "ptr", mode_is_reference,
	                         "reference");
	ir_entity *entity = get_Member_entity(n);
	if (entity == NULL) {
		warn(n, "entity is NULL");
		fine = false;
	} else {
		ir_type *const owner = get_entity_owner(entity);
		if (is_segment_type(owner)) {
			warn(n, "Member from entity with global/segment type owner");
			fine = false;
		} else {
			ir_node  *const ptr = get_Member_ptr(n);
			ir_graph *const irg = get_irn_irg(n);
			if (ptr == get_irg_frame(irg) && owner != get_irg_frame_type(irg)) {
				warn(n, "entity of Member with frame base is not in frame");
				fine = false;
			}
		}
	}
	return fine;
}

static int mode_is_data_not_b(const ir_mode *mode)
{
	return mode_is_data(mode) && mode != mode_b;
}

static int verify_node_Call(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	fine &= check_input_mode(n, n_Call_mem, "mem", mode_M);
	fine &= check_input_func(n, n_Call_ptr, "ptr", mode_is_reference, "reference");

	ir_type *mt = get_Call_type(n);
	if (!is_Method_type(mt)) {
		warn(n, "call_type is not a method type");
		return false;
	}

	if ((size_t)get_Call_n_params(n) < get_method_n_params(mt)) {
		warn(n, "call has fewer arguments than method type");
		return false;
	} else if ((size_t)get_Call_n_params(n) > get_method_n_params(mt) && !is_method_variadic(mt)) {
		warn(n, "call has more arguments than method type");
		return false;
	} else {
		for (int i = 0, n_params = get_Call_n_params(n); i < n_params; ++i) {
			if (i < (int)get_method_n_params(mt)) {
				const ir_type *param_type = get_method_param_type(mt, i);
				if (is_atomic_type(param_type)) {
					ir_mode *mode = get_type_mode(param_type);
					fine &= check_input_mode(n, n_Call_max+1+i, NULL, mode);
				} else {
					fine &= check_input_func(n, n_Call_max+1+i, NULL,
					                         mode_is_reference, "reference");
				}
			} else {
				fine &= check_input_func(n, n_Call_max+1+i, NULL,
				                         mode_is_data_not_b, "data_not_b");
			}
		}
	}
	return fine;
}

static int verify_node_Add(const ir_node *n)
{
	bool     fine = true;
	ir_mode *mode = get_irn_mode(n);
	if (mode_is_num(mode)) {
		fine &= check_mode_same_input(n, n_Add_left, "left");
		fine &= check_mode_same_input(n, n_Add_right, "right");
	} else if (mode_is_reference(mode)) {
		ir_mode *left_mode   = get_irn_mode(get_Add_left(n));
		ir_mode *right_mode  = get_irn_mode(get_Add_right(n));
		ir_mode *offset_mode = get_reference_offset_mode(mode);
		if (mode_is_int(left_mode)) {
			fine &= check_input_mode(n, n_Add_left, "left", offset_mode);
			fine &= check_mode_same_input(n, n_Add_right, "right");
		} else if (mode_is_int(right_mode)) {
			fine &= check_mode_same_input(n, n_Add_left, "left");
			fine &= check_input_mode(n, n_Add_right, "right", offset_mode);
		} else {
			warn(n, "AddP has no integer input");
			fine = false;
		}
	} else {
		warn(n, "mode must be numeric or reference but is %+F", mode);
		fine = false;
	}
	return fine;
}

static int verify_node_Sub(const ir_node *n)
{
	bool     fine = true;
	ir_mode *mode = get_irn_mode(n);
	if (mode_is_num(mode)) {
		ir_mode *mode_left = get_irn_mode(get_Sub_left(n));
		if (mode_is_reference(mode_left)) {
			fine &= check_input_mode(n, n_Sub_right, "right", mode_left);
			ir_mode *const offset_mode = get_reference_offset_mode(mode_left);
			fine &= check_mode(n, offset_mode);
		} else {
			fine &= check_mode_same_input(n, n_Sub_left, "left");
			fine &= check_mode_same_input(n, n_Sub_right, "right");
		}
	} else if (mode_is_reference(mode)) {
		fine &= check_mode_same_input(n, n_Sub_left, "left");
		ir_mode *offset_mode = get_reference_offset_mode(mode);
		fine &= check_input_mode(n, n_Sub_right, "right", offset_mode);
	}
	return fine;
}

static int verify_node_Minus(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_num, "numeric");
	fine &= check_mode_same_input(n, n_Minus_op, "op");
	return fine;
}

static int verify_node_Mul(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_num, "numeric");
	fine &= check_mode_same_input(n, n_Mul_left, "left");
	fine &= check_mode_same_input(n, n_Mul_right, "right");
	return fine;
}

static int verify_node_Mulh(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_int, "int");
	fine &= check_mode_same_input(n, n_Mulh_left, "left");
	fine &= check_mode_same_input(n, n_Mulh_right, "right");
	return fine;
}

static int verify_node_Div(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	ir_mode *mode = get_Div_resmode(n);
	fine &= check_input_mode(n, n_Div_left, "left", mode);
	fine &= check_input_mode(n, n_Div_right, "right", mode);
	fine &= check_input_mode(n, n_Div_mem, "mem", mode_M);
	if (!mode_is_num(mode)) {
		warn(n, "div resmode is not a numeric mode");
		fine = false;
	}
	return fine;
}

static int verify_node_Mod(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	ir_mode *mode = get_Mod_resmode(n);
	fine &= check_input_mode(n, n_Mod_left, "left", mode);
	fine &= check_input_mode(n, n_Mod_right, "right", mode);
	fine &= check_input_mode(n, n_Mod_mem, "mem", mode_M);
	if (!mode_is_int(mode)) {
		warn(n, "mod resmode is not a int mode");
		fine = false;
	}
	return fine;
}

static int mode_is_intb(const ir_mode *mode)
{
	return mode_is_int(mode) || mode == mode_b;
}

static int verify_node_And(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_intb, "int or mode_b");
	fine &= check_mode_same_input(n, n_And_left, "left");
	fine &= check_mode_same_input(n, n_And_right, "right");
	return fine;
}

static int verify_node_Or(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_intb, "int or mode_b");
	fine &= check_mode_same_input(n, n_Or_left, "left");
	fine &= check_mode_same_input(n, n_Or_right, "right");
	return fine;
}

static int verify_node_Eor(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_intb, "int or mode_b");
	fine &= check_mode_same_input(n, n_Eor_left, "left");
	fine &= check_mode_same_input(n, n_Eor_right, "right");
	return fine;
}

static int verify_node_Not(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_intb, "int or mode_b");
	fine &= check_mode_same_input(n, n_Not_op, "op");
	return fine;
}

static int verify_node_Cmp(const ir_node *n)
{
	bool fine = check_mode(n, mode_b);
	fine &= check_input_func(n, n_Cmp_left, "left", mode_is_data_not_b,
	                         "data_not_b");
	fine &= check_input_func(n, n_Cmp_right, "right", mode_is_data_not_b,
	                         "data_not_b");
	ir_mode *model = get_irn_mode(get_Cmp_left(n));
	ir_mode *moder = get_irn_mode(get_Cmp_right(n));
	if (model != moder) {
		warn(n, "modes of left+right input are different: %+F and %+F", model,
		     moder);
		fine = false;
	}
	return fine;
}

static int mode_is_uint(const ir_mode *mode)
{
	return mode_is_int(mode) && !mode_is_signed(mode);
}

static int verify_node_Shl(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_int, "int");
	fine &= check_mode_same_input(n, n_Shl_left, "left");
	fine &= check_input_func(n, n_Shl_right, "right", mode_is_uint, "unsigned int");
	return fine;
}

static int verify_node_Shr(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_int, "int");
	fine &= check_mode_same_input(n, n_Shr_left, "left");
	fine &= check_input_func(n, n_Shr_right, "right", mode_is_uint, "unsigned int");
	return fine;
}

static int verify_node_Shrs(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_int, "int");
	fine &= check_mode_same_input(n, n_Shrs_left, "left");
	fine &= check_input_func(n, n_Shrs_right, "right", mode_is_uint, "unsigned int");
	return fine;
}

static int verify_node_Conv(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_data_not_b, "data_not_b");
	fine &= check_input_func(n, n_Conv_op, "op", mode_is_data_not_b,
	                         "data_not_b");
	return fine;
}

static int verify_node_Bitcast(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_data_not_b, "data_not_b");
	fine &= check_input_func(n, n_Bitcast_op, "op", mode_is_data_not_b,
	                         "data_not_b");
	ir_node *op       = get_Bitcast_op(n);
	ir_mode *src_mode = get_irn_mode(op);
	ir_mode *dst_mode = get_irn_mode(n);
	/* Note: This constraint is currently strict as you can use Conv
	 * for the other cases and we want to avoid having 2 nodes representing the
	 * same operation. We might loosen this constraint in the future. */
	if (get_mode_size_bits(src_mode) != get_mode_size_bits(dst_mode)
	    || get_mode_arithmetic(src_mode) == get_mode_arithmetic(dst_mode)) {
	    warn(n, "bitcast only allowed for modes with same size and different arithmetic");
	    fine = false;
	}
	return fine;
}

static int mode_is_dataMb(const ir_mode *mode)
{
	return mode_is_data(mode) || mode == mode_M;
}

static int verify_node_Phi(const ir_node *n)
{
	/* a Phi node MUST have the same number of inputs as its block
	 * Exception is a phi with 0 inputs which is used when (re)constructing the
	 * SSA form */
	bool      fine  = true;
	ir_node  *block = get_nodes_block(n);
	ir_graph *irg   = get_irn_irg(block);
	if (!is_Bad(block) && get_irn_arity(n) != get_irn_arity(block)
	    && (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION)
	        || get_irn_arity(n) != 0)) {
	    warn(n, "wrong number of inputs in Phi node");
	    fine = false;
	}

	/* Phi: BB x dataM --> dataM */
	fine &= check_mode_func(n, mode_is_dataMb, "data, memory or mode_b");
	for (int i = 0, n_preds = get_Phi_n_preds(n); i < n_preds; ++i) {
		fine &= check_mode_same_input(n, i, NULL);
	}

	if (get_Phi_loop(n))
		check_mode(n, mode_M);
	return fine;
}

static int verify_node_Load(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	fine &= check_input_mode(n, n_Load_mem, "mem", mode_M);
	fine &= check_input_func(n, n_Load_ptr, "ptr", mode_is_reference,
	                         "reference");
	ir_mode *loadmode = get_Load_mode(n);
	if (!mode_is_data_not_b(loadmode)) {
		warn(n, "load mode is not a data mode, but %+F", loadmode);
		fine = false;
	}
	return fine;
}

static int verify_node_Store(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	fine &= check_input_mode(n, n_Store_mem, "mem", mode_M);
	fine &= check_input_func(n, n_Store_ptr, "ptr", mode_is_reference, "reference");
	fine &= check_input_func(n, n_Store_value, "value", mode_is_data_not_b, "data_not_b");
	return fine;
}

static int verify_node_Alloc(const ir_node *n)
{
	bool fine = check_mode(n, mode_T);
	fine &= check_input_mode(n, n_Alloc_mem, "mem", mode_M);
	fine &= check_input_func(n, n_Alloc_size, "size", mode_is_uint, "unsigned int");
	return fine;
}

static int verify_node_Free(const ir_node *n)
{
	bool fine = check_mode(n, mode_M);
	fine &= check_input_mode(n, n_Free_mem, "mem", mode_M);
	fine &= check_input_func(n, n_Free_ptr, "ptr", mode_is_reference, "reference");
	return fine;
}

static int verify_node_Sync(const ir_node *n)
{
	bool fine = check_mode(n, mode_M);
	for (int i = 0, n_preds = get_Sync_n_preds(n); i < n_preds; ++i) {
		fine &= check_input_mode(n, i, NULL, mode_M);
	}
	return fine;
}

static int verify_node_Confirm(const ir_node *n)
{
	bool fine = check_mode_same_input(n, n_Confirm_value, "value");
	fine &= check_mode_same_input(n, n_Confirm_bound, "bound");
	return fine;
}

static int verify_node_Mux(const ir_node *n)
{
	bool fine = check_mode_func(n, mode_is_data, "data");
	fine &= check_input_mode(n, n_Mux_sel, "sel", mode_b);
	fine &= check_mode_same_input(n, n_Mux_true, "true");
	fine &= check_mode_same_input(n, n_Mux_false, "false");
	return fine;
}

static int verify_node_CopyB(const ir_node *n)
{
	bool fine = check_mode(n, mode_M);
	fine &= check_input_func(n, n_CopyB_src, "src", mode_is_reference,
	                         "reference");
	fine &= check_input_func(n, n_CopyB_dst, "dst", mode_is_reference,
	                         "reference");
	fine &= check_input_mode(n, n_CopyB_mem, "mem", mode_M);
	ir_type *type = get_CopyB_type(n);
	if (!is_compound_type(type) && !is_Array_type(type)) {
		warn(n, "CopyB_type is no compound or Array type but %+F", type);
		fine = false;
	}
	return fine;
}

/**
 * Check dominance.
 * For each usage of a node, it is checked, if the block of the
 * node dominates the block of the usage (for phis: the predecessor
 * block of the phi for the corresponding edge).
 *
 * @return true on success, false on dominance error
 */
static bool check_dominance_for_node(const ir_node *use)
{
	bool fine = true;
	/* This won't work for blocks and the end node */
	if (is_Block(use) || is_End(use) || is_Anchor(use))
		return true;

	ir_node *bl = get_nodes_block(use);
	foreach_irn_in_r(use, i, def) {
		ir_node *def_bl = get_nodes_block(def);
		/* we have no dominance relation for unreachable blocks, so we can't
		 * check the dominance property there */
		if (!is_Block(def_bl) || get_Block_dom_depth(def_bl) == -1)
			continue;

		ir_node *use_bl = bl;
		if (is_Phi(use)) {
			use_bl = get_Block_cfgpred_block(bl, i);
			if (use_bl == NULL)
				continue;
		}
		if (get_Block_dom_depth(use_bl) == -1)
			continue;

		if (!block_dominates(def_bl, use_bl)) {
			warn(use, "not dominated by operand %+F", def);
			fine = false;
		}
	}
	return fine;
}

#ifdef DEBUG_libfirm
/**
 * Check if node is stored on the obstack belonging to the ir graph
 */
static bool check_irn_storage(ir_graph *irg, const ir_node *node)
{
	if (!node_is_in_irgs_storage(irg, node)) {
		warn(node, "node is not stored on graph obstack");
		return false;
	}
	return true;
}
#endif

int irn_verify(const ir_node *const n)
{
	ir_graph *const irg = get_irn_irg(n);

	bool fine = true;
	/* check_irn_storage is an expensive check for large graphs (it has a
	 * quadratic runtime but with a small constant); so do NOT run it in
	 * release mode */
#ifdef DEBUG_libfirm
	fine &= check_irn_storage(irg, n);
#endif
	/* abort if idx map is corrupt */
	unsigned idx           = get_irn_idx(n);
	ir_node *node_from_map = get_idx_irn(irg, idx);
	if (node_from_map != n) {
		warn(n, "node index and index map entry differ");
		return false;
	}

	ir_op *op = get_irn_op(n);
	if (op != op_Block && op != op_Anchor) {
		ir_node *block = get_nodes_block(n);
		if (!is_Block(block) && (!is_Bad(block)
		    || !irg_has_properties(irg, IR_GRAPH_PROPERTY_NO_BADS))) {
			warn(n, "block input is not a block (but %+F)", block);
			fine = false;
		} else if (is_op_start_block_placed(op)
		           && block != get_irg_start_block(irg)) {
			warn(n, "not placed in start block");
			fine = false;
		}
	}

	if (op->ops.verify_node != NULL)
		fine &= op->ops.verify_node(n);

	return fine;
}

/**
 * Walker to check every node
 */
static void verify_wrap(ir_node *node, void *env)
{
	bool *fine = (bool*)env;
	*fine &= irn_verify(node);
}

/**
 * Walker to check every node including SSA property.
 * Only called if dominance info is available.
 */
static void verify_wrap_ssa(ir_node *node, void *env)
{
	bool *fine = (bool*)env;

	*fine &= irn_verify(node);
	if (*fine) {
		*fine &= check_dominance_for_node(node);
	}
}

typedef struct check_cfg_env_t {
	pmap *branch_nodes; /**< map blocks to their branching nodes,
	                         map mode_X nodes to the blocks they branch to */
	bool  fine;
	ir_nodeset_t reachable_blocks;
	ir_nodeset_t kept_nodes;
	ir_nodeset_t true_projs;
	ir_nodeset_t false_projs;
} check_cfg_env_t;

static int check_block_cfg(const ir_node *block, check_cfg_env_t *env)
{
	bool fine = true;
	if (!ir_nodeset_contains(&env->reachable_blocks, block)) {
		warn(block, "not reachable by blockwalker (endless loop with no kept block?)");
		fine = false;
	}

	if (!get_Block_matured(block)) {
		warn(block, "immature block found");
		fine = false;
	}

	ir_graph *irg = get_irn_irg(block);
	if (get_Block_n_cfgpreds(block) == 0 && block != get_irg_start_block(irg)
	    && block != get_irg_end_block(irg)) {
		/* normal blocks must have at least 1 input (which may be a Bad node) */
		warn(block, "normal block must have at least 1 input");
		fine = false;
	}

	pmap *branch_nodes = env->branch_nodes;
	for (int i = 0, n_cfgpreds = get_Block_n_cfgpreds(block);
	     i < n_cfgpreds; ++i) {
		/* check that each mode_X node is only connected to 1 user */
		ir_node *branch = get_Block_cfgpred(block, i);
		branch = skip_Tuple(branch);
		if (is_Bad(branch))
			continue;
		ir_node *former_dest = pmap_get(ir_node, branch_nodes, branch);
		if (former_dest != NULL && !is_unknown_jump(skip_Proj(branch))) {
			warn(branch, "multiple users on mode_X node");
			fine = false;
			continue;
		}
		pmap_insert(branch_nodes, branch, (void*)block);

		/* check that there's only 1 branching instruction in each block */
		ir_node *branch_block = get_nodes_block(branch);
		ir_node *branch_proj  = branch;
		if (is_Proj(branch)) {
			branch = skip_Proj(branch);
		}
		ir_node *former_branch = pmap_get(ir_node, branch_nodes, branch_block);

		if (former_branch != NULL && former_branch != branch) {
			warn(branch_block, "multiple branching nodes (%+F,%+F) in block",
			     branch, former_branch);
			fine = false;
			continue;
		}
		pmap_insert(branch_nodes, branch_block, branch);

		if (is_Cond(branch)) {
			unsigned pn = get_Proj_num(branch_proj);
			if (pn == pn_Cond_true)
				ir_nodeset_insert(&env->true_projs, branch);
			if (pn == pn_Cond_false)
				ir_nodeset_insert(&env->false_projs, branch);
		} else if (is_Switch(branch)) {
			unsigned pn = get_Proj_num(branch_proj);
			if (pn == pn_Switch_default)
				ir_nodeset_insert(&env->true_projs, branch);
		}
	}
	return fine;
}

static void check_cfg_walk_func(ir_node *node, void *data)
{
	check_cfg_env_t *env = (check_cfg_env_t*)data;
	if (!is_Block(node))
		return;
	env->fine &= check_block_cfg(node, env);
}

static bool verify_block_branch(const ir_node *block, check_cfg_env_t *env)
{
	ir_node *branch = pmap_get(ir_node, env->branch_nodes, block);
	if (branch == NULL && !ir_nodeset_contains(&env->kept_nodes, block)
	    && block != get_irg_end_block(get_irn_irg(block))) {
		warn(block, "no cfopt in block");
		return false;
	}
	return true;
}

static int verify_cond_projs(const ir_node *cond, check_cfg_env_t *env)
{
	bool fine = true;
	if (!ir_nodeset_contains(&env->true_projs, cond)) {
		warn(cond, "Cond node lacks true proj");
		fine = false;
	}
	if (!ir_nodeset_contains(&env->false_projs, cond)) {
		warn(cond, "Cond node lacks false proj");
		fine = false;
	}
	return fine;
}

static int verify_switch_projs(const ir_node *sw, check_cfg_env_t *env)
{
	if (!ir_nodeset_contains(&env->true_projs, sw)) {
		warn(sw, "Switch lacks a default proj");
		return false;
	}
	return true;
}

static void assert_branch(ir_node *node, void *data)
{
	check_cfg_env_t *env = (check_cfg_env_t*)data;
	if (is_Block(node)) {
		env->fine &= verify_block_branch(node, env);
	} else if (is_Cond(node)) {
		env->fine &= verify_cond_projs(node, env);
	} else if (is_Switch(node)) {
		env->fine &= verify_switch_projs(node, env);
	}
}

static void collect_reachable_blocks(ir_node *block, void *data)
{
	ir_nodeset_t *reachable_blocks = (ir_nodeset_t*) data;
	ir_nodeset_insert(reachable_blocks, block);
}

/**
 * Checks CFG well-formedness
 */
static bool check_cfg(ir_graph *irg)
{
	check_cfg_env_t env;
	env.branch_nodes = pmap_create(); /**< map blocks to branch nodes */
	env.fine         = true;
	ir_nodeset_init(&env.reachable_blocks);
	ir_nodeset_init(&env.true_projs);
	ir_nodeset_init(&env.false_projs);

	irg_block_walk_graph(irg, collect_reachable_blocks, NULL,
	                     &env.reachable_blocks);

	/* note that we do not use irg_walk_block because it will miss these
	 * invalid blocks without a jump instruction which we want to detect
	 * here */
	irg_walk_graph(irg, check_cfg_walk_func, NULL, &env);

	ir_nodeset_init(&env.kept_nodes);
	foreach_irn_in(get_irg_end(irg), i, n) {
		ir_nodeset_insert(&env.kept_nodes, n);
	}
	irg_walk_graph(irg, assert_branch, NULL, &env);

	ir_nodeset_destroy(&env.false_projs);
	ir_nodeset_destroy(&env.true_projs);
	ir_nodeset_destroy(&env.kept_nodes);
	ir_nodeset_destroy(&env.reachable_blocks);
	pmap_destroy(env.branch_nodes);
	return env.fine;
}

static bool check_has_memory(ir_graph *irg)
{
	/* see if we can find any memory edge (either through a Return node
	 * or an endless loop) */
	ir_node *end_block = get_irg_end_block(irg);
	foreach_irn_in(end_block, i, ret) {
		if (!is_Bad(ret))
			return true;
	}
	/* we can't reliably detect backend calls yet */
	if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND))
		return true;
	ir_node *end = get_irg_end(irg);
	foreach_irn_in(end, i, kept) {
		if (get_irn_mode(kept) == mode_M || is_Call(kept))
			return true;
	}
	warn(NULL, "could not find any memory chain in graph %+F", irg);
	return false;
}

static bool check_graph_properties(ir_graph *irg);

int irg_verify(ir_graph *irg)
{
	bool fine   = true;
	bool pinned = get_irg_pinned(irg) == op_pin_state_pinned;

	if (pinned) {
		fine &= check_cfg(irg);
		if (fine)
			compute_doms(irg);
	}

	irg_walk_anchors(irg,
		pinned && irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE) ? verify_wrap_ssa : verify_wrap,
		NULL, &fine
	);

	if (fine) {
		fine = check_graph_properties(irg);
		fine &= check_has_memory(irg);
	}

	return fine;
}

void irg_assert_verify(ir_graph *irg)
{
	bool fine = irg_verify(irg);
	if (!fine) {
		dump_ir_graph(irg, "assert");
		abort();
	}
}

void ir_register_verify_node_ops(void)
{
	set_op_verify(op_Add,      verify_node_Add);
	set_op_verify(op_Address,  verify_node_Address);
	set_op_verify(op_Align,    verify_node_int);
	set_op_verify(op_Alloc,    verify_node_Alloc);
	set_op_verify(op_And,      verify_node_And);
	set_op_verify(op_Bitcast,  verify_node_Bitcast);
	set_op_verify(op_Block,    verify_node_Block);
	set_op_verify(op_Call,     verify_node_Call);
	set_op_verify(op_Cmp,      verify_node_Cmp);
	set_op_verify(op_Cond,     verify_node_Cond);
	set_op_verify(op_Confirm,  verify_node_Confirm);
	set_op_verify(op_Const,    verify_node_Const);
	set_op_verify(op_Conv,     verify_node_Conv);
	set_op_verify(op_CopyB,    verify_node_CopyB);
	set_op_verify(op_Deleted,  verify_node_Deleted);
	set_op_verify(op_Div,      verify_node_Div);
	set_op_verify(op_End,      verify_node_End);
	set_op_verify(op_Eor,      verify_node_Eor);
	set_op_verify(op_Free,     verify_node_Free);
	set_op_verify(op_IJmp,     verify_node_IJmp);
	set_op_verify(op_Jmp,      verify_node_Jmp);
	set_op_verify(op_Load,     verify_node_Load);
	set_op_verify(op_Member,   verify_node_Member);
	set_op_verify(op_Minus,    verify_node_Minus);
	set_op_verify(op_Mod,      verify_node_Mod);
	set_op_verify(op_Mul,      verify_node_Mul);
	set_op_verify(op_Mulh,     verify_node_Mulh);
	set_op_verify(op_Mux,      verify_node_Mux);
	set_op_verify(op_Not,      verify_node_Not);
	set_op_verify(op_Offset,   verify_node_int);
	set_op_verify(op_Or,       verify_node_Or);
	set_op_verify(op_Phi,      verify_node_Phi);
	set_op_verify(op_Proj,     verify_node_Proj);
	set_op_verify(op_Raise,    verify_node_Raise);
	set_op_verify(op_Return,   verify_node_Return);
	set_op_verify(op_Sel,      verify_node_Sel);
	set_op_verify(op_Shl,      verify_node_Shl);
	set_op_verify(op_Shr,      verify_node_Shr);
	set_op_verify(op_Shrs,     verify_node_Shrs);
	set_op_verify(op_Size,     verify_node_int);
	set_op_verify(op_Start,    verify_node_Start);
	set_op_verify(op_Store,    verify_node_Store);
	set_op_verify(op_Sub,      verify_node_Sub);
	set_op_verify(op_Switch,   verify_node_Switch);
	set_op_verify(op_Sync,     verify_node_Sync);

	set_op_verify_proj(op_Alloc,  verify_node_Proj_Alloc);
	set_op_verify_proj(op_Call,   verify_node_Proj_Call);
	set_op_verify_proj(op_Cond,   verify_node_Proj_Cond);
	set_op_verify_proj(op_Div,    verify_node_Proj_Div);
	set_op_verify_proj(op_Load,   verify_node_Proj_Load);
	set_op_verify_proj(op_Mod,    verify_node_Proj_Mod);
	set_op_verify_proj(op_Proj,   verify_node_Proj_Proj);
	set_op_verify_proj(op_Raise,  verify_node_Proj_Raise);
	set_op_verify_proj(op_Start,  verify_node_Proj_Start);
	set_op_verify_proj(op_Store,  verify_node_Proj_Store);
	set_op_verify_proj(op_Switch, verify_node_Proj_Switch);
	set_op_verify_proj(op_Tuple,  verify_node_Proj_Tuple);
}

static bool has_multiple_X_succs(const ir_node *node)
{
	return is_Cond(node) || is_Switch(node)
	    || (is_fragile_op(node) && ir_throws_exception(node));
}

static unsigned n_returns;
static bool     properties_fine;

static void check_simple_properties(ir_node *node, void *env)
{
	ir_graph *irg = (ir_graph*)env;
	if (is_Bad(node)) {
		if (irg_has_properties(irg, IR_GRAPH_PROPERTY_NO_BADS)) {
			warn(node, "IR_GRAPH_PROPERTY_NO_BADS is set, but Bad exists");
			properties_fine = false;
		}
	}
	if (is_Tuple(node)) {
		if (irg_has_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES)) {
			warn(node, "IR_GRAPH_PROPERTY_NO_TUPLES is set, but Tuple exists");
			properties_fine = false;
		}
	}
	if (is_Block(node)) {
		if (irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		                          | IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE)
		    && get_Block_dom_depth(node) == -1) {
			warn(node, "IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE + IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE is set, but block has depth -1");
			properties_fine = false;
		}
		if (irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE
		                          | IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE)
		    && get_Block_postdom_depth(node) == -1) {
			warn(node, "IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE + IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE is set, but block has depth -1");
			properties_fine = false;
		}

		if (get_irn_arity(node) > 1
		    && irg_has_properties(irg, IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES)) {
			foreach_irn_in(node, p, pred) {
				ir_node *skipped = skip_Proj(skip_Tuple(pred));
				if (has_multiple_X_succs(skipped)) {
					warn(node, "IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES set, but edge %+F->%+F is critical", node, skipped);
					properties_fine = false;
				}
			}
		}
	}
	if (is_Return(node)) {
		++n_returns;
		if (n_returns > 1
		    && irg_has_properties(irg, IR_GRAPH_PROPERTY_ONE_RETURN)) {
			warn(node, "IR_GRAPH_PROPERTY_ONE_RETURN set, but multiple return nodes found");
			properties_fine = false;
		}
	}
}

static void insert_node(ir_node *node, void *env)
{
	ir_nodeset_t *nodes = (ir_nodeset_t*)env;
	ir_nodeset_insert(nodes, node);
}

static void assure_in_set(ir_node *node, void *env)
{
	ir_nodeset_t *nodes = (ir_nodeset_t*)env;
	if (!ir_nodeset_contains(nodes, node)) {
		warn(node, "IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES set, but node cannot be found with normal edges");
		properties_fine = false;
	}
	ir_nodeset_remove(nodes, node);
}

static void check_consistent_out_edges(ir_graph *irg)
{
	if (!irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES))
		return;

	if (!edges_verify(irg))
		properties_fine = false;

	ir_nodeset_t nodes;
	ir_nodeset_init(&nodes);
	irg_walk_anchors(irg, insert_node, NULL, &nodes);

	irg_walk_edges(get_irg_start_block(irg), assure_in_set, NULL, &nodes);
	ir_node *end_block = get_irg_end_block(irg);
	if (!irn_visited(end_block)) {
		assure_in_set(end_block, &nodes);
	}

	foreach_ir_nodeset(&nodes, node, i) {
		warn(node, "IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES set, but node cannot be found with out edges");
		properties_fine = false;
	}
	ir_nodeset_destroy(&nodes);
}

static bool check_graph_properties(ir_graph *irg)
{
	properties_fine = true;
	n_returns = 0;
	irg_walk_graph(irg, check_simple_properties, NULL, irg);
	check_consistent_out_edges(irg);
	return properties_fine;
}
