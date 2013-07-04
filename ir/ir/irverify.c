/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Check irnodes for correctness.
 * @author   Christian Schaefer, Goetz Lindenmaier, Till Riedel, Michael Beck
 */
#include "irnode_t.h"
#include "irprog.h"
#include "irop_t.h"
#include "irgraph_t.h"
#include "irverify_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom_t.h"
#include "irprintf.h"
#include "irouts.h"
#include "irflag_t.h"
#include "irnodeset.h"
#include "ircons.h"

const char *firm_verify_failure_msg;

/**
 * little helper for NULL modes
 */
static const char *get_mode_name_ex(ir_mode *mode)
{
	if (!mode)
		return "<no mode>";
	return get_mode_name(mode);
}

/** the last IRG, on which a verification error was found */
static ir_graph *last_irg_error = NULL;

/**
 * print the name of the entity of an verification failure
 *
 * @param node  the node caused the failure
 */
static void show_entity_failure(const ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	if (last_irg_error == irg)
		return;

	last_irg_error = irg;
	if (irg == get_const_code_irg()) {
		fprintf(stderr, "\nFIRM: irn_verify_irg() <of CONST_CODE_IRG> failed\n");
	} else {
		ir_entity *ent = get_irg_entity(irg);

		if (ent) {
			ir_type *ent_type = get_entity_owner(ent);
			ir_fprintf(stderr, "\nFIRM: irn_verify_irg() %+F::%s failed\n", ent_type, get_entity_name(ent));
		} else {
			fprintf(stderr, "\nFIRM: irn_verify_irg() <IRG %p> failed\n", (void *)irg);
		}
	}
}

static const char *get_irn_modename(const ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	return get_mode_name(mode);
}

/**
 * Prints a failure for a Node
 */
static void show_node_failure(const ir_node *n)
{
	show_entity_failure(n);
	fprintf(stderr, "  node %ld %s%s\n" ,
		get_irn_node_nr(n),
		get_irn_opname(n), get_irn_modename(n)
	);
}

static void show_node_mode_mismatch(const ir_node *n, const char *text)
{
	show_entity_failure(n);
	fprintf(stderr, "  node %ld %s%s(", get_irn_node_nr(n),
	        get_irn_opname(n), get_irn_modename(n));
	const char *comma = "";
	for (int i = 0, arity = get_irn_arity(n); i < arity; ++i) {
		ir_node *op = get_irn_n(n, i);
		fprintf(stderr, "%s%s%s", comma, get_irn_opname(op),
		        get_irn_modename(op));
		comma = ", ";
	}
	fprintf(stderr, ") did not match (%s)\n", text);
}

/**
 * Prints a failure message for a proj
 */
static void show_proj_failure(const ir_node *n)
{
	ir_node *op   = get_Proj_pred(n);
	int      proj = get_Proj_proj(n);

	show_entity_failure(n);
	fprintf(stderr, "  node %ld %s%s %d(%s%s) failed\n" ,
	        get_irn_node_nr(n), get_irn_opname(n), get_irn_modename(n), proj,
	        get_irn_opname(op), get_irn_modename(op));
}

/**
 * Prints a failure message for a proj from Start
 */
static void show_proj_mode_failure(const ir_node *n, ir_type *ty)
{
	long     proj = get_Proj_proj(n);
	ir_mode *m    = get_type_mode(ty);
	char type_name[256];
	ir_print_type(type_name, sizeof(type_name), ty);

	show_entity_failure(n);
	fprintf(stderr, "  Proj %ld mode %s proj %ld (type %s mode %s) failed\n" ,
	        get_irn_node_nr(n), get_irn_modename(n), proj, type_name,
	        get_mode_name_ex(m));
}

#ifdef DEBUG_libfirm
/**
 * Show a node and a graph
 */
static void show_node_on_graph(const ir_graph *irg, const ir_node *n)
{
	ir_fprintf(stderr, "\nFIRM: irn_verify_irg() of %+F, node %+F\n", irg, n);
}
#endif

/**
 * Show call parameters
 */
static void show_call_param(const ir_node *n, ir_type *mt)
{
	char type_name[256];
	ir_print_type(type_name, sizeof(type_name), mt);

	show_entity_failure(n);
	fprintf(stderr, "  Call type-check failed: %s(", type_name);
	size_t n_method_params = get_method_n_params(mt);
	for (size_t i = 0; i < n_method_params; ++i) {
		fprintf(stderr, "%s ", get_mode_name_ex(get_type_mode(get_method_param_type(mt, i))));
	}
	fprintf(stderr, ") != CALL(");

	int n_params = get_Call_n_params(n);
	for (int i = 0; i < n_params; ++i) {
		fprintf(stderr, "%s ", get_mode_name_ex(get_irn_mode(get_Call_param(n, i))));
	}
	fprintf(stderr, ")\n");
}

/**
 * Show return modes
 */
static void show_return_modes(const ir_graph *irg, const ir_node *n,
                              ir_type *mt, int i)
{
	ir_entity *ent = get_irg_entity(irg);

	show_entity_failure(n);
	fprintf(stderr, "  Return node %ld in entity \"%s\" mode %s different from type mode %s\n",
	        get_irn_node_nr(n), get_entity_name(ent),
	        get_mode_name_ex(get_irn_mode(get_Return_res(n, i))),
	        get_mode_name_ex(get_type_mode(get_method_res_type(mt, i)))
	);
}

/**
 * Show return number of results
 */
static void show_return_nres(const ir_graph *irg, const ir_node *n, ir_type *mt)
{
	ir_entity *ent = get_irg_entity(irg);

	show_entity_failure(n);
	fprintf(stderr, "  Return node %ld in entity \"%s\" has %lu results different from type %lu\n",
	        get_irn_node_nr(n), get_entity_name(ent),
	        (unsigned long) get_Return_n_ress(n),
	        (unsigned long) get_method_n_ress(mt));
}

/**
 * Show Phi input
 */
static void show_phi_failure(const ir_node *phi, const ir_node *pred, int pos)
{
	(void) pos;
	show_entity_failure(phi);
	fprintf(stderr, "  Phi node %ld has mode %s different from predeccessor node %ld mode %s\n",
	        get_irn_node_nr(phi), get_mode_name_ex(get_irn_mode(phi)),
	        get_irn_node_nr(pred), get_mode_name_ex(get_irn_mode(pred)));
}

/**
 * Show Phi inputs
 */
static void show_phi_inputs(const ir_node *phi, const ir_node *block)
{
	show_entity_failure(phi);
	fprintf(stderr, "  Phi node %ld has %d inputs, its Block %ld has %d\n",
	        get_irn_node_nr(phi),   get_irn_arity(phi),
	        get_irn_node_nr(block), get_irn_arity(block));
}

/**
 * verify a Proj(Start) node
 */
static int verify_node_Proj_Start(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		(
			(proj == pn_Start_X_initial_exec && mode == mode_X) ||
			(proj == pn_Start_M              && mode == mode_M) ||
			(proj == pn_Start_P_frame_base   && mode_is_reference(mode)) ||
			(proj == pn_Start_T_args         && mode == mode_T)
		),
		"wrong Proj from Start", 0,
		show_proj_failure(p);
	);
	return 1;
}

/**
 * verify a Proj(Cond) node
 */
static int verify_node_Proj_Cond(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		mode == mode_X && (proj == pn_Cond_false || proj == pn_Cond_true),
		"wrong Proj from Cond", 0,
		show_proj_failure(p);
	);
	return 1;
}

static int verify_node_Proj_Switch(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	long     pn   = get_Proj_proj(p);
	ir_node *pred = get_Proj_pred(p);
	ASSERT_AND_RET_DBG(
		mode == mode_X && (pn >= 0 && pn < (long)get_Switch_n_outs(pred)),
		"wrong Proj from Switch", 0,
		show_proj_failure(p);
	);
	return 1;
}

/**
 * verify a Proj(Raise) node
 */
static int verify_node_Proj_Raise(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		((proj == pn_Raise_X && mode == mode_X) || (proj == pn_Raise_M && mode == mode_M)),
		"wrong Proj from Raise", 0,
		show_proj_failure(p);
	);
	return 1;
}

/**
 * verify a Proj(Call) node
 */
static int verify_node_Proj_Call(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	ir_node *n    = get_Proj_pred(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		(
			(proj == pn_Call_M                && mode == mode_M) ||
			(proj == pn_Call_X_regular        && mode == mode_X) ||
			(proj == pn_Call_X_except         && mode == mode_X) ||
			(proj == pn_Call_T_result         && mode == mode_T)
		),
		"wrong Proj from Call", 0,
		show_proj_failure(p);
	);
	/* if we have exception flow, we must have a real Memory input */
	if (proj == pn_Call_X_regular) {
		ASSERT_AND_RET(
			!is_NoMem(get_Call_mem(n)),
			"Regular Proj from FunctionCall", 0);
	} else if (proj == pn_Call_X_except) {
		ASSERT_AND_RET(
			!is_NoMem(get_Call_mem(n)),
			"Exception Proj from FunctionCall", 0);
	}
	return 1;
}

/**
 * verify a Proj(Div) node
 */
static int verify_node_Proj_Div(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	ir_node *n    = get_Proj_pred(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		(
			(proj == pn_Div_M         && mode == mode_M) ||
			(proj == pn_Div_X_regular && mode == mode_X) ||
			(proj == pn_Div_X_except  && mode == mode_X) ||
			(proj == pn_Div_res       && mode == get_Div_resmode(n))
		),
		"wrong Proj from Div", 0,
		show_proj_failure(p);
	);
	if (proj == pn_Div_X_regular) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Regular Proj from unpinned Div", 0);
	} else if (proj == pn_Div_X_except) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Exception Proj from unpinned Div", 0);
	} else if (proj == pn_Div_M) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Memory Proj from unpinned Div", 0);
	}
	return 1;
}

/**
 * verify a Proj(Mod) node
 */
static int verify_node_Proj_Mod(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	ir_node *n    = get_Proj_pred(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		(
			(proj == pn_Mod_M         && mode == mode_M) ||
			(proj == pn_Mod_X_regular && mode == mode_X) ||
			(proj == pn_Mod_X_except  && mode == mode_X) ||
			(proj == pn_Mod_res       && mode == get_Mod_resmode(n))
		),
		"wrong Proj from Mod", 0,
		show_proj_failure(p);
	);
	if (proj == pn_Mod_X_regular) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Regular Proj from unpinned Mod", 0);
	} else if (proj == pn_Mod_X_except) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Exception Proj from unpinned Mod", 0);
	} else if (proj == pn_Mod_M) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Memory Proj from unpinned Div", 0);
	}
	return 1;
}

/**
 * verify a Proj(Load) node
 */
static int verify_node_Proj_Load(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	ir_node *n    = get_Proj_pred(p);
	long     proj = get_Proj_proj(p);

	if (proj == pn_Load_res) {
		ASSERT_AND_RET_DBG(
			mode_is_data(mode) && mode == get_Load_mode(n),
			"wrong data Proj from Load", 0,
			show_proj_failure(p);
		);
	} else {
		ASSERT_AND_RET_DBG(
			(
				(proj == pn_Load_M         && mode == mode_M) ||
				(proj == pn_Load_X_regular && mode == mode_X) ||
				(proj == pn_Load_X_except  && mode == mode_X)
			),
			"wrong Proj from Load", 0,
			show_proj_failure(p);
		);
	}
	if (proj == pn_Load_X_regular) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Regular Proj from unpinned Load", 0);
	} else if (proj == pn_Load_X_except) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Exception Proj from unpinned Load", 0);
	}
	return 1;
}

/**
 * verify a Proj(Store) node
 */
static int verify_node_Proj_Store(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	ir_node *n    = get_Proj_pred(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		(
			(proj == pn_Store_M         && mode == mode_M) ||
			(proj == pn_Store_X_regular && mode == mode_X) ||
			(proj == pn_Store_X_except  && mode == mode_X)
		),
		"wrong Proj from Store", 0,
		show_proj_failure(p);
	);
	if (proj == pn_Store_X_regular) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Regular Proj from unpinned Store", 0);
	} else if (proj == pn_Store_X_except) {
		ASSERT_AND_RET(
			get_irn_pinned(n) == op_pin_state_pinned,
			"Exception Proj from unpinned Store", 0);
	}
	return 1;
}

/**
 * verify a Proj(Alloc) node
 */
static int verify_node_Proj_Alloc(const ir_node *p)
{
	ir_mode *mode = get_irn_mode(p);
	long     proj = get_Proj_proj(p);

	ASSERT_AND_RET_DBG(
		(
			(proj == pn_Alloc_M   && mode == mode_M) ||
			(proj == pn_Alloc_res && mode_is_reference(mode))
		),
		"wrong Proj from Alloc", 0,
		show_proj_failure(p);
	);
	return 1;
}

/**
 * verify a Proj(Proj) node
 */
static int verify_node_Proj_Proj(const ir_node *p)
{
	ir_mode *mode     = get_irn_mode(p);
	ir_node *pred     = get_Proj_pred(p);
	ir_node *predpred = get_Proj_pred(pred);
	long     proj     = get_Proj_proj(p);
	long     nr       = get_Proj_proj(pred);

	switch (get_irn_opcode(predpred)) {
	case iro_Start: {
		ir_graph *irg = get_irn_irg(p);
		ir_type  *mt  = get_entity_type(get_irg_entity(irg));

		if (nr == pn_Start_T_args) {
			ASSERT_AND_RET(
				(proj >= 0 && mode_is_datab(mode)),
				"wrong Proj from Proj from Start", 0);
			ASSERT_AND_RET(
				(proj < (int)get_method_n_params(mt)),
				"More Projs for args than args in type", 0
				);
			ir_type *param_type = get_method_param_type(mt, proj);
			if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
				ASSERT_AND_RET_DBG(
						(mode == get_type_mode(param_type)),
						"Mode of Proj from Start doesn't match mode of param type.", 0,
						show_proj_mode_failure(p, get_method_param_type(mt, proj));
						);
			}
		}
		break;
	}

	case iro_Call: {
		ASSERT_AND_RET(
			(proj >= 0 && mode_is_datab(mode)),
			"wrong Proj from Proj from Call", 0);
		ir_type *mt = get_Call_type(predpred);
		ASSERT_AND_RET(is_unknown_type(mt) || is_Method_type(mt),
				"wrong call type on call", 0);
		ASSERT_AND_RET(
			(proj < (int)get_method_n_ress(mt)),
			"More Projs for results than results in type.", 0);
		ir_type *res_type = get_method_res_type(mt, proj);
		/* value result */
		if ((mode_is_reference(mode)) &&
			(is_compound_type(res_type) || is_Array_type(res_type)))
			break;

		ASSERT_AND_RET(
			(mode == get_type_mode(get_method_res_type(mt, proj))),
			"Mode of Proj from Call doesn't match mode of result type.", 0);
		break;
	}

	case iro_Tuple:
		/* We don't test */
		break;

	default:
		/* ASSERT_AND_RET(0, "Unknown opcode", 0); */
		break;
	}
	return 1;
}

/**
 * verify a Proj(Tuple) node
 */
static int verify_node_Proj_Tuple(const ir_node *p)
{
	(void) p;
	/* We don't test */
	return 1;
}

static int verify_node_Proj_fragile(const ir_node *node)
{
	ir_node *pred             = get_Proj_pred(node);
	int      throws_exception = ir_throws_exception(pred);
	ASSERT_AND_RET((!is_x_except_Proj(node) || throws_exception)
	    && (!is_x_regular_Proj(node) || throws_exception),
	    "X_except und X_regular Proj only allowed when throws_exception is set",
	    0);
	return 1;
}

/**
 * verify a Proj node
 */
static int verify_node_Proj(const ir_node *p)
{
	ir_graph *irg  = get_irn_irg(p);
	ir_node  *pred = skip_Id(get_Proj_pred(p));
	ASSERT_AND_RET(get_irn_mode(pred) == mode_T, "mode of a 'projed' node is not Tuple", 0);
	ASSERT_AND_RET(get_irg_pinned(irg) == op_pin_state_floats || get_nodes_block(pred) == get_nodes_block(p), "Proj must be in same block as its predecessor", 0);

	if (is_fragile_op(pred)) {
		int res = verify_node_Proj_fragile(p);
		if (res != 1)
			return res;
	}

	ir_op *op = get_irn_op(pred);
	if (op->ops.verify_proj_node)
		return op->ops.verify_proj_node(p);

	/* all went ok */
	return 1;
}

/**
 * verify a Block node
 */
static int verify_node_Block(const ir_node *n)
{
	ir_graph *irg = get_irn_irg(n);
	for (int i = get_Block_n_cfgpreds(n); i-- > 0; ) {
		ir_node *pred         = get_Block_cfgpred(n, i);
		ir_node *skipped_pred = skip_Proj(skip_Tuple(pred));
		ASSERT_AND_RET(get_irn_mode(pred) == mode_X,
			"Block node must have a mode_X predecessor", 0);
		ASSERT_AND_RET(is_cfop(skipped_pred) || is_Bad(skipped_pred), "Block predecessor must be a cfop (or Bad)", 0);
	}

	if (n == get_irg_start_block(irg)) {
		ASSERT_AND_RET(get_Block_n_cfgpreds(n) == 0, "Start Block node", 0);
	}

	if (n == get_irg_end_block(irg) && !irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
		/* End block may only have Return, Raise or fragile ops as preds. */
		for (int i = get_Block_n_cfgpreds(n); i-- > 0; ) {
			ir_node *pred =  skip_Proj(get_Block_cfgpred(n, i));
			if (is_Proj(pred) || is_Tuple(pred))
				break;   /*  We can not test properly.  How many tuples are there? */
			ASSERT_AND_RET(
				(
					is_Return(pred) ||
					is_Bad(pred)    ||
					is_Raise(pred)  ||
					is_fragile_op(pred)
				),
				"End Block node", 0);
		}
	}
	/*  irg attr must == graph we are in. */
	ASSERT_AND_RET(((get_irn_irg(n) && get_irn_irg(n) == irg)), "Block node has wrong irg attribute", 0);
	return 1;
}

/**
 * verify a Start node
 */
static int verify_node_Start(const ir_node *n)
{
	ir_mode *mymode = get_irn_mode(n);

	ASSERT_AND_RET(
		/* Start: BB --> X x M x ref x data1 x ... x datan x ref */
		mymode == mode_T, "Start node", 0
		);
	return 1;
}

/**
 * verify a Jmp node
 */
static int verify_node_Jmp(const ir_node *n)
{
	ir_mode *mymode = get_irn_mode(n);

	ASSERT_AND_RET(
		/* Jmp: BB --> X */
		mymode == mode_X, "Jmp node", 0
	);
	return 1;
}

/**
 * verify an IJmp node
 */
static int verify_node_IJmp(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_IJmp_target(n));

	ASSERT_AND_RET(
		/* IJmp: BB x ref --> X */
		mymode == mode_X && mode_is_reference(op1mode), "IJmp node", 0
	);
	return 1;
}

/**
 * verify a Cond node
 */
static int verify_node_Cond(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Cond_selector(n));

	ASSERT_AND_RET(op1mode == mode_b, "Cond operand not mode_b", 0);
	ASSERT_AND_RET(mymode == mode_T, "Cond mode is not a tuple", 0);
	return 1;
}

static int verify_switch_table(const ir_node *n)
{
	const ir_switch_table *table     = get_Switch_table(n);
	unsigned               n_outs    = get_Switch_n_outs(n);
	ir_node               *selector  = get_Switch_selector(n);
	ir_mode               *mode      = get_irn_mode(selector);

	ASSERT_AND_RET(table != NULL, "switch table is NULL", 0);

	size_t n_entries = ir_switch_table_get_n_entries(table);
	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		if (entry->pn == 0)
			continue;
		ASSERT_AND_RET(entry->min != NULL && entry->max != NULL,
		               "switch table entry without min+max value", 0);
		ASSERT_AND_RET(get_tarval_mode(entry->min) == mode &&
		               get_tarval_mode(entry->max) == mode,
		               "switch table entry with wrong modes", 0);
		ASSERT_AND_RET(tarval_cmp(entry->min, entry->max) != ir_relation_greater,
		               "switch table entry without min+max value", 0);
		ASSERT_AND_RET(entry->pn >= 0 && entry->pn < (long)n_outs,
					   "switch table entry with invalid proj number", 0);
	}
	return 1;
}

static int verify_node_Switch(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Switch_selector(n));
	if (!verify_switch_table(n))
		return 0;

	ASSERT_AND_RET(mode_is_int(op1mode), "Switch operand not integer", 0);
	ASSERT_AND_RET(mymode == mode_T, "Switch mode is not a tuple", 0);
	return 1;
}

/**
 * verify a Return node
 */
static int verify_node_Return(const ir_node *n)
{
	ir_graph *irg      = get_irn_irg(n);
	ir_mode  *mymode   = get_irn_mode(n);
	ir_mode  *mem_mode = get_irn_mode(get_Return_mem(n));

	/* Return: BB x M x data1 x ... x datan --> X */

	ASSERT_AND_RET( mem_mode == mode_M, "Return node", 0 );  /* operand M */

	for (int i = get_Return_n_ress(n); i-- > 0; ) {
		ASSERT_AND_RET( mode_is_datab(get_irn_mode(get_Return_res(n, i))), "Return node", 0 );  /* operand datai */
	}
	ASSERT_AND_RET( mymode == mode_X, "Result X", 0 );   /* result X */
	/* Compare returned results with result types of method type */
	ir_type *mt = get_entity_type(get_irg_entity(irg));
	ASSERT_AND_RET_DBG((size_t)get_Return_n_ress(n) == get_method_n_ress(mt),
		"Number of results for Return doesn't match number of results in type.", 0,
		show_return_nres(irg, n, mt););
	for (int i = get_Return_n_ress(n); i-- > 0; ) {
		ir_type *res_type = get_method_res_type(mt, i);

		if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
			if (is_atomic_type(res_type)) {
				ASSERT_AND_RET_DBG(
					get_irn_mode(get_Return_res(n, i)) == get_type_mode(res_type),
					"Mode of result for Return doesn't match mode of result type.", 0,
					show_return_modes(irg, n, mt, i);
				);
			} else {
				ASSERT_AND_RET_DBG(
					mode_is_reference(get_irn_mode(get_Return_res(n, i))),
					"Mode of result for Return doesn't match mode of result type.", 0,
					show_return_modes(irg, n, mt, i);
				);
			}
		}
	}
	return 1;
}

/**
 * verify a Raise node
 */
static int verify_node_Raise(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Raise_mem(n));
	ir_mode *op2mode = get_irn_mode(get_Raise_exo_ptr(n));

	ASSERT_AND_RET(
		/* Sel: BB x M x ref --> X x M */
		op1mode == mode_M && mode_is_reference(op2mode) &&
		mymode == mode_T, "Raise node", 0
	);
	return 1;
}

/**
 * verify a Const node
 */
static int verify_node_Const(const ir_node *n)
{
	ir_mode *mymode = get_irn_mode(n);

	ASSERT_AND_RET(
		/* Const: BB --> data */
		(mode_is_data(mymode) ||
		mymode == mode_b)      /* we want boolean constants for static evaluation */
		,"Const node", 0       /* of Cmp. */
	);
	ASSERT_AND_RET(
		/* the modes of the constant and teh tarval must match */
		mymode == get_tarval_mode(get_Const_tarval(n)),
		"Const node, tarval and node mode mismatch", 0
	);
	return 1;
}

/**
 * verify a SymConst node
 */
static int verify_node_SymConst(const ir_node *n)
{
	ir_mode *mymode = get_irn_mode(n);

	ASSERT_AND_RET(
		/* SymConst: BB --> int*/
		(mode_is_int(mymode) ||
		/* SymConst: BB --> ref */
		mode_is_reference(mymode))
		,"SymConst node", 0);
	if (get_SymConst_kind(n) == symconst_addr_ent) {
		ir_entity *ent = get_SymConst_entity(n);
		/* the is_method_entity(ent) exception is for nested functions... */
		ASSERT_AND_RET_DBG((get_entity_owner(ent)->flags & tf_segment)
		                   || is_method_entity(ent),
		                   "SymConst node with non-segment entity", 0,
		                   show_node_failure(n););
	}
	return 1;
}

/**
 * verify a Sel node
 */
static int verify_node_Sel(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Sel_mem(n));
	ir_mode *op2mode = get_irn_mode(get_Sel_ptr(n));
	ASSERT_AND_RET_DBG(
		/* Sel: BB x M x ref x int^n --> ref */
		(op1mode == mode_M && op2mode == mymode && mode_is_reference(mymode)),
		"Sel node", 0, show_node_failure(n);
	);

	for (int i = get_Sel_n_indexs(n); i-- > 0; ) {
		ASSERT_AND_RET_DBG(mode_is_int(get_irn_mode(get_Sel_index(n, i))), "Sel node", 0, show_node_failure(n););
	}
	ir_entity *ent = get_Sel_entity(n);
	ASSERT_AND_RET_DBG(ent, "Sel node with empty entity", 0, show_node_failure(n););
	ASSERT_AND_RET_DBG(!(get_entity_owner(ent)->flags & tf_segment),
	                   "Sel node with global entity", 0, show_node_failure(n););
	return 1;
}

/**
 * Check if the pinned state is right.
 */
static int verify_right_pinned(const ir_node *n)
{
	if (get_irn_pinned(n) == op_pin_state_pinned)
		return 1;
	ir_node *mem = get_Call_mem(n);

	/* if it's not pinned, its memory predecessor must be NoMem or Pin */
	if (is_NoMem(mem) || is_Pin(mem))
		return 1;
	return 0;
}

/**
 * verify a Call node
 */
static int verify_node_Call(const ir_node *n)
{
	ir_graph *irg     = get_irn_irg(n);
	ir_mode  *mymode  = get_irn_mode(n);
	ir_mode  *op1mode = get_irn_mode(get_Call_mem(n));
	ir_mode  *op2mode = get_irn_mode(get_Call_ptr(n));

	/* Call: BB x M x ref x data1 x ... x datan
	--> M x datan+1 x ... x data n+m */
	ASSERT_AND_RET( op1mode == mode_M && mode_is_reference(op2mode), "Call node", 0 );  /* operand M x ref */

	/* NoMem nodes are only allowed as memory input if the Call is NOT pinned */
	ASSERT_AND_RET(verify_right_pinned(n),"Call node with wrong memory input", 0 );

	ir_type *mt = get_Call_type(n);
	if (get_unknown_type() == mt) {
		return 1;
	}

	for (size_t i = 0, n_params = get_Call_n_params(n); i < n_params; ++i) {
		ASSERT_AND_RET( mode_is_datab(get_irn_mode(get_Call_param(n, i))), "Call node", 0 );  /* operand datai */
	}

	ASSERT_AND_RET( mymode == mode_T, "Call result not a tuple", 0 );   /* result T */
	/* Compare arguments of node with those of type */

	if (get_method_variadicity(mt) == variadicity_variadic) {
		ASSERT_AND_RET_DBG(
			(size_t)get_Call_n_params(n) >= get_method_n_params(mt),
			"Number of args for Call doesn't match number of args in variadic type.",
			0,
			ir_fprintf(stderr, "Call %+F has %d params, type %d\n",
			n, get_Call_n_params(n), get_method_n_params(mt));
		);
	} else {
		ASSERT_AND_RET_DBG(
			(size_t)get_Call_n_params(n) == get_method_n_params(mt),
			"Number of args for Call doesn't match number of args in non variadic type.",
			0,
			ir_fprintf(stderr, "Call %+F has %d params, type %d\n",
			n, get_Call_n_params(n), get_method_n_params(mt));
		);
	}

	for (size_t i = 0, n_params = get_method_n_params(mt); i < n_params; i++) {
		const ir_type *t = get_method_param_type(mt, i);

		if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
			if (is_atomic_type(t)) {
				ASSERT_AND_RET_DBG(
					get_irn_mode(get_Call_param(n, i)) == get_type_mode(t),
					"Mode of arg for Call doesn't match mode of arg type.", 0,
					show_call_param(n, mt);
				);
			} else {
				/* call with a compound type, mode must be reference */
				ASSERT_AND_RET_DBG(
					mode_is_reference(get_irn_mode(get_Call_param(n, i))),
					"Mode of arg for Call doesn't match mode of arg type.", 0,
					show_call_param(n, mt);
				);
			}
		}
	}

	return 1;
}

/**
 * verify an Add node
 */
static int verify_node_Add(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Add_left(n));
	ir_mode *op2mode = get_irn_mode(get_Add_right(n));

	ASSERT_AND_RET_DBG(
		(
			/* common Add: BB x numP x numP --> numP */
			(op1mode == mymode && op2mode == op1mode && mode_is_data(mymode)) ||
			/* Pointer Add: BB x ref x int --> ref */
			(mode_is_reference(op1mode) && mode_is_int(op2mode) && op1mode == mymode) ||
			/* Pointer Add: BB x int x ref --> ref */
			(mode_is_int(op1mode) && op2mode == mymode && mode_is_reference(mymode))
		),
		"Add node", 0,
		show_node_mode_mismatch(n,
			"/* common Add: BB x numP x numP --> numP */ |\n"
			"/* Pointer Add: BB x ref x int --> ref */   |\n"
			"/* Pointer Add: BB x int x ref --> ref */");
	);
	return 1;
}

/**
 * verify a Sub node
 */
static int verify_node_Sub(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Sub_left(n));
	ir_mode *op2mode = get_irn_mode(get_Sub_right(n));

	ASSERT_AND_RET_DBG(
		(
			/* common Sub: BB x numP x numP --> numP */
			(mymode ==op1mode && mymode == op2mode && mode_is_data(op1mode)) ||
			/* Pointer Sub: BB x ref x int --> ref */
			(op1mode == mymode && mode_is_int(op2mode) && mode_is_reference(mymode)) ||
			/* Pointer Sub: BB x ref x ref --> int */
			(op1mode == op2mode && mode_is_reference(op2mode) && mode_is_int(mymode))
		),
		"Sub node", 0,
		show_node_mode_mismatch(n,
			"/* common Sub: BB x numP x numP --> numP */ |\n"
			"/* Pointer Sub: BB x ref x int --> ref */   |\n"
			"/* Pointer Sub: BB x ref x ref --> int */" );
		);
	return 1;
}

/**
 * verify a Minus node
 */
static int verify_node_Minus(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Minus_op(n));

	ASSERT_AND_RET_DBG(
		/* Minus: BB x num --> num */
		op1mode == mymode && mode_is_num(op1mode), "Minus node", 0,
		show_node_mode_mismatch(n , "/* Minus: BB x num --> num */");
	);
	return 1;
}

/**
 * verify a Mul node
 */
static int verify_node_Mul(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Mul_left(n));
	ir_mode *op2mode = get_irn_mode(get_Mul_right(n));

	ASSERT_AND_RET_DBG(
		(
			/* Mul: BB x int_n x int_n --> int_n|int_2n */
			(mode_is_int(op1mode)   && op2mode == op1mode && mode_is_int(mymode) &&
			 (op1mode == mymode || get_mode_size_bits(op1mode) * 2 == get_mode_size_bits(mymode))) ||
			/* Mul: BB x float x float --> float */
			(mode_is_float(op1mode) && op2mode == op1mode && mymode == op1mode)
		),
		"Mul node",0,
		show_node_mode_mismatch(n,
			"/* Mul: BB x int_n x int_n --> int_n|int_2n */ |\n"
			"/* Mul: BB x float x float --> float */");
	);
	return 1;
}

/**
 * verify a Mulh node
 */
static int verify_node_Mulh(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Mulh_left(n));
	ir_mode *op2mode = get_irn_mode(get_Mulh_right(n));

	ASSERT_AND_RET_DBG(
		(
			/* Mulh: BB x int x int --> int */
			(mode_is_int(op1mode) && op2mode == op1mode && op1mode == mymode)
		),
		"Mulh node",0,
		show_node_mode_mismatch(n, "/* Mulh: BB x int x int --> int */");
	);
	return 1;
}

/**
 * verify a Div node
 */
static int verify_node_Div(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Div_mem(n));
	ir_mode *op2mode = get_irn_mode(get_Div_left(n));
	ir_mode *op3mode = get_irn_mode(get_Div_right(n));
	ir_mode *resmode = get_Div_resmode(n);

	ASSERT_AND_RET(
		/* Div: BB x M x num x num --> M x X x num */
		op1mode == mode_M    &&
		op2mode == resmode   &&
		op3mode == resmode   &&
		mode_is_num(resmode) &&
		mymode == mode_T,
		"Div node", 0
		);
	return 1;
}

/**
 * verify a Mod node
 */
static int verify_node_Mod(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Mod_mem(n));
	ir_mode *op2mode = get_irn_mode(get_Mod_left(n));
	ir_mode *op3mode = get_irn_mode(get_Mod_right(n));
	ir_mode *resmode = get_Mod_resmode(n);

	ASSERT_AND_RET(
		/* Mod: BB x M x int x int --> M x X x int */
		op1mode == mode_M    &&
		op2mode == resmode   &&
		op3mode == resmode   &&
		mode_is_int(resmode) &&
		mymode == mode_T,
		"Mod node", 0
		);
	return 1;
}

/**
 * verify a logical And, Or, Eor node
 */
static int verify_node_Logic(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_binop_left(n));
	ir_mode *op2mode = get_irn_mode(get_binop_right(n));

	ASSERT_AND_RET_DBG(
		/* And or Or or Eor: BB x int x int --> int */
		(mode_is_int(mymode) || mode_is_reference(mymode) || mymode == mode_b) &&
		op2mode == op1mode &&
		mymode == op2mode,
		"And, Or or Eor node", 0,
		show_node_mode_mismatch(n, "/* And or Or or Eor: BB x int x int --> int */");
	);
	return 1;
}

static int verify_node_And(const ir_node *n)
{
	return verify_node_Logic(n);
}

static int verify_node_Or(const ir_node *n)
{
	return verify_node_Logic(n);
}

static int verify_node_Eor(const ir_node *n)
{
	return verify_node_Logic(n);
}

/**
 * verify a Not node
 */
static int verify_node_Not(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Not_op(n));

	ASSERT_AND_RET_DBG(
		/* Not: BB x int --> int */
		(mode_is_int(mymode) || mymode == mode_b) &&
		mymode == op1mode,
		"Not node", 0,
		show_node_mode_mismatch(n, "/* Not: BB x int --> int */");
	);
	return 1;
}

/**
 * verify a Cmp node
 */
static int verify_node_Cmp(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Cmp_left(n));
	ir_mode *op2mode = get_irn_mode(get_Cmp_right(n));

	ASSERT_AND_RET_DBG(
		/* Cmp: BB x datab x datab --> b16 */
		mode_is_datab(op1mode) &&
		op2mode == op1mode &&
		mymode == mode_b,
		"Cmp node", 0,
		show_node_mode_mismatch(n, "/* Cmp: BB x datab x datab --> b16 */");
	);
	return 1;
}

/**
 * verify a Shift node
 */
static int verify_node_Shift(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_binop_left(n));
	ir_mode *op2mode = get_irn_mode(get_binop_right(n));

	ASSERT_AND_RET_DBG(
		/* Shl, Shr or Shrs: BB x int x int_u --> int */
		mode_is_int(op1mode) &&
		mode_is_int(op2mode) &&
		!mode_is_signed(op2mode) &&
		mymode == op1mode,
		"Shl, Shr or Shrs node", 0,
		show_node_mode_mismatch(n, "/* Shl, Shr or Shrs: BB x int x int_u --> int */");
	);
	return 1;
}

static int verify_node_Shl(const ir_node *n)
{
	return verify_node_Shift(n);
}

static int verify_node_Shr(const ir_node *n)
{
	return verify_node_Shift(n);
}

static int verify_node_Shrs(const ir_node *n)
{
	return verify_node_Shift(n);
}

/**
 * verify a Conv node
 */
static int verify_node_Conv(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Conv_op(n));

	ASSERT_AND_RET_DBG(mode_is_data(op1mode) && mode_is_data(mymode),
		"Conv node", 0,
		show_node_mode_mismatch(n, "/* Conv: BB x data --> data */");
	);
	return 1;
}

/**
 * verify a Phi node
 */
static int verify_node_Phi(const ir_node *n)
{
	ir_mode *mymode = get_irn_mode(n);
	ir_node *block  = get_nodes_block(n);

	/* a Phi node MUST have the same number of inputs as its block
	 * Exception is a phi with 0 inputs which is used when (re)constructing the
	 * SSA form */
	if (!is_Bad(block)
	    && !irg_is_constrained(get_irn_irg(n), IR_GRAPH_CONSTRAINT_CONSTRUCTION)
	    && get_irn_arity(n) > 0) {
		ASSERT_AND_RET_DBG(
			get_irn_arity(n) == get_irn_arity(block),
			"wrong number of inputs in Phi node", 0,
			show_phi_inputs(n, block);
		);
	}

	/* Phi: BB x dataM --> dataM */
	for (int i = get_Phi_n_preds(n) - 1; i >= 0; --i) {
		ir_node *pred = get_Phi_pred(n, i);
		ASSERT_AND_RET_DBG(get_irn_mode(pred) == mymode,
		                   "Phi node", 0, show_phi_failure(n, pred, i);
		);
	}
	ASSERT_AND_RET(mode_is_dataM(mymode) || mymode == mode_b, "Phi node", 0 );

	return 1;
}

/**
 * verify a Load node
 */
static int verify_node_Load(const ir_node *n)
{
	ir_graph *irg     = get_irn_irg(n);
	ir_mode  *mymode  = get_irn_mode(n);
	ir_mode  *op1mode = get_irn_mode(get_Load_mem(n));
	ir_mode  *op2mode = get_irn_mode(get_Load_ptr(n));

	ASSERT_AND_RET(op1mode == mode_M, "Load node", 0);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
		ASSERT_AND_RET(mode_is_reference(op2mode), "Load node", 0 );
	}
	ASSERT_AND_RET( mymode == mode_T, "Load node", 0 );

	return 1;
}

/**
 * verify a Store node
 */
static int verify_node_Store(const ir_node *n)
{
	ir_graph *irg = get_irn_irg(n);
	ir_mode  *mymode  = get_irn_mode(n);
	ir_mode  *op1mode = get_irn_mode(get_Store_mem(n));
	ir_mode  *op2mode = get_irn_mode(get_Store_ptr(n));
	ir_mode  *op3mode = get_irn_mode(get_Store_value(n));

	ASSERT_AND_RET(op1mode == mode_M && mode_is_datab(op3mode), "Store node", 0 );
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
		ASSERT_AND_RET(mode_is_reference(op2mode), "Store node", 0 );
	}
	ASSERT_AND_RET(mymode == mode_T, "Store node", 0);

	return 1;
}

/**
 * verify an Alloc node
 */
static int verify_node_Alloc(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Alloc_mem(n));
	ir_mode *op2mode = get_irn_mode(get_Alloc_size(n));

	ASSERT_AND_RET_DBG(
		/* Alloc: BB x M x int_u --> M x ref */
		op1mode == mode_M &&
		mode_is_int(op2mode) &&
		!mode_is_signed(op2mode) &&
		mymode == mode_T,
		"Alloc node", 0,
		show_node_failure(n);
	);
	return 1;
}

/**
 * verify a Free node
 */
static int verify_node_Free(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Free_mem(n));
	ir_mode *op2mode = get_irn_mode(get_Free_ptr(n));

	ASSERT_AND_RET_DBG(
		/* Free: BB x M x ref x int_u --> M */
		op1mode == mode_M && mode_is_reference(op2mode) &&
		mymode == mode_M,
		"Free node", 0,
		show_node_mode_mismatch(n, "/* Free: BB x M x ref x int_u --> M */");
	);
	return 1;
}

/**
 * verify a Sync node
 */
static int verify_node_Sync(const ir_node *n)
{
	/* Sync: BB x M^n --> M */
	for (int i = get_Sync_n_preds(n); i-- > 0; ) {
		ASSERT_AND_RET( get_irn_mode(get_Sync_pred(n, i)) == mode_M, "Sync node", 0 );
	}
	ir_mode *mymode  = get_irn_mode(n);
	ASSERT_AND_RET( mymode == mode_M, "Sync node", 0 );
	return 1;
}

/**
 * verify a Confirm node
 */
static int verify_node_Confirm(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Confirm_value(n));
	ir_mode *op2mode = get_irn_mode(get_Confirm_bound(n));

	ASSERT_AND_RET_DBG(
		/* Confirm: BB x T x T --> T */
		op1mode == mymode &&
		op2mode == mymode,
		"Confirm node", 0,
		show_node_mode_mismatch(n, "/* Confirm: BB x T x T --> T */");
	);
	return 1;
}

/**
 * verify a Mux node
 */
static int verify_node_Mux(const ir_node *n)
{
	ir_mode *mymode  = get_irn_mode(n);
	ir_mode *op1mode = get_irn_mode(get_Mux_sel(n));
	ir_mode *op2mode = get_irn_mode(get_Mux_true(n));
	ir_mode *op3mode = get_irn_mode(get_Mux_false(n));

	ASSERT_AND_RET(
		/* Mux: BB x b x datab x datab --> datab */
		op1mode == mode_b &&
		op2mode == mymode &&
		op3mode == mymode &&
		mode_is_datab(mymode),
		"Mux node", 0
		);
	return 1;
}

/**
 * verify a CopyB node
 */
static int verify_node_CopyB(const ir_node *n)
{
	ir_graph *irg     = get_irn_irg(n);
	ir_mode  *mymode  = get_irn_mode(n);
	ir_mode  *op1mode = get_irn_mode(get_CopyB_mem(n));
	ir_mode  *op2mode = get_irn_mode(get_CopyB_dst(n));
	ir_mode  *op3mode = get_irn_mode(get_CopyB_src(n));
	ir_type  *t = get_CopyB_type(n);

	/* CopyB: BB x M x ref x ref --> M x X */
	ASSERT_AND_RET(mymode == mode_M && op1mode == mode_M, "CopyB node", 0);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND)) {
		ASSERT_AND_RET(mode_is_reference(op2mode) && mode_is_reference(op3mode),
			"CopyB node", 0 );
	}

	ASSERT_AND_RET(
		is_compound_type(t) || is_Array_type(t),
		"CopyB node should copy compound types only", 0 );

	return 1;
}

/**
 * Check dominance.
 * For each usage of a node, it is checked, if the block of the
 * node dominates the block of the usage (for phis: the predecessor
 * block of the phi for the corresponding edge).
 *
 * @return non-zero on success, 0 on dominance error
 */
static int check_dominance_for_node(const ir_node *use)
{
	/* This won't work for blocks and the end node */
	if (!is_Block(use) && !is_End(use) && !is_Anchor(use)) {
		ir_node *bl = get_nodes_block(use);

		for (int i = get_irn_arity(use); i-- > 0; ) {
			ir_node  *def    = get_irn_n(use, i);
			ir_node  *def_bl = get_nodes_block(def);
			ir_node  *use_bl = bl;

			/* we have no dominance relation for unreachable blocks, so we can't
			 * check the dominance property there */
			if (!is_Block(def_bl) || get_Block_dom_depth(def_bl) == -1)
				continue;

			if (is_Phi(use)) {
				if (is_Bad(def))
					continue;
				use_bl = get_Block_cfgpred_block(bl, i);
			}

			if (!is_Block(use_bl) || get_Block_dom_depth(use_bl) == -1)
				continue;

			ASSERT_AND_RET_DBG(
				block_dominates(def_bl, use_bl),
				"the definition of a value used violates the dominance property", 0,
				ir_fprintf(stderr,
				"graph %+F: %+F of %+F must dominate %+F of user %+F input %d\n",
				get_irn_irg(use), def_bl, def, use_bl, use, i
				);
			);
		}
	}
	return 1;
}

int irn_verify_irg(const ir_node *n, ir_graph *irg)
{
	/*
	 * do NOT check placement in interprocedural view, as we don't always
	 * know the "right" graph ...
	 */

#ifdef DEBUG_libfirm
	/* this is an expensive check for large graphs (it has a quadratic
	 * runtime but with a small constant); so do NOT run it in release mode
	 */
	ASSERT_AND_RET_DBG(
		node_is_in_irgs_storage(irg, n),
		"Node is not stored on proper IR graph!", 0,
		show_node_on_graph(irg, n);
	);
#endif
	assert(get_irn_irg(n) == irg);
	unsigned idx           = get_irn_idx(n);
	ir_node *node_from_map = get_idx_irn(irg, idx);
	ASSERT_AND_RET_DBG(node_from_map == n, "Node index and index map entry differ", 0,
		ir_printf("node %+F node in map %+F(%p)\n", n, node_from_map, node_from_map);
	);

	ir_op *op = get_irn_op(n);

	if (get_op_pinned(op) >= op_pin_state_exc_pinned) {
		op_pin_state state = get_irn_pinned(n);
		ASSERT_AND_RET_DBG(
			state == op_pin_state_floats ||
			state == op_pin_state_pinned,
			"invalid pin state", 0,
			ir_printf("node %+F", n);
		);
	} else if (!is_Block(n)
	           && irg_has_properties(irg, IR_GRAPH_PROPERTY_NO_BADS)) {
		ASSERT_AND_RET_DBG(is_Block(get_nodes_block(n)) || is_Anchor(n),
				"block input is not a block", 0,
				ir_printf("node %+F", n);
		);
	}

	ASSERT_AND_RET_DBG(
		!is_irn_start_block_placed(n) || get_nodes_block(n) == get_irg_start_block(irg),
		"node should be in start block", 0,
		ir_printf("node %+F", n);
	);

	if (op->ops.verify_node)
		return op->ops.verify_node(n);

	/* All went ok */
	return 1;
}

int irn_verify(const ir_node *n)
{
	return irn_verify_irg(n, get_irn_irg(n));
}

/*-----------------------------------------------------------------*/
/* Verify the whole graph.                                         */
/*-----------------------------------------------------------------*/

#ifdef DEBUG_libfirm
/**
 * Walker to check every node
 */
static void verify_wrap(ir_node *node, void *env)
{
	int *res = (int*)env;
	*res = irn_verify_irg(node, get_irn_irg(node));
}

/**
 * Walker to check every node including SSA property.
 * Only called if dominance info is available.
 */
static void verify_wrap_ssa(ir_node *node, void *env)
{
	int *res = (int*)env;

	*res = irn_verify_irg(node, get_irn_irg(node));
	if (*res) {
		*res = check_dominance_for_node(node);
	}
}

#endif /* DEBUG_libfirm */

typedef struct check_cfg_env_t {
	pmap *branch_nodes; /**< map blocks to their branching nodes,
	                         map mode_X nodes to the blocks they branch to */
	int   res;
	ir_nodeset_t reachable_blocks;
	ir_nodeset_t kept_nodes;
	ir_nodeset_t true_projs;
	ir_nodeset_t false_projs;
} check_cfg_env_t;

#ifdef DEBUG_libfirm
static int check_block_cfg(const ir_node *block, check_cfg_env_t *env)
{
	ASSERT_AND_RET_DBG(ir_nodeset_contains(&env->reachable_blocks, block),
	                   "Block is not reachable by blockwalker (endless loop with no kept block?)", 0,
	                   ir_printf("block %+F\n", block);
	);

	int   n_cfgpreds   = get_Block_n_cfgpreds(block);
	pmap *branch_nodes = env->branch_nodes;
	for (int i = 0; i < n_cfgpreds; ++i) {
		/* check that each mode_X node is only connected
		 * to 1 user */
		ir_node *branch = get_Block_cfgpred(block, i);
		branch = skip_Tuple(branch);
		if (is_Bad(branch))
			continue;
		ir_node *former_dest = pmap_get(ir_node, branch_nodes, branch);
		ASSERT_AND_RET_DBG(former_dest==NULL || is_unknown_jump(skip_Proj(branch)),
						   "Multiple users on mode_X node", 0,
						   ir_printf("node %+F\n", branch);
		);
		pmap_insert(branch_nodes, branch, (void*)block);

		/* check that there's only 1 branching instruction in each block */
		ir_node *branch_block = get_nodes_block(branch);
		ir_node *branch_proj  = branch;
		if (is_Proj(branch)) {
			branch = skip_Proj(branch);
		}
		ir_node *former_branch = pmap_get(ir_node, branch_nodes, branch_block);

		ASSERT_AND_RET_DBG(former_branch == NULL || former_branch == branch,
						   "Multiple branching nodes in a block", 0,
						   ir_printf("nodes %+F,%+F in block %+F\n",
									 branch, former_branch, branch_block);
		);
		pmap_insert(branch_nodes, branch_block, branch);

		if (is_Cond(branch)) {
			long pn = get_Proj_proj(branch_proj);
			if (pn == pn_Cond_true)
				ir_nodeset_insert(&env->true_projs, branch);
			if (pn == pn_Cond_false)
				ir_nodeset_insert(&env->false_projs, branch);
		} else if (is_Switch(branch)) {
			long pn = get_Proj_proj(branch_proj);
			if (pn == pn_Switch_default)
				ir_nodeset_insert(&env->true_projs, branch);
		}
	}

	return 1;
}

static void check_cfg_walk_func(ir_node *node, void *data)
{
	check_cfg_env_t *env = (check_cfg_env_t*)data;
	if (!is_Block(node))
		return;
	env->res &= check_block_cfg(node, env);
}

static int verify_block_branch(const ir_node *block, check_cfg_env_t *env)
{
	ir_node *branch = pmap_get(ir_node, env->branch_nodes, block);
	ASSERT_AND_RET_DBG(branch != NULL
	                   || ir_nodeset_contains(&env->kept_nodes, block)
	                   || block == get_irg_end_block(get_irn_irg(block)),
	                   "block contains no cfop", 0,
	                   ir_printf("block %+F\n", block);
	);
	return 1;
}

static int verify_cond_projs(const ir_node *cond, check_cfg_env_t *env)
{
	ASSERT_AND_RET_DBG(ir_nodeset_contains(&env->true_projs, cond),
					   "Cond node lacks true proj", 0,
					   ir_printf("Cond %+F\n", cond);
	);
	ASSERT_AND_RET_DBG(ir_nodeset_contains(&env->false_projs, cond),
					   "Cond node lacks false proj", 0,
					   ir_printf("Cond %+F\n", cond);
	);
	return 1;
}

static int verify_switch_projs(const ir_node *sw, check_cfg_env_t *env)
{
	ASSERT_AND_RET_DBG(ir_nodeset_contains(&env->true_projs, sw),
					   "Switch node lacks default Proj", 0,
					   ir_printf("Switch %+F\n", sw);
	);
	return 1;
}

static void assert_branch(ir_node *node, void *data)
{
	check_cfg_env_t *env = (check_cfg_env_t*)data;
	if (is_Block(node)) {
		env->res &= verify_block_branch(node, env);
	} else if (is_Cond(node)) {
		env->res &= verify_cond_projs(node, env);
	} else if (is_Switch(node)) {
		env->res &= verify_switch_projs(node, env);
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
static int check_cfg(ir_graph *irg)
{
	check_cfg_env_t env;
	env.branch_nodes = pmap_create(); /**< map blocks to branch nodes */
	env.res          = 1;
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
	ir_node *end = get_irg_end(irg);
	for (int i = 0, arity = get_irn_arity(end); i < arity; ++i) {
		ir_node *n = get_irn_n(end, i);
		ir_nodeset_insert(&env.kept_nodes, n);
	}
	irg_walk_graph(irg, assert_branch, NULL, &env);

	ir_nodeset_destroy(&env.false_projs);
	ir_nodeset_destroy(&env.true_projs);
	ir_nodeset_destroy(&env.kept_nodes);
	ir_nodeset_destroy(&env.reachable_blocks);
	pmap_destroy(env.branch_nodes);
	return env.res;
}
#endif

int irg_verify(ir_graph *irg, unsigned flags)
{
	int res = 1;
#ifdef DEBUG_libfirm
	int pinned = get_irg_pinned(irg) == op_pin_state_pinned;

	last_irg_error = NULL;

	if (pinned && !check_cfg(irg))
		res = 0;

	if (res == 1 && (flags & VERIFY_ENFORCE_SSA) && pinned)
		compute_doms(irg);

	irg_walk_anchors(
		irg,
		pinned && irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE)
			? verify_wrap_ssa : verify_wrap,
		NULL,
		&res
	);

#else
	(void)irg;
	(void)flags;
#endif /* DEBUG_libfirm */

	return res;
}

int irn_verify_irg_dump(const ir_node *n, ir_graph *irg,
                        const char **bad_string)
{
	firm_verify_failure_msg = NULL;
	int res = irn_verify_irg(n, irg);
	if (res && irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE) &&
	    get_irg_pinned(irg) == op_pin_state_pinned)
		res = check_dominance_for_node(n);
	*bad_string = firm_verify_failure_msg;

	return res;
}

typedef struct verify_bad_env_t {
	int flags;
	int res;
} verify_bad_env_t;

/**
 * Pre-Walker: check Bad predecessors of node.
 */
static void check_bads(ir_node *node, void *env)
{
	verify_bad_env_t *venv = (verify_bad_env_t*)env;
	ir_graph *irg = get_irn_irg(node);

	if (is_Block(node)) {
		if ((venv->flags & BAD_CF) == 0) {

			/* check for Bad Block predecessor */
			for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
				ir_node *pred = get_irn_n(node, i);

				if (is_Bad(pred)) {
					venv->res |= BAD_CF;

					fprintf(stderr, "irg_verify_bads: Block %ld has Bad predecessor\n", get_irn_node_nr(node));
					dump_ir_graph(irg, "assert");
				}
			}
		}
	} else {
		if ((venv->flags & BAD_BLOCK) == 0) {

			/* check for Bad Block */
			if (is_Bad(get_nodes_block(node))) {
				venv->res |= BAD_BLOCK;

				fprintf(stderr, "irg_verify_bads: node %ld has Bad Block\n", get_irn_node_nr(node));
				dump_ir_graph(irg, "assert");
			}
		}

		if ((venv->flags & TUPLE) == 0) {
			if (is_Tuple(node)) {
				venv->res |= TUPLE;

				fprintf(stderr, "irg_verify_bads: node %ld is a Tuple\n", get_irn_node_nr(node));
				dump_ir_graph(irg, "assert");
			}
		}

		for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			ir_node *pred = get_irn_n(node, i);

			if (is_Bad(pred)) {
				/* check for Phi with Bad inputs */
				if (is_Phi(node) && !is_Bad(get_nodes_block(node)) && is_Bad(get_irn_n(get_nodes_block(node), i))) {
					if (venv->flags & BAD_CF)
						continue;
					else {
						venv->res |= BAD_CF;

						fprintf(stderr, "irg_verify_bads: Phi %ld has Bad Input\n", get_irn_node_nr(node));
						dump_ir_graph(irg, "assert");
					}
				}

				/* Bad node input */
				if ((venv->flags & BAD_DF) == 0) {
					venv->res |= BAD_DF;

					fprintf(stderr, "irg_verify_bads: node %ld has Bad Input\n", get_irn_node_nr(node));
					dump_ir_graph(irg, "assert");
				}
			}
		}
	}
}

int irg_verify_bads(ir_graph *irg, int flags)
{
	verify_bad_env_t env;
	env.flags = flags;
	env.res   = 0;

	irg_walk_graph(irg, check_bads, NULL, &env);
	return env.res;
}

static void register_verify_node_func(ir_op *op, verify_node_func func)
{
	op->ops.verify_node = func;
}

static void register_verify_node_func_proj(ir_op *op, verify_node_func func)
{
	op->ops.verify_proj_node = func;
}

void ir_register_verify_node_ops(void)
{
	register_verify_node_func(op_Add,      verify_node_Add);
	register_verify_node_func(op_Alloc,    verify_node_Alloc);
	register_verify_node_func(op_And,      verify_node_And);
	register_verify_node_func(op_Block,    verify_node_Block);
	register_verify_node_func(op_Call,     verify_node_Call);
	register_verify_node_func(op_Cmp,      verify_node_Cmp);
	register_verify_node_func(op_Cond,     verify_node_Cond);
	register_verify_node_func(op_Confirm,  verify_node_Confirm);
	register_verify_node_func(op_Const,    verify_node_Const);
	register_verify_node_func(op_Conv,     verify_node_Conv);
	register_verify_node_func(op_CopyB,    verify_node_CopyB);
	register_verify_node_func(op_Div,      verify_node_Div);
	register_verify_node_func(op_Eor,      verify_node_Eor);
	register_verify_node_func(op_Free,     verify_node_Free);
	register_verify_node_func(op_IJmp,     verify_node_IJmp);
	register_verify_node_func(op_Jmp,      verify_node_Jmp);
	register_verify_node_func(op_Load,     verify_node_Load);
	register_verify_node_func(op_Minus,    verify_node_Minus);
	register_verify_node_func(op_Mod,      verify_node_Mod);
	register_verify_node_func(op_Mul,      verify_node_Mul);
	register_verify_node_func(op_Mulh,     verify_node_Mulh);
	register_verify_node_func(op_Mux,      verify_node_Mux);
	register_verify_node_func(op_Not,      verify_node_Not);
	register_verify_node_func(op_Or,       verify_node_Or);
	register_verify_node_func(op_Phi,      verify_node_Phi);
	register_verify_node_func(op_Proj,     verify_node_Proj);
	register_verify_node_func(op_Raise,    verify_node_Raise);
	register_verify_node_func(op_Return,   verify_node_Return);
	register_verify_node_func(op_Sel,      verify_node_Sel);
	register_verify_node_func(op_Shl,      verify_node_Shl);
	register_verify_node_func(op_Shr,      verify_node_Shr);
	register_verify_node_func(op_Shrs,     verify_node_Shrs);
	register_verify_node_func(op_Start,    verify_node_Start);
	register_verify_node_func(op_Store,    verify_node_Store);
	register_verify_node_func(op_Sub,      verify_node_Sub);
	register_verify_node_func(op_Switch,   verify_node_Switch);
	register_verify_node_func(op_SymConst, verify_node_SymConst);
	register_verify_node_func(op_Sync,     verify_node_Sync);

	register_verify_node_func_proj(op_Alloc,  verify_node_Proj_Alloc);
	register_verify_node_func_proj(op_Call,   verify_node_Proj_Call);
	register_verify_node_func_proj(op_Cond,   verify_node_Proj_Cond);
	register_verify_node_func_proj(op_Div,    verify_node_Proj_Div);
	register_verify_node_func_proj(op_Load,   verify_node_Proj_Load);
	register_verify_node_func_proj(op_Mod,    verify_node_Proj_Mod);
	register_verify_node_func_proj(op_Proj,   verify_node_Proj_Proj);
	register_verify_node_func_proj(op_Raise,  verify_node_Proj_Raise);
	register_verify_node_func_proj(op_Start,  verify_node_Proj_Start);
	register_verify_node_func_proj(op_Store,  verify_node_Proj_Store);
	register_verify_node_func_proj(op_Switch, verify_node_Proj_Switch);
	register_verify_node_func_proj(op_Tuple,  verify_node_Proj_Tuple);
}
