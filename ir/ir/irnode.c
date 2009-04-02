/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Representation of an intermediate operation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <string.h>

#include "pset_new.h"
#include "ident.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irbackedge_t.h"
#include "irdump.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "iredgekinds.h"
#include "iredges_t.h"
#include "ircons.h"

#include "irhooks.h"
#include "irtools.h"

#include "beinfo.h"

/* some constants fixing the positions of nodes predecessors
   in the in array */
#define CALL_PARAM_OFFSET     2
#define BUILDIN_PARAM_OFFSET  1
#define SEL_INDEX_OFFSET      2
#define RETURN_RESULT_OFFSET  1  /* mem is not a result */
#define END_KEEPALIVE_OFFSET  0

static const char *pnc_name_arr [] = {
	"pn_Cmp_False", "pn_Cmp_Eq", "pn_Cmp_Lt", "pn_Cmp_Le",
	"pn_Cmp_Gt", "pn_Cmp_Ge", "pn_Cmp_Lg", "pn_Cmp_Leg",
	"pn_Cmp_Uo", "pn_Cmp_Ue", "pn_Cmp_Ul", "pn_Cmp_Ule",
	"pn_Cmp_Ug", "pn_Cmp_Uge", "pn_Cmp_Ne", "pn_Cmp_True"
};

/**
 * returns the pnc name from an pnc constant
 */
const char *get_pnc_string(int pnc) {
	assert(pnc >= 0 && pnc <
			(int) (sizeof(pnc_name_arr)/sizeof(pnc_name_arr[0])));
	return pnc_name_arr[pnc];
}

/*
 * Calculates the negated (Complement(R)) pnc condition.
 */
pn_Cmp get_negated_pnc(long pnc, ir_mode *mode) {
	pnc ^= pn_Cmp_True;

	/* do NOT add the Uo bit for non-floating point values */
	if (! mode_is_float(mode))
		pnc &= ~pn_Cmp_Uo;

	return (pn_Cmp) pnc;
}

/* Calculates the inversed (R^-1) pnc condition, i.e., "<" --> ">" */
pn_Cmp get_inversed_pnc(long pnc) {
	long code    = pnc & ~(pn_Cmp_Lt|pn_Cmp_Gt);
	long lesser  = pnc & pn_Cmp_Lt;
	long greater = pnc & pn_Cmp_Gt;

	code |= (lesser ? pn_Cmp_Gt : 0) | (greater ? pn_Cmp_Lt : 0);

	return (pn_Cmp) code;
}

/**
 * Indicates, whether additional data can be registered to ir nodes.
 * If set to 1, this is not possible anymore.
 */
static int forbid_new_data = 0;

/**
 * The amount of additional space for custom data to be allocated upon
 * creating a new node.
 */
unsigned firm_add_node_size = 0;


/* register new space for every node */
unsigned firm_register_additional_node_data(unsigned size) {
	assert(!forbid_new_data && "Too late to register additional node data");

	if (forbid_new_data)
		return 0;

	return firm_add_node_size += size;
}


void init_irnode(void) {
	/* Forbid the addition of new data to an ir node. */
	forbid_new_data = 1;
}

/*
 * irnode constructor.
 * Create a new irnode in irg, with an op, mode, arity and
 * some incoming irnodes.
 * If arity is negative, a node with a dynamic array is created.
 */
ir_node *
new_ir_node(dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op, ir_mode *mode,
            int arity, ir_node **in)
{
	ir_node *res;
	size_t node_size = offsetof(ir_node, attr) + op->attr_size + firm_add_node_size;
	char *p;
	int i;

	assert(irg);
	assert(op);
	assert(mode);
	p = obstack_alloc(irg->obst, node_size);
	memset(p, 0, node_size);
	res = (ir_node *)(p + firm_add_node_size);

	res->kind     = k_ir_node;
	res->op       = op;
	res->mode     = mode;
	res->visited  = 0;
	res->node_idx = irg_register_node_idx(irg, res);
	res->link     = NULL;
	res->deps     = NULL;

	if (arity < 0) {
		res->in = NEW_ARR_F(ir_node *, 1);  /* 1: space for block */
	} else {
		/* not nice but necessary: End and Sync must always have a flexible array */
		if (op == op_End || op == op_Sync)
			res->in = NEW_ARR_F(ir_node *, (arity+1));
		else
			res->in = NEW_ARR_D(ir_node *, irg->obst, (arity+1));
		memcpy(&res->in[1], in, sizeof(ir_node *) * arity);
	}

	res->in[0]   = block;
	set_irn_dbg_info(res, db);
	res->out     = NULL;
	res->node_nr = get_irp_new_node_nr();

	for (i = 0; i < EDGE_KIND_LAST; ++i)
		INIT_LIST_HEAD(&res->edge_info[i].outs_head);

	/* don't put this into the for loop, arity is -1 for some nodes! */
	edges_notify_edge(res, -1, res->in[0], NULL, irg);
	for (i = 1; i <= arity; ++i)
		edges_notify_edge(res, i - 1, res->in[i], NULL, irg);

	hook_new_node(irg, res);
	if (get_irg_phase_state(irg) == phase_backend) {
		be_info_new_node(res);
	}

	return res;
}

/*-- getting some parameters from ir_nodes --*/

int (is_ir_node)(const void *thing) {
	return _is_ir_node(thing);
}

int (get_irn_intra_arity)(const ir_node *node) {
	return _get_irn_intra_arity(node);
}

int (get_irn_inter_arity)(const ir_node *node) {
	return _get_irn_inter_arity(node);
}

int (*_get_irn_arity)(const ir_node *node) = _get_irn_intra_arity;

int (get_irn_arity)(const ir_node *node) {
	return _get_irn_arity(node);
}

/* Returns the array with ins. This array is shifted with respect to the
   array accessed by get_irn_n: The block operand is at position 0 not -1.
   (@@@ This should be changed.)
   The order of the predecessors in this array is not guaranteed, except that
   lists of operands as predecessors of Block or arguments of a Call are
   consecutive. */
ir_node **get_irn_in(const ir_node *node) {
	assert(node);
#ifdef INTERPROCEDURAL_VIEW
	if (get_interprocedural_view()) { /* handle Filter and Block specially */
		if (get_irn_opcode(node) == iro_Filter) {
			assert(node->attr.filter.in_cg);
			return node->attr.filter.in_cg;
		} else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
			return node->attr.block.in_cg;
		}
		/* else fall through */
	}
#endif /* INTERPROCEDURAL_VIEW */
	return node->in;
}

void set_irn_in(ir_node *node, int arity, ir_node **in) {
	int i;
	ir_node *** pOld_in;
	ir_graph *irg = current_ir_graph;

	assert(node);
#ifdef INTERPROCEDURAL_VIEW
	if (get_interprocedural_view()) { /* handle Filter and Block specially */
		ir_opcode code = get_irn_opcode(node);
		if (code  == iro_Filter) {
			assert(node->attr.filter.in_cg);
			pOld_in = &node->attr.filter.in_cg;
		} else if (code == iro_Block && node->attr.block.in_cg) {
			pOld_in = &node->attr.block.in_cg;
		} else {
			pOld_in = &node->in;
		}
	} else
#endif /* INTERPROCEDURAL_VIEW */
		pOld_in = &node->in;


	for (i = 0; i < arity; i++) {
		if (i < ARR_LEN(*pOld_in)-1)
			edges_notify_edge(node, i, in[i], (*pOld_in)[i+1], irg);
		else
			edges_notify_edge(node, i, in[i], NULL,            irg);
	}
	for (;i < ARR_LEN(*pOld_in)-1; i++) {
		edges_notify_edge(node, i, NULL, (*pOld_in)[i+1], irg);
	}

	if (arity != ARR_LEN(*pOld_in) - 1) {
		ir_node * block = (*pOld_in)[0];
		*pOld_in = NEW_ARR_D(ir_node *, irg->obst, arity + 1);
		(*pOld_in)[0] = block;
	}
	fix_backedges(irg->obst, node);

	memcpy((*pOld_in) + 1, in, sizeof(ir_node *) * arity);
}

ir_node *(get_irn_intra_n)(const ir_node *node, int n) {
	return _get_irn_intra_n (node, n);
}

ir_node *(get_irn_inter_n)(const ir_node *node, int n) {
	return _get_irn_inter_n (node, n);
}

ir_node *(*_get_irn_n)(const ir_node *node, int n) = _get_irn_intra_n;

ir_node *(get_irn_n)(const ir_node *node, int n) {
	return _get_irn_n(node, n);
}

void set_irn_n(ir_node *node, int n, ir_node *in) {
	assert(node && node->kind == k_ir_node);
	assert(-1 <= n);
	assert(n < get_irn_arity(node));
	assert(in && in->kind == k_ir_node);

	if ((n == -1) && (get_irn_opcode(node) == iro_Filter)) {
		/* Change block pred in both views! */
		node->in[n + 1] = in;
		assert(node->attr.filter.in_cg);
		node->attr.filter.in_cg[n + 1] = in;
		return;
	}
#ifdef INTERPROCEDURAL_VIEW
	if (get_interprocedural_view()) { /* handle Filter and Block specially */
		if (get_irn_opcode(node) == iro_Filter) {
			assert(node->attr.filter.in_cg);
			node->attr.filter.in_cg[n + 1] = in;
			return;
		} else if (get_irn_opcode(node) == iro_Block && node->attr.block.in_cg) {
			node->attr.block.in_cg[n + 1] = in;
			return;
		}
		/* else fall through */
	}
#endif /* INTERPROCEDURAL_VIEW */

	/* Call the hook */
	hook_set_irn_n(node, n, in, node->in[n + 1]);

	/* Here, we rely on src and tgt being in the current ir graph */
	edges_notify_edge(node, n, in, node->in[n + 1], current_ir_graph);

	node->in[n + 1] = in;
}

int add_irn_n(ir_node *node, ir_node *in) {
	int pos;
	ir_graph *irg = get_irn_irg(node);

	assert(node->op->opar == oparity_dynamic);
	pos = ARR_LEN(node->in) - 1;
	ARR_APP1(ir_node *, node->in, in);
	edges_notify_edge(node, pos, node->in[pos + 1], NULL, irg);

	/* Call the hook */
	hook_set_irn_n(node, pos, node->in[pos + 1], NULL);

	return pos;
}

void del_Sync_n(ir_node *n, int i)
{
	int      arity     = get_Sync_n_preds(n);
	ir_node *last_pred = get_Sync_pred(n, arity - 1);
	set_Sync_pred(n, i, last_pred);
	edges_notify_edge(n, arity - 1, NULL, last_pred, get_irn_irg(n));
	ARR_SHRINKLEN(get_irn_in(n), arity);
}

int (get_irn_deps)(const ir_node *node) {
	return _get_irn_deps(node);
}

ir_node *(get_irn_dep)(const ir_node *node, int pos) {
	return _get_irn_dep(node, pos);
}

void (set_irn_dep)(ir_node *node, int pos, ir_node *dep) {
	_set_irn_dep(node, pos, dep);
}

int add_irn_dep(ir_node *node, ir_node *dep) {
	int res = 0;

	/* DEP edges are only allowed in backend phase */
	assert(get_irg_phase_state(get_irn_irg(node)) == phase_backend);
	if (node->deps == NULL) {
		node->deps = NEW_ARR_F(ir_node *, 1);
		node->deps[0] = dep;
	} else {
		int i, n;
		int first_zero = -1;

		for(i = 0, n = ARR_LEN(node->deps); i < n; ++i) {
			if(node->deps[i] == NULL)
				first_zero = i;

			if(node->deps[i] == dep)
				return i;
		}

		if (first_zero >= 0) {
			node->deps[first_zero] = dep;
			res = first_zero;
		} else {
			ARR_APP1(ir_node *, node->deps, dep);
			res = n;
		}
	}

	edges_notify_edge_kind(node, res, dep, NULL, EDGE_KIND_DEP, get_irn_irg(node));

	return res;
}

void add_irn_deps(ir_node *tgt, ir_node *src) {
	int i, n;

	for (i = 0, n = get_irn_deps(src); i < n; ++i)
		add_irn_dep(tgt, get_irn_dep(src, i));
}


ir_mode *(get_irn_mode)(const ir_node *node) {
	return _get_irn_mode(node);
}

void (set_irn_mode)(ir_node *node, ir_mode *mode) {
	_set_irn_mode(node, mode);
}

ir_modecode get_irn_modecode(const ir_node *node) {
	assert(node);
	return node->mode->code;
}

/** Gets the string representation of the mode .*/
const char *get_irn_modename(const ir_node *node) {
	assert(node);
	return get_mode_name(node->mode);
}

ident *get_irn_modeident(const ir_node *node) {
	assert(node);
	return get_mode_ident(node->mode);
}

ir_op *(get_irn_op)(const ir_node *node) {
	return _get_irn_op(node);
}

/* should be private to the library: */
void (set_irn_op)(ir_node *node, ir_op *op) {
	_set_irn_op(node, op);
}

unsigned (get_irn_opcode)(const ir_node *node) {
	return _get_irn_opcode(node);
}

const char *get_irn_opname(const ir_node *node) {
	assert(node);
	if (is_Phi0(node)) return "Phi0";
	return get_id_str(node->op->name);
}

ident *get_irn_opident(const ir_node *node) {
	assert(node);
	return node->op->name;
}

ir_visited_t (get_irn_visited)(const ir_node *node) {
	return _get_irn_visited(node);
}

void (set_irn_visited)(ir_node *node, ir_visited_t visited) {
	_set_irn_visited(node, visited);
}

void (mark_irn_visited)(ir_node *node) {
	_mark_irn_visited(node);
}

int (irn_visited)(const ir_node *node) {
	return _irn_visited(node);
}

int (irn_visited_else_mark)(ir_node *node) {
	return _irn_visited_else_mark(node);
}

void (set_irn_link)(ir_node *node, void *link) {
	_set_irn_link(node, link);
}

void *(get_irn_link)(const ir_node *node) {
	return _get_irn_link(node);
}

op_pin_state (get_irn_pinned)(const ir_node *node) {
	return _get_irn_pinned(node);
}

op_pin_state (is_irn_pinned_in_irg) (const ir_node *node) {
	return _is_irn_pinned_in_irg(node);
}

void set_irn_pinned(ir_node *node, op_pin_state state) {
	/* due to optimization an opt may be turned into a Tuple */
	if (is_Tuple(node))
		return;

	assert(node && get_op_pinned(get_irn_op(node)) >= op_pin_state_exc_pinned);
	assert(state == op_pin_state_pinned || state == op_pin_state_floats);

	node->attr.except.pin_state = state;
}

/* Outputs a unique number for this node */
long get_irn_node_nr(const ir_node *node) {
	assert(node);
	return node->node_nr;
}

const_attr *get_irn_const_attr(ir_node *node) {
	assert(is_Const(node));
	return &node->attr.con;
}

long get_irn_proj_attr(ir_node *node) {
	/* BEWARE: check for true Proj node here, no Filter */
	assert(node->op == op_Proj);
	return node->attr.proj;
}

alloc_attr *get_irn_alloc_attr(ir_node *node) {
	assert(is_Alloc(node));
	return &node->attr.alloc;
}

free_attr *get_irn_free_attr(ir_node *node) {
	assert(is_Free(node));
	return &node->attr.free;
}

symconst_attr *get_irn_symconst_attr(ir_node *node) {
	assert(is_SymConst(node));
	return &node->attr.symc;
}

ir_type *get_irn_call_attr(ir_node *node) {
	assert(is_Call(node));
	return node->attr.call.cld_tp = skip_tid(node->attr.call.cld_tp);
}

sel_attr *get_irn_sel_attr(ir_node *node) {
	assert(is_Sel(node));
	return &node->attr.sel;
}

phi_attr *get_irn_phi_attr(ir_node *node) {
	return &node->attr.phi;
}

block_attr *get_irn_block_attr(ir_node *node) {
	assert(is_Block(node));
	return &node->attr.block;
}

load_attr *get_irn_load_attr(ir_node *node) {
	assert(is_Load(node));
	return &node->attr.load;
}

store_attr *get_irn_store_attr(ir_node *node) {
	assert(is_Store(node));
	return &node->attr.store;
}

except_attr *get_irn_except_attr(ir_node *node) {
	assert(node->op == op_Div || node->op == op_Quot ||
	       node->op == op_DivMod || node->op == op_Mod || node->op == op_Call || node->op == op_Alloc || node->op == op_Bound);
	return &node->attr.except;
}

divmod_attr *get_irn_divmod_attr(ir_node *node) {
	assert(node->op == op_Div || node->op == op_Quot ||
	       node->op == op_DivMod || node->op == op_Mod);
	return &node->attr.divmod;
}

builtin_attr *get_irn_builtin_attr(ir_node *node) {
	assert(is_Builtin(node));
	return &node->attr.builtin;
}

void *(get_irn_generic_attr)(ir_node *node) {
	assert(is_ir_node(node));
	return _get_irn_generic_attr(node);
}

const void *(get_irn_generic_attr_const)(const ir_node *node) {
	assert(is_ir_node(node));
	return _get_irn_generic_attr_const(node);
}

unsigned (get_irn_idx)(const ir_node *node) {
	assert(is_ir_node(node));
	return _get_irn_idx(node);
}

int get_irn_pred_pos(ir_node *node, ir_node *arg) {
	int i;
	for (i = get_irn_arity(node) - 1; i >= 0; i--) {
		if (get_irn_n(node, i) == arg)
			return i;
	}
	return -1;
}

/** manipulate fields of individual nodes **/

/* this works for all except Block */
ir_node *get_nodes_block(const ir_node *node) {
	assert(node->op != op_Block);
	return get_irn_n(node, -1);
}

void set_nodes_block(ir_node *node, ir_node *block) {
	assert(node->op != op_Block);
	set_irn_n(node, -1, block);
}

/* this works for all except Block */
ir_node *get_nodes_MacroBlock(const ir_node *node) {
	assert(node->op != op_Block);
	return get_Block_MacroBlock(get_irn_n(node, -1));
}

/* Test whether arbitrary node is frame pointer, i.e. Proj(pn_Start_P_frame_base)
 * from Start.  If so returns frame type, else Null. */
ir_type *is_frame_pointer(const ir_node *n) {
	if (is_Proj(n) && (get_Proj_proj(n) == pn_Start_P_frame_base)) {
		ir_node *start = get_Proj_pred(n);
		if (is_Start(start)) {
			return get_irg_frame_type(get_irn_irg(start));
		}
	}
	return NULL;
}

/* Test whether arbitrary node is tls pointer, i.e. Proj(pn_Start_P_tls)
 * from Start.  If so returns tls type, else Null. */
ir_type *is_tls_pointer(const ir_node *n) {
	if (is_Proj(n) && (get_Proj_proj(n) == pn_Start_P_tls)) {
		ir_node *start = get_Proj_pred(n);
		if (is_Start(start)) {
			return get_tls_type();
		}
	}
	return NULL;
}

ir_node **get_Block_cfgpred_arr(ir_node *node) {
	assert(is_Block(node));
	return (ir_node **)&(get_irn_in(node)[1]);
}

int (get_Block_n_cfgpreds)(const ir_node *node) {
	return _get_Block_n_cfgpreds(node);
}

ir_node *(get_Block_cfgpred)(const ir_node *node, int pos) {
	return _get_Block_cfgpred(node, pos);
}

void set_Block_cfgpred(ir_node *node, int pos, ir_node *pred) {
	assert(is_Block(node));
	set_irn_n(node, pos, pred);
}

int get_Block_cfgpred_pos(const ir_node *block, const ir_node *pred) {
	int i;

	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		if (get_Block_cfgpred_block(block, i) == pred)
			return i;
	}
	return -1;
}

ir_node *(get_Block_cfgpred_block)(const ir_node *node, int pos) {
	return _get_Block_cfgpred_block(node, pos);
}

int get_Block_matured(const ir_node *node) {
	assert(is_Block(node));
	return (int)node->attr.block.is_matured;
}

void set_Block_matured(ir_node *node, int matured) {
	assert(is_Block(node));
	node->attr.block.is_matured = matured;
}

ir_visited_t (get_Block_block_visited)(const ir_node *node) {
	return _get_Block_block_visited(node);
}

void (set_Block_block_visited)(ir_node *node, ir_visited_t visit) {
	_set_Block_block_visited(node, visit);
}

/* For this current_ir_graph must be set. */
void (mark_Block_block_visited)(ir_node *node) {
	_mark_Block_block_visited(node);
}

int (Block_block_visited)(const ir_node *node) {
	return _Block_block_visited(node);
}

ir_node *get_Block_graph_arr(ir_node *node, int pos) {
	assert(is_Block(node));
	return node->attr.block.graph_arr[pos+1];
}

void set_Block_graph_arr(ir_node *node, int pos, ir_node *value) {
	assert(is_Block(node));
	node->attr.block.graph_arr[pos+1] = value;
}

#ifdef INTERPROCEDURAL_VIEW
void set_Block_cg_cfgpred_arr(ir_node *node, int arity, ir_node *in[]) {
	assert(is_Block(node));
	if (node->attr.block.in_cg == NULL || arity != ARR_LEN(node->attr.block.in_cg) - 1) {
		node->attr.block.in_cg = NEW_ARR_D(ir_node *, current_ir_graph->obst, arity + 1);
		node->attr.block.in_cg[0] = NULL;
		node->attr.block.cg_backedge = new_backedge_arr(current_ir_graph->obst, arity);
		{
			/* Fix backedge array.  fix_backedges() operates depending on
			   interprocedural_view. */
			int ipv = get_interprocedural_view();
			set_interprocedural_view(1);
			fix_backedges(current_ir_graph->obst, node);
			set_interprocedural_view(ipv);
		}
	}
	memcpy(node->attr.block.in_cg + 1, in, sizeof(ir_node *) * arity);
}

void set_Block_cg_cfgpred(ir_node *node, int pos, ir_node *pred) {
	assert(is_Block(node) && node->attr.block.in_cg &&
	       0 <= pos && pos < ARR_LEN(node->attr.block.in_cg) - 1);
	node->attr.block.in_cg[pos + 1] = pred;
}

ir_node **get_Block_cg_cfgpred_arr(ir_node *node) {
	assert(is_Block(node));
	return node->attr.block.in_cg == NULL ? NULL : node->attr.block.in_cg  + 1;
}

int get_Block_cg_n_cfgpreds(const ir_node *node) {
	assert(is_Block(node));
	return node->attr.block.in_cg == NULL ? 0 : ARR_LEN(node->attr.block.in_cg) - 1;
}

ir_node *get_Block_cg_cfgpred(const ir_node *node, int pos) {
	assert(is_Block(node) && node->attr.block.in_cg);
	return node->attr.block.in_cg[pos + 1];
}

void remove_Block_cg_cfgpred_arr(ir_node *node) {
	assert(is_Block(node));
	node->attr.block.in_cg = NULL;
}
#endif /* INTERPROCEDURAL_VIEW */

ir_node *(set_Block_dead)(ir_node *block) {
	return _set_Block_dead(block);
}

int (is_Block_dead)(const ir_node *block) {
	return _is_Block_dead(block);
}

ir_extblk *get_Block_extbb(const ir_node *block) {
	ir_extblk *res;
	assert(is_Block(block));
	res = block->attr.block.extblk;
	assert(res == NULL || is_ir_extbb(res));
	return res;
}

void set_Block_extbb(ir_node *block, ir_extblk *extblk) {
	assert(is_Block(block));
	assert(extblk == NULL || is_ir_extbb(extblk));
	block->attr.block.extblk = extblk;
}

/* Returns the macro block header of a block.*/
ir_node *get_Block_MacroBlock(const ir_node *block) {
	ir_node *mbh;
	assert(is_Block(block));
	mbh = get_irn_n(block, -1);
	/* once macro block header is respected by all optimizations,
	   this assert can be removed */
	assert(mbh != NULL);
	return mbh;
}

/* Sets the macro block header of a block. */
void set_Block_MacroBlock(ir_node *block, ir_node *mbh) {
	assert(is_Block(block));
	assert(is_Block(mbh));
	set_irn_n(block, -1, mbh);
}

/* returns the macro block header of a node. */
ir_node *get_irn_MacroBlock(const ir_node *n) {
	if (! is_Block(n)) {
		n = get_nodes_block(n);
		/* if the Block is Bad, do NOT try to get it's MB, it will fail. */
		if (is_Bad(n))
			return (ir_node *)n;
	}
	return get_Block_MacroBlock(n);
}

/* returns the graph of a Block. */
ir_graph *get_Block_irg(const ir_node *block) {
	assert(is_Block(block));
	return block->attr.block.irg;
}

int has_Block_label(const ir_node *block) {
	assert(is_Block(block));
	return block->attr.block.has_label;
}

ir_label_t get_Block_label(const ir_node *block) {
	assert(is_Block(block));
	return block->attr.block.label;
}

void set_Block_label(ir_node *block, ir_label_t label) {
	assert(is_Block(block));
	block->attr.block.has_label = 1;
	block->attr.block.label = label;
}

ir_node *(get_Block_phis)(const ir_node *block) {
	return _get_Block_phis(block);
}

void (set_Block_phis)(ir_node *block, ir_node *phi) {
	_set_Block_phis(block, phi);
}

void (add_Block_phi)(ir_node *block, ir_node *phi) {
	_add_Block_phi(block, phi);
}

/* Get the Block mark (single bit). */
unsigned (get_Block_mark)(const ir_node *block) {
	return _get_Block_mark(block);
}

/* Set the Block mark (single bit). */
void (set_Block_mark)(ir_node *block, unsigned mark) {
	_set_Block_mark(block, mark);
}

int get_End_n_keepalives(const ir_node *end) {
	assert(is_End(end));
	return (get_irn_arity(end) - END_KEEPALIVE_OFFSET);
}

ir_node *get_End_keepalive(const ir_node *end, int pos) {
	assert(is_End(end));
	return get_irn_n(end, pos + END_KEEPALIVE_OFFSET);
}

void add_End_keepalive(ir_node *end, ir_node *ka) {
	assert(is_End(end));
	add_irn_n(end, ka);
}

void set_End_keepalive(ir_node *end, int pos, ir_node *ka) {
	assert(is_End(end));
	set_irn_n(end, pos + END_KEEPALIVE_OFFSET, ka);
}

/* Set new keep-alives */
void set_End_keepalives(ir_node *end, int n, ir_node *in[]) {
	int i;
	ir_graph *irg = get_irn_irg(end);

	/* notify that edges are deleted */
	for (i = END_KEEPALIVE_OFFSET; i < ARR_LEN(end->in) - 1; ++i) {
		edges_notify_edge(end, i, NULL, end->in[i + 1], irg);
	}
	ARR_RESIZE(ir_node *, end->in, n + 1 + END_KEEPALIVE_OFFSET);

	for (i = 0; i < n; ++i) {
		end->in[1 + END_KEEPALIVE_OFFSET + i] = in[i];
		edges_notify_edge(end, END_KEEPALIVE_OFFSET + i, end->in[1 + END_KEEPALIVE_OFFSET + i], NULL, irg);
	}
}

/* Set new keep-alives from old keep-alives, skipping irn */
void remove_End_keepalive(ir_node *end, ir_node *irn) {
	int      n = get_End_n_keepalives(end);
	int      i, idx;
	ir_graph *irg;

	idx = -1;
	for (i = n -1; i >= 0; --i) {
		ir_node *old_ka = end->in[1 + END_KEEPALIVE_OFFSET + i];

		/* find irn */
		if (old_ka == irn) {
			idx = i;
			goto found;
		}
	}
	return;
found:
	irg = get_irn_irg(end);

	/* remove the edge */
	edges_notify_edge(end, idx, NULL, irn, irg);

	if (idx != n - 1) {
		/* exchange with the last one */
		ir_node *old = end->in[1 + END_KEEPALIVE_OFFSET + n - 1];
		edges_notify_edge(end, n - 1, NULL, old, irg);
		end->in[1 + END_KEEPALIVE_OFFSET + idx] = old;
		edges_notify_edge(end, idx, old, NULL, irg);
	}
	/* now n - 1 keeps, 1 block input */
	ARR_RESIZE(ir_node *, end->in, (n - 1) + 1 + END_KEEPALIVE_OFFSET);
}

/* remove Bads, NoMems and doublets from the keep-alive set */
void remove_End_Bads_and_doublets(ir_node *end) {
	pset_new_t keeps;
	int        idx, n = get_End_n_keepalives(end);
	ir_graph   *irg;

	if (n <= 0)
		return;

	irg = get_irn_irg(end);
	pset_new_init(&keeps);

	for (idx = n - 1; idx >= 0; --idx) {
		ir_node *ka = get_End_keepalive(end, idx);

		if (is_Bad(ka) || is_NoMem(ka) || pset_new_contains(&keeps, ka)) {
			/* remove the edge */
			edges_notify_edge(end, idx, NULL, ka, irg);

			if (idx != n - 1) {
				/* exchange with the last one */
				ir_node *old = end->in[1 + END_KEEPALIVE_OFFSET + n - 1];
				edges_notify_edge(end, n - 1, NULL, old, irg);
				end->in[1 + END_KEEPALIVE_OFFSET + idx] = old;
				edges_notify_edge(end, idx, old, NULL, irg);
			}
			--n;
		} else {
			pset_new_insert(&keeps, ka);
		}
	}
	/* n keeps, 1 block input */
	ARR_RESIZE(ir_node *, end->in, n + 1 + END_KEEPALIVE_OFFSET);

	pset_new_destroy(&keeps);
}

void free_End(ir_node *end) {
	assert(is_End(end));
	end->kind = k_BAD;
	DEL_ARR_F(end->in);
	end->in = NULL;   /* @@@ make sure we get an error if we use the
	                     in array afterwards ... */
}

/* Return the target address of an IJmp */
ir_node *get_IJmp_target(const ir_node *ijmp) {
	assert(is_IJmp(ijmp));
	return get_irn_n(ijmp, 0);
}

/** Sets the target address of an IJmp */
void set_IJmp_target(ir_node *ijmp, ir_node *tgt) {
	assert(is_IJmp(ijmp));
	set_irn_n(ijmp, 0, tgt);
}

/*
> Implementing the case construct (which is where the constant Proj node is
> important) involves far more than simply determining the constant values.
> We could argue that this is more properly a function of the translator from
> Firm to the target machine.  That could be done if there was some way of
> projecting "default" out of the Cond node.
I know it's complicated.
Basically there are two problems:
 - determining the gaps between the Projs
 - determining the biggest case constant to know the proj number for
   the default node.
I see several solutions:
1. Introduce a ProjDefault node.  Solves both problems.
   This means to extend all optimizations executed during construction.
2. Give the Cond node for switch two flavors:
   a) there are no gaps in the Projs  (existing flavor)
   b) gaps may exist, default proj is still the Proj with the largest
      projection number.  This covers also the gaps.
3. Fix the semantic of the Cond to that of 2b)

Solution 2 seems to be the best:
Computing the gaps in the Firm representation is not too hard, i.e.,
libFIRM can implement a routine that transforms between the two
flavours.  This is also possible for 1) but 2) does not require to
change any existing optimization.
Further it should be far simpler to determine the biggest constant than
to compute all gaps.
I don't want to choose 3) as 2a) seems to have advantages for
dataflow analysis and 3) does not allow to convert the representation to
2a).
*/

const char *get_cond_kind_name(cond_kind kind)
{
#define X(a)    case a: return #a;
	switch (kind) {
		X(dense);
		X(fragmentary);
	}
	return "<unknown>";
#undef X
}

ir_node *
get_Cond_selector(const ir_node *node) {
	assert(is_Cond(node));
	return get_irn_n(node, 0);
}

void
set_Cond_selector(ir_node *node, ir_node *selector) {
	assert(is_Cond(node));
	set_irn_n(node, 0, selector);
}

cond_kind
get_Cond_kind(const ir_node *node) {
	assert(is_Cond(node));
	return node->attr.cond.kind;
}

void
set_Cond_kind(ir_node *node, cond_kind kind) {
	assert(is_Cond(node));
	node->attr.cond.kind = kind;
}

long
get_Cond_default_proj(const ir_node *node) {
	assert(is_Cond(node));
	return node->attr.cond.default_proj;
}

void set_Cond_default_proj(ir_node *node, long defproj) {
	assert(is_Cond(node));
	node->attr.cond.default_proj = defproj;
}

ir_node *
get_Return_mem(const ir_node *node) {
	assert(is_Return(node));
	return get_irn_n(node, 0);
}

void
set_Return_mem(ir_node *node, ir_node *mem) {
	assert(is_Return(node));
	set_irn_n(node, 0, mem);
}

int
get_Return_n_ress(const ir_node *node) {
	assert(is_Return(node));
	return (get_irn_arity(node) - RETURN_RESULT_OFFSET);
}

ir_node **
get_Return_res_arr(ir_node *node) {
	assert(is_Return(node));
	if (get_Return_n_ress(node) > 0)
		return (ir_node **)&(get_irn_in(node)[1 + RETURN_RESULT_OFFSET]);
	else
		return NULL;
}

/*
void
set_Return_n_res(ir_node *node, int results) {
	assert(is_Return(node));
}
*/

ir_node *
get_Return_res(const ir_node *node, int pos) {
	assert(is_Return(node));
	assert(get_Return_n_ress(node) > pos);
	return get_irn_n(node, pos + RETURN_RESULT_OFFSET);
}

void
set_Return_res(ir_node *node, int pos, ir_node *res){
	assert(is_Return(node));
	set_irn_n(node, pos + RETURN_RESULT_OFFSET, res);
}

tarval *(get_Const_tarval)(const ir_node *node) {
	return _get_Const_tarval(node);
}

void
set_Const_tarval(ir_node *node, tarval *con) {
	assert(is_Const(node));
	node->attr.con.tv = con;
}

int (is_Const_null)(const ir_node *node) {
	return _is_Const_null(node);
}

int (is_Const_one)(const ir_node *node) {
	return _is_Const_one(node);
}

int (is_Const_all_one)(const ir_node *node) {
	return _is_Const_all_one(node);
}


/* The source language type.  Must be an atomic type.  Mode of type must
   be mode of node. For tarvals from entities type must be pointer to
   entity type. */
ir_type *
get_Const_type(ir_node *node) {
	assert(is_Const(node));
	node->attr.con.tp = skip_tid(node->attr.con.tp);
	return node->attr.con.tp;
}

void
set_Const_type(ir_node *node, ir_type *tp) {
	assert(is_Const(node));
	if (tp != firm_unknown_type) {
		assert(is_atomic_type(tp));
		assert(get_type_mode(tp) == get_irn_mode(node));
	}
	node->attr.con.tp = tp;
}


symconst_kind
get_SymConst_kind(const ir_node *node) {
	assert(is_SymConst(node));
	return node->attr.symc.kind;
}

void
set_SymConst_kind(ir_node *node, symconst_kind kind) {
	assert(is_SymConst(node));
	node->attr.symc.kind = kind;
}

ir_type *
get_SymConst_type(const ir_node *node) {
	/* the cast here is annoying, but we have to compensate for
	   the skip_tip() */
	ir_node *irn = (ir_node *)node;
	assert(is_SymConst(node) &&
	       (SYMCONST_HAS_TYPE(get_SymConst_kind(node))));
	return irn->attr.symc.sym.type_p = skip_tid(irn->attr.symc.sym.type_p);
}

void
set_SymConst_type(ir_node *node, ir_type *tp) {
	assert(is_SymConst(node) &&
	       (SYMCONST_HAS_TYPE(get_SymConst_kind(node))));
	node->attr.symc.sym.type_p = tp;
}

ident *
get_SymConst_name(const ir_node *node) {
	assert(is_SymConst(node) && SYMCONST_HAS_ID(get_SymConst_kind(node)));
	return node->attr.symc.sym.ident_p;
}

void
set_SymConst_name(ir_node *node, ident *name) {
	assert(is_SymConst(node) && SYMCONST_HAS_ID(get_SymConst_kind(node)));
	node->attr.symc.sym.ident_p = name;
}


/* Only to access SymConst of kind symconst_addr_ent.  Else assertion: */
ir_entity *get_SymConst_entity(const ir_node *node) {
	assert(is_SymConst(node) && SYMCONST_HAS_ENT(get_SymConst_kind(node)));
	return node->attr.symc.sym.entity_p;
}

void set_SymConst_entity(ir_node *node, ir_entity *ent) {
	assert(is_SymConst(node) && SYMCONST_HAS_ENT(get_SymConst_kind(node)));
	node->attr.symc.sym.entity_p  = ent;
}

ir_enum_const *get_SymConst_enum(const ir_node *node) {
	assert(is_SymConst(node) && SYMCONST_HAS_ENUM(get_SymConst_kind(node)));
	return node->attr.symc.sym.enum_p;
}

void set_SymConst_enum(ir_node *node, ir_enum_const *ec) {
	assert(is_SymConst(node) && SYMCONST_HAS_ENUM(get_SymConst_kind(node)));
	node->attr.symc.sym.enum_p  = ec;
}

union symconst_symbol
get_SymConst_symbol(const ir_node *node) {
	assert(is_SymConst(node));
	return node->attr.symc.sym;
}

void
set_SymConst_symbol(ir_node *node, union symconst_symbol sym) {
	assert(is_SymConst(node));
	node->attr.symc.sym = sym;
}

ir_label_t get_SymConst_label(const ir_node *node) {
	assert(is_SymConst(node) && SYMCONST_HAS_LABEL(get_SymConst_kind(node)));
	return node->attr.symc.sym.label;
}

void set_SymConst_label(ir_node *node, ir_label_t label) {
	assert(is_SymConst(node) && SYMCONST_HAS_LABEL(get_SymConst_kind(node)));
	node->attr.symc.sym.label = label;
}

ir_type *
get_SymConst_value_type(ir_node *node) {
	assert(is_SymConst(node));
	if (node->attr.symc.tp) node->attr.symc.tp = skip_tid(node->attr.symc.tp);
	return node->attr.symc.tp;
}

void
set_SymConst_value_type(ir_node *node, ir_type *tp) {
	assert(is_SymConst(node));
	node->attr.symc.tp = tp;
}

ir_node *
get_Sel_mem(const ir_node *node) {
	assert(is_Sel(node));
	return get_irn_n(node, 0);
}

void
set_Sel_mem(ir_node *node, ir_node *mem) {
	assert(is_Sel(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Sel_ptr(const ir_node *node) {
	assert(is_Sel(node));
	return get_irn_n(node, 1);
}

void
set_Sel_ptr(ir_node *node, ir_node *ptr) {
	assert(is_Sel(node));
	set_irn_n(node, 1, ptr);
}

int
get_Sel_n_indexs(const ir_node *node) {
	assert(is_Sel(node));
	return (get_irn_arity(node) - SEL_INDEX_OFFSET);
}

ir_node **
get_Sel_index_arr(ir_node *node) {
	assert(is_Sel(node));
	if (get_Sel_n_indexs(node) > 0)
		return (ir_node **)& get_irn_in(node)[SEL_INDEX_OFFSET + 1];
	else
		return NULL;
}

ir_node *
get_Sel_index(const ir_node *node, int pos) {
	assert(is_Sel(node));
	return get_irn_n(node, pos + SEL_INDEX_OFFSET);
}

void
set_Sel_index(ir_node *node, int pos, ir_node *index) {
	assert(is_Sel(node));
	set_irn_n(node, pos + SEL_INDEX_OFFSET, index);
}

ir_entity *
get_Sel_entity(const ir_node *node) {
	assert(is_Sel(node));
	return node->attr.sel.entity;
}

/* need a version without const to prevent warning */
static ir_entity *_get_Sel_entity(ir_node *node) {
	return get_Sel_entity(node);
}

void
set_Sel_entity(ir_node *node, ir_entity *ent) {
	assert(is_Sel(node));
	node->attr.sel.entity = ent;
}


/* For unary and binary arithmetic operations the access to the
   operands can be factored out.  Left is the first, right the
   second arithmetic value  as listed in tech report 0999-33.
   unops are: Minus, Abs, Not, Conv, Cast
   binops are: Add, Sub, Mul, Quot, DivMod, Div, Mod, And, Or, Eor, Shl,
   Shr, Shrs, Rotate, Cmp */


ir_node *
get_Call_mem(const ir_node *node) {
	assert(is_Call(node));
	return get_irn_n(node, 0);
}

void
set_Call_mem(ir_node *node, ir_node *mem) {
	assert(is_Call(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Call_ptr(const ir_node *node) {
	assert(is_Call(node));
	return get_irn_n(node, 1);
}

void
set_Call_ptr(ir_node *node, ir_node *ptr) {
	assert(is_Call(node));
	set_irn_n(node, 1, ptr);
}

ir_node **
get_Call_param_arr(ir_node *node) {
	assert(is_Call(node));
	return &get_irn_in(node)[CALL_PARAM_OFFSET + 1];
}

int
get_Call_n_params(const ir_node *node)  {
	assert(is_Call(node));
	return (get_irn_arity(node) - CALL_PARAM_OFFSET);
}

ir_node *
get_Call_param(const ir_node *node, int pos) {
	assert(is_Call(node));
	return get_irn_n(node, pos + CALL_PARAM_OFFSET);
}

void
set_Call_param(ir_node *node, int pos, ir_node *param) {
	assert(is_Call(node));
	set_irn_n(node, pos + CALL_PARAM_OFFSET, param);
}

ir_type *
get_Call_type(ir_node *node) {
	assert(is_Call(node));
	return node->attr.call.cld_tp = skip_tid(node->attr.call.cld_tp);
}

void
set_Call_type(ir_node *node, ir_type *tp) {
	assert(is_Call(node));
	assert((get_unknown_type() == tp) || is_Method_type(tp));
	node->attr.call.cld_tp = tp;
}

ir_node *
get_Builtin_mem(const ir_node *node) {
	assert(is_Builtin(node));
	return get_irn_n(node, 0);
}

void
set_Builin_mem(ir_node *node, ir_node *mem) {
	assert(is_Builtin(node));
	set_irn_n(node, 0, mem);
}

ir_builtin_kind
get_Builtin_kind(const ir_node *node) {
	assert(is_Builtin(node));
	return node->attr.builtin.kind;
}

void
set_Builtin_kind(ir_node *node, ir_builtin_kind kind) {
	assert(is_Builtin(node));
	node->attr.builtin.kind = kind;
}

ir_node **
get_Builtin_param_arr(ir_node *node) {
	assert(is_Builtin(node));
	return &get_irn_in(node)[BUILDIN_PARAM_OFFSET + 1];
}

int
get_Builtin_n_params(const ir_node *node)  {
	assert(is_Builtin(node));
	return (get_irn_arity(node) - BUILDIN_PARAM_OFFSET);
}

ir_node *
get_Builtin_param(const ir_node *node, int pos) {
	assert(is_Builtin(node));
	return get_irn_n(node, pos + BUILDIN_PARAM_OFFSET);
}

void
set_Builtin_param(ir_node *node, int pos, ir_node *param) {
	assert(is_Builtin(node));
	set_irn_n(node, pos + BUILDIN_PARAM_OFFSET, param);
}

ir_type *
get_Builtin_type(ir_node *node) {
	assert(is_Builtin(node));
	return node->attr.builtin.builtin_tp = skip_tid(node->attr.builtin.builtin_tp);
}

void
set_Builtin_type(ir_node *node, ir_type *tp) {
	assert(is_Builtin(node));
	assert((get_unknown_type() == tp) || is_Method_type(tp));
	node->attr.builtin.builtin_tp = tp;
}

/* Returns a human readable string for the ir_builtin_kind. */
const char *get_builtin_kind_name(ir_builtin_kind kind) {
#define X(a)    case a: return #a;
	switch (kind) {
		X(ir_bk_trap);
		X(ir_bk_debugbreak);
		X(ir_bk_return_address);
		X(ir_bk_frame_addess);
		X(ir_bk_prefetch);
		X(ir_bk_ffs);
		X(ir_bk_clz);
		X(ir_bk_ctz);
		X(ir_bk_popcount);
		X(ir_bk_parity);
		X(ir_bk_bswap);
		X(ir_bk_inport);
		X(ir_bk_outport);
		X(ir_bk_inner_trampoline);
	}
	return "<unknown>";
#undef X
}


int Call_has_callees(const ir_node *node) {
	assert(is_Call(node));
	return ((get_irg_callee_info_state(get_irn_irg(node)) != irg_callee_info_none) &&
	        (node->attr.call.callee_arr != NULL));
}

int get_Call_n_callees(const ir_node *node) {
  assert(is_Call(node) && node->attr.call.callee_arr);
  return ARR_LEN(node->attr.call.callee_arr);
}

ir_entity *get_Call_callee(const ir_node *node, int pos) {
	assert(pos >= 0 && pos < get_Call_n_callees(node));
	return node->attr.call.callee_arr[pos];
}

void set_Call_callee_arr(ir_node *node, const int n, ir_entity ** arr) {
	assert(is_Call(node));
	if (node->attr.call.callee_arr == NULL || get_Call_n_callees(node) != n) {
		node->attr.call.callee_arr = NEW_ARR_D(ir_entity *, current_ir_graph->obst, n);
	}
	memcpy(node->attr.call.callee_arr, arr, n * sizeof(ir_entity *));
}

void remove_Call_callee_arr(ir_node *node) {
	assert(is_Call(node));
	node->attr.call.callee_arr = NULL;
}

ir_node *get_CallBegin_ptr(const ir_node *node) {
	assert(is_CallBegin(node));
	return get_irn_n(node, 0);
}

void set_CallBegin_ptr(ir_node *node, ir_node *ptr) {
	assert(is_CallBegin(node));
	set_irn_n(node, 0, ptr);
}

ir_node *get_CallBegin_call(const ir_node *node) {
	assert(is_CallBegin(node));
	return node->attr.callbegin.call;
}

void set_CallBegin_call(ir_node *node, ir_node *call) {
	assert(is_CallBegin(node));
	node->attr.callbegin.call = call;
}

/*
 * Returns non-zero if a Call is surely a self-recursive Call.
 * Beware: if this functions returns 0, the call might be self-recursive!
 */
int is_self_recursive_Call(const ir_node *call) {
	const ir_node *callee = get_Call_ptr(call);

	if (is_SymConst_addr_ent(callee)) {
		const ir_entity *ent = get_SymConst_entity(callee);
		const ir_graph  *irg = get_entity_irg(ent);
		if (irg == get_irn_irg(call))
			return 1;
	}
	return 0;
}

#define BINOP(OP)                                      \
ir_node * get_##OP##_left(const ir_node *node) {       \
  assert(is_##OP(node));                               \
  return get_irn_n(node, node->op->op_index);          \
}                                                      \
void set_##OP##_left(ir_node *node, ir_node *left) {   \
  assert(is_##OP(node));                               \
  set_irn_n(node, node->op->op_index, left);           \
}                                                      \
ir_node *get_##OP##_right(const ir_node *node) {       \
  assert(is_##OP(node));                               \
  return get_irn_n(node, node->op->op_index + 1);      \
}                                                      \
void set_##OP##_right(ir_node *node, ir_node *right) { \
  assert(is_##OP(node));                               \
  set_irn_n(node, node->op->op_index + 1, right);      \
}

#define UNOP(OP)                                  \
ir_node *get_##OP##_op(const ir_node *node) {     \
  assert(is_##OP(node));                          \
  return get_irn_n(node, node->op->op_index);     \
}                                                 \
void set_##OP##_op(ir_node *node, ir_node *op) {  \
  assert(is_##OP(node));                          \
  set_irn_n(node, node->op->op_index, op);        \
}

#define BINOP_MEM(OP)                         \
BINOP(OP)                                     \
                                              \
ir_node *                                     \
get_##OP##_mem(const ir_node *node) {         \
  assert(is_##OP(node));                      \
  return get_irn_n(node, 0);                  \
}                                             \
                                              \
void                                          \
set_##OP##_mem(ir_node *node, ir_node *mem) { \
  assert(is_##OP(node));                      \
  set_irn_n(node, 0, mem);                    \
}

#define DIVOP(OP)                                       \
BINOP_MEM(OP)                                           \
                                                        \
ir_mode *get_##OP##_resmode(const ir_node *node) {      \
  assert(is_##OP(node));                                \
  return node->attr.divmod.res_mode;                    \
}                                                       \
                                                        \
void set_##OP##_resmode(ir_node *node, ir_mode *mode) { \
  assert(is_##OP(node));                                \
  node->attr.divmod.res_mode = mode;                    \
}


BINOP(Add)
BINOP(Carry)
BINOP(Sub)
UNOP(Minus)
BINOP(Mul)
BINOP(Mulh)
DIVOP(Quot)
DIVOP(DivMod)
DIVOP(Div)
DIVOP(Mod)
UNOP(Abs)
BINOP(And)
BINOP(Or)
BINOP(Eor)
UNOP(Not)
BINOP(Shl)
BINOP(Shr)
BINOP(Shrs)
BINOP(Rotl)
BINOP(Cmp)
UNOP(Conv)
UNOP(Cast)

int get_Div_no_remainder(const ir_node *node) {
	assert(is_Div(node));
	return node->attr.divmod.no_remainder;
}

int get_Conv_strict(const ir_node *node) {
	assert(is_Conv(node));
	return node->attr.conv.strict;
}

void set_Conv_strict(ir_node *node, int strict_flag) {
	assert(is_Conv(node));
	node->attr.conv.strict = (char)strict_flag;
}

ir_type *
get_Cast_type(ir_node *node) {
	assert(is_Cast(node));
	node->attr.cast.type = skip_tid(node->attr.cast.type);
	return node->attr.cast.type;
}

void
set_Cast_type(ir_node *node, ir_type *to_tp) {
	assert(is_Cast(node));
	node->attr.cast.type = to_tp;
}


/* Checks for upcast.
 *
 * Returns true if the Cast node casts a class type to a super type.
 */
int is_Cast_upcast(ir_node *node) {
	ir_type *totype   = get_Cast_type(node);
	ir_type *fromtype = get_irn_typeinfo_type(get_Cast_op(node));

	assert(get_irg_typeinfo_state(get_irn_irg(node)) == ir_typeinfo_consistent);
	assert(fromtype);

	while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
		totype   = get_pointer_points_to_type(totype);
		fromtype = get_pointer_points_to_type(fromtype);
	}

	assert(fromtype);

	if (!is_Class_type(totype)) return 0;
	return is_SubClass_of(fromtype, totype);
}

/* Checks for downcast.
 *
 * Returns true if the Cast node casts a class type to a sub type.
 */
int is_Cast_downcast(ir_node *node) {
	ir_type *totype   = get_Cast_type(node);
	ir_type *fromtype = get_irn_typeinfo_type(get_Cast_op(node));

	assert(get_irg_typeinfo_state(get_irn_irg(node)) == ir_typeinfo_consistent);
	assert(fromtype);

	while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
		totype   = get_pointer_points_to_type(totype);
		fromtype = get_pointer_points_to_type(fromtype);
	}

	assert(fromtype);

	if (!is_Class_type(totype)) return 0;
	return is_SubClass_of(totype, fromtype);
}

int
(is_unop)(const ir_node *node) {
	return _is_unop(node);
}

ir_node *
get_unop_op(const ir_node *node) {
	if (node->op->opar == oparity_unary)
		return get_irn_n(node, node->op->op_index);

	assert(node->op->opar == oparity_unary);
	return NULL;
}

void
set_unop_op(ir_node *node, ir_node *op) {
	if (node->op->opar == oparity_unary)
		set_irn_n(node, node->op->op_index, op);

	assert(node->op->opar == oparity_unary);
}

int
(is_binop)(const ir_node *node) {
	return _is_binop(node);
}

ir_node *
get_binop_left(const ir_node *node) {
	assert(node->op->opar == oparity_binary);
	return get_irn_n(node, node->op->op_index);
}

void
set_binop_left(ir_node *node, ir_node *left) {
	assert(node->op->opar == oparity_binary);
	set_irn_n(node, node->op->op_index, left);
}

ir_node *
get_binop_right(const ir_node *node) {
	assert(node->op->opar == oparity_binary);
	return get_irn_n(node, node->op->op_index + 1);
}

void
set_binop_right(ir_node *node, ir_node *right) {
	assert(node->op->opar == oparity_binary);
	set_irn_n(node, node->op->op_index + 1, right);
}

int
(is_Phi)(const ir_node *n) {
	return _is_Phi(n);
}

int is_Phi0(const ir_node *n) {
	assert(n);

	return ((get_irn_op(n) == op_Phi) &&
	        (get_irn_arity(n) == 0) &&
	        (get_irg_phase_state(get_irn_irg(n)) ==  phase_building));
}

ir_node **
get_Phi_preds_arr(ir_node *node) {
  assert(node->op == op_Phi);
  return (ir_node **)&(get_irn_in(node)[1]);
}

int
get_Phi_n_preds(const ir_node *node) {
	assert(is_Phi(node) || is_Phi0(node));
	return (get_irn_arity(node));
}

/*
void set_Phi_n_preds(ir_node *node, int n_preds) {
	assert(node->op == op_Phi);
}
*/

ir_node *
get_Phi_pred(const ir_node *node, int pos) {
	assert(is_Phi(node) || is_Phi0(node));
	return get_irn_n(node, pos);
}

void
set_Phi_pred(ir_node *node, int pos, ir_node *pred) {
	assert(is_Phi(node) || is_Phi0(node));
	set_irn_n(node, pos, pred);
}

ir_node *(get_Phi_next)(const ir_node *phi) {
	return _get_Phi_next(phi);
}

void (set_Phi_next)(ir_node *phi, ir_node *next) {
	_set_Phi_next(phi, next);
}

int is_memop(const ir_node *node) {
	ir_opcode code = get_irn_opcode(node);
	return (code == iro_Load || code == iro_Store);
}

ir_node *get_memop_mem(const ir_node *node) {
	assert(is_memop(node));
	return get_irn_n(node, 0);
}

void set_memop_mem(ir_node *node, ir_node *mem) {
	assert(is_memop(node));
	set_irn_n(node, 0, mem);
}

ir_node *get_memop_ptr(const ir_node *node) {
	assert(is_memop(node));
	return get_irn_n(node, 1);
}

void set_memop_ptr(ir_node *node, ir_node *ptr) {
	assert(is_memop(node));
	set_irn_n(node, 1, ptr);
}

ir_node *
get_Load_mem(const ir_node *node) {
	assert(is_Load(node));
	return get_irn_n(node, 0);
}

void
set_Load_mem(ir_node *node, ir_node *mem) {
	assert(is_Load(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Load_ptr(const ir_node *node) {
	assert(is_Load(node));
	return get_irn_n(node, 1);
}

void
set_Load_ptr(ir_node *node, ir_node *ptr) {
	assert(is_Load(node));
	set_irn_n(node, 1, ptr);
}

ir_mode *
get_Load_mode(const ir_node *node) {
	assert(is_Load(node));
	return node->attr.load.mode;
}

void
set_Load_mode(ir_node *node, ir_mode *mode) {
	assert(is_Load(node));
	node->attr.load.mode = mode;
}

ir_volatility
get_Load_volatility(const ir_node *node) {
	assert(is_Load(node));
	return node->attr.load.volatility;
}

void
set_Load_volatility(ir_node *node, ir_volatility volatility) {
	assert(is_Load(node));
	node->attr.load.volatility = volatility;
}

ir_align
get_Load_align(const ir_node *node) {
	assert(is_Load(node));
	return node->attr.load.aligned;
}

void
set_Load_align(ir_node *node, ir_align align) {
	assert(is_Load(node));
	node->attr.load.aligned = align;
}


ir_node *
get_Store_mem(const ir_node *node) {
	assert(is_Store(node));
	return get_irn_n(node, 0);
}

void
set_Store_mem(ir_node *node, ir_node *mem) {
	assert(is_Store(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Store_ptr(const ir_node *node) {
	assert(is_Store(node));
	return get_irn_n(node, 1);
}

void
set_Store_ptr(ir_node *node, ir_node *ptr) {
	assert(is_Store(node));
	set_irn_n(node, 1, ptr);
}

ir_node *
get_Store_value(const ir_node *node) {
	assert(is_Store(node));
	return get_irn_n(node, 2);
}

void
set_Store_value(ir_node *node, ir_node *value) {
	assert(is_Store(node));
	set_irn_n(node, 2, value);
}

ir_volatility
get_Store_volatility(const ir_node *node) {
	assert(is_Store(node));
	return node->attr.store.volatility;
}

void
set_Store_volatility(ir_node *node, ir_volatility volatility) {
	assert(is_Store(node));
	node->attr.store.volatility = volatility;
}

ir_align
get_Store_align(const ir_node *node) {
	assert(is_Store(node));
	return node->attr.store.aligned;
}

void
set_Store_align(ir_node *node, ir_align align) {
	assert(is_Store(node));
	node->attr.store.aligned = align;
}


ir_node *
get_Alloc_mem(const ir_node *node) {
	assert(is_Alloc(node));
	return get_irn_n(node, 0);
}

void
set_Alloc_mem(ir_node *node, ir_node *mem) {
	assert(is_Alloc(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Alloc_size(const ir_node *node) {
	assert(is_Alloc(node));
	return get_irn_n(node, 1);
}

void
set_Alloc_size(ir_node *node, ir_node *size) {
	assert(is_Alloc(node));
	set_irn_n(node, 1, size);
}

ir_type *
get_Alloc_type(ir_node *node) {
	assert(is_Alloc(node));
	return node->attr.alloc.type = skip_tid(node->attr.alloc.type);
}

void
set_Alloc_type(ir_node *node, ir_type *tp) {
	assert(is_Alloc(node));
	node->attr.alloc.type = tp;
}

ir_where_alloc
get_Alloc_where(const ir_node *node) {
	assert(is_Alloc(node));
	return node->attr.alloc.where;
}

void
set_Alloc_where(ir_node *node, ir_where_alloc where) {
	assert(is_Alloc(node));
	node->attr.alloc.where = where;
}


ir_node *
get_Free_mem(const ir_node *node) {
	assert(is_Free(node));
	return get_irn_n(node, 0);
}

void
set_Free_mem(ir_node *node, ir_node *mem) {
	assert(is_Free(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Free_ptr(const ir_node *node) {
	assert(is_Free(node));
	return get_irn_n(node, 1);
}

void
set_Free_ptr(ir_node *node, ir_node *ptr) {
	assert(is_Free(node));
	set_irn_n(node, 1, ptr);
}

ir_node *
get_Free_size(const ir_node *node) {
	assert(is_Free(node));
	return get_irn_n(node, 2);
}

void
set_Free_size(ir_node *node, ir_node *size) {
	assert(is_Free(node));
	set_irn_n(node, 2, size);
}

ir_type *
get_Free_type(ir_node *node) {
	assert(is_Free(node));
	return node->attr.free.type = skip_tid(node->attr.free.type);
}

void
set_Free_type(ir_node *node, ir_type *tp) {
	assert(is_Free(node));
	node->attr.free.type = tp;
}

ir_where_alloc
get_Free_where(const ir_node *node) {
	assert(is_Free(node));
	return node->attr.free.where;
}

void
set_Free_where(ir_node *node, ir_where_alloc where) {
	assert(is_Free(node));
	node->attr.free.where = where;
}

ir_node **get_Sync_preds_arr(ir_node *node) {
	assert(is_Sync(node));
	return (ir_node **)&(get_irn_in(node)[1]);
}

int get_Sync_n_preds(const ir_node *node) {
	assert(is_Sync(node));
	return (get_irn_arity(node));
}

/*
void set_Sync_n_preds(ir_node *node, int n_preds) {
	assert(is_Sync(node));
}
*/

ir_node *get_Sync_pred(const ir_node *node, int pos) {
	assert(is_Sync(node));
	return get_irn_n(node, pos);
}

void set_Sync_pred(ir_node *node, int pos, ir_node *pred) {
	assert(is_Sync(node));
	set_irn_n(node, pos, pred);
}

/* Add a new Sync predecessor */
void add_Sync_pred(ir_node *node, ir_node *pred) {
	assert(is_Sync(node));
	add_irn_n(node, pred);
}

/* Returns the source language type of a Proj node. */
ir_type *get_Proj_type(ir_node *n) {
	ir_type *tp   = firm_unknown_type;
	ir_node *pred = get_Proj_pred(n);

	switch (get_irn_opcode(pred)) {
	case iro_Proj: {
		ir_node *pred_pred;
		/* Deal with Start / Call here: we need to know the Proj Nr. */
		assert(get_irn_mode(pred) == mode_T);
		pred_pred = get_Proj_pred(pred);

		if (is_Start(pred_pred))  {
			ir_type *mtp = get_entity_type(get_irg_entity(get_irn_irg(pred_pred)));
			tp = get_method_param_type(mtp, get_Proj_proj(n));
		} else if (is_Call(pred_pred)) {
			ir_type *mtp = get_Call_type(pred_pred);
			tp = get_method_res_type(mtp, get_Proj_proj(n));
		}
	} break;
	case iro_Start: break;
	case iro_Call: break;
	case iro_Load: {
		ir_node *a = get_Load_ptr(pred);
		if (is_Sel(a))
			tp = get_entity_type(get_Sel_entity(a));
	} break;
	default:
		break;
	}
	return tp;
}

ir_node *
get_Proj_pred(const ir_node *node) {
	assert(is_Proj(node));
	return get_irn_n(node, 0);
}

void
set_Proj_pred(ir_node *node, ir_node *pred) {
	assert(is_Proj(node));
	set_irn_n(node, 0, pred);
}

long
get_Proj_proj(const ir_node *node) {
#ifdef INTERPROCEDURAL_VIEW
	ir_opcode code = get_irn_opcode(node);

	if (code == iro_Proj) {
		return node->attr.proj;
	}
	else {
		assert(code == iro_Filter);
		return node->attr.filter.proj;
	}
#else
	assert(is_Proj(node));
	return node->attr.proj;
#endif /* INTERPROCEDURAL_VIEW */
}

void
set_Proj_proj(ir_node *node, long proj) {
#ifdef INTERPROCEDURAL_VIEW
	ir_opcode code = get_irn_opcode(node);

	if (code == iro_Proj) {
		node->attr.proj = proj;
	}
	else {
		assert(code == iro_Filter);
		node->attr.filter.proj = proj;
	}
#else
	assert(is_Proj(node));
	node->attr.proj = proj;
#endif /* INTERPROCEDURAL_VIEW */
}

/* Returns non-zero if a node is a routine parameter. */
int (is_arg_Proj)(const ir_node *node) {
	return _is_arg_Proj(node);
}

ir_node **
get_Tuple_preds_arr(ir_node *node) {
	assert(is_Tuple(node));
	return (ir_node **)&(get_irn_in(node)[1]);
}

int
get_Tuple_n_preds(const ir_node *node) {
	assert(is_Tuple(node));
	return get_irn_arity(node);
}

/*
void
set_Tuple_n_preds(ir_node *node, int n_preds) {
	assert(is_Tuple(node));
}
*/

ir_node *
get_Tuple_pred(const ir_node *node, int pos) {
  assert(is_Tuple(node));
  return get_irn_n(node, pos);
}

void
set_Tuple_pred(ir_node *node, int pos, ir_node *pred) {
	assert(is_Tuple(node));
	set_irn_n(node, pos, pred);
}

ir_node *
get_Id_pred(const ir_node *node) {
	assert(is_Id(node));
	return get_irn_n(node, 0);
}

void
set_Id_pred(ir_node *node, ir_node *pred) {
	assert(is_Id(node));
	set_irn_n(node, 0, pred);
}

ir_node *get_Confirm_value(const ir_node *node) {
	assert(is_Confirm(node));
	return get_irn_n(node, 0);
}

void set_Confirm_value(ir_node *node, ir_node *value) {
	assert(is_Confirm(node));
	set_irn_n(node, 0, value);
}

ir_node *get_Confirm_bound(const ir_node *node) {
	assert(is_Confirm(node));
	return get_irn_n(node, 1);
}

void set_Confirm_bound(ir_node *node, ir_node *bound) {
	assert(is_Confirm(node));
	set_irn_n(node, 0, bound);
}

pn_Cmp get_Confirm_cmp(const ir_node *node) {
	assert(is_Confirm(node));
	return node->attr.confirm.cmp;
}

void set_Confirm_cmp(ir_node *node, pn_Cmp cmp) {
	assert(is_Confirm(node));
	node->attr.confirm.cmp = cmp;
}

ir_node *
get_Filter_pred(ir_node *node) {
	assert(is_Filter(node));
	return node->in[1];
}

void
set_Filter_pred(ir_node *node, ir_node *pred) {
	assert(is_Filter(node));
	node->in[1] = pred;
}

long
get_Filter_proj(ir_node *node) {
	assert(is_Filter(node));
	return node->attr.filter.proj;
}

void
set_Filter_proj(ir_node *node, long proj) {
	assert(is_Filter(node));
	node->attr.filter.proj = proj;
}

/* Don't use get_irn_arity, get_irn_n in implementation as access
   shall work independent of view!!! */
void set_Filter_cg_pred_arr(ir_node *node, int arity, ir_node ** in) {
	assert(is_Filter(node));
	if (node->attr.filter.in_cg == NULL || arity != ARR_LEN(node->attr.filter.in_cg) - 1) {
		ir_graph *irg = get_irn_irg(node);
		node->attr.filter.in_cg = NEW_ARR_D(ir_node *, current_ir_graph->obst, arity + 1);
		node->attr.filter.backedge = new_backedge_arr(irg->obst, arity);
		node->attr.filter.in_cg[0] = node->in[0];
	}
	memcpy(node->attr.filter.in_cg + 1, in, sizeof(ir_node *) * arity);
}

void set_Filter_cg_pred(ir_node * node, int pos, ir_node * pred) {
	assert(is_Filter(node) && node->attr.filter.in_cg &&
	       0 <= pos && pos < ARR_LEN(node->attr.filter.in_cg) - 1);
	node->attr.filter.in_cg[pos + 1] = pred;
}

int get_Filter_n_cg_preds(ir_node *node) {
	assert(is_Filter(node) && node->attr.filter.in_cg);
	return (ARR_LEN(node->attr.filter.in_cg) - 1);
}

ir_node *get_Filter_cg_pred(ir_node *node, int pos) {
	int arity;
	assert(is_Filter(node) && node->attr.filter.in_cg &&
	       0 <= pos);
	arity = ARR_LEN(node->attr.filter.in_cg);
	assert(pos < arity - 1);
	return node->attr.filter.in_cg[pos + 1];
}

/* Mux support */
ir_node *get_Mux_sel(const ir_node *node) {
	assert(is_Mux(node));
	return node->in[1];
}

void set_Mux_sel(ir_node *node, ir_node *sel) {
	assert(is_Mux(node));
	node->in[1] = sel;
}

ir_node *get_Mux_false(const ir_node *node) {
	assert(is_Mux(node));
	return node->in[2];
}

void set_Mux_false(ir_node *node, ir_node *ir_false) {
	assert(is_Mux(node));
	node->in[2] = ir_false;
}

ir_node *get_Mux_true(const ir_node *node) {
	assert(is_Mux(node));
	return node->in[3];
}

void set_Mux_true(ir_node *node, ir_node *ir_true) {
	assert(is_Mux(node));
	node->in[3] = ir_true;
}

/* CopyB support */
ir_node *get_CopyB_mem(const ir_node *node) {
	assert(is_CopyB(node));
	return get_irn_n(node, 0);
}

void set_CopyB_mem(ir_node *node, ir_node *mem) {
	assert(node->op == op_CopyB);
	set_irn_n(node, 0, mem);
}

ir_node *get_CopyB_dst(const ir_node *node) {
	assert(is_CopyB(node));
	return get_irn_n(node, 1);
}

void set_CopyB_dst(ir_node *node, ir_node *dst) {
	assert(is_CopyB(node));
	set_irn_n(node, 1, dst);
}

ir_node *get_CopyB_src(const ir_node *node) {
  assert(is_CopyB(node));
  return get_irn_n(node, 2);
}

void set_CopyB_src(ir_node *node, ir_node *src) {
	assert(is_CopyB(node));
	set_irn_n(node, 2, src);
}

ir_type *get_CopyB_type(ir_node *node) {
	assert(is_CopyB(node));
	return node->attr.copyb.data_type = skip_tid(node->attr.copyb.data_type);
}

void set_CopyB_type(ir_node *node, ir_type *data_type) {
	assert(is_CopyB(node) && data_type);
	node->attr.copyb.data_type = data_type;
}


ir_type *
get_InstOf_type(ir_node *node) {
	assert(node->op == op_InstOf);
	return node->attr.instof.type = skip_tid(node->attr.instof.type);
}

void
set_InstOf_type(ir_node *node, ir_type *type) {
	assert(node->op == op_InstOf);
	node->attr.instof.type = type;
}

ir_node *
get_InstOf_store(const ir_node *node) {
	assert(node->op == op_InstOf);
	return get_irn_n(node, 0);
}

void
set_InstOf_store(ir_node *node, ir_node *obj) {
	assert(node->op == op_InstOf);
	set_irn_n(node, 0, obj);
}

ir_node *
get_InstOf_obj(const ir_node *node) {
	assert(node->op == op_InstOf);
	return get_irn_n(node, 1);
}

void
set_InstOf_obj(ir_node *node, ir_node *obj) {
	assert(node->op == op_InstOf);
	set_irn_n(node, 1, obj);
}

/* Returns the memory input of a Raise operation. */
ir_node *
get_Raise_mem(const ir_node *node) {
	assert(is_Raise(node));
	return get_irn_n(node, 0);
}

void
set_Raise_mem(ir_node *node, ir_node *mem) {
	assert(is_Raise(node));
	set_irn_n(node, 0, mem);
}

ir_node *
get_Raise_exo_ptr(const ir_node *node) {
	assert(is_Raise(node));
	return get_irn_n(node, 1);
}

void
set_Raise_exo_ptr(ir_node *node, ir_node *exo_ptr) {
	assert(is_Raise(node));
	set_irn_n(node, 1, exo_ptr);
}

/* Bound support */

/* Returns the memory input of a Bound operation. */
ir_node *get_Bound_mem(const ir_node *bound) {
	assert(is_Bound(bound));
	return get_irn_n(bound, 0);
}

void set_Bound_mem(ir_node *bound, ir_node *mem) {
	assert(is_Bound(bound));
	set_irn_n(bound, 0, mem);
}

/* Returns the index input of a Bound operation. */
ir_node *get_Bound_index(const ir_node *bound) {
	assert(is_Bound(bound));
	return get_irn_n(bound, 1);
}

void set_Bound_index(ir_node *bound, ir_node *idx) {
	assert(is_Bound(bound));
	set_irn_n(bound, 1, idx);
}

/* Returns the lower bound input of a Bound operation. */
ir_node *get_Bound_lower(const ir_node *bound) {
	assert(is_Bound(bound));
	return get_irn_n(bound, 2);
}

void set_Bound_lower(ir_node *bound, ir_node *lower) {
	assert(is_Bound(bound));
	set_irn_n(bound, 2, lower);
}

/* Returns the upper bound input of a Bound operation. */
ir_node *get_Bound_upper(const ir_node *bound) {
	assert(is_Bound(bound));
	return get_irn_n(bound, 3);
}

void set_Bound_upper(ir_node *bound, ir_node *upper) {
	assert(is_Bound(bound));
	set_irn_n(bound, 3, upper);
}

/* Return the operand of a Pin node. */
ir_node *get_Pin_op(const ir_node *pin) {
	assert(is_Pin(pin));
	return get_irn_n(pin, 0);
}

void set_Pin_op(ir_node *pin, ir_node *node) {
	assert(is_Pin(pin));
	set_irn_n(pin, 0, node);
}

/* Return the assembler text of an ASM pseudo node. */
ident *get_ASM_text(const ir_node *node) {
	assert(is_ASM(node));
	return node->attr.assem.asm_text;
}

/* Return the number of input constraints for an ASM node. */
int get_ASM_n_input_constraints(const ir_node *node) {
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.inputs);
}

/* Return the input constraints for an ASM node. This is a flexible array. */
const ir_asm_constraint *get_ASM_input_constraints(const ir_node *node) {
	assert(is_ASM(node));
	return node->attr.assem.inputs;
}

/* Return the number of output constraints for an ASM node.  */
int get_ASM_n_output_constraints(const ir_node *node) {
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.outputs);
}

/* Return the output constraints for an ASM node. */
const ir_asm_constraint *get_ASM_output_constraints(const ir_node *node) {
	assert(is_ASM(node));
	return node->attr.assem.outputs;
}

/* Return the number of clobbered registers for an ASM node.  */
int get_ASM_n_clobbers(const ir_node *node) {
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.clobber);
}

/* Return the list of clobbered registers for an ASM node. */
ident **get_ASM_clobbers(const ir_node *node) {
	assert(is_ASM(node));
	return node->attr.assem.clobber;
}

/* returns the graph of a node */
ir_graph *
get_irn_irg(const ir_node *node) {
	/*
	 * Do not use get_nodes_Block() here, because this
	 * will check the pinned state.
	 * However even a 'wrong' block is always in the proper
	 * irg.
	 */
	if (! is_Block(node))
		node = get_irn_n(node, -1);
	if (is_Bad(node))  /* sometimes bad is predecessor of nodes instead of block: in case of optimization */
		node = get_irn_n(node, -1);
	assert(is_Block(node));
	return node->attr.block.irg;
}


/*----------------------------------------------------------------*/
/*  Auxiliary routines                                            */
/*----------------------------------------------------------------*/

ir_node *
skip_Proj(ir_node *node) {
	/* don't assert node !!! */
	if (node == NULL)
		return NULL;

	if (is_Proj(node))
		node = get_Proj_pred(node);

	return node;
}

const ir_node *
skip_Proj_const(const ir_node *node) {
	/* don't assert node !!! */
	if (node == NULL)
		return NULL;

	if (is_Proj(node))
		node = get_Proj_pred(node);

	return node;
}

ir_node *
skip_Tuple(ir_node *node) {
  ir_node *pred;
  ir_op   *op;

restart:
	if (is_Proj(node)) {
	    pred = get_Proj_pred(node);
	    op   = get_irn_op(pred);

		/*
		 * Looks strange but calls get_irn_op() only once
		 * in most often cases.
		 */
		if (op == op_Proj) { /* nested Tuple ? */
		    pred = skip_Tuple(pred);

			if (is_Tuple(pred)) {
				node = get_Tuple_pred(pred, get_Proj_proj(node));
				goto restart;
			}
		} else if (op == op_Tuple) {
			node = get_Tuple_pred(pred, get_Proj_proj(node));
			goto restart;
		}
	}
	return node;
}

/* returns operand of node if node is a Cast */
ir_node *skip_Cast(ir_node *node) {
	if (is_Cast(node))
		return get_Cast_op(node);
	return node;
}

/* returns operand of node if node is a Cast */
const ir_node *skip_Cast_const(const ir_node *node) {
	if (is_Cast(node))
		return get_Cast_op(node);
	return node;
}

/* returns operand of node if node is a Pin */
ir_node *skip_Pin(ir_node *node) {
	if (is_Pin(node))
		return get_Pin_op(node);
	return node;
}

/* returns operand of node if node is a Confirm */
ir_node *skip_Confirm(ir_node *node) {
	if (is_Confirm(node))
		return get_Confirm_value(node);
	return node;
}

/* skip all high-level ops */
ir_node *skip_HighLevel_ops(ir_node *node) {
	while (is_op_highlevel(get_irn_op(node))) {
		node = get_irn_n(node, 0);
	}
	return node;
}


/* This should compact Id-cycles to self-cycles. It has the same (or less?) complexity
 * than any other approach, as Id chains are resolved and all point to the real node, or
 * all id's are self loops.
 *
 * Note: This function takes 10% of mostly ANY the compiler run, so it's
 * a little bit "hand optimized".
 *
 * Moreover, it CANNOT be switched off using get_opt_normalize() ...
 */
ir_node *
skip_Id(ir_node *node) {
	ir_node *pred;
	/* don't assert node !!! */

	if (!node || (node->op != op_Id)) return node;

	/* Don't use get_Id_pred():  We get into an endless loop for
	   self-referencing Ids. */
	pred = node->in[0+1];

	if (pred->op != op_Id) return pred;

	if (node != pred) {  /* not a self referencing Id. Resolve Id chain. */
		ir_node *rem_pred, *res;

		if (pred->op != op_Id) return pred; /* shortcut */
		rem_pred = pred;

		assert(get_irn_arity (node) > 0);

		node->in[0+1] = node;   /* turn us into a self referencing Id:  shorten Id cycles. */
		res = skip_Id(rem_pred);
		if (res->op == op_Id) /* self-loop */ return node;

		node->in[0+1] = res;    /* Turn Id chain into Ids all referencing the chain end. */
		return res;
	} else {
		return node;
	}
}

void skip_Id_and_store(ir_node **node) {
	ir_node *n = *node;

	if (!n || (n->op != op_Id)) return;

	/* Don't use get_Id_pred():  We get into an endless loop for
	   self-referencing Ids. */
	*node = skip_Id(n);
}

int
(is_Bad)(const ir_node *node) {
	return _is_Bad(node);
}

int
(is_NoMem)(const ir_node *node) {
	return _is_NoMem(node);
}

int
(is_Minus)(const ir_node *node) {
	return _is_Minus(node);
}

int
(is_Abs)(const ir_node *node) {
	return _is_Abs(node);
}

int
(is_Mod)(const ir_node *node) {
	return _is_Mod(node);
}

int
(is_Div)(const ir_node *node) {
	return _is_Div(node);
}

int
(is_DivMod)(const ir_node *node) {
	return _is_DivMod(node);
}

int
(is_Quot)(const ir_node *node) {
	return _is_Quot(node);
}

int
(is_Add)(const ir_node *node) {
	return _is_Add(node);
}

int
(is_Carry)(const ir_node *node) {
	return _is_Carry(node);
}

int
(is_And)(const ir_node *node) {
	return _is_And(node);
}

int
(is_Or)(const ir_node *node) {
	return _is_Or(node);
}

int
(is_Eor)(const ir_node *node) {
	return _is_Eor(node);
}

int
(is_Sub)(const ir_node *node) {
	return _is_Sub(node);
}

int
(is_Shl)(const ir_node *node) {
	return _is_Shl(node);
}

int
(is_Shr)(const ir_node *node) {
	return _is_Shr(node);
}

int
(is_Shrs)(const ir_node *node) {
	return _is_Shrs(node);
}

int
(is_Rotl)(const ir_node *node) {
	return _is_Rotl(node);
}

int
(is_Not)(const ir_node *node) {
	return _is_Not(node);
}

int
(is_Id)(const ir_node *node) {
	return _is_Id(node);
}

int
(is_Tuple)(const ir_node *node) {
	return _is_Tuple(node);
}

int
(is_Bound)(const ir_node *node) {
	return _is_Bound(node);
}

int
(is_Start)(const ir_node *node) {
  return _is_Start(node);
}

int
(is_End)(const ir_node *node) {
	return _is_End(node);
}

int
(is_Const)(const ir_node *node) {
	return _is_Const(node);
}

int
(is_Conv)(const ir_node *node) {
	return _is_Conv(node);
}

int
(is_strictConv)(const ir_node *node) {
	return _is_strictConv(node);
}

int
(is_Cast)(const ir_node *node) {
	return _is_Cast(node);
}

int
(is_no_Block)(const ir_node *node) {
	return _is_no_Block(node);
}

int
(is_Block)(const ir_node *node) {
	return _is_Block(node);
}

/* returns true if node is an Unknown node. */
int
(is_Unknown)(const ir_node *node) {
	return _is_Unknown(node);
}

/* returns true if node is a Return node. */
int
(is_Return)(const ir_node *node) {
	return _is_Return(node);
}

/* returns true if node is a Call node. */
int
(is_Call)(const ir_node *node) {
	return _is_Call(node);
}

/* returns true if node is a Builtin node. */
int
(is_Builtin)(const ir_node *node) {
	return _is_Builtin(node);
}

/* returns true if node is a CallBegin node. */
int
(is_CallBegin)(const ir_node *node) {
	return _is_CallBegin(node);
}

/* returns true if node is a Sel node. */
int
(is_Sel)(const ir_node *node) {
	return _is_Sel(node);
}

/* returns true if node is a Mux node. */
int
(is_Mux)(const ir_node *node) {
	return _is_Mux(node);
}

/* returns true if node is a Load node. */
int
(is_Load)(const ir_node *node) {
	return _is_Load(node);
}

/* returns true if node is a Load node. */
int
(is_Store)(const ir_node *node) {
	return _is_Store(node);
}

/* returns true if node is a Sync node. */
int
(is_Sync)(const ir_node *node) {
	return _is_Sync(node);
}

/* Returns true if node is a Confirm node. */
int
(is_Confirm)(const ir_node *node) {
	return _is_Confirm(node);
}

/* Returns true if node is a Pin node. */
int
(is_Pin)(const ir_node *node) {
	return _is_Pin(node);
}

/* Returns true if node is a SymConst node. */
int
(is_SymConst)(const ir_node *node) {
	return _is_SymConst(node);
}

/* Returns true if node is a SymConst node with kind symconst_addr_ent. */
int
(is_SymConst_addr_ent)(const ir_node *node) {
	return _is_SymConst_addr_ent(node);
}

/* Returns true if node is a Cond node. */
int
(is_Cond)(const ir_node *node) {
	return _is_Cond(node);
}

int
(is_CopyB)(const ir_node *node) {
	return _is_CopyB(node);
}

/* returns true if node is a Cmp node. */
int
(is_Cmp)(const ir_node *node) {
	return _is_Cmp(node);
}

/* returns true if node is an Alloc node. */
int
(is_Alloc)(const ir_node *node) {
	return _is_Alloc(node);
}

/* returns true if node is a Free node. */
int
(is_Free)(const ir_node *node) {
	return _is_Free(node);
}

/* returns true if a node is a Jmp node. */
int
(is_Jmp)(const ir_node *node) {
	return _is_Jmp(node);
}

/* returns true if a node is a IJmp node. */
int
(is_IJmp)(const ir_node *node) {
	return _is_IJmp(node);
}

/* returns true if a node is a Raise node. */
int
(is_Raise)(const ir_node *node) {
	return _is_Raise(node);
}

/* returns true if a node is an ASM node. */
int
(is_ASM)(const ir_node *node) {
	return _is_ASM(node);
}

/* returns true if a node is an Dummy node. */
int
(is_Dummy)(const ir_node *node) {
	return _is_Dummy(node);
}

int
(is_Proj)(const ir_node *node) {
	return _is_Proj(node);
}

/* Returns true if node is a Filter node. */
int
(is_Filter)(const ir_node *node) {
	return _is_Filter(node);
}

/* Returns true if the operation manipulates control flow. */
int is_cfop(const ir_node *node) {
	return is_op_cfopcode(get_irn_op(node));
}

/* Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
int is_ip_cfop(const ir_node *node) {
	return is_ip_cfopcode(get_irn_op(node));
}

/* Returns true if the operation can change the control flow because
   of an exception. */
int
is_fragile_op(const ir_node *node) {
	return is_op_fragile(get_irn_op(node));
}

/* Returns the memory operand of fragile operations. */
ir_node *get_fragile_op_mem(ir_node *node) {
	assert(node && is_fragile_op(node));

	switch (get_irn_opcode(node)) {
	case iro_Call  :
	case iro_Quot  :
	case iro_DivMod:
	case iro_Div   :
	case iro_Mod   :
	case iro_Load  :
	case iro_Store :
	case iro_Alloc :
	case iro_Bound :
	case iro_CopyB :
		return get_irn_n(node, pn_Generic_M_regular);
	case iro_Bad   :
	case iro_Unknown:
		return node;
	default: ;
		assert(0 && "should not be reached");
		return NULL;
	}
}

/* Returns the result mode of a Div operation. */
ir_mode *get_divop_resmod(const ir_node *node) {
	switch (get_irn_opcode(node)) {
	case iro_Quot  : return get_Quot_resmode(node);
	case iro_DivMod: return get_DivMod_resmode(node);
	case iro_Div   : return get_Div_resmode(node);
	case iro_Mod   : return get_Mod_resmode(node);
	default: ;
		assert(0 && "should not be reached");
		return NULL;
	}
}

/* Returns true if the operation is a forking control flow operation. */
int (is_irn_forking)(const ir_node *node) {
	return _is_irn_forking(node);
}

/* Return the type associated with the value produced by n
 * if the node remarks this type as it is the case for
 * Cast, Const, SymConst and some Proj nodes. */
ir_type *(get_irn_type)(ir_node *node) {
	return _get_irn_type(node);
}

/* Return the type attribute of a node n (SymConst, Call, Alloc, Free,
   Cast) or NULL.*/
ir_type *(get_irn_type_attr)(ir_node *node) {
	return _get_irn_type_attr(node);
}

/* Return the entity attribute of a node n (SymConst, Sel) or NULL. */
ir_entity *(get_irn_entity_attr)(ir_node *node) {
	return _get_irn_entity_attr(node);
}

/* Returns non-zero for constant-like nodes. */
int (is_irn_constlike)(const ir_node *node) {
	return _is_irn_constlike(node);
}

/*
 * Returns non-zero for nodes that are allowed to have keep-alives and
 * are neither Block nor PhiM.
 */
int (is_irn_keep)(const ir_node *node) {
	return _is_irn_keep(node);
}

/*
 * Returns non-zero for nodes that are always placed in the start block.
 */
int (is_irn_start_block_placed)(const ir_node *node) {
	return _is_irn_start_block_placed(node);
}

/* Returns non-zero for nodes that are machine operations. */
int (is_irn_machine_op)(const ir_node *node) {
	return _is_irn_machine_op(node);
}

/* Returns non-zero for nodes that are machine operands. */
int (is_irn_machine_operand)(const ir_node *node) {
	return _is_irn_machine_operand(node);
}

/* Returns non-zero for nodes that have the n'th user machine flag set. */
int (is_irn_machine_user)(const ir_node *node, unsigned n) {
	return _is_irn_machine_user(node, n);
}


/* Gets the string representation of the jump prediction .*/
const char *get_cond_jmp_predicate_name(cond_jmp_predicate pred) {
#define X(a)    case a: return #a;
	switch (pred) {
		X(COND_JMP_PRED_NONE);
		X(COND_JMP_PRED_TRUE);
		X(COND_JMP_PRED_FALSE);
	}
	return "<unknown>";
#undef X
}

/* Returns the conditional jump prediction of a Cond node. */
cond_jmp_predicate (get_Cond_jmp_pred)(const ir_node *cond) {
	return _get_Cond_jmp_pred(cond);
}

/* Sets a new conditional jump prediction. */
void (set_Cond_jmp_pred)(ir_node *cond, cond_jmp_predicate pred) {
	_set_Cond_jmp_pred(cond, pred);
}

/** the get_type operation must be always implemented and return a firm type */
static ir_type *get_Default_type(ir_node *n) {
	(void) n;
	return get_unknown_type();
}

/* Sets the get_type operation for an ir_op_ops. */
ir_op_ops *firm_set_default_get_type(ir_opcode code, ir_op_ops *ops) {
	switch (code) {
	case iro_Const:    ops->get_type = get_Const_type; break;
	case iro_SymConst: ops->get_type = get_SymConst_value_type; break;
	case iro_Cast:     ops->get_type = get_Cast_type; break;
	case iro_Proj:     ops->get_type = get_Proj_type; break;
	default:
		/* not allowed to be NULL */
		if (! ops->get_type)
			ops->get_type = get_Default_type;
		break;
	}
	return ops;
}

/** Return the attribute type of a SymConst node if exists */
static ir_type *get_SymConst_attr_type(ir_node *self) {
	symconst_kind kind = get_SymConst_kind(self);
	if (SYMCONST_HAS_TYPE(kind))
		return get_SymConst_type(self);
	return NULL;
}

/** Return the attribute entity of a SymConst node if exists */
static ir_entity *get_SymConst_attr_entity(ir_node *self) {
	symconst_kind kind = get_SymConst_kind(self);
	if (SYMCONST_HAS_ENT(kind))
		return get_SymConst_entity(self);
	return NULL;
}

/** the get_type_attr operation must be always implemented */
static ir_type *get_Null_type(ir_node *n) {
	(void) n;
	return firm_unknown_type;
}

/* Sets the get_type operation for an ir_op_ops. */
ir_op_ops *firm_set_default_get_type_attr(ir_opcode code, ir_op_ops *ops) {
	switch (code) {
	case iro_SymConst: ops->get_type_attr = get_SymConst_attr_type; break;
	case iro_Call:     ops->get_type_attr = get_Call_type; break;
	case iro_Alloc:    ops->get_type_attr = get_Alloc_type; break;
	case iro_Free:     ops->get_type_attr = get_Free_type; break;
	case iro_Cast:     ops->get_type_attr = get_Cast_type; break;
	default:
		/* not allowed to be NULL */
		if (! ops->get_type_attr)
			ops->get_type_attr = get_Null_type;
		break;
	}
	return ops;
}

/** the get_entity_attr operation must be always implemented */
static ir_entity *get_Null_ent(ir_node *n) {
	(void) n;
	return NULL;
}

/* Sets the get_type operation for an ir_op_ops. */
ir_op_ops *firm_set_default_get_entity_attr(ir_opcode code, ir_op_ops *ops) {
	switch (code) {
	case iro_SymConst: ops->get_entity_attr = get_SymConst_attr_entity; break;
	case iro_Sel:      ops->get_entity_attr = _get_Sel_entity; break;
	default:
		/* not allowed to be NULL */
		if (! ops->get_entity_attr)
			ops->get_entity_attr = get_Null_ent;
		break;
	}
	return ops;
}

/* Sets the debug information of a node. */
void (set_irn_dbg_info)(ir_node *n, dbg_info *db) {
	_set_irn_dbg_info(n, db);
}

/**
 * Returns the debug information of an node.
 *
 * @param n   The node.
 */
dbg_info *(get_irn_dbg_info)(const ir_node *n) {
	return _get_irn_dbg_info(n);
}

#if 0 /* allow the global pointer */

/* checks whether a node represents a global address */
int is_Global(const ir_node *node) {
	ir_node *ptr;

	if (is_SymConst_addr_ent(node))
		return 1;
	if (! is_Sel(node))
		return 0;

	ptr = get_Sel_ptr(node);
	return is_globals_pointer(ptr) != NULL;
}

/* returns the entity of a global address */
ir_entity *get_Global_entity(const ir_node *node) {
	if (is_SymConst(node))
		return get_SymConst_entity(node);
	else
		return get_Sel_entity(node);
}
#else

/* checks whether a node represents a global address */
int is_Global(const ir_node *node) {
	return is_SymConst_addr_ent(node);
}

/* returns the entity of a global address */
ir_entity *get_Global_entity(const ir_node *node) {
	return get_SymConst_entity(node);
}
#endif

/*
 * Calculate a hash value of a node.
 */
unsigned firm_default_hash(const ir_node *node) {
	unsigned h;
	int i, irn_arity;

	/* hash table value = 9*(9*(9*(9*(9*arity+in[0])+in[1])+ ...)+mode)+code */
	h = irn_arity = get_irn_intra_arity(node);

	/* consider all in nodes... except the block if not a control flow. */
	for (i = is_cfop(node) ? -1 : 0;  i < irn_arity;  ++i) {
		h = 9*h + HASH_PTR(get_irn_intra_n(node, i));
	}

	/* ...mode,... */
	h = 9*h + HASH_PTR(get_irn_mode(node));
	/* ...and code */
	h = 9*h + HASH_PTR(get_irn_op(node));

	return h;
}  /* firm_default_hash */
