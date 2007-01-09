/*
 * Project:     libFIRM
 * File name:   ir/ir/firmstat.c
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef FIRM_STATISTICS

#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif

#include "irouts.h"
#include "irdump.h"
#include "hashptr.h"
#include "firmstat_t.h"
#include "pattern.h"
#include "dags.h"
#include "stat_dmp.h"
#include "xmalloc.h"
#include "irhooks.h"

/*
 * need this to be static:
 * Special pseudo Opcodes that we need to count some interesting cases
 */

/**
 * The Phi0, a node that is created during SSA construction
 */
static ir_op _op_Phi0;

/** The PhiM, just to count memory Phi's. */
static ir_op _op_PhiM;

/** The Mul by Const node. */
static ir_op _op_MulC;

/** The Div by Const node. */
static ir_op _op_DivC;

/** The Div by Const node. */
static ir_op _op_ModC;

/** The Div by Const node. */
static ir_op _op_DivModC;

/** The memory Proj node. */
static ir_op _op_ProjM;

/** A Sel of a Sel */
static ir_op _op_SelSel;

/** A Sel of a Sel of a Sel */
static ir_op _op_SelSelSel;

/* ---------------------------------------------------------------------------------- */

/** Marks the begin of a statistic (hook) function. */
#define STAT_ENTER		++status->recursive

/** Marks the end of a statistic (hook) functions. */
#define STAT_LEAVE		--status->recursive

/** Allows to enter a statistic function only when we are not already in a hook. */
#define STAT_ENTER_SINGLE	do { if (status->recursive > 0) return; ++status->recursive; } while (0)

/**
 * global status
 */
static const int status_disable = 0;
static stat_info_t *status = (stat_info_t *)&status_disable;

/**
 * Compare two elements of the opcode hash.
 */
static int opcode_cmp(const void *elt, const void *key) {
	const node_entry_t *e1 = elt;
	const node_entry_t *e2 = key;

	return e1->op->code - e2->op->code;
}  /* opcode_cmp */

/**
 * Compare two elements of the graph hash.
 */
static int graph_cmp(const void *elt, const void *key) {
	const graph_entry_t *e1 = elt;
	const graph_entry_t *e2 = key;

	return e1->irg != e2->irg;
}  /* graph_cmp */

/**
 * Compare two elements of the optimization hash.
 */
static int opt_cmp(const void *elt, const void *key) {
	const opt_entry_t *e1 = elt;
	const opt_entry_t *e2 = key;

	return e1->op->code != e2->op->code;
}  /* opt_cmp */

/**
 * Compare two elements of the block/extbb hash.
 */
static int block_cmp(const void *elt, const void *key) {
	const block_entry_t *e1 = elt;
	const block_entry_t *e2 = key;

	return e1->block_nr != e2->block_nr;
}  /* block_cmp */

/**
 * Compare two elements of the be_block hash.
 */
static int be_block_cmp(const void *elt, const void *key) {
	const be_block_entry_t *e1 = elt;
	const be_block_entry_t *e2 = key;

	return e1->block_nr != e2->block_nr;
}  /* be_block_cmp */

/**
 * Compare two elements of reg pressure hash.
 */
static int reg_pressure_cmp(const void *elt, const void *key) {
	const reg_pressure_entry_t *e1 = elt;
	const reg_pressure_entry_t *e2 = key;

	return e1->class_name != e2->class_name;
}  /* reg_pressure_cmp */

/**
 * Compare two elements of the perm_stat hash.
 */
static int perm_stat_cmp(const void *elt, const void *key) {
	const perm_stat_entry_t *e1 = elt;
	const perm_stat_entry_t *e2 = key;

	return e1->perm != e2->perm;
}  /* perm_stat_cmp */

/**
 * Compare two elements of the perm_class hash.
 */
static int perm_class_cmp(const void *elt, const void *key) {
	const perm_class_entry_t *e1 = elt;
	const perm_class_entry_t *e2 = key;

	return e1->class_name != e2->class_name;
}  /* perm_class_cmp */

/**
 * Compare two elements of the ir_op hash.
 */
static int opcode_cmp_2(const void *elt, const void *key) {
	const ir_op *e1 = elt;
	const ir_op *e2 = key;

	return e1->code != e2->code;
}  /* opcode_cmp_2 */

/**
 * Compare two elements of the address_mark set.
 */
static int address_mark_cmp(const void *elt, const void *key, size_t size) {
	const address_mark_entry_t *e1 = elt;
	const address_mark_entry_t *e2 = key;

	/* compare only the nodes, the rest is used as data container */
	return e1->node != e2->node;
}  /* address_mark_cmp */

/**
 * Clear all counter in a node_entry_t.
 */
static void opcode_clear_entry(node_entry_t *elem) {
	cnt_clr(&elem->cnt_alive);
	cnt_clr(&elem->new_node);
	cnt_clr(&elem->into_Id);
}  /* opcode_clear_entry */

/**
 * Returns the associates node_entry_t for an ir_op
 *
 * @param op    the IR operation
 * @param hmap  a hash map containing ir_op* -> node_entry_t*
 */
static node_entry_t *opcode_get_entry(const ir_op *op, hmap_node_entry_t *hmap) {
	node_entry_t key;
	node_entry_t *elem;

	key.op = op;

	elem = pset_find(hmap, &key, op->code);
	if (elem)
		return elem;

	elem = obstack_alloc(&status->cnts, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));

	/* clear counter */
	opcode_clear_entry(elem);

	elem->op = op;

	return pset_insert(hmap, elem, op->code);
}  /* opcode_get_entry */

/**
 * Returns the associates ir_op for an opcode
 *
 * @param code  the IR opcode
 * @param hmap  the hash map containing opcode -> ir_op*
 */
static ir_op *opcode_find_entry(opcode code, hmap_ir_op *hmap) {
	ir_op key;

	key.code = code;
	return pset_find(hmap, &key, code);
}  /* opcode_find_entry */

/**
 * Clears all counter in a graph_entry_t.
 *
 * @param elem  the graph entry
 * @param all   if non-zero, clears all counters, else leave accumulated ones
 */
static void graph_clear_entry(graph_entry_t *elem, int all) {
	if (all) {
		cnt_clr(&elem->cnt_walked);
		cnt_clr(&elem->cnt_walked_blocks);
		cnt_clr(&elem->cnt_was_inlined);
		cnt_clr(&elem->cnt_got_inlined);
		cnt_clr(&elem->cnt_strength_red);
		cnt_clr(&elem->cnt_real_func_call);
	}  /* if */
	cnt_clr(&elem->cnt_edges);
	cnt_clr(&elem->cnt_all_calls);
	cnt_clr(&elem->cnt_call_with_cnst_arg);
	cnt_clr(&elem->cnt_indirect_calls);
	cnt_clr(&elem->cnt_pure_adr_ops);
	cnt_clr(&elem->cnt_all_adr_ops);

	if (elem->block_hash) {
		del_pset(elem->block_hash);
		elem->block_hash = NULL;
	}  /* if */

	if (elem->extbb_hash) {
		del_pset(elem->extbb_hash);
		elem->extbb_hash = NULL;
	}  /* if */

	obstack_free(&elem->recalc_cnts, NULL);
	obstack_init(&elem->recalc_cnts);
}  /* graph_clear_entry */

/**
 * Returns the associated graph_entry_t for an IR graph.
 *
 * @param irg   the IR graph
 * @param hmap  the hash map containing ir_graph* -> graph_entry_t*
 */
static graph_entry_t *graph_get_entry(ir_graph *irg, hmap_graph_entry_t *hmap)
{
	graph_entry_t key;
	graph_entry_t *elem;
	int i;

	key.irg = irg;

	elem = pset_find(hmap, &key, HASH_PTR(irg));

	if (elem) {
		/* create hash map backend block information */
		if (! elem->be_block_hash)
			elem->be_block_hash = new_pset(be_block_cmp, 5);

		return elem;
	}  /* if */

	/* allocate a new one */
	elem = obstack_alloc(&status->cnts, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));
	obstack_init(&elem->recalc_cnts);

	/* clear counter */
	graph_clear_entry(elem, 1);

	/* new hash table for opcodes here  */
	elem->opcode_hash   = new_pset(opcode_cmp, 5);
	elem->address_mark  = new_set(address_mark_cmp, 5);
	elem->irg           = irg;

	/* these hash tables are created on demand */
	elem->block_hash = NULL;
	elem->extbb_hash = NULL;

	for (i = 0; i < sizeof(elem->opt_hash)/sizeof(elem->opt_hash[0]); ++i)
		elem->opt_hash[i] = new_pset(opt_cmp, 4);

	return pset_insert(hmap, elem, HASH_PTR(irg));
}  /* graph_get_entry */

/**
 * Clear all counter in an opt_entry_t.
 */
static void opt_clear_entry(opt_entry_t *elem) {
	cnt_clr(&elem->count);
}  /* opt_clear_entry */

/**
 * Returns the associated opt_entry_t for an IR operation.
 *
 * @param op    the IR operation
 * @param hmap  the hash map containing ir_op* -> opt_entry_t*
 */
static opt_entry_t *opt_get_entry(const ir_op *op, hmap_opt_entry_t *hmap)
{
	opt_entry_t key;
	opt_entry_t *elem;

	key.op = op;

	elem = pset_find(hmap, &key, op->code);
	if (elem)
		return elem;

	elem = obstack_alloc(&status->cnts, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));

	/* clear new counter */
	opt_clear_entry(elem);

	elem->op = op;

	return pset_insert(hmap, elem, op->code);
}  /* opt_get_entry */

/**
 * clears all counter in a block_entry_t
 */
static void block_clear_entry(block_entry_t *elem) {
	cnt_clr(&elem->cnt_nodes);
	cnt_clr(&elem->cnt_edges);
	cnt_clr(&elem->cnt_in_edges);
	cnt_clr(&elem->cnt_out_edges);
	cnt_clr(&elem->cnt_phi_data);
}  /* block_clear_entry */

/**
 * Returns the associated block_entry_t for an block.
 *
 * @param block_nr  an IR  block number
 * @param hmap      a hash map containing long -> block_entry_t
 */
static block_entry_t *block_get_entry(struct obstack *obst, long block_nr, hmap_block_entry_t *hmap)
{
	block_entry_t key;
	block_entry_t *elem;

	key.block_nr = block_nr;

	elem = pset_find(hmap, &key, block_nr);
	if (elem)
		return elem;

	elem = obstack_alloc(obst, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));

	/* clear new counter */
	block_clear_entry(elem);

	elem->block_nr = block_nr;

	return pset_insert(hmap, elem, block_nr);
}  /* block_get_entry */

/**
 * Clear all sets in be_block_entry_t.
 */
static void be_block_clear_entry(be_block_entry_t *elem)
{
	if (elem->reg_pressure)
		del_pset(elem->reg_pressure);

	if (elem->sched_ready)
		stat_delete_distrib_tbl(elem->sched_ready);

	if (elem->perm_class_stat)
		del_pset(elem->perm_class_stat);

	elem->reg_pressure    = new_pset(reg_pressure_cmp, 5);
	elem->sched_ready     = stat_new_int_distrib_tbl();
	elem->perm_class_stat = new_pset(perm_class_cmp, 5);
}  /* be_block_clear_entry */

/**
 * Returns the associated be_block_entry_t for an block.
 *
 * @param block_nr  an IR  block number
 * @param hmap      a hash map containing long -> be_block_entry_t
 */
static be_block_entry_t *be_block_get_entry(struct obstack *obst, long block_nr, hmap_be_block_entry_t *hmap)
{
	be_block_entry_t key;
	be_block_entry_t *elem;

	key.block_nr = block_nr;

	elem = pset_find(hmap, &key, block_nr);
	if (elem)
		return elem;

	elem = obstack_alloc(obst, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));

	/* clear new counter */
	be_block_clear_entry(elem);

	elem->block_nr = block_nr;

	return pset_insert(hmap, elem, block_nr);
}  /* be_block_get_entry */

/**
 * clears all sets in perm_class_entry_t
 */
static void perm_class_clear_entry(perm_class_entry_t *elem) {
	if (elem->perm_stat)
		del_pset(elem->perm_stat);

	elem->perm_stat = new_pset(perm_stat_cmp, 5);
}  /* perm_class_clear_entry */

/**
 * Returns the associated perm_class entry for a register class.
 *
 * @param class_name  the register class name
 * @param hmap        a hash map containing class_name -> perm_class_entry_t
 */
static perm_class_entry_t *perm_class_get_entry(struct obstack *obst, const char *class_name,
                                                hmap_perm_class_entry_t *hmap)
{
	perm_class_entry_t key;
	perm_class_entry_t *elem;

	key.class_name = class_name;

	elem = pset_find(hmap, &key, HASH_PTR(class_name));
	if (elem)
		return elem;

	elem = obstack_alloc(obst, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));

	/* clear new counter */
	perm_class_clear_entry(elem);

	elem->class_name = class_name;

	return pset_insert(hmap, elem, HASH_PTR(class_name));
}  /* perm_class_get_entry */

/**
 * clears all sets in perm_stat_entry_t
 */
static void perm_stat_clear_entry(perm_stat_entry_t *elem) {
	if (elem->chains)
		stat_delete_distrib_tbl(elem->chains);

	if (elem->cycles)
		stat_delete_distrib_tbl(elem->cycles);

	elem->chains = stat_new_int_distrib_tbl();
	elem->cycles = stat_new_int_distrib_tbl();
}  /* perm_stat_clear_entry */

/**
 * Returns the associated perm_stat entry for a perm.
 *
 * @param perm      the perm node
 * @param hmap      a hash map containing perm -> perm_stat_entry_t
 */
static perm_stat_entry_t *perm_stat_get_entry(struct obstack *obst, ir_node *perm, hmap_perm_stat_entry_t *hmap)
{
	perm_stat_entry_t key;
	perm_stat_entry_t *elem;

	key.perm = perm;

	elem = pset_find(hmap, &key, HASH_PTR(perm));
	if (elem)
		return elem;

	elem = obstack_alloc(obst, sizeof(*elem));
	memset(elem, 0, sizeof(*elem));

	/* clear new counter */
	perm_stat_clear_entry(elem);

	elem->perm = perm;

	return pset_insert(hmap, elem, HASH_PTR(perm));
}  /* perm_stat_get_entry */

/**
 * Returns the ir_op for an IR-node,
 * handles special cases and return pseudo op codes.
 *
 * @param none  an IR node
 */
static ir_op *stat_get_irn_op(ir_node *node)
{
	ir_op *op = get_irn_op(node);

	if (op == op_Phi && get_irn_arity(node) == 0) {
		/* special case, a Phi0 node, count on extra counter */
		op = status->op_Phi0 ? status->op_Phi0 : op;
	} else if (op == op_Phi && get_irn_mode(node) == mode_M) {
		/* special case, a Memory Phi node, count on extra counter */
		op = status->op_PhiM ? status->op_PhiM : op;
	} else if (op == op_Proj && get_irn_mode(node) == mode_M) {
		/* special case, a Memory Proj node, count on extra counter */
		op = status->op_ProjM ? status->op_ProjM : op;
	} else if (op == op_Mul &&
		(get_irn_op(get_Mul_left(node)) == op_Const || get_irn_op(get_Mul_right(node)) == op_Const)) {
		/* special case, a Multiply by a const, count on extra counter */
		op = status->op_MulC ? status->op_MulC : op;
	} else if (op == op_Div && get_irn_op(get_Div_right(node)) == op_Const) {
		/* special case, a division by a const, count on extra counter */
		op = status->op_DivC ? status->op_DivC : op;
	} else if (op == op_Mod && get_irn_op(get_Mod_right(node)) == op_Const) {
		/* special case, a module by a const, count on extra counter */
		op = status->op_ModC ? status->op_ModC : op;
	} else if (op == op_DivMod && get_irn_op(get_DivMod_right(node)) == op_Const) {
		/* special case, a division/modulo by a const, count on extra counter */
		op = status->op_DivModC ? status->op_DivModC : op;
	} else if (op == op_Sel && get_irn_op(get_Sel_ptr(node)) == op_Sel) {
		/* special case, a Sel of a Sel, count on extra counter */
		op = status->op_SelSel ? status->op_SelSel : op;

		if (get_irn_op(get_Sel_ptr(get_Sel_ptr(node))) == op_Sel) {
			/* special case, a Sel of a Sel of a Sel, count on extra counter */
			op = status->op_SelSelSel ? status->op_SelSelSel : op;
		}  /* if */
	}  /* if */

	return op;
}  /* stat_get_irn_op */

/**
 * update the block counter
 */
static void undate_block_info(ir_node *node, graph_entry_t *graph)
{
	ir_op *op = get_irn_op(node);
	ir_node *block;
	block_entry_t *b_entry;
	int i, arity;

	/* check for block */
	if (op == op_Block) {
		arity = get_irn_arity(node);
		b_entry = block_get_entry(&graph->recalc_cnts, get_irn_node_nr(node), graph->block_hash);

		/* count all incoming edges */
		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_irn_n(node, i);
			ir_node *other_block = get_nodes_block(pred);
			block_entry_t *b_entry_other = block_get_entry(&graph->recalc_cnts, get_irn_node_nr(other_block), graph->block_hash);

			cnt_inc(&b_entry->cnt_in_edges);	/* an edge coming from another block */
			cnt_inc(&b_entry_other->cnt_out_edges);
		}  /* for */
		return;
	}  /* if */

	block   = get_nodes_block(node);
	b_entry = block_get_entry(&graph->recalc_cnts, get_irn_node_nr(block), graph->block_hash);

	if (op == op_Phi && mode_is_datab(get_irn_mode(node))) {
		/* count data Phi per block */
		cnt_inc(&b_entry->cnt_phi_data);
	}  /* if */

	/* we have a new node in our block */
	cnt_inc(&b_entry->cnt_nodes);

	/* don't count keep-alive edges */
	if (get_irn_op(node) == op_End)
		return;

	arity = get_irn_arity(node);

	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		ir_node *other_block;

		other_block = get_nodes_block(pred);

		if (other_block == block)
			cnt_inc(&b_entry->cnt_edges);	/* a in block edge */
		else {
			block_entry_t *b_entry_other = block_get_entry(&graph->recalc_cnts, get_irn_node_nr(other_block), graph->block_hash);

			cnt_inc(&b_entry->cnt_in_edges);	/* an edge coming from another block */
			cnt_inc(&b_entry_other->cnt_out_edges);
		}  /* if */
	}  /* for */
}  /* undate_block_info */

/**
 * Update the extended block counter.
 */
static void update_extbb_info(ir_node *node, graph_entry_t *graph)
{
	ir_op *op = get_irn_op(node);
	ir_extblk *extbb;
	extbb_entry_t *eb_entry;
	int i, arity;

	/* check for block */
	if (op == op_Block) {
		extbb = get_nodes_extbb(node);
		arity = get_irn_arity(node);
		eb_entry = block_get_entry(&graph->recalc_cnts, get_extbb_node_nr(extbb), graph->extbb_hash);

		/* count all incoming edges */
		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_irn_n(node, i);
			ir_extblk *other_extbb = get_nodes_extbb(pred);

			if (extbb != other_extbb) {
				extbb_entry_t *eb_entry_other = block_get_entry(&graph->recalc_cnts, get_extbb_node_nr(other_extbb), graph->extbb_hash);

				cnt_inc(&eb_entry->cnt_in_edges);	/* an edge coming from another extbb */
				cnt_inc(&eb_entry_other->cnt_out_edges);
			}  /* if */
		}  /* for */
		return;
	}  /* if */

	extbb    = get_nodes_extbb(node);
	eb_entry = block_get_entry(&graph->recalc_cnts, get_extbb_node_nr(extbb), graph->extbb_hash);

	if (op == op_Phi && mode_is_datab(get_irn_mode(node))) {
		/* count data Phi per extbb */
		cnt_inc(&eb_entry->cnt_phi_data);
	}  /* if */

	/* we have a new node in our block */
	cnt_inc(&eb_entry->cnt_nodes);

	/* don't count keep-alive edges */
	if (get_irn_op(node) == op_End)
		return;

	arity = get_irn_arity(node);

	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		ir_extblk *other_extbb = get_nodes_extbb(pred);

		if (other_extbb == extbb)
			cnt_inc(&eb_entry->cnt_edges);	/* a in extbb edge */
		else {
			extbb_entry_t *eb_entry_other = block_get_entry(&graph->recalc_cnts, get_extbb_node_nr(other_extbb), graph->extbb_hash);

			cnt_inc(&eb_entry->cnt_in_edges);	/* an edge coming from another extbb */
			cnt_inc(&eb_entry_other->cnt_out_edges);
		}  /* if */
	}  /* for */
}  /* update_extbb_info */

/** Calculates how many arguments of the call are const. */
static int cnt_const_args(ir_node *call) {
	int i, res = 0;
	int n = get_Call_n_params(call);

	for (i = 0; i < n; ++i) {
		ir_node *param = get_Call_param(call, i);
		ir_op   *op = get_irn_op(param);

		if (op == op_Const || op == op_SymConst)
			++res;
	}  /* for */
	return res;
}  /* cnt_const_args */

/**
 * Update info on calls.
 *
 * @param call   The call
 * @param graph  The graph entry containing the call
 */
static void stat_update_call(ir_node *call, graph_entry_t *graph)
{
	ir_node   *block = get_nodes_block(call);
	ir_node   *ptr = get_Call_ptr(call);
	ir_entity *ent = NULL;
	ir_graph  *callee = NULL;
	int       num_const_args;

	/*
	 * If the block is bad, the whole subgraph will collapse later
	 * so do not count this call.
	 * This happens in dead code.
	 */
	if (is_Bad(block))
		return;

	cnt_inc(&graph->cnt_all_calls);

	/* found a call, this function is not a leaf */
	graph->is_leaf = 0;

	if (get_irn_op(ptr) == op_SymConst) {
		if (get_SymConst_kind(ptr) == symconst_addr_ent) {
			/* ok, we seems to know the entity */
			ent = get_SymConst_entity(ptr);
			callee = get_entity_irg(ent);

			/* it is recursive, if it calls at least once */
			if (callee == graph->irg)
				graph->is_recursive = 1;
		}  /* if */
	} else {
		/* indirect call, be could not predict */
		cnt_inc(&graph->cnt_indirect_calls);

		/* NOT a leaf call */
		graph->is_leaf_call = LCS_NON_LEAF_CALL;
	}  /* if */

	/* check, if it's a chain-call: Then, the call-block
	* must dominate the end block. */
	{
		ir_node *curr = get_irg_end_block(graph->irg);
		int depth = get_Block_dom_depth(block);

		for (; curr != block && get_Block_dom_depth(curr) > depth;) {
			curr = get_Block_idom(curr);

			if (! curr || is_no_Block(curr))
				break;
		}  /* for */

		if (curr != block)
			graph->is_chain_call = 0;
	}

	/* check, if the callee is a leaf */
	if (callee) {
		graph_entry_t *called = graph_get_entry(callee, status->irg_hash);

		if (called->is_analyzed) {
			if (! called->is_leaf)
				graph->is_leaf_call = LCS_NON_LEAF_CALL;
		}  /* if */
	}  /* if */

	/* check, if arguments of the call are const */
	num_const_args = cnt_const_args(call);

	if (num_const_args > 0)
		cnt_inc(&graph->cnt_call_with_cnst_arg);
}  /* stat_update_call */

/**
 * Update info on calls for graphs on the wait queue.
 */
static void stat_update_call_2(ir_node *call, graph_entry_t *graph)
{
	ir_node   *block = get_nodes_block(call);
	ir_node   *ptr = get_Call_ptr(call);
	ir_entity *ent = NULL;
	ir_graph  *callee = NULL;

	/*
	 * If the block is bad, the whole subgraph will collapse later
	 * so do not count this call.
	 * This happens in dead code.
	 */
	if (is_Bad(block))
		return;

	if (get_irn_op(ptr) == op_SymConst) {
		if (get_SymConst_kind(ptr) == symconst_addr_ent) {
			/* ok, we seems to know the entity */
			ent = get_SymConst_entity(ptr);
			callee = get_entity_irg(ent);
		}  /* if */
	}  /* if */

	/* check, if the callee is a leaf */
	if (callee) {
		graph_entry_t *called = graph_get_entry(callee, status->irg_hash);

		assert(called->is_analyzed);

		if (! called->is_leaf)
			graph->is_leaf_call = LCS_NON_LEAF_CALL;
	} else
		graph->is_leaf_call = LCS_NON_LEAF_CALL;
}  /* stat_update_call_2 */

/**
 * Walker for reachable nodes count.
 */
static void update_node_stat(ir_node *node, void *env)
{
	graph_entry_t *graph = env;
	node_entry_t *entry;

	ir_op *op = stat_get_irn_op(node);
	int arity = get_irn_arity(node);

	entry = opcode_get_entry(op, graph->opcode_hash);

	cnt_inc(&entry->cnt_alive);
	cnt_add_i(&graph->cnt_edges, arity);

	/* count block edges */
	undate_block_info(node, graph);

	/* count extended block edges */
	if (status->stat_options & FIRMSTAT_COUNT_EXTBB) {
		if (graph->irg != get_const_code_irg())
			update_extbb_info(node, graph);
	}  /* if */

	/* handle statistics for special node types */

	if (op == op_Const) {
		if (status->stat_options & FIRMSTAT_COUNT_CONSTS) {
			/* check properties of constants */
			stat_update_const(status, node, graph);
		}  /* if */
	} else if (op == op_Call) {
		/* check for properties that depends on calls like recursion/leaf/indirect call */
		stat_update_call(node, graph);
	}  /* if */
}  /* update_node_stat */

/**
 * Walker for reachable nodes count for graphs on the wait_q.
 */
static void update_node_stat_2(ir_node *node, void *env) {
	graph_entry_t *graph = env;

	/* check for properties that depends on calls like recursion/leaf/indirect call */
	if (is_Call(node))
		stat_update_call_2(node, graph);
}  /* update_node_stat_2 */

/**
 * Get the current address mark.
 */
static unsigned get_adr_mark(graph_entry_t *graph, ir_node *node) {
	address_mark_entry_t *value = set_find(graph->address_mark, &node, sizeof(*value), HASH_PTR(node));

	return value ? value->mark : 0;
}  /* get_adr_mark */

/**
 * Set the current address mark.
 */
static void set_adr_mark(graph_entry_t *graph, ir_node *node, unsigned val) {
	address_mark_entry_t *value = set_insert(graph->address_mark, &node, sizeof(*value), HASH_PTR(node));

	value->mark = val;
}  /* set_adr_mark */

#undef DUMP_ADR_MODE

#ifdef DUMP_ADR_MODE
/**
 * a vcg attribute hook: Color a node with a different color if
 * it's identified as a part of an address expression or at least referenced
 * by an address expression.
 */
static int stat_adr_mark_hook(FILE *F, ir_node *node, ir_node *local)
{
	ir_node *n           = local ? local : node;
	ir_graph *irg        = get_irn_irg(n);
	graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);
	unsigned mark        = get_adr_mark(graph, n);

	if (mark & MARK_ADDRESS_CALC)
		fprintf(F, "color: purple");
	else if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == MARK_REF_ADR)
		fprintf(F, "color: pink");
	else if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == (MARK_REF_ADR|MARK_REF_NON_ADR))
		fprintf(F, "color: lightblue");
	else
		return 0;

	/* I know the color! */
	return 1;
}  /* stat_adr_mark_hook */
#endif /* DUMP_ADR_MODE */

/**
 * Return the "operational" mode of a Firm node.
 */
static ir_mode *get_irn_op_mode(ir_node *node) {
	switch (get_irn_opcode(node)) {
	case iro_Load:
		return get_Load_mode(node);
	case iro_Store:
		return get_irn_mode(get_Store_value(node));
	case iro_DivMod:
		return get_irn_mode(get_DivMod_left(node));
	case iro_Div:
		return get_irn_mode(get_Div_left(node));
	case iro_Mod:
		return get_irn_mode(get_Mod_left(node));
	case iro_Cmp:
		/* Cmp is no address calculation, or is it? */
	default:
		return get_irn_mode(node);
	}  /* switch */
}  /* get_irn_op_mode */

/**
 * Post-walker that marks every node that is an address calculation.
 *
 * Users of a node must be visited first. We ensure this by
 * calling it in the post of an outs walk. This should work even in cycles,
 * while the normal pre-walk will not.
 */
static void mark_address_calc(ir_node *node, void *env) {
	graph_entry_t *graph = env;
	ir_mode *mode = get_irn_op_mode(node);
	int i, n;
	unsigned mark_preds = MARK_REF_NON_ADR;

	if (! mode_is_numP(mode))
		return;

	if (mode_is_reference(mode)) {
		/* a reference is calculated here, we are sure */
		set_adr_mark(graph, node, MARK_ADDRESS_CALC);

		mark_preds = MARK_REF_ADR;
	} else {
		unsigned mark = get_adr_mark(graph, node);

		if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == MARK_REF_ADR) {
			/*
			 * this node has no reference mode, but is only
			 * referenced by address calculations
			 */
			mark_preds = MARK_REF_ADR;
		}  /* if */
	}  /* if */

	/* mark all predecessors */
	for (i = 0, n = get_irn_arity(node); i < n; ++i) {
		ir_node *pred = get_irn_n(node, i);

		mode = get_irn_op_mode(pred);
		if (! mode_is_numP(mode))
			continue;

		set_adr_mark(graph, pred, get_adr_mark(graph, pred) | mark_preds);
	}  /* for */
}  /* mark_address_calc */

/**
 * Post-walker that marks every node that is an address calculation.
 *
 * Users of a node must be visited first. We ensure this by
 * calling it in the post of an outs walk. This should work even in cycles,
 * while the normal pre-walk will not.
 */
static void count_adr_ops(ir_node *node, void *env) {
	graph_entry_t *graph = env;
	unsigned mark        = get_adr_mark(graph, node);

	if (mark & MARK_ADDRESS_CALC)
		cnt_inc(&graph->cnt_pure_adr_ops);
	else if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == MARK_REF_ADR)
		cnt_inc(&graph->cnt_pure_adr_ops);
	else if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == (MARK_REF_ADR|MARK_REF_NON_ADR))
		cnt_inc(&graph->cnt_all_adr_ops);
}  /* count_adr_ops */

/**
 * Called for every graph when the graph is either deleted or stat_dump_snapshot()
 * is called, must recalculate all statistic info.
 *
 * @param global    The global entry
 * @param graph     The current entry
 */
static void update_graph_stat(graph_entry_t *global, graph_entry_t *graph)
{
	node_entry_t *entry;

	/* clear first the alive counter in the graph */
	for (entry = pset_first(graph->opcode_hash); entry; entry = pset_next(graph->opcode_hash)) {
		cnt_clr(&entry->cnt_alive);
	}  /* for */

	/* set pessimistic values */
	graph->is_leaf       = 1;
	graph->is_leaf_call  = LCS_UNKNOWN;
	graph->is_recursive  = 0;
	graph->is_chain_call = 1;

	/* create new block counter */
	graph->block_hash = new_pset(block_cmp, 5);

	/* we need dominator info */
	if (graph->irg != get_const_code_irg()) {
		assure_doms(graph->irg);

		if (status->stat_options & FIRMSTAT_COUNT_EXTBB) {
			/* we need extended basic blocks */
			compute_extbb(graph->irg);

			/* create new extbb counter */
			graph->extbb_hash = new_pset(block_cmp, 5);
		}  /* if */
	}  /* if */

	/* count the nodes in the graph */
	irg_walk_graph(graph->irg, update_node_stat, NULL, graph);

#if 0
	/* Uncomment this code if chain-call means call exact one. */
	entry = opcode_get_entry(op_Call, graph->opcode_hash);

	/* check if we have more than 1 call */
	if (cnt_gt(entry->cnt_alive, 1))
		graph->is_chain_call = 0;
#endif

	/* recursive functions are never chain calls, leafs don't have calls */
	if (graph->is_recursive || graph->is_leaf)
		graph->is_chain_call = 0;

	/* assume we walk every graph only ONCE, we could sum here the global count */
	for (entry = pset_first(graph->opcode_hash); entry; entry = pset_next(graph->opcode_hash)) {
		node_entry_t *g_entry = opcode_get_entry(entry->op, global->opcode_hash);

		/* update the node counter */
		cnt_add(&g_entry->cnt_alive, &entry->cnt_alive);
	}  /* for */

	/* update the edge counter */
	cnt_add(&global->cnt_edges, &graph->cnt_edges);

	/* count the number of address calculation */
	if (graph->irg != get_const_code_irg()) {
		ir_graph *rem = current_ir_graph;

		assure_irg_outs(graph->irg);

		/* Must be done an the outs graph */
		current_ir_graph = graph->irg;
		irg_out_walk(get_irg_start(graph->irg), NULL, mark_address_calc, graph);
		current_ir_graph = rem;

#ifdef DUMP_ADR_MODE
		/* register the vcg hook and dump the graph for test */
		set_dump_node_vcgattr_hook(stat_adr_mark_hook);
		dump_ir_block_graph(graph->irg, "-adr");
		set_dump_node_vcgattr_hook(NULL);
#endif /* DUMP_ADR_MODE */

		irg_walk_graph(graph->irg, NULL, count_adr_ops, graph);
	}  /* if */

	/* count the DAG's */
	if (status->stat_options & FIRMSTAT_COUNT_DAG)
		count_dags_in_graph(global, graph);

	/* calculate the patterns of this graph */
	stat_calc_pattern_history(graph->irg);

	/* leaf function did not call others */
	if (graph->is_leaf)
		graph->is_leaf_call = LCS_NON_LEAF_CALL;
	else if (graph->is_leaf_call == LCS_UNKNOWN) {
		/* we still don't know if this graph calls leaf-functions, so enqueue */
		pdeq_putl(status->wait_q, graph);
	}  /* if */

	/* we have analyzed this graph */
	graph->is_analyzed = 1;
}  /* update_graph_stat */

/**
 * Called for every graph that was on the wait_q in stat_dump_snapshot()
 * must finish all statistic info calculations.
 *
 * @param global    The global entry
 * @param graph     The current entry
 */
static void update_graph_stat_2(graph_entry_t *global, graph_entry_t *graph)
{
	if (graph->is_deleted) {
		/* deleted, ignore */
		return;
	}

	if (graph->irg) {
		/* count the nodes in the graph */
		irg_walk_graph(graph->irg, update_node_stat_2, NULL, graph);

		if (graph->is_leaf_call == LCS_UNKNOWN)
			graph->is_leaf_call = LCS_LEAF_CALL;
	}  /* if */
}  /* update_graph_stat_2 */

/**
 * Register a dumper.
 */
static void stat_register_dumper(const dumper_t *dumper) {
	dumper_t *p = xmalloc(sizeof(*p));

	if (p) {
		memcpy(p, dumper, sizeof(*p));

		p->next        = status->dumper;
		p->status      = status;
		status->dumper = p;
	}

	/* FIXME: memory leak */
}  /* stat_register_dumper */

/**
 * Dumps the statistics of an IR graph.
 */
static void stat_dump_graph(graph_entry_t *entry) {
	dumper_t *dumper;

	for (dumper = status->dumper; dumper; dumper = dumper->next) {
		if (dumper->dump_graph)
			dumper->dump_graph(dumper, entry);
	}  /* for */
}  /* stat_dump_graph */

/**
 * Calls all registered dumper functions.
 */
static void stat_dump_registered(graph_entry_t *entry) {
	dumper_t *dumper;

	for (dumper = status->dumper; dumper; dumper = dumper->next) {
		if (dumper->func_map) {
			dump_graph_FUNC func;

			foreach_pset(dumper->func_map, func)
				func(dumper, entry);
		}  /* if */
	}  /* for */
}  /* stat_dump_registered */

/**
 * Dumps a constant table.
 */
static void stat_dump_consts(const constant_info_t *tbl) {
	dumper_t *dumper;

	for (dumper = status->dumper; dumper; dumper = dumper->next) {
		if (dumper->dump_const_tbl)
			dumper->dump_const_tbl(dumper, tbl);
	}  /* for */
}  /* stat_dump_consts */

/**
 * Initialize the dumper.
 */
static void stat_dump_init(const char *name) {
	dumper_t *dumper;

	for (dumper = status->dumper; dumper; dumper = dumper->next) {
		if (dumper->init)
			dumper->init(dumper, name);
	}  /* for */
}  /* stat_dump_init */

/**
 * Finish the dumper.
 */
static void stat_dump_finish(void) {
	dumper_t *dumper;

	for (dumper = status->dumper; dumper; dumper = dumper->next) {
		if (dumper->finish)
			dumper->finish(dumper);
	}  /* for */
}  /* stat_dump_finish */

/**
 * Register an additional function for all dumper.
 */
void stat_register_dumper_func(dump_graph_FUNC func) {
	dumper_t *dumper;

	for (dumper = status->dumper; dumper; dumper = dumper->next) {
		if (! dumper->func_map)
			dumper->func_map = pset_new_ptr(3);
		pset_insert_ptr(dumper->func_map, func);
	}  /* for */
}  /* stat_register_dumper_func */

/* ---------------------------------------------------------------------- */

/*
 * Helper: get an ir_op from an opcode.
 */
ir_op *stat_get_op_from_opcode(opcode code) {
	return opcode_find_entry(code, status->ir_op_hash);
}  /* stat_get_op_from_opcode */

/**
 * Hook: A new IR op is registered.
 *
 * @param ctx  the hook context
 * @param op   the new IR opcode that was created.
 */
static void stat_new_ir_op(void *ctx, ir_op *op) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(NULL, status->irg_hash);

		/* execute for side effect :-) */
		(void)opcode_get_entry(op, graph->opcode_hash);

		pset_insert(status->ir_op_hash, op, op->code);
	}
	STAT_LEAVE;
}  /* stat_new_ir_op */

/**
 * Hook: An IR op is freed.
 *
 * @param ctx  the hook context
 * @param op   the IR opcode that is freed
 */
static void stat_free_ir_op(void *ctx, ir_op *op) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
	}
	STAT_LEAVE;
}  /* stat_free_ir_op */

/**
 * Hook: A new node is created.
 *
 * @param ctx   the hook context
 * @param irg   the IR graph on which the node is created
 * @param node  the new IR node that was created
 */
static void stat_new_node(void *ctx, ir_graph *irg, ir_node *node) {
	if (! status->stat_options)
		return;

	/* do NOT count during dead node elimination */
	if (status->in_dead_node_elim > 0)
		return;

	STAT_ENTER;
	{
		node_entry_t *entry;
		graph_entry_t *graph;
		ir_op *op = stat_get_irn_op(node);

		/* increase global value */
		graph = graph_get_entry(NULL, status->irg_hash);
		entry = opcode_get_entry(op, graph->opcode_hash);
		cnt_inc(&entry->new_node);

		/* increase local value */
		graph = graph_get_entry(current_ir_graph, status->irg_hash);
		entry = opcode_get_entry(op, graph->opcode_hash);
		cnt_inc(&entry->new_node);
	}
	STAT_LEAVE;
}  /* stat_new_node */

/**
 * Hook: A node is changed into a Id node
 *
 * @param ctx   the hook context
 * @param node  the IR node that will be turned into an ID
 */
static void stat_turn_into_id(void *ctx, ir_node *node) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		node_entry_t *entry;
		graph_entry_t *graph;
		ir_op *op = stat_get_irn_op(node);

		/* increase global value */
		graph = graph_get_entry(NULL, status->irg_hash);
		entry = opcode_get_entry(op, graph->opcode_hash);
		cnt_inc(&entry->into_Id);

		/* increase local value */
		graph = graph_get_entry(current_ir_graph, status->irg_hash);
		entry = opcode_get_entry(op, graph->opcode_hash);
		cnt_inc(&entry->into_Id);
	}
	STAT_LEAVE;
}  /* stat_turn_into_id */

/**
 * Hook: A new graph was created
 *
 * @param ctx  the hook context
 * @param irg  the new IR graph that was created
 * @param ent  the entity of this graph
 */
static void stat_new_graph(void *ctx, ir_graph *irg, ir_entity *ent) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		/* execute for side effect :-) */
		graph_entry_t * graph = graph_get_entry(irg, status->irg_hash);

		graph->ent           = ent;
		graph->is_deleted    = 0;
		graph->is_leaf       = 0;
		graph->is_leaf_call  = 0;
		graph->is_recursive  = 0;
		graph->is_chain_call = 0;
		graph->is_analyzed   = 0;
	}
	STAT_LEAVE;
}  /* stat_new_graph */

/**
 * Hook: A graph will be deleted
 *
 * @param ctx  the hook context
 * @param irg  the IR graph that will be deleted
 *
 * Note that we still hold the information for this graph
 * in our hash maps, only a flag is set which prevents this
 * information from being changed, it's "frozen" from now.
 */
static void stat_free_graph(void *ctx, ir_graph *irg) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph  = graph_get_entry(irg, status->irg_hash);
		graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

		graph->is_deleted = 1;

		if (status->stat_options & FIRMSTAT_COUNT_DELETED) {
			/* count the nodes of the graph yet, it will be destroyed later */
			update_graph_stat(global, graph);
		}  /* if */
	}
	STAT_LEAVE;
}  /* stat_free_graph */

/**
 * Hook: A walk over a graph is initiated. Do not count walks from statistic code.
 *
 * @param ctx  the hook context
 * @param irg  the IR graph that will be walked
 * @param pre  the pre walker
 * @param post the post walker
 */
static void stat_irg_walk(void *ctx, ir_graph *irg, generic_func *pre, generic_func *post)
{
	if (! status->stat_options)
		return;

	STAT_ENTER_SINGLE;
	{
		graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

		cnt_inc(&graph->cnt_walked);
	}
	STAT_LEAVE;
}  /* stat_irg_walk */

/**
 * Hook: A walk over a graph in block-wise order is initiated. Do not count walks from statistic code.
 *
 * @param ctx  the hook context
 * @param irg  the IR graph that will be walked
 * @param pre  the pre walker
 * @param post the post walker
 */
static void stat_irg_walk_blkwise(void *ctx, ir_graph *irg, generic_func *pre, generic_func *post)
{
	/* for now, do NOT differentiate between blockwise and normal */
	stat_irg_walk(ctx, irg, pre, post);
}  /* stat_irg_walk_blkwise */

/**
 * Hook: A walk over the graph's blocks is initiated. Do not count walks from statistic code.
 *
 * @param ctx  the hook context
 * @param irg  the IR graph that will be walked
 * @param node the IR node
 * @param pre  the pre walker
 * @param post the post walker
 */
static void stat_irg_block_walk(void *ctx, ir_graph *irg, ir_node *node, generic_func *pre, generic_func *post)
{
	if (! status->stat_options)
		return;

	STAT_ENTER_SINGLE;
	{
		graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

		cnt_inc(&graph->cnt_walked_blocks);
	}
	STAT_LEAVE;
}  /* stat_irg_block_walk */

/**
 * Called for every node that is removed due to an optimization.
 *
 * @param n     the IR node that will be removed
 * @param hmap  the hash map containing ir_op* -> opt_entry_t*
 */
static void removed_due_opt(ir_node *n, hmap_opt_entry_t *hmap) {
	ir_op *op          = stat_get_irn_op(n);
	opt_entry_t *entry = opt_get_entry(op, hmap);

	/* increase global value */
	cnt_inc(&entry->count);
}  /* removed_due_opt */

/**
 * Hook: Some nodes were optimized into some others due to an optimization.
 *
 * @param ctx  the hook context
 */
static void stat_merge_nodes(
    void *ctx,
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    hook_opt_kind opt)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		int i, j;
		graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

		if (status->reassoc_run)
			opt = HOOK_OPT_REASSOC;

		for (i = 0; i < old_num_entries; ++i) {
			/* nodes might be in new and old, so if we found a node
			   in both sets, this one  is NOT removed */
			for (j = 0; j < new_num_entries; ++j) {
				if (old_node_array[i] == new_node_array[j])
					break;
			}  /* for */
			if (j >= new_num_entries) {
				int xopt = opt;

				/* sometimes we did not detect, that it is replaced by a Const */
				if (opt == HOOK_OPT_CONFIRM && new_num_entries == 1) {
					ir_op *op = get_irn_op(new_node_array[0]);

					if (op == op_Const || op == op_SymConst)
						xopt = HOOK_OPT_CONFIRM_C;
				}  /* if */

				removed_due_opt(old_node_array[i], graph->opt_hash[xopt]);
			}  /* if */
		}  /* for */
	}
	STAT_LEAVE;
}  /* stat_merge_nodes */

/**
 * Hook: Reassociation is started/stopped.
 *
 * @param ctx   the hook context
 * @param flag  if non-zero, reassociation is started else stopped
 */
static void stat_reassociate(void *ctx, int flag) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		status->reassoc_run = flag;
	}
	STAT_LEAVE;
}  /* stat_reassociate */

/**
 * Hook: A node was lowered into other nodes
 *
 * @param ctx  the hook context
 * @param node the IR node that will be lowered
 */
static void stat_lower(void *ctx, ir_node *node) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

		removed_due_opt(node, graph->opt_hash[HOOK_LOWERED]);
	}
	STAT_LEAVE;
}  /* stat_lower */

/**
 * Hook: A graph was inlined.
 *
 * @param ctx  the hook context
 * @param call the IR call that will re changed into the body of
 *             the called IR graph
 * @param called_irg  the IR graph representing the called routine
 */
static void stat_inline(void *ctx, ir_node *call, ir_graph *called_irg)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		ir_graph *irg = get_irn_irg(call);
		graph_entry_t *i_graph = graph_get_entry(called_irg, status->irg_hash);
		graph_entry_t *graph   = graph_get_entry(irg, status->irg_hash);

		cnt_inc(&graph->cnt_got_inlined);
		cnt_inc(&i_graph->cnt_was_inlined);
	}
	STAT_LEAVE;
}  /* stat_inline */

/**
 * Hook: A graph with tail-recursions was optimized.
 *
 * @param ctx  the hook context
 */
static void stat_tail_rec(void *ctx, ir_graph *irg, int n_calls) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

		graph->num_tail_recursion += n_calls;
	}
	STAT_LEAVE;
}  /* stat_tail_rec */

/**
 * Strength reduction was performed on an iteration variable.
 *
 * @param ctx  the hook context
 */
static void stat_strength_red(void *ctx, ir_graph *irg, ir_node *strong) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);
		cnt_inc(&graph->cnt_strength_red);

		removed_due_opt(strong, graph->opt_hash[HOOK_OPT_STRENGTH_RED]);
	}
	STAT_LEAVE;
}  /* stat_strength_red */

/**
 * Hook: Start/Stop the dead node elimination.
 *
 * @param ctx  the hook context
 */
static void stat_dead_node_elim(void *ctx, ir_graph *irg, int start) {
	if (! status->stat_options)
		return;

	if (start)
		++status->in_dead_node_elim;
	else
		--status->in_dead_node_elim;
}  /* stat_dead_node_elim */

/**
 * Hook: if-conversion was tried.
 */
static void stat_if_conversion(void *context, ir_graph *irg, ir_node *phi,
                               int pos, ir_node *mux, if_result_t reason)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

		cnt_inc(&graph->cnt_if_conv[reason]);
	}
	STAT_LEAVE;
}  /* stat_if_conversion */

/**
 * Hook: real function call was optimized.
 */
static void stat_func_call(void *context, ir_graph *irg, ir_node *call)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

		cnt_inc(&graph->cnt_real_func_call);
	}
	STAT_LEAVE;
}  /* stat_func_call */

/**
 * Hook: A multiply was replaced by a series of Shifts/Adds/Subs.
 *
 * @param ctx  the hook context
 */
static void stat_arch_dep_replace_mul_with_shifts(void *ctx, ir_node *mul) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
		removed_due_opt(mul, graph->opt_hash[HOOK_OPT_ARCH_DEP]);
	}
	STAT_LEAVE;
}  /* stat_arch_dep_replace_mul_with_shifts */

/**
 * Hook: A division by const was replaced.
 *
 * @param ctx   the hook context
 * @param node  the division node that will be optimized
 */
static void stat_arch_dep_replace_division_by_const(void *ctx, ir_node *node) {
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
		removed_due_opt(node, graph->opt_hash[HOOK_OPT_ARCH_DEP]);
	}
	STAT_LEAVE;
}  /* stat_arch_dep_replace_division_by_const */

/*
 * Update the register pressure of a block.
 *
 * @param irg        the irg containing the block
 * @param block      the block for which the reg pressure should be set
 * @param pressure   the pressure
 * @param class_name the name of the register class
 */
void stat_be_block_regpressure(ir_graph *irg, ir_node *block, int pressure, const char *class_name)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t        *graph = graph_get_entry(irg, status->irg_hash);
		be_block_entry_t     *block_ent;
		reg_pressure_entry_t *rp_ent;

		block_ent = be_block_get_entry(&status->be_data, get_irn_node_nr(block), graph->be_block_hash);
		rp_ent    = obstack_alloc(&status->be_data, sizeof(*rp_ent));
		memset(rp_ent, 0, sizeof(*rp_ent));

		rp_ent->class_name = class_name;
		rp_ent->pressure   = pressure;

		pset_insert(block_ent->reg_pressure, rp_ent, HASH_PTR(class_name));
	}
	STAT_LEAVE;
}  /* stat_be_block_regpressure */

/**
 * Update the distribution of ready nodes of a block
 *
 * @param irg        the irg containing the block
 * @param block      the block for which the reg pressure should be set
 * @param num_ready  the number of ready nodes
 */
void stat_be_block_sched_ready(ir_graph *irg, ir_node *block, int num_ready)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t    *graph = graph_get_entry(irg, status->irg_hash);
		be_block_entry_t *block_ent;

		block_ent = be_block_get_entry(&status->be_data, get_irn_node_nr(block), graph->be_block_hash);

		/* increase the counter of corresponding number of ready nodes */
		stat_inc_int_distrib_tbl(block_ent->sched_ready, num_ready);
	}
	STAT_LEAVE;
}  /* stat_be_block_sched_ready */

/**
 * Update the permutation statistic of a block.
 *
 * @param class_name the name of the register class
 * @param n_regs     number of registers in the register class
 * @param perm       the perm node
 * @param block      the block containing the perm
 * @param size       the size of the perm
 * @param real_size  number of pairs with different registers
 */
void stat_be_block_stat_perm(const char *class_name, int n_regs, ir_node *perm, ir_node *block,
                             int size, int real_size)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t      *graph = graph_get_entry(get_irn_irg(block), status->irg_hash);
		be_block_entry_t   *block_ent;
		perm_class_entry_t *pc_ent;
		perm_stat_entry_t  *ps_ent;

		block_ent = be_block_get_entry(&status->be_data, get_irn_node_nr(block), graph->be_block_hash);
		pc_ent    = perm_class_get_entry(&status->be_data, class_name, block_ent->perm_class_stat);
		ps_ent    = perm_stat_get_entry(&status->be_data, perm, pc_ent->perm_stat);

		pc_ent->n_regs = n_regs;

		/* update information */
		ps_ent->size      = size;
		ps_ent->real_size = real_size;
	}
	STAT_LEAVE;
}  /* stat_be_block_stat_perm */

/**
 * Update the permutation statistic of a single perm.
 *
 * @param class_name the name of the register class
 * @param perm       the perm node
 * @param block      the block containing the perm
 * @param is_chain   1 if chain, 0 if cycle
 * @param size       length of the cycle/chain
 * @param n_ops      the number of ops representing this cycle/chain after lowering
 */
void stat_be_block_stat_permcycle(const char *class_name, ir_node *perm, ir_node *block,
                                  int is_chain, int size, int n_ops)
{
	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t      *graph = graph_get_entry(get_irn_irg(block), status->irg_hash);
		be_block_entry_t   *block_ent;
		perm_class_entry_t *pc_ent;
		perm_stat_entry_t  *ps_ent;

		block_ent = be_block_get_entry(&status->be_data, get_irn_node_nr(block), graph->be_block_hash);
		pc_ent    = perm_class_get_entry(&status->be_data, class_name, block_ent->perm_class_stat);
		ps_ent    = perm_stat_get_entry(&status->be_data, perm, pc_ent->perm_stat);

		if (is_chain) {
			ps_ent->n_copies += n_ops;
			stat_inc_int_distrib_tbl(ps_ent->chains, size);
		} else {
			ps_ent->n_exchg += n_ops;
			stat_inc_int_distrib_tbl(ps_ent->cycles, size);
		}  /* if */
	}
	STAT_LEAVE;
}  /* stat_be_block_stat_permcycle */

/* Dumps a statistics snapshot. */
void stat_dump_snapshot(const char *name, const char *phase)
{
	char fname[2048];
	const char *p;
	int l;

	if (! status->stat_options)
		return;

	STAT_ENTER;
	{
		graph_entry_t *entry;
		graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

		/*
		 * The constant counter is only global, so we clear it here.
		 * Note that it does NOT contain the constants in DELETED
		 * graphs due to this.
		 */
		if (status->stat_options & FIRMSTAT_COUNT_CONSTS)
			stat_const_clear(status);

		/* build the name */
		p = strrchr(name, '/');
#ifdef _WIN32
		{
			const char *q;

			q = strrchr(name, '\\');

			/* NULL might be not the smallest pointer */
			if (q && (!p || q > p))
				p = q;
		}
#endif /* _WIN32 */
		if (p) {
			++p;
			l = p - name;

			if (l > sizeof(fname) - 1)
				l = sizeof(fname) - 1;

			memcpy(fname, name, l);
			fname[l] = '\0';
		} else {
			fname[0] = '\0';
			p = name;
		}  /* if */
		strncat(fname, "firmstat-", sizeof(fname));
		strncat(fname, phase, sizeof(fname));
		strncat(fname, "-", sizeof(fname));
		strncat(fname, p, sizeof(fname));

		stat_dump_init(fname);

		/* calculate the graph statistics */
		for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {
			if (entry->irg == NULL) {
				/* special entry for the global count */
				continue;
			}  /* if */
			if (! entry->is_deleted) {
				/* the graph is still alive, count the nodes on it */
				update_graph_stat(global, entry);
			}  /* if */
		}  /* for */

		/* some calculations are dependent, we pushed them on the wait_q */
		while (! pdeq_empty(status->wait_q)) {
			entry = pdeq_getr(status->wait_q);

			update_graph_stat_2(global, entry);
		}  /* while */

		/* dump per graph */
		for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {
			if (entry->irg == NULL) {
				/* special entry for the global count */
				continue;
			}  /* if */

			if (! entry->is_deleted || status->stat_options & FIRMSTAT_COUNT_DELETED) {
				stat_dump_graph(entry);
				stat_dump_registered(entry);
			}  /* if */

			if (! entry->is_deleted) {
				/* clear the counter that are not accumulated */
				graph_clear_entry(entry, 0);
			}  /* if */
		}  /* for */

		/* dump global */
		stat_dump_graph(global);

		/* dump the const info */
		if (status->stat_options & FIRMSTAT_COUNT_CONSTS)
			stat_dump_consts(&status->const_info);

		stat_dump_finish();

		stat_finish_pattern_history(fname);

		/* clear the global counter here */
		{
			node_entry_t *entry;

			for (entry = pset_first(global->opcode_hash); entry; entry = pset_next(global->opcode_hash)) {
				opcode_clear_entry(entry);
			}  /* for */
			/* clear all global counter */
			graph_clear_entry(global, 1);
		}
	}
	STAT_LEAVE;
}  /* stat_dump_snapshot */

/** the hook entries for the Firm statistics module */
static hook_entry_t stat_hooks[hook_last];

/* initialize the statistics module. */
void firm_init_stat(unsigned enable_options)
{
#define X(a)  a, sizeof(a)-1
#define HOOK(h, fkt) \
	stat_hooks[h].hook._##h = fkt; register_hook(h, &stat_hooks[h])
	unsigned num = 0;

	if (! (enable_options & FIRMSTAT_ENABLED))
		return;

	status = xmalloc(sizeof(*status));
	memset(status, 0, sizeof(*status));

	/* enable statistics */
	status->stat_options = enable_options & FIRMSTAT_ENABLED ? enable_options : 0;

	/* register all hooks */
	HOOK(hook_new_ir_op,                          stat_new_ir_op);
	HOOK(hook_free_ir_op,                         stat_free_ir_op);
	HOOK(hook_new_node,                           stat_new_node);
	HOOK(hook_turn_into_id,                       stat_turn_into_id);
	HOOK(hook_new_graph,                          stat_new_graph);
	HOOK(hook_free_graph,                         stat_free_graph);
	HOOK(hook_irg_walk,                           stat_irg_walk);
	HOOK(hook_irg_walk_blkwise,                   stat_irg_walk_blkwise);
	HOOK(hook_irg_block_walk,                     stat_irg_block_walk);
	HOOK(hook_merge_nodes,                        stat_merge_nodes);
	HOOK(hook_reassociate,                        stat_reassociate);
	HOOK(hook_lower,                              stat_lower);
	HOOK(hook_inline,                             stat_inline);
	HOOK(hook_tail_rec,                           stat_tail_rec);
	HOOK(hook_strength_red,                       stat_strength_red);
	HOOK(hook_dead_node_elim,                     stat_dead_node_elim);
	HOOK(hook_if_conversion,                      stat_if_conversion);
	HOOK(hook_func_call,                          stat_func_call);
	HOOK(hook_arch_dep_replace_mul_with_shifts,   stat_arch_dep_replace_mul_with_shifts);
	HOOK(hook_arch_dep_replace_division_by_const, stat_arch_dep_replace_division_by_const);

	obstack_init(&status->cnts);
	obstack_init(&status->be_data);

	/* create the hash-tables */
	status->irg_hash   = new_pset(graph_cmp, 8);
	status->ir_op_hash = new_pset(opcode_cmp_2, 1);

	/* create the wait queue */
	status->wait_q     = new_pdeq();

	if (enable_options & FIRMSTAT_COUNT_STRONG_OP) {
		/* build the pseudo-ops */

		_op_Phi0.code    = --num;
		_op_Phi0.name    = new_id_from_chars(X("Phi0"));

		_op_PhiM.code    = --num;
		_op_PhiM.name    = new_id_from_chars(X("PhiM"));

		_op_ProjM.code   = --num;
		_op_ProjM.name   = new_id_from_chars(X("ProjM"));

		_op_MulC.code    = --num;
		_op_MulC.name    = new_id_from_chars(X("MulC"));

		_op_DivC.code    = --num;
		_op_DivC.name    = new_id_from_chars(X("DivC"));

		_op_ModC.code    = --num;
		_op_ModC.name    = new_id_from_chars(X("ModC"));

		_op_DivModC.code = --num;
		_op_DivModC.name = new_id_from_chars(X("DivModC"));

		status->op_Phi0    = &_op_Phi0;
		status->op_PhiM    = &_op_PhiM;
		status->op_ProjM   = &_op_ProjM;
		status->op_MulC    = &_op_MulC;
		status->op_DivC    = &_op_DivC;
		status->op_ModC    = &_op_ModC;
		status->op_DivModC = &_op_DivModC;
	} else {
		status->op_Phi0    = NULL;
		status->op_PhiM    = NULL;
		status->op_ProjM   = NULL;
		status->op_MulC    = NULL;
		status->op_DivC    = NULL;
		status->op_ModC    = NULL;
		status->op_DivModC = NULL;
	}  /* if */

	/* for Florian: count the Sel depth */
	if (enable_options & FIRMSTAT_COUNT_SELS) {
		_op_SelSel.code    = --num;
		_op_SelSel.name    = new_id_from_chars(X("Sel(Sel)"));

		_op_SelSelSel.code = --num;
		_op_SelSelSel.name = new_id_from_chars(X("Sel(Sel(Sel))"));

		status->op_SelSel    = &_op_SelSel;
		status->op_SelSelSel = &_op_SelSelSel;
	} else {
		status->op_SelSel    = NULL;
		status->op_SelSelSel = NULL;
	}  /* if */

	/* register the dumper */
	stat_register_dumper(&simple_dumper);

	if (enable_options & FIRMSTAT_CSV_OUTPUT)
		stat_register_dumper(&csv_dumper);

	/* initialize the pattern hash */
	stat_init_pattern_history(enable_options & FIRMSTAT_PATTERN_ENABLED);

	/* initialize the Const options */
	if (enable_options & FIRMSTAT_COUNT_CONSTS)
		stat_init_const_cnt(status);

#undef HOOK
#undef X
}  /* firm_init_stat */

/**
 * Frees all dumper structures.
 */
static void stat_term_dumper(void) {
	dumper_t *dumper, *next_dumper;

	for (dumper = status->dumper; dumper; /* iteration done in loop body */ ) {
		if (dumper->func_map)
			del_pset(dumper->func_map);

		next_dumper = dumper->next;
		free(dumper);
		dumper = next_dumper;
	}  /* for */
}  /* stat_term_dumper */


/* Terminates the statistics module, frees all memory. */
void stat_term(void) {
	if (status != (stat_info_t *)&status_disable) {
		obstack_free(&status->be_data, NULL);
		obstack_free(&status->cnts, NULL);

		stat_term_dumper();

		xfree(status);
		status = (stat_info_t *)&status_disable;
	}
}  /* stat_term */

/* returns 1 if statistics were initialized, 0 otherwise */
int stat_is_active(void) {
	return status != (stat_info_t *)&status_disable;
}  /* stat_is_active */

#else

/* initialize the statistics module. */
void firm_init_stat(unsigned enable_options) {}

/* Dumps a statistics snapshot */
void stat_dump_snapshot(const char *name, const char *phase) {}

/* terminates the statistics module, frees all memory */
void stat_term(void);

#endif /* FIRM_STATISTICS */
