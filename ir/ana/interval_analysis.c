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
 * @brief   Implementation of interval analysis
 * @version $Id$
 */
#include "config.h"

#include <string.h>

#include "debug.h"
#include "interval_analysis.h"
#include "execution_frequency.h"
#include "set.h"
#include "array.h"

#include "irloop.h"
#include "irnode.h"
#include "irdump_t.h"
#include "irdom.h"
#include "irflag.h"
#include "irprintf.h"
#include "hashptr.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/*------------------------------------------------------------------*/
/* A new in array via a hashmap. */
/* The in array refers to the loop the block is contained in if the */
/* block is not in blocks loop. */
/*------------------------------------------------------------------*/

/** The attributes of a region. */
typedef struct {
	void *reg;        /**< The region: A block or a loop. */
	void **in_array;  /**< New in-array for this region, may contain NULL (to be synchronized with block inputs). */
	void **op_array;  /**< If reg is a loop, the control flow operations leading to this loop. */
	int n_outs;       /**< The number of out edges of this region. */
	int n_exc_outs;   /**< The number of exception out edges of this region. */
} region_attr;

/** A Hashset containing the region attributes. */
static set *region_attr_set = NULL;

/**
 * Compare two region attributes for identical regions.
 */
static int region_attr_cmp(const void *e1, const void *e2, size_t size) {
	region_attr *ra1 = (region_attr *)e1;
	region_attr *ra2 = (region_attr *)e2;
	(void) size;
	return (ra1->reg != ra2->reg);
}

/** Hash a region attribute (the region only). */
static inline int attr_set_hash(region_attr *a) {
	return HASH_PTR(a->reg);
}

/**
 * Return the region attribute for a given region.
 * Allocate one if not exists.
 *
 * @param region  the region
 */
static inline region_attr *get_region_attr(void *region) {
	region_attr r_attr, *res;
	r_attr.reg = region;

	res = set_find(region_attr_set, &r_attr, sizeof(r_attr), attr_set_hash(&r_attr));

	if (res == NULL) {
		r_attr.in_array = NEW_ARR_F(void *, 0);
		if (is_ir_loop(region))
			r_attr.op_array = NEW_ARR_F(void *, 0);
		else
			r_attr.op_array = NULL;
		r_attr.n_outs     = 0;
		r_attr.n_exc_outs = 0;
		res = set_insert(region_attr_set, &r_attr, sizeof(r_attr), attr_set_hash(&r_attr));
	}

	return res;
}

int get_region_n_ins(void *region) {
	return ARR_LEN(get_region_attr(region)->in_array);
}

void *get_region_in(void *region, int pos) {
	assert(0 <= pos && pos < get_region_n_ins(region));
	return ((get_region_attr(region)->in_array)[pos]);
}

void add_region_in(void *region, void *in) {
	ARR_APP1(void *, get_region_attr(region)->in_array, in);
	get_region_attr(in)->n_outs++;
}

int get_region_n_outs(void *region) {
	return get_region_attr(region)->n_outs;
}

int get_region_n_exc_outs(void *region) {
	return get_region_attr(region)->n_exc_outs;
}

void inc_region_n_exc_outs(void *region) {
	(get_region_attr(region)->n_exc_outs)++;
}

void *get_loop_cfop(void *region, int pos) {
	assert(0 <= pos && pos < get_region_n_ins(region));
	return ((get_region_attr(region)->op_array)[pos]);
}

/** Add a control flow op to a loop region. */
static inline void add_loop_cfop(void *region, void *cfop) {
	assert(cfop);
	ARR_APP1(void *, get_region_attr(region)->op_array, cfop);
}

/**
 * Increase the number of exception outputs if a control flow
 * operation (that is inside the given region) is a fragile operation.
 *
 * @param reg   a region
 * @param cfop  a control flow operation leaving this region
 */
static inline void exc_outs(void *reg, ir_node *cfop) {
	if (is_fragile_op(cfop) || is_fragile_Proj(cfop))
		inc_region_n_exc_outs(reg);
}

/*------------------------------------------------------------------*/
/* Algorithm to construct the interval edges based on a loop tree. */
/* Walk a loop and add all edges.  Walk inner loops by recursion. */
/*------------------------------------------------------------------*/

/**
 * Check if the loop of a given block is the outer loop of the current one.
 * If yes, add an edge from the block to the region of the current loop.
 *
 * @param inner  the current (possible inner) loop
 * @param outer  the loop of blk
 * @param blk    a block
 * @param cfop   the control flow op leaving blk
 *
 * @return  non-zero if outer can be reached from inner via the outer loop relation
 */
static int find_outer_loop(ir_loop *inner, ir_loop *outer, ir_node *blk, ir_node *cfop) {
	if (get_loop_outer_loop(inner) == outer) {
		add_region_in(inner, blk);
		add_loop_cfop(inner, cfop);
		exc_outs(blk, cfop);
		return 1;
	}
	return 0;
}

/**
 * Check if a given block can be found in a given loop
 * or its nesting loops.
 *
 * @param blk   a block
 * @param loop  a loop
 */
static int test_loop_nest(ir_node *blk, ir_loop *loop) {
	int i, n_elems = get_loop_n_elements(loop);

	for (i = 0; i < n_elems; ++i) {
		loop_element e = get_loop_element(loop, i);
		switch (*e.kind) {
		case k_ir_node:
			if (e.node == blk)
				return 1;
			break;
		case k_ir_loop:
			if (test_loop_nest(blk, e.son))
				return 1;
			break;
		default:
			break;
		}
	}
	return 0;
}

/**
 * Check if pred is a block from an inner loop jumping via cfop to the block blk.
 * If yes, add an edge from pred's loop to the region blk.

 * @param blk   a block
 * @param l     the loop of blk
 * @param pred  a predecessor block of blk
 * @param cfop  the control flow op from pred to blk
 *
 * @return non-zero if pred is from an inner loop
 */
static int find_inner_loop(ir_node *blk, ir_loop *l, ir_node *pred, ir_node *cfop) {
	int i, n_elems = get_loop_n_elements(l);
	int found = 0;

	for (i = 0; i < n_elems; ++i) {
		loop_element e = get_loop_element(l, i);
		switch (*e.kind) {
		case k_ir_node:
			if (e.node == blk) {
				/* stop the search if we reach blk, pred cannot be found
				   later in the loop */
				return 0;
			}
			break;
		case k_ir_loop:
			found = test_loop_nest(pred, e.son);
			if (found) {
				add_region_in(blk, e.son);
				exc_outs(e.son, cfop);
				return found;
			}
			break;
		default:
			break;
		}
	}
	return found;
}

/**
 * Check if a predecessor block pred_b is from a previous loop of the
 * block b.
 *
 * @param l       the loop of the block b
 * @param pred_l  the loop of the block pred_b
 * @param b       the block
 * @param pred_b  the predecessor block
 * @param cfop    the control flow node leaving pred_b for b
 *
 * @return non-zero if pred is from an previous loop
 */
static int find_previous_loop(ir_loop *l, ir_loop *pred_l, ir_node *b,
                              ir_node *pred_b, ir_node *cfop)
{
	ir_loop *outer = get_loop_outer_loop(l);
	int found, i;
	int l_pos = get_loop_element_pos(outer, l);
	(void) pred_l;
	assert(l_pos > -1);
	assert(l_pos > 0 && "Is this a necessary condition?  There could be a perfect nest ...");

	for (i = l_pos -1, found = 0; i > -1 && !found; --i) {
		ir_loop *k = get_loop_element(outer, i).son;
		if (is_ir_loop(k)) {
			found = test_loop_nest(pred_b, k);
			if (found) {
				add_region_in(l, k);
				exc_outs(k, cfop);
				add_loop_cfop(l, cfop);
				/* placeholder: the edge is in the loop region */
				add_region_in(b, NULL);
			}
		}
	}

	return found;
}


/**
 * Compute the edges for the interval graph.
 *
 * @param blk  The block for which to construct the edges.
 * @param l    The loop of blk.
 *
 * There are four cases:
 * - A predecessor block is in the same loop.  Add a normal block-block edge.
 * - A predecessor block is in a loop contained in this loop, somewhere down in
 *   the nesting.  The predecessor of this block is the outermost loop of the nest
 *   directly contained in l.
 * - A predecessor block is in the outer loop of l.  l gets an edge to the predecessor block.
 * - The outer loop of l contains another loop k just before l.  The control flow
 *   branches directly from loop k to loop l.  Add an edge l->k.  Watch it: k must
 *   not be a direct predecessor of l in the loop tree!
 */
static void construct_interval_block(ir_node *blk, ir_loop *l) {
	int i, n_cfgpreds;

	if (blk == get_irg_start_block(current_ir_graph))
		return;

	n_cfgpreds = get_Block_n_cfgpreds(blk);
	/* We want nice blocks. */
	assert(n_cfgpreds > 0);

	for (i = 0; i < n_cfgpreds; ++i) {
		ir_node *cfop, *pred;
		ir_loop *pred_l;

		if (is_backedge(blk, i)) {
			if (blk != get_loop_element(l, 0).node) {
				DB((dbg, LEVEL_1, "Loophead not at loop position 0. %+F\n", blk));
			}
			/* There are no backedges in the interval decomposition. */
			add_region_in(blk, NULL);
			continue;
		}

		cfop = get_Block_cfgpred(blk, i);
		if (is_Proj(cfop)) {
			ir_node *op = skip_Proj(cfop);
			if (is_fragile_op(op) && get_Proj_proj(cfop) == pn_Generic_X_except) {
				/*
				 * Skip the Proj for the exception flow only, leave the
				 * not exception flow Proj's intact.
				 * If the old semantic is used (only one exception Proj) this
				 * should lead to the same representation as before.
				 */
				cfop = op;
			} else {
				assert(get_nodes_block(cfop) == get_nodes_block(skip_Proj(cfop)));
			}
		}

		pred = skip_Proj(get_nodes_block(cfop));
		/* We want nice blocks. */
		assert(!is_Bad(pred) && !is_Bad(skip_Proj(get_Block_cfgpred(blk, i))));
		pred_l = get_irn_loop(pred);
		if (pred_l == l) {
			/* first case: both blocks are in the same loop */
			add_region_in(blk, pred);
			exc_outs(pred, cfop);
		} else {
			/* check for the second case: pred is from an inner loop */
			int found = find_inner_loop(blk, l, pred, cfop);
			if (!found) {
				if (blk != get_loop_element(l, 0).node) {
					DB((dbg, LEVEL_1, "Loop entry not at loop position 0. %+F\n", blk));
				}
				/* check for the third case: pred is in an outer loop */
				found = find_outer_loop(l, pred_l, pred, cfop);
				if (found) {
					  /* placeholder: the edge is added to the loop region */
					add_region_in(blk, NULL);
				} else {
					/* fourth case: pred is from the previous loop */
					found = find_previous_loop(l, pred_l, blk, pred, cfop);

					assert(found && "decomposition failed");
				}
			}
		}

#ifdef DEBUG_libfirm
		if (blk != get_loop_element(l, 0).node) {
			/* Check for improper region. But these can happen, so what? */
			if (has_backedges(blk)) {
				DB((dbg, LEVEL_1, "Improper Region %+F\n", blk));
			}
		}
#endif
	}
}

/**
 * Construct interval edges for a given (control flow) loop.
 *
 * @param l  the cf loop
 */
static void construct_interval_edges(ir_loop *l) {
	int i, n_elems = get_loop_n_elements(l);
	for (i = 0; i < n_elems; ++i) {
		loop_element e = get_loop_element(l, i);
		switch (*e.kind) {
		case k_ir_node:
			construct_interval_block(e.node, l);
			break;
		case k_ir_loop:
			construct_interval_edges(e.son);
			break;
		default:
			break;
		}
	}
}

void construct_intervals(ir_graph *irg) {
	ir_loop  *l;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.ana.interval");

	if (region_attr_set == NULL)
		region_attr_set = new_set(region_attr_cmp, 256);

	construct_cf_backedges(current_ir_graph);

	l = get_irg_loop(current_ir_graph);

	construct_interval_edges(l);

	current_ir_graph = rem;
}

void free_intervals(void) {
	region_attr *res;

	if (region_attr_set == NULL)
		return;

	for (res = set_first(region_attr_set);
	     res != NULL;
	     res = set_next(region_attr_set)) {
		DEL_ARR_F(res->in_array);
		if (res->op_array != NULL)
			DEL_ARR_F(res->op_array);
	}

	del_set(region_attr_set);
	region_attr_set = NULL;
}

/*------------------------------------------------------------------*/
/* A vcg dumper showing an interval decomposition of a cfg.         */
/*                                                                  */
/*------------------------------------------------------------------*/

void dump_region_edges(FILE *F, void *reg) {
	int i, n_ins = get_region_n_ins(reg);

	if (is_ir_node(reg)) {
		ir_node *irn = reg;
		if (get_Block_n_cfgpreds(irn) > get_region_n_ins(reg)) {
			for (i = n_ins; i < get_Block_n_cfgpreds(irn); ++i) {
				if (is_backedge(irn, i))
					fprintf(F, "backedge: { sourcename: \"");
				else
					fprintf(F, "edge: { sourcename: \"");
				PRINT_NODEID(irn);
				fprintf(F, "\" targetname: \"");
				PRINT_NODEID(get_nodes_block(skip_Proj(get_Block_cfgpred(irn, i))));
				fprintf(F, "\" " BLOCK_EDGE_ATTR "}\n");
			}
		}
	}

	for (i = 0; i < n_ins; ++i) {
		void *target = get_region_in(reg, i);

		if (is_ir_node(reg)) {
			ir_node *irn = reg;
			if (get_Block_n_cfgpreds(irn) != get_region_n_ins(reg)) {
				ir_printf("n_cfgpreds = %d, n_ins = %d\n %+F\n", get_Block_n_cfgpreds(irn), get_region_n_ins(reg), irn);
			}
		}

		if ((!target || (is_ir_node(reg) && !is_ir_node(target))) && i < get_Block_n_cfgpreds((ir_node *)reg)) {
			assert(is_ir_node(reg));
			if (is_backedge((ir_node *)reg, i))
				fprintf(F, "backedge: { sourcename: \"");
			else
				fprintf(F, "edge: { sourcename: \"");
			PRINT_NODEID(((ir_node *)reg));
			fprintf(F, "\" targetname: \"");
			PRINT_NODEID(get_nodes_block(skip_Proj(get_Block_cfgpred((ir_node *)reg, i))));
			fprintf(F, "\" " BLOCK_EDGE_ATTR "}\n");

			if (!target) continue;
		}

		fprintf(F, "edge: { sourcename: \"");
		if (is_ir_node(reg)) {
			PRINT_NODEID(((ir_node *)reg));
		} else {
			PRINT_LOOPID(((ir_loop *)reg));
		}
		fprintf(F, "\" targetname: \"");
		if (is_ir_node(target)) {
			PRINT_NODEID(((ir_node *)target));
		} else {
			PRINT_LOOPID(((ir_loop *)target));
		}
		fprintf(F, "\"");
		if (is_ir_node(reg) && is_fragile_op(skip_Proj(get_Block_cfgpred(reg, i))))
			fprintf(F, EXC_CF_EDGE_ATTR);
		fprintf(F, "}\n");
	}
}

#include "execution_frequency.h"

static void dump_interval_block(FILE *F, ir_node *block) {
	int i, fl;
	/* This is a block. Dump a node for the block. */
	fprintf(F, "node: {title: \""); PRINT_NODEID(block);
	fprintf(F, "\" label: \"");
	if (block == get_irg_start_block(get_irn_irg(block)))
		fprintf(F, "Start ");
	if (block == get_irg_end_block(get_irn_irg(block)))
		fprintf(F, "End ");

	fprintf(F, "%s ", get_op_name(get_irn_op(block)));
	PRINT_NODEID(block);
	fprintf(F, " freq: %9.4lf", get_region_exec_freq(block));
	fprintf(F, " n_outs: %d", get_region_n_outs(block));
	fprintf(F, " n_exc_outs: %d", get_region_n_exc_outs(block));
	fprintf(F, "\" ");
	fprintf(F, "info1:\"");
	if (dump_dominator_information_flag)
		fprintf(F, "dom depth %d\n", get_Block_dom_depth(block));

	/* show arity and possible Bad predecessors of the block */
	fprintf(F, "arity: %d\n", get_Block_n_cfgpreds(block));
	for (fl = i = 0; i < get_Block_n_cfgpreds(block); ++i) {
		ir_node *pred = get_Block_cfgpred(block, i);
		if (is_Bad(pred)) {
			if (! fl)
				fprintf(F, "Bad pred at pos: ");
			fprintf(F, "%d ", i);
			fl = 1;
		}
	}
	if (fl)
		fprintf(F, "\n");

	fprintf(F, "\"");  /* closing quote of info */

	if ((block == get_irg_start_block(get_irn_irg(block))) ||
		(block == get_irg_end_block(get_irn_irg(block)))     )
		fprintf(F, " color:blue ");
	else if (fl)
		fprintf(F, " color:yellow ");

	fprintf(F, "}\n");
}

static void dump_interval_loop(FILE *F, ir_loop *l) {
	int i, n_elems = get_loop_n_elements(l);

	fprintf(F, "graph: { title: \"");
	PRINT_LOOPID(l);
	fprintf(F, "\" label: \"loop %d", get_loop_loop_nr(l));
	fprintf(F, " freq: %9.4lf", get_region_exec_freq(l));
	fprintf(F, " n_outs: %d", get_region_n_outs(l));
	fprintf(F, " n_exc_outs: %d", get_region_n_exc_outs(l));
	fprintf(F, "\" status:clustered color:white \n");

	for (i = 0; i < n_elems; ++i) {
		loop_element e = get_loop_element(l, i);
		dump_region_edges(F, e.node);
		switch (*e.kind) {
		case k_ir_node:
			dump_interval_block(F, e.node);
			break;
		case k_ir_loop:
			dump_interval_loop(F, e.son);
			break;
		default:
			break;
		}
	}

	fprintf(F, "}\n\n");
}


void dump_interval_graph(ir_graph *irg, const char *suffix) {
	FILE     *f;
	ir_graph *rem;

	if (!is_filtered_dump_name(get_entity_ident(get_irg_entity(irg))))
		return;

	f = vcg_open(irg, suffix, "-intervals");
	dump_vcg_header(f, get_irg_dump_name(irg), NULL, NULL);

	rem              = current_ir_graph;
	current_ir_graph = irg;

	dump_interval_loop(f, get_irg_loop(irg));

	dump_vcg_footer(f);
	fclose(f);

	current_ir_graph = rem;
}
