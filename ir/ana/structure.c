/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Structure Analysis
 * @author   Michael Beck
 * @date     5.4.2007
 * @version  $Id$
 */
#include "config.h"

#include <stdbool.h>

#include "firm_common.h"
#include "irnode_t.h"
#include "structure.h"
#include "irdom.h"
#include "irouts.h"
#include "irtools.h"
#include "irgwalk.h"
#include "array.h"
#include "irdump.h"

#include "debug.h"

typedef union ir_reg_or_blk ir_reg_or_blk;

/* The structure tree. */
struct ir_reg_tree {
	struct obstack obst;   /**< The obstack where the data is allocated. */
	ir_region *top;        /**< The top region. */
	ir_graph *irg;         /**< Associated graph. */
};

/* A region. */
struct ir_region {
	firm_kind      kind;      /**< Must be k_ir_region. */
	ir_region_kind type;      /**< The type of this region. */
	ir_region      *parent;   /**< points to the parent. */
	ir_reg_or_blk  *parts;    /**< The list of all region parts. */
	ir_region      **pred;    /**< The predecessor (control flow) regions of this region. */
	ir_region      **succ;    /**< The successor (control flow) regions of this region. */
	size_t         prenum;    /**< DFS pre-oder number */
	size_t         postnum;   /**< DFS post-oder number */
	void           *link;     /**< A link field. */
	unsigned long  nr;        /**< for debugging */
	unsigned       visited:1; /**< The visited flag. */
	unsigned       exit:1;    /**< If set, the parent region can be left by this node. */
	unsigned       enter:1;   /**< If set, the parent region can be entered by this node. */
};

/* A helper type for unioning blocks and regions. */
union ir_reg_or_blk {
	firm_kind *kind;    /**< For easier check. */
	ir_node   *blk;     /**< A node */
	ir_region *region;  /**< A region. */
};

/* The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Returns the link of a region.
 */
void *get_region_link(const ir_region *reg)
{
	return reg->link;
}

/**
 * Sets the link of a region.
 */
void set_region_link(ir_region *reg, void *data)
{
	reg->link = data;
}

/**
 * Get the immediate region of a block.
 */
ir_region *get_block_region(const ir_node *block)
{
	assert(is_Block(block));
	return block->attr.block.region;
}

/**
 * Sets the immediate region of a block.
 */
void set_block_region(ir_node *block, ir_region *reg)
{
	assert(is_Block(block));
	block->attr.block.region = reg;
}

/**
 * Get the immediate region of a node.
 */
ir_region *get_irn_region(ir_node *n)
{
	if (!is_Block(n))
		n = get_nodes_block(n);
	return get_block_region(n);
}

/**
 * Return non-zero if a given firm thing is a region.
 */
int is_region(const void *thing)
{
	const firm_kind *kind = (const firm_kind*) thing;
	return *kind == k_ir_region;
}

/**
 * Return the number of predecessors of a region.
 */
size_t get_region_n_preds(const ir_region *reg)
{
	return ARR_LEN(reg->pred);
}

/**
 * Return the predecessor region at position pos.
 */
ir_region *get_region_pred(const ir_region *reg, size_t pos)
{
	assert(pos <= get_region_n_preds(reg));
	return reg->pred[pos];
}

/**
 * Set the predecessor region at position pos.
 */
void set_region_pred(ir_region *reg, size_t pos, ir_region *n)
{
	assert(pos <= get_region_n_preds(reg));
	reg->pred[pos] = n;
}

/**
 * Return the number of successors in a region.
 */
size_t get_region_n_succs(const ir_region *reg)
{
	return ARR_LEN(reg->succ);
}

/**
 * Return the successor region at position pos.
 */
ir_region *get_region_succ(const ir_region *reg, size_t pos)
{
	assert(pos <= get_region_n_succs(reg));
	return reg->succ[pos];
}

/**
 * Set the successor region at position pos.
 */
void set_region_succ(ir_region *reg, size_t pos, ir_region *n)
{
	assert(pos <= get_region_n_succs(reg));
	reg->succ[pos] = n;
}

/* ----------------------- construction -------------------------- */

/** Walker environment. */
typedef struct walk_env {
	struct obstack *obst;  /**< An obstack to allocate from. */
	ir_region **post;      /**< The list of all currently existent top regions. */
	size_t l_post;         /**< The length of the allocated regions array. */
	size_t premax;         /**< maximum pre counter */
	size_t postmax;        /**< maximum post counter */
	ir_node *start_block;  /**< The start block of the graph. */
	ir_node *end_block;    /**< The end block of the graph. */
} walk_env;

/**
 * Do a DFS search on the initial regions, assign a prenum and a postnum to every
 * node and store the region nodes into the post array.
 */
static void dfs_walk2(ir_region *reg, walk_env *env)
{
	size_t i, n;

	if (reg->visited == 0) {
		reg->visited = 1;

		reg->prenum = env->premax++;
		for (i = 0, n = get_region_n_succs(reg); i < n; ++i) {
			/* recursion */
			ir_region *succ = get_region_succ(reg, i);
			dfs_walk2(succ, env);
		}

		env->post[env->postmax] = reg;
		reg->postnum = env->postmax++;
	}
}

/**
 * Do a DFS search on the initial regions, assign a prenum and a postnum to every
 * node and store the region nodes into the post array.
 */
static void dfs_walk(ir_graph *irg, walk_env *env)
{
	ir_graph *rem = current_ir_graph;
	ir_region *reg;

	current_ir_graph = irg;
	reg              = (ir_region*) get_irn_link(get_irg_start_block(irg));

	env->premax  = 0;
	env->postmax = 0;
	dfs_walk2(reg, env);
	current_ir_graph = rem;
}

/**
 * Post-walker: wrap all blocks with a BasicBlock region
 * and count them
 */
static void wrap_BasicBlocks(ir_node *block, void *ctx)
{
	walk_env *env = (walk_env*) ctx;
	ir_region *reg;

	/* Allocate a Block wrapper */
	reg          = OALLOC(env->obst, ir_region);
	reg->kind    = k_ir_region;
	reg->type    = ir_rk_BasicBlock;
	reg->parent  = NULL;
	reg->prenum  = 0;
	reg->postnum = 0;
	reg->visited = 0;
	reg->exit    = 0;
	reg->enter   = 0;
	reg->link    = NULL;
	reg->nr      = get_irn_node_nr(block);
	reg->parts   = NEW_ARR_D(ir_reg_or_blk, env->obst, 1);

	reg->parts[0].blk = block;
	set_irn_link(block, reg);

	++env->l_post;
}  /* wrap_BasicBlocks */

/**
 * Post-walker: Create the pred and succ edges for Block wrapper.
 * Kill edges to the Start and End blocks.
 */
static void update_BasicBlock_regions(ir_node *blk, void *ctx)
{
	walk_env *env = (walk_env*) ctx;
	ir_region *reg = (ir_region*) get_irn_link(blk);
	int i, len;
	size_t j;

	if (blk == env->start_block) {
		/* handle Firm's self loop: Start block has no predecessors */
		reg->pred = NEW_ARR_D(ir_region *, env->obst, 0);
	} else {
		len = get_Block_n_cfgpreds(blk);
		reg->pred = NEW_ARR_D(ir_region *, env->obst, len);
		for (j = i = 0; i < len; ++i) {
			ir_node *pred = get_Block_cfgpred_block(blk, i);
			reg->pred[j++] = (ir_region*) get_irn_link(pred);
		}
		ARR_SHRINKLEN(reg->pred, j);
	}

	len = get_Block_n_cfg_outs(blk);
	reg->succ = NEW_ARR_D(ir_region *, env->obst, len);
	for (j = i = 0; i < len; ++i) {
		ir_node *succ = get_Block_cfg_out(blk, i);
		reg->succ[j++] = (ir_region*) get_irn_link(succ);
	}
	ARR_SHRINKLEN(reg->succ, j);
}  /* update_BasicBlock_regions */

/** Allocate a new region on an obstack */
static ir_region *alloc_region(struct obstack *obst, ir_region_kind type)
{
	ir_region *reg = OALLOC(obst, ir_region);
	reg->kind    = k_ir_region;
	reg->type    = type;
	reg->parent  = NULL;
	reg->prenum  = 0;
	reg->postnum = 0;
	reg->visited = 0;
	reg->exit    = 0;
	reg->enter   = 0;
	reg->link    = NULL;

	return reg;
}  /* alloc_region */

/**
 * Creates a new Sequence region.
 */
static ir_region *new_Sequence(struct obstack *obst, ir_region *nset, size_t nset_len)
{
	ir_region *reg  = alloc_region(obst, ir_rk_Sequence);
	ir_region *next;
	size_t    i;

	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, nset_len);

	/* beware: list is in reverse order, reverse */
	next = nset;
	for (i = nset_len; i > 0;) {
		--i;
		nset = next;
		reg->parts[i].region = nset;
		nset->parent = reg;
		next = (ir_region*) nset->link;
		nset->link = NULL;
	}

	reg->nr   = reg->parts[0].region->nr;
	reg->pred = DUP_ARR_D(ir_region *, obst, reg->parts[0].region->pred);
	reg->succ = DUP_ARR_D(ir_region *, obst, reg->parts[nset_len - 1].region->succ);

	DEBUG_ONLY(
		DB((dbg, LEVEL_2, " Created Sequence "));
		for (i = 0; i < nset_len; ++i) {
			DB((dbg, LEVEL_2, "(%lu)", reg->parts[i].region->nr));
		}
		DB((dbg, LEVEL_2, "\n"));
	)
	return reg;
}  /* new_Sequence */

/**
 * Create a new IfThenElse region.
 */
static ir_region *new_IfThenElse(struct obstack *obst, ir_region *if_b, ir_region *then_b, ir_region *else_b)
{
	ir_region *reg = alloc_region(obst, ir_rk_IfThenElse);

	reg->nr    = if_b->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 3);

	reg->parts[0].region = if_b;   if_b->parent   = reg;
	reg->parts[1].region = then_b; then_b->parent = reg;
	reg->parts[2].region = else_b; else_b->parent = reg;

	reg->pred = DUP_ARR_D(ir_region *, obst, if_b->pred);
	reg->succ = DUP_ARR_D(ir_region *, obst, then_b->succ);

	DB((dbg, LEVEL_2, " Created If(%lu)Then(%lu)Else(%lu)\n", reg->nr, then_b->nr, else_b->nr));

	return reg;
}  /* new_IfThenElse */

/**
 * Create a new IfThen region.
 */
static ir_region *new_IfThen(struct obstack *obst, ir_region *if_b, ir_region *then_b)
{
	ir_region *reg = alloc_region(obst, ir_rk_IfThen);

	reg->nr    = if_b->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 2);

	reg->parts[0].region = if_b;   if_b->parent   = reg;
	reg->parts[1].region = then_b; then_b->parent = reg;

	reg->pred = DUP_ARR_D(ir_region *, obst, if_b->pred);
	reg->succ = DUP_ARR_D(ir_region *, obst, then_b->succ);

	DB((dbg, LEVEL_2, " Created If(%lu)Then(%lu)\n", reg->nr, then_b->nr));

	return reg;
}  /* new_IfThenElse */

/**
 * Create a new Switch/case region.
 */
static ir_region *new_SwitchCase(struct obstack *obst, ir_region_kind type, ir_region *head, ir_region *exit,
                                 ir_region *cases, size_t cases_len)
{
	ir_region *reg = alloc_region(obst, type);
	ir_region *c, *n;
	int       add = 1;
	size_t    i;

	/* check, if the exit block is in the list */
	for (c = cases; c != NULL; c = (ir_region*) c->link) {
		if (c == exit) {
			add = 0;
			break;
		}
	}

	reg->nr    = head->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, cases_len + add);

	reg->parts[0].region = head; head->parent = reg;
	i = 1;
	for (c = cases; c != NULL; c = n) {
		n = (ir_region*) c->link;
		if (c != exit) {
			reg->parts[i++].region = c;
			c->parent = reg;
		}
		c->link = NULL;
	}

	reg->pred = DUP_ARR_D(ir_region *, obst, head->pred);
	reg->succ = NEW_ARR_D(ir_region *, obst, 1);
	reg->succ[0] = exit;

	DEBUG_ONLY({
		DB((dbg, LEVEL_2, " Created %s(%u)\n", reg->type == ir_rk_Switch ? "Switch" : "Case", reg->nr));
		for (i = 1; i < ARR_LEN(reg->parts); ++i) {
			DB((dbg, LEVEL_2, "  Case(%lu)\n", reg->parts[i].region->nr));
		}
		DB((dbg, LEVEL_2, "  Exit(%lu)\n", exit->nr));
	})
	return reg;
}  /* new_SwitchCase */

/**
 * Create a new SelfLoop region.
 */
static ir_region *new_SelfLoop(struct obstack *obst, ir_region *head)
{
	ir_region *reg = alloc_region(obst, ir_rk_SelfLoop);
	ir_region *succ;
	size_t i, j, len;

	reg->nr    = head->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 1);

	reg->parts[0].region = head; head->parent = reg;

	len = ARR_LEN(head->pred);
	reg->pred = NEW_ARR_D(ir_region *, obst, len - 1);
	for (i = j = 0; i < len; ++i) {
		ir_region *pred = get_region_pred(head, i);
		if (pred != head)
			reg->pred[j++] = pred;
	}
	assert(j == len - 1);

	reg->succ = NEW_ARR_D(ir_region *, obst, 1);
	assert(ARR_LEN(head->succ) == 2);

	succ = get_region_succ(head, 0);
	if (succ != head)
		reg->succ[0] = succ;
	else
		reg->succ[0] = get_region_succ(head, 1);

	DB((dbg, LEVEL_2, " Created SelfLoop(%lu)\n", reg->nr));

	return reg;
}  /* new_SelfLoop */

/**
 * Create a new RepeatLoop region.
 */
static ir_region *new_RepeatLoop(struct obstack *obst, ir_region *head, ir_region *body)
{
	ir_region *reg = alloc_region(obst, ir_rk_RepeatLoop);
	ir_region *succ;

	reg->nr    = head->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 2);

	reg->parts[0].region = head; head->parent = reg;
	reg->parts[1].region = body; body->parent = reg;

	reg->pred = DUP_ARR_D(ir_region *, obst, head->pred);
	reg->succ = NEW_ARR_D(ir_region *, obst, 1);
	assert(ARR_LEN(body->succ) == 2);

	succ = get_region_succ(body, 0);
	if (succ != head)
		reg->succ[0] = succ;
	else
		reg->succ[0] = get_region_succ(body, 1);

	DB((dbg, LEVEL_2, " Created RepeatLoop(%lu)Body(%lu)\n", reg->nr, body->nr));

	return reg;
}  /* new_RepeatLoop */

/**
 * Create a new WhileLoop region.
 */
static ir_region *new_WhileLoop(struct obstack *obst, ir_region *head)
{
	ir_region *reg  = alloc_region(obst, ir_rk_WhileLoop);
	ir_region *body = (ir_region*) head->link;
	ir_region *succ;
	size_t i, j, len;

	head->link = NULL;

	reg->nr    = head->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 2);

	reg->parts[0].region = head; head->parent = reg;
	reg->parts[1].region = body; body->parent = reg;

	len = ARR_LEN(head->pred);
	reg->pred = NEW_ARR_D(ir_region *, obst, len - 1);
	for (i = j = 0; i < len; ++i) {
		ir_region *pred = get_region_pred(head, i);
		if (pred != body)
			reg->pred[j++] = pred;
	}
	assert(j == len - 1);

	reg->succ = NEW_ARR_D(ir_region *, obst, 1);
	assert(ARR_LEN(head->succ) == 2);

	succ = get_region_succ(head, 0);
	if (succ != body)
		reg->succ[0] = succ;
	else
		reg->succ[0] = get_region_succ(head, 1);

	DB((dbg, LEVEL_2, " Created WhileLoop(%lu)Body(%lu)\n", reg->nr, body->nr));

	return reg;
}  /* new_WhileLoop */

/**
 * Create a new new_NaturalLoop region.
 */
static ir_region *new_NaturalLoop(struct obstack *obst, ir_region *head)
{
	ir_region *reg = alloc_region(obst, ir_rk_WhileLoop);
	ir_region *c, *n;
	size_t i, j, k, len, n_pred, n_succ;

	/* count number of parts */
	for (len = 0, c = head; c != NULL; c = (ir_region*) c->link)
		++len;

	reg->nr    = head->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, len);

	/* enter all parts */
	for (i = 0, c = head; c != NULL; c = n) {
		reg->parts[i++].region = c;
		c->parent = reg;
		n = (ir_region*) c->link;
		c->link = NULL;
	}

	/* count number of preds */
	n_pred = 0;
	for (i = get_region_n_preds(head); i > 0;) {
		ir_region *pred = get_region_pred(head, --i);
		if (pred->parent != reg)
			++n_pred;
	}
	reg->pred = NEW_ARR_D(ir_region *, obst, n_pred);
	for (j = 0, i = get_region_n_preds(head); i > 0;) {
		ir_region *pred = get_region_pred(head, --i);
		if (pred->parent != reg)
			reg->pred[j++] = pred;
	}

	/* count number of succs */
	n_succ = 0;
	for (j = 0; j < len; ++j) {
		ir_region *pc = reg->parts[j].region;
		for (i = get_region_n_succs(pc); i > 0;) {
			ir_region *succ = get_region_succ(pc, --i);
			if (succ->parent != reg)
				++n_succ;
		}
	}
	reg->succ = NEW_ARR_D(ir_region *, obst, n_succ);
	k = 0;
	for (j = 0; j < len; ++j) {
		ir_region *pc = reg->parts[j].region;
		for (i = get_region_n_succs(pc); i > 0;) {
			ir_region *succ = get_region_succ(pc, --i);
			if (succ->parent != reg)
				reg->succ[k++] = succ;
		}
	}

	DEBUG_ONLY(
		DB((dbg, LEVEL_2, " Created NaturalLoop(%u)Head(%u)\n", reg->nr, head->nr));
		for (i = 1; i < len; ++i) {
			ir_region *p = reg->parts[i].region;
			DB((dbg, LEVEL_2, "  Body(%u)\n", p->nr));
		}
	)
	return reg;
}  /* new_NaturalLoop */

/**
 * Return true if region a is an ancestor of region b in DFS search.
 */
static int is_ancestor(const ir_region *a, const ir_region *b)
{
	return (a->prenum <= b->prenum && a->postnum > b->postnum);
}

/**
 * Return true if region pred is a predecessor of region n.
 */
static bool pred_of(const ir_region *pred, const ir_region *n)
{
	size_t i, n_preds;
	for (i = 0, n_preds = get_region_n_preds(n); i < n_preds; ++i) {
		if (get_region_pred(n, i) == pred)
			return true;
	}
	return false;
}

/**
 * Return true if region succ is a successor of region n.
 */
static bool succ_of(const ir_region *succ, const ir_region *n)
{
	size_t i, n_succs;
	for (i = 0, n_succs = get_region_n_succs(n); i < n_succs; ++i) {
		if (get_region_succ(n, i) == succ)
			return true;
	}
	return false;
}

/**
 * Reverse a linked list of regions.
 */
static struct ir_region *reverse_list(ir_region *n)
{
	ir_region *prev = NULL, *next;

	for (; n; n = next) {
		next = (ir_region*) n->link;
		n->link = prev;
		prev = n;
	}
	return prev;
}

/**
 * Find the cyclic region in the subgraph entered by node.
 */
static ir_region *find_cyclic_region(ir_region *node)
{
	size_t i;
	ir_region *last = node;
	int improper = 0;

	for (i = get_region_n_preds(node); i > 0;) {
		ir_region *pred = get_region_pred(node, --i);

		/* search backedges */
		if (!pred->link && pred != last && is_ancestor(node, pred)) {
			ir_region *rem = last;
			size_t j;

			last->link = pred;
			last       = pred;
			for (j = get_region_n_preds(pred); j > 0;) {
				ir_region *p = get_region_pred(pred, --j);

				/* Search regions we didn't visited yet and
				   link them into the list. */
				if (!p->link && p != last) {
					if (is_ancestor(node, p)) {
						last->link = p;
						last       = p;
					} else {
						improper = 1;
					}
				}
			}
			/* reverse the list. */
			last = (ir_region*) rem->link;
			rem->link = reverse_list(last);
		}
	}

	if (node->link && improper) {
		/* found an improper region, do minimization */

	}
	return node;
}

/**
 * Detect a cyclic region.
 */
static ir_region *cyclic_region_type(struct obstack *obst, ir_region *node)
{
	ir_region *list, *next;

	/* simple cases first */
	if (succ_of(node, node)) {
		return new_SelfLoop(obst, node);
	}
	if (get_region_n_succs(node) == 1) {
		ir_region *succ = get_region_succ(node, 0);
		if (get_region_n_preds(succ) == 1 && succ_of(node, succ)) {
			return new_RepeatLoop(obst, node, succ);
		}
	}
	list = find_cyclic_region(node);

	next = (ir_region*) list->link;
	if (next) {
		if (!next->link && get_region_n_succs(next) == 1) {
			/* only one body block with only one successor (the head) */
			return new_WhileLoop(obst, list);
		}
		/* A Loop with one head */
		return new_NaturalLoop(obst, list);
	}

	return NULL;
}

/**
 * Clear all links on a list. Needed, because we expect cleared links.
 */
static void clear_list(ir_region *list)
{
	ir_region *next;

	for (next = list; next; list = next) {
		next = (ir_region*) list->link;
		list->link = NULL;
	}
}

#define ADD_LIST(list, n) do { n->link = list; list = n; ++list##_len; } while (0)

/**
 * Detect an acyclic region.
 */
static ir_region *acyclic_region_type(struct obstack *obst, ir_region *node)
{
	ir_region *n, *m;
	bool p, s;
	size_t k;
	ir_region *nset = NULL;
	size_t nset_len = 0;
	ir_region *res;

	/* check for a block containing node */
	n = node;
	p = get_region_n_preds(n) == 1;
	s = true;
	while (p & s) {
		n = get_region_pred(n, 0);
		p = get_region_n_preds(n) == 1;
		s = get_region_n_succs(n) == 1;
	}
	p = true;
	s = get_region_n_succs(n) == 1;
	while (p & s) {
		ADD_LIST(nset, n);
		n = get_region_succ(n, 0);
		p = get_region_n_preds(n) == 1;
		s = get_region_n_succs(n) == 1;
	}
	if (p) {
		ADD_LIST(nset, n);
	}
	if (nset_len > 1) {
		/* node --> .. --> .. */
		res = new_Sequence(obst, nset, nset_len);
		return res;
	}
	node = n;

	/* check for IfThenElse */
	k = get_region_n_succs(node);
	if (k == 2) {
		size_t n_succs, m_succs, n_preds, m_preds;

		n = get_region_succ(node, 0);
		m = get_region_succ(node, 1);

		n_succs = get_region_n_succs(n);
		m_succs = get_region_n_succs(m);
		n_preds = get_region_n_preds(n);
		m_preds = get_region_n_preds(m);
		if (n_succs == 1 && n_succs == m_succs && n_preds == m_preds &&
		    get_region_succ(n, 0) == get_region_succ(m, 0)) {
			/*
			 *    /-->n---\
			 * node      ...
			 *    \-->m---/
			 */
			return new_IfThenElse(obst, node, n, m);
		}
		if (n_succs == 1 &&
		    get_region_succ(n, 0) == m &&
		    pred_of(node, m)) {
			/*
			 * node -->n-->m
			 *    \-------/
			 */
			return new_IfThen(obst, node, n);
		}
		if (m_succs == 1 &&
		    get_region_succ(m, 0) == m &&
		    pred_of(node, n)) {
			/*
			 * node -->m-->n
			 *    \-------/
			 */
			return new_IfThen(obst, node, m);
		}
	}
	/* check for Switch, case */
	if (k > 0) {
		ir_region *rexit = NULL;
		size_t i, pos = 0;
		nset = NULL; nset_len = 0;
		for (i = k; i > 0;) {
			n = get_region_succ(node, i--);
			ADD_LIST(nset, n);
			if (get_region_n_succs(n) != 1) {
				/* must be the exit */
				rexit = n;
				++pos;
				if (pos > 1)
					break;
			}
		}
		if (pos <= 1) {
			ir_region_kind kind = ir_rk_Case;
			ir_region *pos_exit_1 = NULL;
			ir_region *pos_exit_2 = NULL;

			/* find the exit */
			for (m = nset; m != NULL; m = (ir_region*) m->link) {
				if (get_region_n_succs(m) != 1) {
					/* must be the exit block */
					if (rexit == NULL) {
						rexit = m;
					} else if (rexit != m) {
						/* two exits */
						rexit = NULL;
						break;
					}
				} else {
					ir_region *succ = get_region_succ(m, 0);

					if (succ->link == NULL) {
						if (rexit == NULL) {
							if (succ == pos_exit_1)
								rexit = succ;
							else if (succ == pos_exit_2)
								rexit = succ;
							else if (pos_exit_1 == NULL)
								pos_exit_1 = succ;
							else if (pos_exit_2 == NULL)
								pos_exit_2 = succ;
							else {
								/* more than two possible exits */
								break;
							}
						} else if (rexit != succ) {
							/* two exits */
							rexit = NULL;
							break;
						}
					}
				}
			}
			if (rexit != NULL) {
				/* do the checks */
				for (n = nset; n != NULL; n = (ir_region*) n->link) {
					ir_region *succ;
					if (n == rexit) {
						/* good, default fall through */
						continue;
					}
					succ = get_region_succ(n, 0);
					if (succ == rexit) {
						/* good, switch to exit */
						continue;
					}
					if (succ->link == NULL) {
						/* another exit */
						break;
					} else {
						/* a fall through */
						kind = ir_rk_Switch;
					}
				}

				if (n == NULL) {
					/* detected */
					return new_SwitchCase(obst, kind, node, rexit, nset, nset_len);
				}
			}
		}
		clear_list(nset);
	}
	return NULL;
}

/**
 * replace all pred edges from region pred that points to any of the set set
 * to ONE edge to reg.
 */
static void replace_pred(ir_region *succ, ir_region *reg)
{
	int    have_one = 0;
	size_t len      = get_region_n_preds(succ);
	size_t i;

	for (i = 0; i < len; ++i) {
		ir_region *pred = get_region_pred(succ, i);

		if (pred->parent == reg) {
			ir_region *r;

			if (have_one) {
				/* kill it */
				r = get_region_pred(succ, --len);
			} else {
				/* replace it */
				have_one = 1;
				r = reg;
			}
			set_region_pred(succ, i, r);
		} else {
			/* the current region can be entered by this node */
			pred->enter = 1;
		}
	}
	ARR_SHRINKLEN(succ->pred, len);
}

/**
 * replace all succ edges from region pred that points to any of the set set
 * to ONE edge to reg.
 */
static void replace_succ(ir_region *pred, ir_region *reg)
{
	int    have_one = 0;
	size_t len      = get_region_n_succs(pred);
	size_t i;

	for (i = 0; i < len; ++i) {
		ir_region *succ = get_region_succ(pred, i);

		if (succ->parent == reg) {
			ir_region *r;

			if (have_one) {
				/* kill it */
				r = get_region_succ(pred, --len);
			} else {
				/* replace it */
				have_one = 1;
				r = reg;
			}
			set_region_succ(pred, i, r);
		} else {
			/* current region can be left by this node */
			succ->exit = 1;
		}
	}
	ARR_SHRINKLEN(pred->succ, len);
}

/**
 * Reduce the graph by the node reg.
 */
static void reduce(walk_env *env, ir_region *reg)
{
	size_t i;
	ir_region *head = reg->parts[0].region;
	size_t maxorder = head->postnum;
	size_t minorder = head->prenum;

	/* second step: replace all preds in successors */
	for (i = get_region_n_succs(reg); i > 0;) {
		ir_region *succ = get_region_succ(reg, --i);

		replace_pred(succ, reg);
	}

	/* third step: replace all succs in predessors */
	for (i = get_region_n_preds(reg); i > 0;) {
		ir_region *pred = get_region_pred(reg, --i);

		replace_succ(pred, reg);
	}

	reg->prenum  = minorder;
	reg->postnum = maxorder;
	env->post[maxorder] = reg;
}

/**
 * Construct the region tree of a graph by doing
 * structural analysis.
 *
 * Uses link fields of nodes.
 *
 * @param irg  the graph
 */
ir_reg_tree *construct_region_tree(ir_graph *irg)
{
	walk_env env;
	ir_graph *rem = current_ir_graph;
	ir_reg_tree *res = XMALLOC(ir_reg_tree);

	obstack_init(&res->obst);

	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.ana.structure");
	firm_dbg_set_mask(dbg, SET_LEVEL_5);

	DB((dbg, LEVEL_1, "Structural analysis on %+F started ...\n", irg));

	/* we need dominance info */
	assure_doms(irg);
	/* and out edges */
	assure_irg_outs(irg);

	env.start_block = get_irg_start_block(irg);
	env.end_block   = get_irg_end_block(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* create the Block wrapper and count them */
	env.l_post = 0;
	env.obst   = &res->obst;
	irg_block_walk_graph(irg, NULL, wrap_BasicBlocks, &env);
	irg_block_walk_graph(irg, NULL, update_BasicBlock_regions, &env);

	env.post = NEW_ARR_F(ir_region *, env.l_post);

	/* do the DFS walk */
	dfs_walk(irg, &env);

	DB((dbg, LEVEL_1, "%zu regions left\n", env.postmax));
	if (env.postmax > 1) {
		size_t postctr = 0;
		do {
			ir_region *reg, *n = env.post[postctr];
			do {
				if (n->parent != NULL) {
					/* already folded */
					break;
				}
				/* locate an acyclic region if present */
				reg = acyclic_region_type(env.obst, n);
				if (reg == NULL) {
					/* locate a cyclic region */
					reg = cyclic_region_type(env.obst, n);
				}
				if (reg != NULL) {
					/* found a new region */
					reduce(&env, reg);
					n = reg;
				}
			} while (reg != NULL);
			++postctr;
		} while (postctr < env.postmax);
	}
	DB((dbg, LEVEL_1, "Structural analysis finished.\n"));

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	DEL_ARR_F(env.post);
	current_ir_graph = rem;

	res->top = env.post[0];
	res->irg = irg;
	return res;
}

/**
 * Walk over the region tree.
 *
 * @param reg   a region node
 * @param pre   walker function, executed before the children of a tree node are visited
 * @param post  walker function, executed after the children of a tree node are visited
 * @param env   environment, passed to pre and post
 */
static void region_tree_walk2(ir_region *reg, irg_reg_walk_func *pre, irg_reg_walk_func *post, void *env)
{
	size_t i, n;

	if (pre)
		pre(reg, env);
	if (reg->type != ir_rk_BasicBlock) {
		for (i = 0, n = ARR_LEN(reg->parts); i < n; ++i)
			region_tree_walk2(reg->parts[i].region, pre, post, env);
	}
	if (post)
		post(reg, env);
}

/**
 * Walk over the region tree.
 *
 * @param tree  the tree
 * @param pre   walker function, executed before the children of a tree node are visited
 * @param post  walker function, executed after the children of a tree node are visited
 * @param env   environment, passed to pre and post
 */
void region_tree_walk(ir_reg_tree *tree, irg_reg_walk_func *pre, irg_reg_walk_func *post, void *env)
{
	region_tree_walk2(tree->top, pre, post, env);
}
