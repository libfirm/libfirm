/*
 * Project:     libFIRM
 * File name:   ir/ana/structure.c
 * Purpose:     Structure Analysis
 * Author:      Michael Beck
 * Modified by:
 * Created:     5.4.2007
 * CVS-ID:      $Id: $
 * Copyright:   (c) 2007 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "firm_common.h"
#include "irnode_t.h"
#include "structure.h"
#include "irdom.h"
#include "irouts.h"
#include "irtools.h"
#include "irgwalk.h"
#include "array.h"
#include "irphase_t.h"

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
	firm_kind      kind;    /**< Must be k_ir_region. */
	ir_region_kind type;    /**< The type of this region. */
	ir_region      *parent; /**< points to the parent. */
	ir_reg_or_blk  *parts;  /**< The list of all region parts. */
	ir_region      **pred;  /**< The predecessor (control flow) regions of this region. */
	ir_region      **succ;  /**< The successor (control flow) regions of this region. */
	unsigned       prenum;  /**< DFS pre-oder number */
	unsigned       postnum; /**< DFS post-oder number */
	void           *link;   /**< A link field. */
	unsigned long  nr;      /**< for debugging */
	char           visited; /**< The visited flag. */
};

/* A helper type for unioning blocks and regions. */
union ir_reg_or_blk {
	firm_kind *kind;    /**< For easier check. */
	ir_node   *blk;     /**< A node */
	ir_region *region;  /**< A region. */
};

/* The debug handle. */
DEBUG_ONLY(firm_dbg_module_t *dbg;)

/**
 * Returns the link of a region.
 */
void *get_region_link(const ir_region *reg) {
	return reg->link;
}

/**
 * Sets the link of a region.
 */
void set_region_link(ir_region *reg, void *data) {
	reg->link = data;
}

/**
 * Get the immediate region of a block.
 */
ir_region *get_block_region(const ir_node *block) {
	assert(is_Block(block));
	return get_irn_link(block);
}

/**
 * Sets the immediate region of a block.
 */
void set_block_region(ir_node *block, ir_region *reg) {
	assert(is_Block(block));
	set_irn_link(block, reg);
}

/**
 * Get the immediate region of a node.
 */
ir_region *get_irn_region(ir_node *n) {
	if (is_no_Block(n))
		n = get_nodes_block(n);
	return get_block_region(n);
}

/**
 * Return non-if a given firm thing is a region.
 */
int is_region(const void *thing) {
	const firm_kind *kind = thing;
	return *kind == k_ir_region;
}

/**
 * Return the number of predecessors in a region.
 */
int get_region_n_preds(const ir_region *reg) {
	return ARR_LEN(reg->pred);
}

/**
 * Return the predecessor region at position pos.
 */
ir_region *get_region_pred(const ir_region *reg, int pos) {
	assert(0 <= pos && pos <= get_region_n_preds(reg));
	return reg->pred[pos];
}

/**
 * Set the predecessor region at position pos.
 */
void set_region_pred(ir_region *reg, int pos, ir_region *n) {
	assert(0 <= pos && pos <= get_region_n_preds(reg));
	reg->pred[pos] = n;
}

/**
 * Return the number of successors in a region.
 */
int get_region_n_succs(const ir_region *reg) {
	return ARR_LEN(reg->succ);
}

/**
 * Return the successor region at position pos.
 */
ir_region *get_region_succ(const ir_region *reg, int pos) {
	assert(0 <= pos && pos <= get_region_n_succs(reg));
	return reg->succ[pos];
}

/**
 * Set the successor region at position pos.
 */
void set_region_succ(ir_region *reg, int pos, ir_region *n) {
	assert(0 <= pos && pos <= get_region_n_succs(reg));
	reg->succ[pos] = n;
}

/* ----------------------- construction -------------------------- */

/** Walker environment. */
typedef struct walk_env {
	struct obstack *obst;  /**< an obstack to allocate from. */
	ir_region **post;      /**< The list of all currently existent top regions. */
	unsigned l_post;       /**< length of the allocated regions array. */
	unsigned premax;       /**< maximum pre counter */
	unsigned postmax;      /**< maximum post counter */
	ir_node *start_block;  /**< the start block of the graph. */
	ir_node *end_block;    /**< the end block of the graph. */
} walk_env;

/**
 * Do a DFS search on the initial regions, assign a prenum and a postnum to every
 * node and store the region nodes into the post array.
 */
static void dfs_walk2(ir_region *reg, walk_env *env) {
	int i, n;

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
static void dfs_walk(ir_graph *irg, walk_env *env) {
	ir_graph *rem = current_ir_graph;
	ir_region *reg;

	current_ir_graph = irg;
	reg              = get_irn_link(get_irg_start_block(irg));

	env->premax  = 0;
	env->postmax = 0;
	dfs_walk2(reg, env);
	current_ir_graph = rem;
}

/**
 * Post-walker: wrap all blocks with a Block region
 * and count them
 */
static void wrap_blocks(ir_node *block, void *ctx) {
	walk_env *env = ctx;
	ir_region *reg;

	/* Allocate a Block wrapper */
	reg          = obstack_alloc(env->obst, sizeof(*reg));
	reg->kind    = k_ir_region;
	reg->type    = ir_rk_Block;
	reg->parent  = NULL;
	reg->prenum  = 0;
	reg->postnum = 0;
	reg->visited = 0;
	reg->link    = NULL;
	reg->nr      = get_irn_node_nr(block);
	reg->parts   = NEW_ARR_D(ir_reg_or_blk, env->obst, 1);

	reg->parts[0].blk = block;
	set_irn_link(block, reg);

	++env->l_post;
}  /* wrap_blocks */

/**
 * Create the pred and succ edges for Block wrapper.
 * Kill edges to the Start and End blocks.
 */
static void update_Block_regions(ir_node *blk, void *ctx) {
	walk_env *env = ctx;
	ir_region *reg = get_irn_link(blk);
	int i, j, len;

	if (blk == env->start_block) {
		/* handle Firm's self loop */
		reg->pred = NEW_ARR_D(ir_region *, env->obst, 0);
	} else {
		len = get_Block_n_cfgpreds(blk);
		reg->pred = NEW_ARR_D(ir_region *, env->obst, len);
		for (i = j = 0; i < len; ++i) {
			ir_node *pred = get_Block_cfgpred_block(blk, i);
			reg->pred[j++] = get_irn_link(pred);
		}
		ARR_SHRINKLEN(reg->pred, j);
	}

	len = get_Block_n_cfg_outs(blk);
	reg->succ = NEW_ARR_D(ir_region *, env->obst, len);
	for (i = j = 0; i < len; ++i) {
		ir_node *succ = get_Block_cfg_out(blk, i);
		reg->succ[j++] = get_irn_link(succ);
	}
	ARR_SHRINKLEN(reg->succ, j);
}

/** Allocate a new region of a obstack */
#define ALLOC_REG(obst, reg, tp) \
	do { \
		(reg)          = obstack_alloc((obst), sizeof(*(reg))); \
		(reg)->kind    = k_ir_region; \
		(reg)->type    = tp; \
		(reg)->parent  = NULL; \
		(reg)->prenum  = 0; \
		(reg)->postnum = 0; \
		(reg)->visited = 0; \
		(reg)->link    = NULL; \
	} while (0)

/**
 * Creates a new Sequence region.
 */
static ir_region *new_Sequence(struct obstack *obst, ir_region *nset[]) {
	ir_region *reg;
	int i, len = ARR_LEN(nset);

	ALLOC_REG(obst, reg, ir_rk_Sequence);

	reg->nr    = nset[0]->nr;
	reg->parts = CLONE_ARR_D(ir_reg_or_blk, obst, nset);
	reg->pred  = DUP_ARR_D(ir_region *, obst, nset[0]->pred);
	reg->succ  = DUP_ARR_D(ir_region *, obst, nset[len - 1]->succ);

	for (i = 0; i < len; ++i) {
		reg->parts[i].region = nset[i];
		nset[i]->parent = reg;
	}

	DEBUG_ONLY(
		DB((dbg, LEVEL_2, " Created Sequence "));
		for (i = 0; i < len; ++i) {
			DB((dbg, LEVEL_2, "(%u)", reg->parts[i].region->nr));
		}
		DB((dbg, LEVEL_2, "\n"));
	)
	return reg;
}  /* new_Sequence */

/**
 * Create a new IfThenElse region.
 */
static ir_region *new_IfThenElse(struct obstack *obst, ir_region *if_b, ir_region *then_b, ir_region *else_b) {
	ir_region *reg;

	ALLOC_REG(obst, reg, ir_rk_IfThenElse);

	reg->nr    = if_b->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 3);

	reg->parts[0].region = if_b;   if_b->parent   = reg;
	reg->parts[1].region = then_b; then_b->parent = reg;
	reg->parts[2].region = else_b; else_b->parent = reg;

	reg->pred = DUP_ARR_D(ir_region *, obst, if_b->pred);
	reg->succ = DUP_ARR_D(ir_region *, obst, then_b->succ);

	DB((dbg, LEVEL_2, " Created If(%u)Then(%u)Else(%u)\n", reg->nr, then_b->nr, else_b->nr));

	return reg;
}  /* new_IfThenElse */

/**
 * Create a new IfThen region.
 */
static ir_region *new_IfThen(struct obstack *obst, ir_region *if_b, ir_region *then_b) {
	ir_region *reg;

	ALLOC_REG(obst, reg, ir_rk_IfThen);

	reg->nr    = if_b->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, 2);

	reg->parts[0].region = if_b;   if_b->parent   = reg;
	reg->parts[1].region = then_b; then_b->parent = reg;

	reg->pred = DUP_ARR_D(ir_region *, obst, if_b->pred);
	reg->succ = DUP_ARR_D(ir_region *, obst, then_b->succ);

	DB((dbg, LEVEL_2, " Created If(%u)Then(%u)\n", reg->nr, then_b->nr));

	return reg;
}  /* new_IfThenElse */

/**
 * Create a new Switch/case region.
 */
static ir_region *new_SwitchCase(struct obstack *obst, ir_region_kind type, ir_region *head, ir_region *end, pset *cases) {
	ir_region *reg, *c;
	int i;

	ALLOC_REG(obst, reg, type);

	reg->nr    = head->nr;
	reg->parts = NEW_ARR_D(ir_reg_or_blk, obst, pset_count(cases) + 1);

	reg->parts[0].region = head; head->parent = reg;
	i = 0;
	foreach_pset(cases, c) {
		reg->parts[i].region = c;
		c->parent = reg;
	}

	reg->pred = DUP_ARR_D(ir_region *, obst, head->pred);
	reg->succ = NEW_ARR_D(ir_region *, obst, 1);
	reg->succ[0] = end;

	DB((dbg, LEVEL_2, " Created Switch(%u)Exit(%u)\n", reg->nr, end->nr));

	del_pset(cases);
	return reg;
}  /* new_SwitchCase */

/**
 * Create a new SelfLoop region.
 */
static ir_region *new_SelfLoop(struct obstack *obst, ir_region *head) {
	ir_region *reg, *succ;
	int i, j, len;

	ALLOC_REG(obst, reg, ir_rk_SelfLoop);

	reg->nr      = head->nr;
	reg->parts   = NEW_ARR_D(ir_reg_or_blk, obst, 1);

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

	DB((dbg, LEVEL_2, " Created SelfLoop(%u)\n", reg->nr));

	return reg;
}  /* new_SelfLoop */

/**
 * Create a new RepeatLoop region.
 */
static ir_region *new_RepeatLoop(struct obstack *obst, ir_region *head, ir_region *body) {
	ir_region *reg, *succ;

	ALLOC_REG(obst, reg, ir_rk_RepeatLoop);

	reg->nr      = head->nr;
	reg->parts   = NEW_ARR_D(ir_reg_or_blk, obst, 2);

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

	DB((dbg, LEVEL_2, " Created RepeatLoop(%u)Body(%u)\n", reg->nr, body->nr));

	return reg;
}  /* new_RepeatLoop */

/**
 * Create a new WhileLoop region.
 */
static ir_region *new_WhileLoop(struct obstack *obst, ir_region *head) {
	ir_region *reg, *succ;
	ir_region *body = head->link;
	int i, j, len;

	head->link = NULL;

	ALLOC_REG(obst, reg, ir_rk_WhileLoop);

	reg->nr      = head->nr;
	reg->parts   = NEW_ARR_D(ir_reg_or_blk, obst, 2);

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

	DB((dbg, LEVEL_2, " Created WhileLoop(%u)Body(%u)\n", reg->nr, body->nr));

	return reg;
}  /* new_WhileLoop */

/**
 * Return true if a is an ancestor of b in DFS search.
 */
static int is_ancestor(const ir_region *a, const ir_region *b) {
	return (a->prenum <= b->prenum && a->postnum > b->postnum);
}

/**
 *  Return true if region succ is a successor of region n.
 */
static int succ_of(const ir_region *succ, const ir_region *n) {
	int i;
	for (i = get_region_n_succs(n) - 1; i >= 0; --i) {
		if (get_region_succ(n, i) == succ)
			return 1;
	}
	return 0;
}

/**
 *  Reverse linked list.
 */
static struct ir_region *reverse_list(ir_region *n) {
	ir_region *prev = NULL, *next;

	for (; n; n = next) {
		next = n->link;
		n->link = prev;
		prev = n;
	}
	return prev;
}

/**
 * Find the cyclic region in the subgraph entered by node.
 */
static ir_region *find_cyclic_region(ir_region *node) {
	int i;
	ir_region *last = node;
	int improper = 0;

	for (i = get_region_n_preds(node) - 1; i >= 0; --i) {
		ir_region *pred = get_region_pred(node, i);

		/* search backedges */
		if (!pred->link && pred != last && is_ancestor(node, pred)) {
			ir_region *rem = last;
			int j;

			last->link = pred;
			last       = pred;
			for (j = get_region_n_preds(pred) - 1; j >= 0; --j) {
				ir_region *p = get_region_pred(pred, j);

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
			last = rem->link;
			rem->link = reverse_list(rem->link);
		}
	}

	if (node->link && improper) {
		/* found an improper region */
	}
	return node;
}

#define LINK(list) ((ir_region *)list->link)

/**
 * Detect a cyclic region.
 */
static ir_region *cyclic_region_type(struct obstack *obst, ir_region *node) {
	ir_region *list;

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

	if (list->link && !LINK(list)->link && get_region_n_succs(list->link) == 1) {
		return new_WhileLoop(obst, list);
	}

	return NULL;
}

/**
 * Detect an acyclic region.
 */
static ir_region *acyclic_region_type(struct obstack *obst, ir_region *node) {
	ir_region *n, *m;
	int p, s, i, k;
	ir_region **nset = NEW_ARR_F(ir_region *, 0);
	pset *set;
	ir_region *res;

	/* check for a block containing node */
	n = node;
	p = get_region_n_preds(n) == 1;
	s = 1;
	while (p & s) {
		n = get_region_pred(n, 0);
		p = get_region_n_preds(n) == 1;
		s = get_region_n_succs(n) == 1;
	}
	p = 1;
	s = get_region_n_succs(n) == 1;
	while (p & s) {
		ARR_APP1(ir_region *, nset, n);
		n = get_region_succ(n, 0);
		p = get_region_n_preds(n) == 1;
		s = get_region_n_succs(n) == 1;
	}
	if (p) {
		ARR_APP1(ir_region *, nset, n);
	}
	if (ARR_LEN(nset) >= 2) {
		/* node --> .. --> .. */
		res = new_Sequence(obst, nset);
		DEL_ARR_F(nset);
		return res;
	}
	DEL_ARR_F(nset);
	node = n;

	/* check for IfThenElse */
	k = get_region_n_succs(node);
	if (k == 2) {
		int n_succs, m_succs, n_preds, m_preds;

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
		if (n_succs == 1 && m_preds == 2 &&
		    get_region_succ(n, 0) == m &&
		    (get_region_pred(m, 0) == node ||
		     get_region_pred(m, 1) == node)) {
			/*
			 * node -->n-->m
			 *    \-------/
			 */
			return new_IfThen(obst, node, n);
		}
		if (m_succs == 1 && n_preds == 2 &&
		    get_region_succ(m, 0) == m &&
		    (get_region_pred(n, 0) == node ||
		     get_region_pred(n, 1) == node)) {
			/*
			 * node -->m-->n
			 *    \-------/
			 */
			return new_IfThen(obst, node, m);
		}
	}
	/* check for Switch, case */
	set = pset_new_ptr_default();
	p   = k > 0;
	for (i = k - 1; i >= 0; --i) {
		n = get_region_succ(node, i);
		pset_insert_ptr(set, n);
		if (get_region_n_succs(n) != 1) {
			p = 0;
			break;
		}
	}
	if (p) {
		ir_region_kind kind = ir_rk_Case;

		m = pset_first(set);
		if (pset_find_ptr(set, m) != NULL) {
			/* must be a switch, if any, find the exit */
			kind = ir_rk_Switch;

			for (m = pset_next(set); m != NULL; m = pset_next(set)) {
				if (pset_find_ptr(set, m) == NULL) {
					pset_break(set);
					break;
				}
			}
		}
		if (m != NULL) {
			/* m ist the exit, do the checks */
			foreach_pset(set, n) {
				if (n == m) {
					/* good, switch to exit */
					continue;
				}
				if (pset_find_ptr(set, n) == NULL) {
					/* another exit */
					pset_break(set);
					break;
				}
				kind = ir_rk_Switch;
			}

			if (n == NULL) {
				/* detected */
				return new_SwitchCase(obst, kind, node, m, set);
			}
		}
	}
	/* check for proper */

	return NULL;
}

/**
 * Fill the given set recursively with all parts of the region (including itself)
 */
static void fill_parts(pset *set, ir_region *reg) {
	int i;

	if (reg->type == ir_rk_Block) {
		pset_insert_ptr(set, reg);
		return;
	}

	for (i = ARR_LEN(reg->parts) - 1; i >= 0; --i) {
		fill_parts(set, reg->parts[i].region);
	}
}

/**
 * replace all pred edges from region pred that points to any of the set set
 * to ONE edge to reg.
 */
static void replace_pred(ir_region *succ, ir_region *reg) {
	int i, len = get_region_n_preds(succ);
	int have_one = 0;

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
		}
	}
	ARR_SHRINKLEN(succ->pred, len);
}

/**
 * replace all succ edges from region pred that points to any of the set set
 * to ONE edge to reg.
 */
static void replace_succ(ir_region *pred, ir_region *reg) {
	int i, len = get_region_n_succs(pred);
	int have_one = 0;

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
		}
	}
	ARR_SHRINKLEN(pred->succ, len);
}

/**
 * Reduce the graph by the node reg.
 */
static void reduce(walk_env *env, ir_region *reg) {
	int i;
	ir_region *head = reg->parts[0].region;
	unsigned maxorder = head->postnum;
	unsigned minorder = head->prenum;

	/* second step: replace all preds in successors */
	for (i = get_region_n_succs(reg) - 1; i >= 0; --i) {
		ir_region *succ = get_region_succ(reg, i);

		replace_pred(succ, reg);
	}

	/* second third: replace all succs in predessors */
	for (i = get_region_n_preds(reg) - 1; i >= 0; --i) {
		ir_region *pred = get_region_pred(reg, i);

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
ir_reg_tree *construct_region_tree(ir_graph *irg) {
	walk_env env;
	ir_graph *rem = current_ir_graph;
	ir_reg_tree *res = xmalloc(sizeof(*res));

	obstack_init(&res->obst);

	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.ana.structure");
	firm_dbg_set_mask(dbg, SET_LEVEL_5);

	DB((dbg, LEVEL_1, "Structural analysis on %+F starts...\n", irg));

	dump_ir_block_graph(irg, "-structure_start");

	/* we need dominance info */
	assure_doms(irg);
	/* and out edges */
	assure_irg_outs(irg);

	env.start_block = get_irg_start_block(irg);
	env.end_block   = get_irg_end_block(irg);

	/* create the Block wrapper and count them */
	env.l_post = 0;
	env.obst   = &res->obst;
	irg_block_walk_graph(irg, NULL, wrap_blocks, &env);
	irg_block_walk_graph(irg, NULL, update_Block_regions, &env);

	env.post = NEW_ARR_F(ir_region *, env.l_post);

	/* do the DFS walk */
	dfs_walk(irg, &env);

	DB((dbg, LEVEL_1, "%d regions left\n", env.postmax));
	if (env.postmax > 1) {
		unsigned postctr = 0;
		do {
			ir_region *reg, *n = env.post[postctr];
			do {
				if (n->parent) {
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
static void region_tree_walk2(ir_region *reg, irg_reg_walk_func *pre, irg_reg_walk_func *post, void *env) {
	int i, n;

	if (pre)
		pre(reg, env);
	if (reg->type != ir_rk_Block) {
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
void region_tree_walk(ir_reg_tree *tree, irg_reg_walk_func *pre, irg_reg_walk_func *post, void *env) {
	region_tree_walk2(tree->top, pre, post, env);
}
