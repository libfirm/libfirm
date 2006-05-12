/**
 * Project:     libFIRM
 * File name:   ir/opt/opt_osr.
 * Purpose:     Operator Strength Reduction,
 *              Keith D. Cooper, L. Taylor Simpson, Christopher A. Vick
 * Author:      Michael Beck
 * Modified by:
 * Created:     12.5.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "opt_osr.h"
#include "irgraph.h"
#include "irop_t.h"
#include "irloop.h"
#include "irdom.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "debug.h"
#include "obst.h"
#include "irtools.h"
#include "array.h"

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/* Use the link field to mark IV's */
#define MARK_LOOP_IV(loop)   set_loop_link((loop), (loop))
#define UNMARK_LOOP_IV(loop) set_loop_link((loop), NULL)
#define IS_LOOP_IV(loop)     (get_loop_link(loop) != NULL)

/** A scc. */
typedef struct scc {
	ir_node *head;		/**< the head of the list */
	ir_node *header;	/**< the header block of this scc */
	int     is_iv;      /**< true, if this scc is an IV */
} scc;

/** A node entry */
typedef struct node_entry {
	unsigned DFSnum;    /**< the DFS number of this node */
	unsigned low;       /**< the low number of this node */
	ir_node  *header;   /**< the header of this node */
	int      in_stack;  /**< flag, set if the node is on the stack */
	ir_node  *next;     /**< link to the next node the the same scc */
	scc      *pscc;     /**< the scc of this node */
	unsigned POnum;     /**< the post order number for blocks */
} node_entry;

/** The environment. */
typedef struct iv_env {
	node_entry *entries;   /**< the node entry array */
	struct obstack obst;   /**< an obstack for allocations */
	ir_node  **stack;      /**< the node stack */
	int      tos;          /**< tos index */
	unsigned nextDFSnum;   /**< the current DFS number */
	unsigned replaced;     /**< number of replaced ops */
	unsigned POnum;        /**< current post order number */
} iv_env;

/**
 * Check if irn is an IV.
 */
static scc *is_iv(ir_node *irn, iv_env *env) {
	int idx = get_irn_idx(irn);
	node_entry *e = &env->entries[idx];

	return e->pscc && e->pscc->is_iv ? e->pscc : NULL;
}

/**
 * Check if irn is a region constant.
 */
static int is_rc(ir_node *irn, ir_node *header_block) {
	ir_node *block = get_nodes_block(irn);

	return block_dominates(block, header_block);
}

/**
 * Do the replacement operation.
 *
 * @param irn   the node that will be replaced
 * @param iv    the induction variable
 * @param rc    the region constant
 * @param env   the environment
 */
static void replace(ir_node *irn, ir_node *iv, ir_node *rc, iv_env *env) {
	DB((dbg, LEVEL_2, "  Replacing %+F\n", irn));
}

/**
 * check if a node can be replaced
 */
static void check_replace(ir_node *irn, iv_env *env) {
	ir_node *left, *right, *iv, *rc;
	ir_op   *op  = get_irn_op(irn);
	opcode  code = get_op_code(op);
	scc     *liv, *riv;

	switch (code) {
	case iro_Mul:
	case iro_Add:
	case iro_Sub:
		iv = rc = NULL;

		left  = get_binop_left(irn);
		right = get_binop_right(irn);

		liv = is_iv(left, env);
		riv = is_iv(right, env);
		if (liv && is_rc(right, liv->header)) {
			iv = left; rc = right;
		}
		else if (is_op_commutative(op) &&
			riv && is_rc(left, riv->header)) {
			iv = right; rc = left;
		}

		if (iv) {
			replace(irn, iv, rc, env);
			++env->replaced;
		}
		break;
	}
}

/**
 * check which SCC's are induction variables
 */
static void classify_iv(scc *pscc, iv_env *env) {
	ir_node *irn, *next, *header = NULL;
	node_entry *h, *b;
	int j;

	/* find the header block for this scc */
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_link(irn);
		ir_node *block = get_nodes_block(irn);

		next = e->next;
		b = &env->entries[get_irn_idx(block)];

		if (header) {
			if (h->POnum < b->POnum) {
				header = block;
				h      = b;
			}
		}
		else {
			header = block;
			h      = b;
		}
	}
	pscc->header = header;

	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_link(irn);

		next = e->next;
		switch (get_irn_opcode(irn)) {
		case iro_Add:
		case iro_Sub:
		case iro_Phi:
			for (j = get_irn_arity(irn) - 1; j >= 0; --j) {
				ir_node *pred = get_irn_n(irn, j);
				node_entry *pe = &env->entries[get_irn_idx(pred)];

				if (pe->pscc != e->pscc) {
					/* not in the same SCC, must be a region const */
					if (! is_rc(pred, header)) {
						/* not an induction variable */
						goto fail;
					}
				}
			}
			break;
		default:
			/* not an induction variable */
			goto fail;
		}
	}
	/* found an induction variable */
	DB((dbg, LEVEL_2, "  Found an induction variable in %+F\n", pscc->head));
	pscc->is_iv = 1;
	return;

fail:
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_link(irn);

		next = e->next;
		check_replace(irn, env);
	}
}

/**
 * Do the replacement: We must do this as an additional step because
 * of our loop-tree structure.
 */
static void find_replacement(ir_loop *loop, iv_env *env) {
	int i, n;

	if (! IS_LOOP_IV(loop)) {
		/* do replacements */
		n = get_loop_n_nodes(loop);
		for (i = 0; i < n; ++i) {
			ir_node *irn = get_loop_node(loop, i);
			ir_node *left, *right, *iv, *rc;
			opcode code = get_irn_opcode(irn);

			switch (code) {
			case iro_Mul:
			case iro_Add:
			case iro_Sub:
				iv = rc = NULL;

				left  = get_binop_left(irn);
				right = get_binop_right(irn);

				if (is_iv(left, env) && is_rc(right, left)) {
					iv = left; rc = right;
				}
				else if (code != iro_Sub &&
					is_iv(right, env) && is_rc(left, right)) {
					iv = right; rc = left;
				}

				if (iv) {
					replace(irn, iv, rc, env);
					++env->replaced;
				}
				break;
			}
		}
	}
	n = get_loop_n_sons(loop);
	for (i = 0; i < n; ++i) {
		ir_loop *child = get_loop_son(loop, i);
		find_replacement(child, env);
	}
}

/**
 * Process a SCC given as a list.
 */
static void process_scc(scc *pscc, iv_env *env) {
	ir_node *head = pscc->head;
	node_entry *e = get_irn_link(head);

	pscc->is_iv = 0;

#ifdef DEBUG_libfirm
	{
		ir_node *irn, *next;

		DB((dbg, LEVEL_3, " SCC at %p:\n ", pscc));
		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = get_irn_link(irn);

			next = e->next;

			DB((dbg, LEVEL_3, " %+F,", irn));
		}
		DB((dbg, LEVEL_3, "\n"));
	}
#endif

	if (e->next == NULL) {
		/* this SCC has only a single member */
		check_replace(head, env);
	}
	else {
		classify_iv(pscc, env);
	}
}

/**
 * Push a node onto the stack.
 */
static void push(iv_env *env, ir_node *n) {
	if (env->tos == ARR_LEN(env->stack)) {
		int nlen = ARR_LEN(env->stack) * 2;
		ARR_RESIZE(ir_node *, env->stack, nlen);
	}
	env->stack[env->tos++] = n;
	env->entries[get_irn_idx(n)].in_stack = 1;
}

/**
 * pop a node from the stack
 *
 * @return  The topmost node
 */
static ir_node *pop(iv_env *env)
{
  ir_node *n = env->stack[--env->tos];
  env->entries[get_irn_idx(n)].in_stack = 0;
  return n;
}

static void dfs(ir_node *irn, iv_env *env)
{
	int i, n;
	node_entry *node = &env->entries[get_irn_idx(irn)];

	mark_irn_visited(irn);

	/* do not put blocks into the scc */
	if (is_Block(irn)) {
		n = get_irn_arity(irn);
		for (i = 0; i < n; ++i) {
			ir_node *pred = get_irn_n(irn, i);

			if (irn_not_visited(pred))
				dfs(pred, env);
		}
	}
	else {
		ir_node *block = get_nodes_block(irn);

		node->DFSnum = env->nextDFSnum++;
		node->low    = node->DFSnum;
		push(env, irn);

		/* handle the block */
		if (irn_not_visited(block))
			dfs(block, env);

		n = get_irn_arity(irn);
		for (i = 0; i < n; ++i) {
			ir_node *pred = get_irn_n(irn, i);
			node_entry *o = &env->entries[get_irn_idx(pred)];

			if (irn_not_visited(pred)) {
				dfs(pred, env);
				node->low = MIN(node->low, o->low);
			}
			if (o->DFSnum < node->DFSnum && o->in_stack)
				node->low = MIN(o->DFSnum, node->low);
		}
		if (node->low == node->DFSnum) {
			scc *pscc = obstack_alloc(&env->obst, sizeof(*pscc));
			ir_node *x;

			pscc->head = NULL;
			do {
				node_entry *e;

				x = pop(env);
				e = &env->entries[get_irn_idx(x)];
				e->pscc    = pscc;
				e->next    = pscc->head;
				pscc->head = x;

				/* link the node entry for easier access */
				set_irn_link(x, e);
			} while (x != irn);

			process_scc(pscc, env);
		}
	}
}

/**
 * Do the DFS by starting end the End node
 */
static void do_dfs(ir_graph *irg, iv_env *env) {
	ir_graph *rem = current_ir_graph;
	ir_node *end = get_irg_end(irg);
	int i, n;

	current_ir_graph = irg;
	inc_irg_visited(irg);

	/* visit all visible nodes */
	dfs(end, env);

	/* visit the keep-alives */
	n = get_End_n_keepalives(end);
	for (i = 0; i < n; ++i) {
		ir_node *ka = get_End_keepalive(end, i);

		if (irn_not_visited(ka))
			dfs(ka, env);
	}
	current_ir_graph = rem;
}

/**
 * Post-block-walker: assign the post-order number.
 */
static void assign_po(ir_node *block, void *ctx) {
	iv_env *env = ctx;
	int idx = get_irn_idx(block);

	env->entries[idx].POnum = env->POnum++;
}

/* Performs Operator Strength Reduction for the passed graph. */
void opt_osr(ir_graph *irg) {
	iv_env env;

	if (! get_opt_strength_red())
		return;

	FIRM_DBG_REGISTER(dbg, "firm.opt.osr");
	firm_dbg_set_mask(dbg, SET_LEVEL_2);

	/* and dominance as well */
	assure_doms(irg);

	DB((dbg, LEVEL_1, "Doing Operator Strength Reduction for %+F\n", irg));

	env.entries    = NEW_ARR_F(node_entry, get_irg_last_idx(irg));
	memset(env.entries, 0, get_irg_last_idx(irg));

	obstack_init(&env.obst);
	env.stack      = NEW_ARR_F(ir_node *, 128);
	env.tos        = 0;
	env.nextDFSnum = 0;
	env.replaced   = 0;
	env.POnum      = 0;

	/* calculate the post order number */
	irg_block_walk_graph(irg, NULL, assign_po, &env);

	do_dfs(irg, &env);

	if (env.replaced) {
		set_irg_outs_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}
	DB((dbg, LEVEL_1, "Replacements: %u\n\n", env.replaced));

	DEL_ARR_F(env.stack);
	obstack_free(&env.obst, NULL);
	DEL_ARR_F(env.entries);
}
