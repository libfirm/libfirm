/**
 * Project:     libFIRM
 * File name:   ir/opt/opt_osr.c
 * Purpose:     Operator Strength Reduction, based on
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

#include "opt_osr.h"
#include "irgraph.h"
#include "ircons.h"
#include "irop_t.h"
#include "irloop.h"
#include "irdom.h"
#include "irgmod.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "irouts.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "tv.h"
#include "hashptr.h"
#include "irtools.h"
#include "irloop_t.h"
#include "array.h"
#include "firmstat.h"
#include "xmalloc.h"

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** A scc. */
typedef struct scc {
	ir_node *head;		/**< the head of the list */
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
	struct obstack obst;    /**< an obstack for allocations */
	ir_node  **stack;       /**< the node stack */
	int      tos;           /**< tos index */
	unsigned nextDFSnum;    /**< the current DFS number */
	unsigned POnum;         /**< current post order number */
	set      *quad_map;     /**< a map from (op, iv, rc) to node */
	set      *lftr_edges;   /**< the set of lftr edges */
	unsigned replaced;      /**< number of replaced ops */
	unsigned lftr_replaced; /**< number of applied linear function test replacements */
	unsigned flags;         /**< additional flags */
	/** Function called to process a SCC. */
	void (*process_scc)(scc *pscc, struct iv_env *env);
} iv_env;

/**
 * An entry in the (op, node, node) -> node map.
 */
typedef struct quadruple_t {
	ir_opcode code;  /**< the opcode of the reduced operation */
	ir_node   *op1;  /**< the first operand the reduced operation */
	ir_node   *op2;  /**< the second operand of the reduced operation */

	ir_node   *res; /**< the reduced operation */
} quadruple_t;

/**
 * A LFTR edge.
 */
typedef struct LFTR_edge {
	ir_node   *src;   /**< the source node */
	ir_node   *dst;   /**< the destination node */
	ir_opcode code;   /**< the opcode that must be applied */
	ir_node   *rc;    /**< the region const that must be applied */
} LFTR_edge;

/* forward */
static ir_node *reduce(ir_node *orig, ir_node *iv, ir_node *rc, iv_env *env);

/**
 * Compare two LFTR edges.
 */
static int LFTR_cmp(const void *e1, const void *e2, size_t size) {
	const LFTR_edge *l1 = e1;
	const LFTR_edge *l2 = e2;

	return l1->src != l2->src;
}

#if 0
/**
 * Find a LFTR edge.
 */
static LFTR_edge *LFTR_find(ir_node *src, iv_env *env) {
	LFTR_edge key;

	key.src  = src;

	return set_find(env->lftr_edges, &key, sizeof(key), HASH_PTR(src));
}
#endif

/**
 * Add a LFTR edge.
 */
static void LFTR_add(ir_node *src, ir_node *dst, ir_opcode code, ir_node *rc, iv_env *env) {
	LFTR_edge key;

	key.src  = src;
	key.dst  = dst;
	key.code = code;
	key.rc   = rc;

	/*
	 * There might be more than one edge here. This is rather bad
	 * because we currently store only one.
	 */
//	assert(LFTR_find(src, env) == NULL);
	set_insert(env->lftr_edges, &key, sizeof(key), HASH_PTR(src));
}

/**
 * Gets the node_entry of a node
 */
static node_entry *get_irn_ne(ir_node *irn, iv_env *env) {
	node_entry *e = get_irn_link(irn);

	if (! e) {
		e = obstack_alloc(&env->obst, sizeof(*e));
		memset(e, 0, sizeof(*e));
		set_irn_link(irn, e);
	}
	return e;
}

/**
 * Check if irn is an IV.
 *
 * @param irn  the node to check
 * @param env  the environment
 *
 * @returns the header if it is one, NULL else
 */
static ir_node *is_iv(ir_node *irn, iv_env *env) {
	return get_irn_ne(irn, env)->header;
}

/**
 * Check if irn is a region constant.
 * The block or irn must strictly dominate the header block.
 *
 * @param irn           the node to check
 * @param header_block  the header block of the induction variable
 */
static int is_rc(ir_node *irn, ir_node *header_block) {
	ir_node *block = get_nodes_block(irn);

	return (block != header_block) && block_dominates(block, header_block);
}

/**
 * Set compare function for the quad set.
 */
static int quad_cmp(const void *e1, const void *e2, size_t size) {
	const quadruple_t *c1 = e1;
	const quadruple_t *c2 = e2;

	return c1->code != c2->code || c1->op1 != c2->op1 || c1->op2 != c2->op2;
}

/**
 * Check if an reduced operation was already calculated.
 *
 * @param code  the opcode of the operation
 * @param op1   the first operand of the operation
 * @param op2   the second operand of the operation
 * @param env   the environment
 *
 * @return the already reduced node or NULL if this operation is not yet reduced
 */
static ir_node *search(ir_opcode code, ir_node *op1, ir_node *op2, iv_env *env) {
	quadruple_t key, *entry;

	key.code = code;
	key.op1 = op1;
	key.op2 = op2;

	entry = set_find(env->quad_map, &key, sizeof(key),
	                 (code * 9) ^ HASH_PTR(op1) ^HASH_PTR(op2));
	if (entry)
		return entry->res;
	return NULL;
}

/**
 * Add an reduced operation.
 *
 * @param code    the opcode of the operation
 * @param op1     the first operand of the operation
 * @param op2     the second operand of the operation
 * @param result  the result of the reduced operation
 * @param env     the environment
 */
static void add(ir_opcode code, ir_node *op1, ir_node *op2, ir_node *result, iv_env *env) {
	quadruple_t key;

	key.code = code;
	key.op1  = op1;
	key.op2  = op2;
	key.res  = result;

	set_insert(env->quad_map, &key, sizeof(key),
	           (code * 9) ^ HASH_PTR(op1) ^HASH_PTR(op2));
}

/**
 * Find a location where to place a bin-op whose operands are in
 * block1 and block2.
 *
 * @param block1  the block of the first operand
 * @param block2  the block of the second operand
 *
 * Note that we know here that such a place must exists. Moreover, this means
 * that either block1 dominates block2 or vice versa. So, just return
 * the "smaller" one.
 */
static ir_node *find_location(ir_node *block1, ir_node *block2) {
	if (block_dominates(block1, block2))
		return block2;
	assert(block_dominates(block2, block1));
	return block1;
}

/**
 * Create a node that executes an op1 code op1 operation.
 *
 * @param code   the opcode to execute
 * @param db     debug info to add to the new node
 * @param op1    the first operand
 * @param op2    the second operand
 * @param mode   the mode of the new operation
 *
 * @return the newly created node
 */
static ir_node *do_apply(ir_opcode code, dbg_info *db, ir_node *op1, ir_node *op2, ir_mode *mode) {
	ir_graph *irg = current_ir_graph;
	ir_node *result;
	ir_node *block = find_location(get_nodes_block(op1), get_nodes_block(op2));

	switch (code) {
	case iro_Mul:
		result = new_rd_Mul(db, irg, block, op1, op2, mode);
		break;
	case iro_Add:
		result = new_rd_Add(db, irg, block, op1, op2, mode);
		break;
	case iro_Sub:
		result = new_rd_Sub(db, irg, block, op1, op2, mode);
		break;
	default:
		assert(0);
		result = NULL;
	}
	return result;
}

/**
 * The Apply operation.
 *
 * @param orig   the node that represent the original operation and determines
 *               the opcode, debug-info and mode of a newly created one
 * @param op1    the first operand
 * @param op2    the second operand
 * @param env    the environment
 *
 * @return the newly created node
 */
static ir_node *apply(ir_node *orig, ir_node *op1, ir_node *op2, iv_env *env) {
	ir_opcode code = get_irn_opcode(orig);
	ir_node *result = search(code, op1, op2, env);

	if (! result) {
		dbg_info *db = get_irn_dbg_info(orig);
		ir_node *op1_header = get_irn_ne(op1, env)->header;
		ir_node *op2_header = get_irn_ne(op2, env)->header;

		if (op1_header != NULL && is_rc(op2, op1_header)) {
			result = reduce(orig, op1, op2, env);
		}
		else if (op2_header != NULL && is_rc(op1, op2_header)) {
			result = reduce(orig, op2, op1, env);
		}
		else {
			result = do_apply(code, db, op1, op2, get_irn_mode(orig));
			get_irn_ne(result, env)->header = NULL;		}
	}
	return result;
}

/**
 * The Reduce operation.
 *
 * @param orig   the node that represent the original operation and determines
 *               the opcode, debug-info and mode of a newly created one
 * @param iv     the induction variable
 * @param rc     the region constant
 * @param env    the environment
 *
 * @return the reduced node
 */
static ir_node *reduce(ir_node *orig, ir_node *iv, ir_node *rc, iv_env *env) {
	ir_opcode code = get_irn_opcode(orig);
	ir_node *result = search(code, iv, rc, env);

	if (! result) {
		node_entry *e, *iv_e;
		int i, n;
		ir_mode *mode = get_irn_mode(orig);

		result = exact_copy(iv);

		/* Beware: we must always create a new nduction variable with the same mode
		   as the node we are replacing. Espicially this means the mode might be changed
		   from P to I and back. This is always possible, because we have only Phi, Add
		   and Sub nodes. */
		set_irn_mode(result, mode);
		add(code, iv, rc, result, env);
		DB((dbg, LEVEL_3, "   Created new %+F for %+F (%s %+F)\n", result, iv,
			get_irn_opname(orig), rc));

		iv_e = get_irn_ne(iv, env);
		e    = get_irn_ne(result, env);
		e->header = iv_e->header;

		/* create the LFTR edge */
		LFTR_add(iv, result, code, rc, env);

		n = get_irn_arity(result);
		for (i = 0; i < n; ++i) {
			ir_node *o = get_irn_n(result, i);

			e = get_irn_ne(o, env);
			if (e->header == iv_e->header)
				o = reduce(orig, o, rc, env);
			else if (is_Phi(result))
				o = apply(orig, o, rc, env);
			else {
				if (code == iro_Mul)
					o = apply(orig, o, rc, env);
			}
			set_irn_n(result, i, o);
		}
	}
	else {
		DB((dbg, LEVEL_3, "   Already Created %+F for %+F (%s %+F)\n", result, iv,
			get_irn_opname(orig), rc));
	}
	return result;
}

/**
 * The Replace operation.
 *
 * @param irn   the node that will be replaced
 * @param iv    the induction variable
 * @param rc    the region constant
 * @param env   the environment
 */
static int replace(ir_node *irn, ir_node *iv, ir_node *rc, iv_env *env) {
	ir_node *result;
	ir_loop *iv_loop  = get_irn_loop(get_nodes_block(iv));
	ir_loop *irn_loop = get_irn_loop(get_nodes_block(irn));

	/* only replace nodes that are in the same (or deeper loops) */
	if (get_loop_depth(irn_loop) >= get_loop_depth(iv_loop)) {
		DB((dbg, LEVEL_2, "  Replacing %+F\n", irn));

		result = reduce(irn, iv, rc, env);
		if (result != irn) {
			node_entry *e, *iv_e;

			hook_strength_red(current_ir_graph, irn);
			exchange(irn, result);
			e = get_irn_ne(result, env);
			iv_e = get_irn_ne(iv, env);
			e->header = iv_e->header;
		}
		return 1;
	}
	return 0;
}

/**
 * Check if a node can be replaced (+, -, *).
 *
 * @param irn   the node to check
 * @param env   the environment
 *
 * @return non-zero if irn should be Replace'd
 */
static int check_replace(ir_node *irn, iv_env *env) {
	ir_node   *left, *right, *iv, *rc;
	ir_op     *op  = get_irn_op(irn);
	ir_opcode code = get_op_code(op);
	ir_node   *liv, *riv;

	switch (code) {
	case iro_Mul:
	case iro_Add:
	case iro_Sub:
		iv = rc = NULL;

		left  = get_binop_left(irn);
		right = get_binop_right(irn);

		liv = is_iv(left, env);
		riv = is_iv(right, env);
		if (liv && is_rc(right, liv)) {
			iv = left; rc = right;
		}
		else if (riv && is_op_commutative(op) &&
			            is_rc(left, riv)) {
			iv = right; rc = left;
		}

		if (iv) {
			if (code == iro_Mul && env->flags & osr_flag_ignore_x86_shift) {
				if (is_Const(rc)) {
					tarval *tv = get_Const_tarval(rc);

					if (tarval_is_long(tv)) {
						long value = get_tarval_long(tv);

						if (value == 2 || value == 4 || value == 8) {
							/* do not reduce multiplications by 2, 4, 8 */
							break;
						}
					}
				}
			}

			return replace(irn, iv, rc, env);
		}
		break;
	default:
		break;
	}
	return 0;
}

/**
 * Check which SCC's are induction variables.
 *
 * @param pscc  a SCC
 * @param env   the environment
 */
static void classify_iv(scc *pscc, iv_env *env) {
	ir_node *irn, *next, *header = NULL;
	node_entry *b, *h = NULL;
	int j, only_phi, num_outside;
	ir_node *out_rc;

	/* find the header block for this scc */
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_link(irn);
		ir_node *block = get_nodes_block(irn);

		next = e->next;
		b = get_irn_ne(block, env);

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

	/* check if this scc contains only Phi, Add or Sub nodes */
	only_phi    = 1;
	num_outside = 0;
	out_rc      = NULL;
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);

		next = e->next;
		switch (get_irn_opcode(irn)) {
		case iro_Add:
		case iro_Sub:
			only_phi = 0;
			/* fall through */
		case iro_Phi:
			for (j = get_irn_arity(irn) - 1; j >= 0; --j) {
				ir_node *pred  = get_irn_n(irn, j);
				node_entry *pe = get_irn_ne(pred, env);

				if (pe->pscc != e->pscc) {
					/* not in the same SCC, must be a region const */
					if (! is_rc(pred, header)) {
						/* not an induction variable */
						goto fail;
					}
					if (! out_rc) {
						out_rc = pred;
						++num_outside;
					} else if (out_rc != pred) {
						++num_outside;
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
	DB((dbg, LEVEL_2, "  Found an induction variable:\n  "));
	if (only_phi && num_outside == 1) {
		/* a phi cycle with only one real predecessor can be collapsed */
		DB((dbg, LEVEL_2, "  Found an USELESS Phi cycle:\n  "));

		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = get_irn_ne(irn, env);
			next = e->next;
			e->header = NULL;
			exchange(irn, out_rc);
		}
		++env->replaced;
		return;
	}

	/* set the header for every node in this scc */
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		e->header = header;
		next = e->next;
		DB((dbg, LEVEL_2, " %+F,", irn));
	}
	DB((dbg, LEVEL_2, "\n"));
	return;

fail:
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);

		next = e->next;
		if (! check_replace(irn, env))
			e->header = NULL;
	}
}

/**
 * Process a SCC for the operator strength reduction.
 *
 * @param pscc  the SCC
 * @param env   the environment
 */
static void process_scc(scc *pscc, iv_env *env) {
	ir_node *head = pscc->head;
	node_entry *e = get_irn_link(head);

#ifdef DEBUG_libfirm
	{
		ir_node *irn, *next;

		DB((dbg, LEVEL_4, " SCC at %p:\n ", pscc));
		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = get_irn_link(irn);

			next = e->next;

			DB((dbg, LEVEL_4, " %+F,", irn));
		}
		DB((dbg, LEVEL_4, "\n"));
	}
#endif

	if (e->next == NULL) {
		/* this SCC has only a single member */
		check_replace(head, env);
	} else {
		classify_iv(pscc, env);
	}
}

/**
 * If an SCC is a Phi only cycle, remove it.
 */
static void remove_phi_cycle(scc *pscc, iv_env *env) {
	ir_node *irn, *next;
	int j;
	ir_node *out_rc;

	/* check if this scc contains only Phi, Add or Sub nodes */
	out_rc      = NULL;
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);

		next = e->next;
		if (! is_Phi(irn))
			return;

		for (j = get_irn_arity(irn) - 1; j >= 0; --j) {
			ir_node *pred  = get_irn_n(irn, j);
			node_entry *pe = get_irn_ne(pred, env);

			if (pe->pscc != e->pscc) {
				/* not in the same SCC, must be the only input */
				if (! out_rc) {
					out_rc = pred;
				} else if (out_rc != pred) {
					return;
				}
			}
		}
	}
	/* found a Phi cycle */
	DB((dbg, LEVEL_2, "  Found an USELESS Phi cycle:\n  "));

	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		next = e->next;
		e->header = NULL;
		exchange(irn, out_rc);
	}
	++env->replaced;
}

/**
 * Process a SCC for the Phi cycle removement.
 *
 * @param pscc  the SCC
 * @param env   the environment
 */
static void process_phi_only_scc(scc *pscc, iv_env *env) {
	ir_node *head = pscc->head;
	node_entry *e = get_irn_link(head);

#ifdef DEBUG_libfirm
	{
		ir_node *irn, *next;

		DB((dbg, LEVEL_4, " SCC at %p:\n ", pscc));
		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = get_irn_link(irn);

			next = e->next;

			DB((dbg, LEVEL_4, " %+F,", irn));
		}
		DB((dbg, LEVEL_4, "\n"));
	}
#endif

	if (e->next != NULL)
		remove_phi_cycle(pscc, env);
}


/**
 * Push a node onto the stack.
 *
 * @param env   the environment
 * @param n     the node to push
 */
static void push(iv_env *env, ir_node *n) {
	node_entry *e;

	if (env->tos == ARR_LEN(env->stack)) {
		int nlen = ARR_LEN(env->stack) * 2;
		ARR_RESIZE(ir_node *, env->stack, nlen);
	}
	env->stack[env->tos++] = n;
	e = get_irn_ne(n, env);
	e->in_stack = 1;
}

/**
 * pop a node from the stack
 *
 * @param env   the environment
 *
 * @return  The topmost node
 */
static ir_node *pop(iv_env *env)
{
	ir_node *n = env->stack[--env->tos];
	node_entry *e = get_irn_ne(n, env);

	e->in_stack = 0;
	return n;
}

/**
 * Do Tarjan's SCC algorithm and drive OSR.
 *
 * @param irn  start at this node
 * @param env  the environment
 */
static void dfs(ir_node *irn, iv_env *env)
{
	int i, n;
	node_entry *node = get_irn_ne(irn, env);

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
			node_entry *o = get_irn_ne(pred, env);

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
				e = get_irn_ne(x, env);
				e->pscc    = pscc;
				e->next    = pscc->head;
				pscc->head = x;
			} while (x != irn);

			env->process_scc(pscc, env);
		}
	}
}

/**
 * Do the DFS by starting at the End node of a graph.
 *
 * @param irg  the graph to process
 * @param env  the environment
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
	node_entry *e = get_irn_ne(block, env);

	e->POnum = env->POnum++;
}

#if 0
/**
 * Follows the LFTR edges and return the last node in the chain.
 *
 * @param irn  the node that should be followed
 * @param env  the IV environment
 *
 * @note
 * In the current implementation only the last edge is stored, so
 * only one chain exists. That's why we might miss some opportunities.
 */
static ir_node *followEdges(ir_node *irn, iv_env *env) {
	for (;;) {
		LFTR_edge *e = LFTR_find(irn, env);
		if (e)
			irn = e->dst;
		else
			return irn;
	}
}

/**
 * Apply one LFTR edge operation.
 * Return NULL if the transformation cannot be done safely without
 * an Overflow.
 *
 * @param rc   the IV node that should be translated
 * @param e    the LFTR edge
 * @param env  the IV environment
 *
 * @return the translated region constant or NULL
 *         if the translation was not possible
 *
 * @note
 * In the current implementation only the last edge is stored, so
 * only one chain exists. That's why we might miss some opportunities.
 */
static ir_node *applyOneEdge(ir_node *rc, LFTR_edge *e, iv_env *env) {
	if (env->flags & osr_flag_lftr_with_ov_check) {
		tarval *tv_l, *tv_r, *tv;
		tarval_int_overflow_mode_t ovmode;

		/* overflow can only be decided for Consts */
		if (! is_Const(e->rc)) {
			DB((dbg, LEVEL_4, " = UNKNOWN (%+F)", e->rc));
			return NULL;
		}

		tv_l = get_Const_tarval(rc);
		tv_r = get_Const_tarval(e->rc);

		ovmode = tarval_get_integer_overflow_mode();
		tarval_set_integer_overflow_mode(TV_OVERFLOW_BAD);

		switch (e->code) {
		case iro_Mul:
			tv = tarval_mul(tv_l, tv_r);
			DB((dbg, LEVEL_4, " * %+F", tv_r));
			break;
		case iro_Add:
			tv = tarval_add(tv_l, tv_r);
			DB((dbg, LEVEL_4, " + %+F", tv_r));
			break;
		case iro_Sub:
			tv = tarval_sub(tv_l, tv_r);
			DB((dbg, LEVEL_4, " - %+F", tv_r));
			break;
		default:
			assert(0);
			tv = tarval_bad;
		}
		tarval_set_integer_overflow_mode(ovmode);

		if (tv == tarval_bad) {
			DB((dbg, LEVEL_4, " = OVERFLOW"));
			return NULL;
		}
		return new_r_Const(current_ir_graph, get_irn_n(rc, -1), get_tarval_mode(tv), tv);
	}
	return do_apply(e->code, NULL, rc, e->rc, get_irn_mode(rc));
}

/**
 * Applies the operations represented by the LFTR edges to a
 * region constant and returns the value.
 * Return NULL if the transformation cannot be done safely without
 * an Overflow.
 *
 * @param iv   the IV node that starts the LFTR edge chain
 * @param rc   the region constant that should be translated
 * @param env  the IV environment
 *
 * @return the translated region constant or NULL
 *         if the translation was not possible
 */
static ir_node *applyEdges(ir_node *iv, ir_node *rc, iv_env *env) {
	ir_node *irn = iv;

	if (env->flags & osr_flag_lftr_with_ov_check) {
		/* overflow can only be decided for Consts */
		if (! is_Const(rc)) {
			DB((dbg, LEVEL_4, " = UNKNOWN (%+F)\n", rc));
			return NULL;
		}
		DB((dbg, LEVEL_4, "%+F", get_Const_tarval(rc)));
	}

	for (irn = iv; rc;) {
		LFTR_edge *e = LFTR_find(irn, env);
		if (e) {
			rc = applyOneEdge(rc, e, env);
			irn = e->dst;
		}
		else
			break;
	}
	DB((dbg, LEVEL_3, "\n"));
	return rc;
}

/**
 * Walker, finds Cmp(iv, rc) or Cmp(rc, iv)
 * and tries to optimize them.
 */
static void do_lftr(ir_node *cmp, void *ctx) {
	iv_env *env = ctx;
	ir_node *left, *right, *liv, *riv;
	ir_node *iv, *rc;
	ir_node *nleft = NULL, *nright = NULL;

	if (get_irn_op(cmp) != op_Cmp)
		return;

	left  = get_Cmp_left(cmp);
	right = get_Cmp_right(cmp);

	liv = is_iv(left, env);
	riv = is_iv(right, env);
	if (liv && is_rc(right, liv)) {
		iv = left; rc = right;

		nright = applyEdges(iv, rc, env);
		if (nright && nright != rc) {
			nleft = followEdges(iv, env);
		}
	}
	else if (riv && is_rc(left, riv)) {
		iv = right; rc = left;

		nleft = applyEdges(iv, rc, env);
		if (nleft && nleft != rc) {
			nright = followEdges(iv, env);
		}
	}

	if (nleft && nright) {
		DB((dbg, LEVEL_2, "  LFTR for %+F\n", cmp));
		set_Cmp_left(cmp, nleft);
		set_Cmp_right(cmp, nright);
		++env->lftr_replaced;
	}
}

/**
 * do linear function test replacement.
 *
 * @param irg   the graph that should be optimized
 * @param env   the IV environment
 */
static void lftr(ir_graph *irg, iv_env *env) {
	irg_walk_graph(irg, NULL, do_lftr, env);
}
#endif

/**
 * Pre-walker: set all node links to NULL and fix the
 * block of Proj nodes.
 */
static void clear_and_fix(ir_node *irn, void *env)
{
	set_irn_link(irn, NULL);

	if (is_Proj(irn)) {
		ir_node *pred = get_Proj_pred(irn);
		set_irn_n(irn, -1, get_irn_n(pred, -1));
	}
}

/* Performs Operator Strength Reduction for the passed graph. */
void opt_osr(ir_graph *irg, unsigned flags) {
	iv_env   env;
	ir_graph *rem;

	if (! get_opt_strength_red()) {
		/* only kill Phi cycles  */
		remove_phi_cycles(irg);
		return;
	}

	rem = current_ir_graph;
	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.opt.osr");

	DB((dbg, LEVEL_1, "Doing Operator Strength Reduction for %+F\n", irg));

	obstack_init(&env.obst);
	env.stack         = NEW_ARR_F(ir_node *, 128);
	env.tos           = 0;
	env.nextDFSnum    = 0;
	env.POnum         = 0;
	env.quad_map      = new_set(quad_cmp, 64);
	env.lftr_edges    = new_set(LFTR_cmp, 64);
	env.replaced      = 0;
	env.lftr_replaced = 0;
	env.flags         = flags;
	env.process_scc   = process_scc;

	/* Clear all links and move Proj nodes into the
	   the same block as it's predecessors.
	   This can improve the placement of new nodes.
	 */
	irg_walk_graph(irg, NULL, clear_and_fix, NULL);

	/* we need dominance */
	assure_doms(irg);
	assure_irg_outs(irg);

	/* calculate the post order number for blocks. */
	irg_out_block_walk(get_irg_start_block(irg), NULL, assign_po, &env);

	/* calculate the SCC's and drive OSR. */
	do_dfs(irg, &env);

	if (env.replaced) {
		/* try linear function test replacements */
		//lftr(irg, &env);

		set_irg_outs_inconsistent(irg);
		DB((dbg, LEVEL_1, "Replacements: %u + %u (lftr)\n\n", env.replaced, env.lftr_replaced));
	}

	del_set(env.lftr_edges);
	del_set(env.quad_map);
	DEL_ARR_F(env.stack);
	obstack_free(&env.obst, NULL);

	current_ir_graph = rem;
}

/* Remove any Phi cycles with only one real input. */
void remove_phi_cycles(ir_graph *irg) {
	iv_env   env;
	ir_graph *rem;

	rem = current_ir_graph;
	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.opt.remove_phi");

	DB((dbg, LEVEL_1, "Doing Phi cycle removement for %+F\n", irg));

	obstack_init(&env.obst);
	env.stack         = NEW_ARR_F(ir_node *, 128);
	env.tos           = 0;
	env.nextDFSnum    = 0;
	env.POnum         = 0;
	env.quad_map      = NULL;
	env.lftr_edges    = NULL;
	env.replaced      = 0;
	env.lftr_replaced = 0;
	env.flags         = 0;
	env.process_scc   = process_phi_only_scc;

	/* Clear all links and move Proj nodes into the
	   the same block as it's predecessors.
	   This can improve the placement of new nodes.
	 */
	irg_walk_graph(irg, NULL, clear_and_fix, NULL);

	/* we need dominance */
	assure_irg_outs(irg);

	/* calculate the post order number for blocks. */
	irg_out_block_walk(get_irg_start_block(irg), NULL, assign_po, &env);

	/* calculate the SCC's and drive OSR. */
	do_dfs(irg, &env);

	if (env.replaced) {
		set_irg_outs_inconsistent(irg);
                DB((dbg, LEVEL_1, "remove_phi_cycles: %u Cycles removed\n\n", env.replaced));
	}

	DEL_ARR_F(env.stack);
	obstack_free(&env.obst, NULL);

	current_ir_graph = rem;
}
