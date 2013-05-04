/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Operator Strength Reduction.
 * @date    12.5.2006
 * @author  Michael Beck
 * @brief
 *  Implementation of the Operator Strength Reduction algorithm
 *  by Keith D. Cooper, L. Taylor Simpson, Christopher A. Vick.
 *  Extended version.
 */
#include "adt/pdeq.h"
#include "iroptimize.h"
#include "ircons.h"
#include "irop_t.h"
#include "irdom.h"
#include "irgmod.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "irouts.h"
#include "iredges.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "tv.h"
#include "hashptr.h"
#include "util.h"
#include "irtools.h"
#include "irloop_t.h"
#include "array.h"
#include "firmstat_t.h"
#include "error.h"
#include "irpass_t.h"

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** A scc. */
typedef struct scc {
	ir_node   *head; /**< the head of the list */
	ir_tarval *init; /**< the init value iff only one exists. */
	ir_tarval *incr; /**< the induction variable increment if only a single const exists. */
	unsigned   code; /**< == iro_Add if +incr, iro_Sub if -incr, 0 if not analysed, iro_Bad else */
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
	size_t   tos;           /**< tos index */
	unsigned nextDFSnum;    /**< the current DFS number */
	unsigned POnum;         /**< current post order number */
	set      *quad_map;     /**< a map from (op, iv, rc) to node */
	set      *lftr_edges;   /**< the set of lftr edges */
	unsigned replaced;      /**< number of replaced ops */
	unsigned lftr_replaced; /**< number of applied linear function test replacements */
	unsigned osr_flags;     /**< additional flags steering the transformation */
	unsigned need_postpass; /**< set, if a post pass is needed to fix Add and Sub nodes */
	/** Function called to process a SCC. */
	void (*process_scc)(scc *pscc, struct iv_env *env);
} iv_env;

/**
 * An entry in the (op, node, node) -> node map.
 */
typedef struct quadruple_t {
	unsigned   code; /**< the opcode of the reduced operation */
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
	unsigned  code;   /**< the opcode that must be applied */
	ir_node   *rc;    /**< the region const that must be applied */
} LFTR_edge;

/* forward */
static ir_node *reduce(ir_node *orig, ir_node *iv, ir_node *rc, iv_env *env);

/**
 * Compare two LFTR edges.
 */
static int LFTR_cmp(const void *e1, const void *e2, size_t size)
{
	const LFTR_edge *l1 = (const LFTR_edge*)e1;
	const LFTR_edge *l2 = (const LFTR_edge*)e2;
	(void) size;

	return l1->src != l2->src;
}

/**
 * Find a LFTR edge.
 *
 * @param src  the source node of the transition
 */
static LFTR_edge *LFTR_find(ir_node *src, iv_env *env)
{
	LFTR_edge key;

	key.src  = src;

	return set_find(LFTR_edge, env->lftr_edges, &key, sizeof(key), hash_ptr(src));
}

/**
 * Add a LFTR edge.
 *
 * @param src   the source node of the edge
 * @param dst   the destination node of the edge
 * @param code  the opcode of the transformed transition
 * @param rc    the region const used in the transition
 * @param env   the environment
 */
static void LFTR_add(ir_node *src, ir_node *dst, unsigned code, ir_node *rc, iv_env *env)
{
	LFTR_edge key;

	key.src  = src;
	key.dst  = dst;
	key.code = code;
	key.rc   = rc;

	/*
	 * There might be more than one edge here. This is rather bad
	 * because we currently store only one.
	 */
	(void)set_insert(LFTR_edge, env->lftr_edges, &key, sizeof(key), hash_ptr(src));
}

/**
 * Gets the node_entry of a node.
 *
 * @param irn  the node
 * @param env  the environment
 */
static node_entry *get_irn_ne(ir_node *irn, iv_env *env)
{
	node_entry *e = (node_entry*)get_irn_link(irn);

	if (e == NULL) {
		e = OALLOCZ(&env->obst, node_entry);
		set_irn_link(irn, e);
	}
	return e;
}

/**
 * Gets the scc from an induction variable.
 *
 * @param iv   any node of the induction variable
 * @param env  the environment
 */
static scc *get_iv_scc(ir_node *iv, iv_env *env)
{
	node_entry *e = get_irn_ne(iv, env);
	return e->pscc;
}

/**
 * Check if irn is an IV.
 *
 * @param irn  the node to check
 * @param env  the environment
 *
 * @returns the header if it is one, NULL else
 */
static ir_node *is_iv(ir_node *irn, iv_env *env)
{
	return get_irn_ne(irn, env)->header;
}

/**
 * Check if irn is a region constant.
 * The block or irn must strictly dominate the header block.
 *
 * @param irn           the node to check
 * @param header_block  the header block of the induction variable
 */
static int is_rc(ir_node *irn, ir_node *header_block)
{
	ir_node *block = get_nodes_block(irn);

	return (block != header_block) && block_dominates(block, header_block);
}

/**
 * Set compare function for the quad set.
 */
static int quad_cmp(const void *e1, const void *e2, size_t size)
{
	const quadruple_t *c1 = (const quadruple_t*)e1;
	const quadruple_t *c2 = (const quadruple_t*)e2;
	(void) size;

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
static ir_node *search(unsigned code, ir_node *op1, ir_node *op2, iv_env *env)
{
	quadruple_t key, *entry;

	key.code = code;
	key.op1 = op1;
	key.op2 = op2;

	entry = set_find(quadruple_t, env->quad_map, &key, sizeof(key), (code * 9) ^ hash_ptr(op1) ^ hash_ptr(op2));
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
static void add(unsigned code, ir_node *op1, ir_node *op2, ir_node *result, iv_env *env)
{
	quadruple_t key;

	key.code = code;
	key.op1  = op1;
	key.op2  = op2;
	key.res  = result;

	(void)set_insert(quadruple_t, env->quad_map, &key, sizeof(key), (code * 9) ^ hash_ptr(op1) ^ hash_ptr(op2));
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
static ir_node *find_location(ir_node *block1, ir_node *block2)
{
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
static ir_node *do_apply(unsigned code, dbg_info *db, ir_node *op1, ir_node *op2, ir_mode *mode)
{
	ir_node *result;
	ir_node *block = find_location(get_nodes_block(op1), get_nodes_block(op2));

	switch (code) {
	case iro_Mul:
		result = new_rd_Mul(db, block, op1, op2, mode);
		break;
	case iro_Add:
		result = new_rd_Add(db, block, op1, op2, mode);
		break;
	case iro_Sub:
		result = new_rd_Sub(db, block, op1, op2, mode);
		break;
	default:
		panic("Unsupported opcode");
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
static ir_node *apply(ir_node *header, ir_node *orig, ir_node *op1, ir_node *op2, iv_env *env)
{
	unsigned code = get_irn_opcode(orig);
	ir_node *result = search(code, op1, op2, env);

	if (result == NULL) {
		dbg_info *db = get_irn_dbg_info(orig);
		ir_node *op1_header = get_irn_ne(op1, env)->header;
		ir_node *op2_header = get_irn_ne(op2, env)->header;

		if (op1_header == header && is_rc(op2, op1_header)) {
			result = reduce(orig, op1, op2, env);
		}
		else if (op2_header == header && is_rc(op1, op2_header)) {
			result = reduce(orig, op2, op1, env);
		}
		else {
			result = do_apply(code, db, op1, op2, get_irn_mode(orig));
			get_irn_ne(result, env)->header = NULL;
		}
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
static ir_node *reduce(ir_node *orig, ir_node *iv, ir_node *rc, iv_env *env)
{
	unsigned code = get_irn_opcode(orig);
	ir_node *result = search(code, iv, rc, env);

	/* check if we have already done this operation on the iv */
	if (result == NULL) {
		node_entry *e, *iv_e;
		int i;
		ir_mode *mode = get_irn_mode(orig);

		result = exact_copy(iv);

		if (get_irn_mode(result) != mode) {
			/*
			 * Beware: we must always create a new induction variable with the same mode
			 * as the node we are replacing. Especially this means the mode might be changed
			 * from P to I and back. This is always possible, because we have only Phi, Add
			 * and Sub nodes.
			 * However, this might lead to AddIs(Iu,Is) which we must fix. The best way to do this
			 * seems to be a post-pass, or we might end with useless Conv's.
			 */
			set_irn_mode(result, mode);
			env->need_postpass = 1;
		}
		add(code, iv, rc, result, env);
		DB((dbg, LEVEL_3, "   Created new %+F for %+F (%s %+F)\n", result, iv,
			get_irn_opname(orig), rc));

		iv_e = get_irn_ne(iv, env);
		e    = get_irn_ne(result, env);
		e->header = iv_e->header;

		/* create the LFTR edge */
		LFTR_add(iv, result, code, rc, env);

		for (i = get_irn_arity(result) - 1; i >= 0; --i) {
			ir_node *o = get_irn_n(result, i);

			e = get_irn_ne(o, env);
			if (e->header == iv_e->header)
				o = reduce(orig, o, rc, env);
			else if (is_Phi(result) || code == iro_Mul)
				o = apply(iv_e->header, orig, o, rc, env);
			set_irn_n(result, i, o);
		}
	} else {
		DB((dbg, LEVEL_3, "   Already Created %+F for %+F (%s %+F)\n", result, iv,
			get_irn_opname(orig), rc));
	}
	return result;
}

/**
 * Update the scc for a newly created IV.
 */
static void update_scc(ir_node *iv, node_entry *e, iv_env *env)
{
	scc     *pscc   = e->pscc;
	ir_node *header = e->header;
	waitq    *wq = new_waitq();

	DB((dbg, LEVEL_2, "  Creating SCC for new an induction variable:\n  "));
	pscc->head = NULL;
	waitq_put(wq, iv);
	do {
		ir_node    *irn = (ir_node*)waitq_get(wq);
		node_entry *ne  = get_irn_ne(irn, env);
		int        i;

		ne->pscc   = pscc;
		ne->next   = pscc->head;
		pscc->head = irn;
		DB((dbg, LEVEL_2, " %+F,", irn));

		for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
			ir_node    *pred = get_irn_n(irn, i);
			node_entry *pe   = get_irn_ne(pred, env);

			if (pe->header == header && pe->pscc == NULL) {
				/* set the pscc here to ensure that the node is NOT enqueued another time */
				pe->pscc = pscc;
				waitq_put(wq, pred);
			}
		}
	} while (! waitq_empty(wq));
	del_waitq(wq);
	DB((dbg, LEVEL_2, "\n"));
}

/**
 * The Replace operation. We found a node representing iv (+,-,*) rc
 * that can be removed by replacing the induction variable iv by a new
 * one that 'applies' the operation 'irn'.
 *
 * @param irn   the node that will be replaced
 * @param iv    the induction variable
 * @param rc    the region constant
 * @param env   the environment
 */
static int replace(ir_node *irn, ir_node *iv, ir_node *rc, iv_env *env)
{
	ir_node *result;

	DB((dbg, LEVEL_2, "  Replacing %+F\n", irn));

	result = reduce(irn, iv, rc, env);
	if (result != irn) {
		node_entry *e;

		hook_strength_red(get_irn_irg(irn), irn);
		exchange(irn, result);
		e = get_irn_ne(result, env);
		if (e->pscc == NULL) {
			e->pscc = OALLOCZ(&env->obst, scc);
			update_scc(result, e, env);
		}
		++env->replaced;
		return 1;
	}
	return 0;
}

/**
 * Check if an IV represents a counter with constant limits.
 *
 * @param iv    any node of the induction variable
 * @param env   the environment
 */
static int is_counter_iv(ir_node *iv, iv_env *env)
{
	node_entry *e         = get_irn_ne(iv, env);
	scc        *pscc      = e->pscc;
	ir_node    *have_init = NULL;
	ir_node    *have_incr = NULL;
	ir_opcode  code       = iro_Bad;
	ir_node    *irn;

	if (pscc->code != 0) {
		/* already analysed */
		return pscc->code != iro_Bad;
	}

	pscc->code = iro_Bad;
	for (irn = pscc->head; irn != NULL; irn = e->next) {
		if (is_Add(irn)) {
			if (have_incr != NULL)
				return 0;

			have_incr = get_Add_right(irn);
			if (! is_Const(have_incr)) {
				have_incr = get_Add_left(irn);
				if (! is_Const(have_incr))
					return 0;
			}
			code = iro_Add;
		} else if (is_Sub(irn)) {
			if (have_incr != NULL)
				return 0;

			have_incr = get_Sub_right(irn);
			if (! is_Const(have_incr))
				return 0;
			code = iro_Sub;
		} else if (is_Phi(irn)) {
			int i;

			for (i = get_Phi_n_preds(irn) - 1; i >= 0; --i) {
				ir_node    *pred = get_Phi_pred(irn, i);
				node_entry *ne   = get_irn_ne(pred, env);

				if (ne->header == e->header)
					continue;
				if (have_init != NULL)
					return 0;
				have_init = pred;
				if (! is_Const(pred))
					return 0;
			}
		} else
			return 0;
		e = get_irn_ne(irn, env);
	}
	pscc->init = get_Const_tarval(have_init);
	pscc->incr = get_Const_tarval(have_incr);
	pscc->code = code;
	return code != iro_Bad;
}

/**
 * Check the users of an induction variable for register pressure.
 *
 * @param iv    any node of the induction variable
 * @param env   the environment
 *
 * @return non-zero if the register pressure is estimated
 *         to not increase, zero else
 */
static int check_users_for_reg_pressure(ir_node *iv, iv_env *env)
{
	ir_node    *irn;
	ir_node    *have_user = NULL;
	ir_node    *have_cmp  = NULL;
	node_entry *e         = get_irn_ne(iv, env);
	scc        *pscc      = e->pscc;

	for (irn = pscc->head; irn != NULL; irn = e->next) {
		foreach_out_edge(irn, edge) {
			ir_node    *user = get_edge_src_irn(edge);
			node_entry *ne = get_irn_ne(user, env);

			if (e->header == ne->header) {
				/* found user from the same IV */
				continue;
			}
			if (is_Cmp(user)) {
				if (have_cmp != NULL) {
					/* more than one cmp, for now end here */
					return 0;
				}
				have_cmp = user;
			} else {
				/* user is a real user of the IV */
				if (have_user != NULL) {
					/* found the second user */
					return 0;
				}
				have_user = user;
			}
		}
		e = get_irn_ne(irn, env);
	}

	if (have_user == NULL) {
		/* no user, ignore */
		return 1;
	}

	if (have_cmp == NULL) {
		/* fine, only one user, try to reduce */
		return 1;
	}
	/*
	 * We found one user AND at least one cmp.
	 * We should check here if we can transform the Cmp.
	 *
	 * For now our capabilities for doing linear function test
	 * are limited, so check if the iv has the right form: Only ONE
	 * Phi, only one Add/Sub with a Const.
	 */
	if (! is_counter_iv(iv, env))
		return 0;

	/*
	 * Ok, we have only one increment AND it is a Const, we might be able
	 * to do a linear function test replacement, so go on.
	 */
	return 1;
}

/**
 * Check if a node can be replaced (+, -, *).
 *
 * @param irn   the node to check
 * @param env   the environment
 *
 * @return non-zero if irn should be Replace'd
 */
static int check_replace(ir_node *irn, iv_env *env)
{
	ir_node   *left, *right, *iv, *rc;
	ir_op     *op  = get_irn_op(irn);
	unsigned  code = get_op_code(op);
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
			if (env->osr_flags & osr_flag_keep_reg_pressure) {
				if (! check_users_for_reg_pressure(iv, env))
					return 0;
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
static void classify_iv(scc *pscc, iv_env *env)
{
	ir_node *irn, *next, *header = NULL;
	node_entry *b, *h = NULL;
	int j, only_phi, num_outside;
	ir_node *out_rc;

	/* find the header block for this scc */
	for (irn = pscc->head; irn; irn = next) {
		node_entry *e = (node_entry*)get_irn_link(irn);
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
		case iro_Sub:
			only_phi = 0;
			{
				ir_node    *left        = get_Sub_left(irn);
				node_entry *left_entry  = get_irn_ne(left, env);
				ir_node    *right       = get_Sub_right(irn);
				node_entry *right_entry = get_irn_ne(right, env);

				if (left_entry->pscc != e->pscc ||
				    (right_entry->pscc != e->pscc && !is_rc(right, header))) {
					/*
					 * Not an induction variable.
					 * Region constant are only allowed on right hand side.
					 */
					goto fail;
				}
			}
			break;

		case iro_Add:
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
		e->header = NULL;
	}
}

/**
 * Process an SCC for the operator strength reduction.
 *
 * @param pscc  the SCC
 * @param env   the environment
 */
static void process_scc(scc *pscc, iv_env *env)
{
	ir_node *head = pscc->head;
	node_entry *e = (node_entry*)get_irn_link(head);

#ifdef DEBUG_libfirm
	{
		ir_node *irn, *next;

		DB((dbg, LEVEL_4, " SCC at %p:\n ", pscc));
		for (irn = pscc->head; irn != NULL; irn = next) {
			node_entry *e = (node_entry*)get_irn_link(irn);

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
 *
 * @param pscc  an SCC that consists of Phi nodes only
 * @param env   the environment
 */
static void remove_phi_cycle(scc *pscc, iv_env *env)
{
	ir_node *irn, *next;
	int j;
	ir_node *out_rc;

	/* check if this scc contains only Phi nodes */
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
 * Process a SCC for the Phi cycle remove.
 *
 * @param pscc  the SCC
 * @param env   the environment
 */
static void process_phi_only_scc(scc *pscc, iv_env *env)
{
	ir_node *head = pscc->head;
	node_entry *e = (node_entry*)get_irn_link(head);

#ifdef DEBUG_libfirm
	{
		ir_node *irn, *next;

		DB((dbg, LEVEL_4, " SCC at %p:\n ", pscc));
		for (irn = pscc->head; irn; irn = next) {
			node_entry *e = (node_entry*)get_irn_link(irn);

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
static void push(iv_env *env, ir_node *n)
{
	node_entry *e;

	if (env->tos == ARR_LEN(env->stack)) {
		size_t nlen = ARR_LEN(env->stack) * 2;
		ARR_RESIZE(ir_node *, env->stack, nlen);
	}
	env->stack[env->tos++] = n;
	e = get_irn_ne(n, env);
	e->in_stack = 1;
}

/**
 * Pop a node from the stack.
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

			if (!irn_visited(pred))
				dfs(pred, env);
		}
	} else {
		ir_node *block = get_nodes_block(irn);

		node->DFSnum = env->nextDFSnum++;
		node->low    = node->DFSnum;
		push(env, irn);

		/* handle the block */
		if (!irn_visited(block))
			dfs(block, env);

		n = get_irn_arity(irn);
		for (i = 0; i < n; ++i) {
			ir_node *pred = get_irn_n(irn, i);
			node_entry *o = get_irn_ne(pred, env);

			if (!irn_visited(pred)) {
				dfs(pred, env);
				node->low = MIN(node->low, o->low);
			}
			if (o->DFSnum < node->DFSnum && o->in_stack)
				node->low = MIN(o->DFSnum, node->low);
		}
		if (node->low == node->DFSnum) {
			scc *pscc = OALLOCZ(&env->obst, scc);
			ir_node *x;

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
static void do_dfs(ir_graph *irg, iv_env *env)
{
	ir_node  *end = get_irg_end(irg);
	int i;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);

	inc_irg_visited(irg);

	/* visit all visible nodes */
	dfs(end, env);

	/* visit the keep-alives */
	for (i = get_End_n_keepalives(end) - 1; i >= 0; --i) {
		ir_node *ka = get_End_keepalive(end, i);

		if (!irn_visited(ka))
			dfs(ka, env);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

/**
 * Post-block-walker: assign the post-order number.
 */
static void assign_po(ir_node *block, void *ctx)
{
	iv_env *env = (iv_env*)ctx;
	node_entry *e = get_irn_ne(block, env);

	e->POnum = env->POnum++;
}

/**
 * Apply one LFTR edge operation.
 * Return NULL if the transformation cannot be done safely without
 * an Overflow.
 *
 * @param iv   the induction variable
 * @param rc   the constant that should be translated
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
static ir_node *applyOneEdge(ir_node *iv, ir_node *rc, LFTR_edge *e, iv_env *env)
{
	if (env->osr_flags & osr_flag_lftr_with_ov_check) {
		ir_tarval *tv_l, *tv_r, *tv, *tv_init, *tv_incr, *tv_end;
		tarval_int_overflow_mode_t ovmode;
		scc *pscc;
		ir_graph *irg;

		if (! is_counter_iv(iv, env)) {
			DB((dbg, LEVEL_4, " not counter IV"));
			return NULL;
		}

		/* overflow can only be decided for Consts */
		if (! is_Const(e->rc)) {
			if (e->code == iro_Add && mode_is_reference(get_irn_mode(e->rc))) {
				/* However we allow ONE Pointer Add, as pointer arithmetic with wrap
				   around is undefined anyway */
				return do_apply(e->code, NULL, rc, e->rc, get_irn_mode(e->rc));
			}
			DB((dbg, LEVEL_4, " = UNKNOWN (%+F)", e->rc));
			return NULL;
		}

		tv_l = get_Const_tarval(rc);
		tv_r = get_Const_tarval(e->rc);

		ovmode = tarval_get_integer_overflow_mode();
		tarval_set_integer_overflow_mode(TV_OVERFLOW_BAD);

		pscc    = get_iv_scc(iv, env);
		tv_incr = pscc->incr;
		tv_init = pscc->init;

		/*
		 * Check that no overflow occurs:
		 * init must be transformed without overflow
		 * the new rc must be transformed without overflow
		 * rc +/- incr must be possible without overflow
		 */
		switch (e->code) {
		case iro_Mul:
			tv      = tarval_mul(tv_l, tv_r);
			tv_init = tarval_mul(tv_init, tv_r);
			tv_incr = tarval_mul(tv_incr, tv_r);
			DB((dbg, LEVEL_4, " * %+F", tv_r));
			break;
		case iro_Add:
			tv      = tarval_add(tv_l, tv_r);
			tv_init = tarval_add(tv_init, tv_r);
			DB((dbg, LEVEL_4, " + %+F", tv_r));
			break;
		case iro_Sub:
			tv      = tarval_sub(tv_l, tv_r, NULL);
			tv_init = tarval_sub(tv_init, tv_r, NULL);
			DB((dbg, LEVEL_4, " - %+F", tv_r));
			break;
		default:
			panic("Unsupported opcode");
		}

		if (tv == tarval_bad || tv_init == tarval_bad) {
			tarval_set_integer_overflow_mode(ovmode);
			DB((dbg, LEVEL_4, " = OVERFLOW"));
			return NULL;
		}

		if (pscc->code == iro_Add) {
			tv_end = tarval_add(tv, tv_incr);
		} else {
			assert(pscc->code == iro_Sub);
			tv_end = tarval_sub(tv, tv_incr, NULL);
		}

		tarval_set_integer_overflow_mode(ovmode);

		if (tv_end == tarval_bad) {
			DB((dbg, LEVEL_4, " = OVERFLOW"));
			return NULL;
		}
		irg = get_irn_irg(iv);
		return new_r_Const(irg, tv);
	}
	return do_apply(e->code, NULL, rc, e->rc, get_irn_mode(e->dst));
}

/**
 * Applies the operations represented by the LFTR edges to a
 * region constant and returns the value.
 * Return NULL if the transformation cannot be done safely without
 * an Overflow.
 *
 * @param pIV  points to the IV node that starts the LFTR edge chain
 *             after translation points to the new IV
 * @param rc   the region constant that should be translated
 * @param env  the IV environment
 *
 * @return the translated region constant or NULL
 *         if the translation was not possible
 */
static ir_node *applyEdges(ir_node **pIV, ir_node *rc, iv_env *env)
{
	ir_node *iv = *pIV;
	if (env->osr_flags & osr_flag_lftr_with_ov_check) {
		/* overflow can only be decided for Consts */
		if (! is_counter_iv(iv, env)) {
			DB((dbg, LEVEL_4, "not counter IV\n", rc));
			return NULL;
		}
		if (! is_Const(rc)) {
			DB((dbg, LEVEL_4, " = UNKNOWN (%+F)\n", rc));
			return NULL;
		}
		DB((dbg, LEVEL_4, "%+F", get_Const_tarval(rc)));
	}

	for (; rc;) {
		LFTR_edge *e = LFTR_find(iv, env);
		if (e != NULL) {
			rc = applyOneEdge(iv, rc, e, env);
			iv = e->dst;
		} else
			break;
	}
	DB((dbg, LEVEL_3, "\n"));
	*pIV = iv;
	return rc;
}

/**
 * Walker, finds Cmp(iv, rc) or Cmp(rc, iv)
 * and tries to optimize them.
 */
static void do_lftr(ir_node *cmp, void *ctx)
{
	iv_env *env = (iv_env*)ctx;
	ir_node *left, *right, *liv, *riv;
	ir_node *iv, *rc;
	ir_node *nleft = NULL, *nright = NULL;

	if (!is_Cmp(cmp))
		return;

	left  = get_Cmp_left(cmp);
	right = get_Cmp_right(cmp);

	liv = is_iv(left, env);
	riv = is_iv(right, env);
	if (liv && is_rc(right, liv)) {
		iv = left; rc = right;

		nright = applyEdges(&iv, rc, env);
		nleft  = iv;
	}
	else if (riv && is_rc(left, riv)) {
		iv = right; rc = left;

		nleft  = applyEdges(&iv, rc, env);
		nright = iv;
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
static void lftr(ir_graph *irg, iv_env *env)
{
	irg_walk_graph(irg, NULL, do_lftr, env);
}

/* Remove any Phi cycles with only one real input. */
void remove_phi_cycles(ir_graph *irg)
{
	iv_env env;

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

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
	env.osr_flags     = 0;
	env.need_postpass = 0;
	env.process_scc   = process_phi_only_scc;

	/* Clear all links and move Proj nodes into the
	 * the same block as their predecessors.
	 * This can improve the placement of new nodes.
	 */
	irg_walk_graph(irg, NULL, firm_clear_link, NULL);

	/* calculate the post order number for blocks. */
	irg_out_block_walk(get_irg_start_block(irg), NULL, assign_po, &env);

	/* calculate the SCC's and drive OSR. */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	do_dfs(irg, &env);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	if (env.replaced) {
		DB((dbg, LEVEL_1, "remove_phi_cycles: %u Cycles removed\n\n",
		    env.replaced));
	}

	DEL_ARR_F(env.stack);
	obstack_free(&env.obst, NULL);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}

ir_graph_pass_t *remove_phi_cycles_pass(const char *name)
{
	return def_graph_pass(name ? name : "remove_phi_cycles", remove_phi_cycles);
}

/**
 * Post-walker: fix Add and Sub nodes that where results of I<->P conversions.
 */
static void fix_adds_and_subs(ir_node *irn, void *ctx)
{
	(void) ctx;

	if (is_Add(irn)) {
		ir_mode *mode = get_irn_mode(irn);

		if (mode_is_int(mode)) {
			ir_node *pred;

			pred = get_Add_left(irn);
			if (get_irn_mode(pred) != mode) {
				ir_node *block = get_nodes_block(pred);

				pred = new_r_Conv(block, pred, mode);
				set_Add_left(irn, pred);
			}
			pred = get_Add_right(irn);
			if (get_irn_mode(pred) != mode) {
				ir_node *block = get_nodes_block(pred);

				pred = new_r_Conv(block, pred, mode);
				set_Add_right(irn, pred);
			}
		}
	} else if (is_Sub(irn)) {
		ir_mode *mode = get_irn_mode(irn);

		if (mode_is_int(mode)) {
			ir_node *left   = get_Sub_left(irn);
			ir_node *right  = get_Sub_right(irn);
			ir_mode *l_mode = get_irn_mode(left);
			ir_mode *r_mode = get_irn_mode(right);

			if (mode_is_int(l_mode) && mode_is_int(r_mode)) {
				if (l_mode != mode) {
					ir_node *block = get_nodes_block(left);

					left = new_r_Conv(block, left, mode);
					set_Sub_left(irn, left);
				}
				if (r_mode != mode) {
					ir_node *block = get_nodes_block(right);

					right = new_r_Conv(block, right, mode);
					set_Sub_right(irn, right);
				}
			}
		} else if (mode_is_reference(mode)) {
			ir_node *left   = get_Sub_left(irn);
			ir_node *right  = get_Sub_right(irn);
			ir_mode *l_mode = get_irn_mode(left);
			ir_mode *r_mode = get_irn_mode(right);
			if (mode_is_int(l_mode)) {
				/* Usually, Sub(I*,P) is an error, hence the verifier rejects it.
				 * However, it is correct in this case, so add Conv to make verifier happy. */
				ir_node *block = get_nodes_block(right);
				ir_node *lconv = new_r_Conv(block, left, r_mode);
				assert(mode_is_reference(r_mode));
				set_Sub_left(irn, lconv);
			}
		}
	}
}

/* Performs Operator Strength Reduction for the passed graph. */
void opt_osr(ir_graph *irg, unsigned flags)
{
	iv_env env;

	FIRM_DBG_REGISTER(dbg, "firm.opt.osr");

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

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
	env.osr_flags     = flags;
	env.need_postpass = 0;
	env.process_scc   = process_scc;

	/* Clear all links and move Proj nodes into the
	 * the same block as its predecessors.
	 * This can improve the placement of new nodes.
	 */
	irg_walk_graph(irg, NULL, firm_clear_link, NULL);

	irg_block_edges_walk(get_irg_start_block(irg), NULL, assign_po, &env);

	/* calculate the SCC's and drive OSR. */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	do_dfs(irg, &env);

	if (env.replaced) {
		if (env.need_postpass)
			irg_walk_graph(irg, NULL, fix_adds_and_subs, &env);

		/* try linear function test replacements */
		lftr(irg, &env);
		(void)lftr;

		DB((dbg, LEVEL_1, "Replacements: %u + %u (lftr)\n\n", env.replaced, env.lftr_replaced));
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	del_set(env.lftr_edges);
	del_set(env.quad_map);
	DEL_ARR_F(env.stack);
	obstack_free(&env.obst, NULL);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}

typedef struct pass_t {
	ir_graph_pass_t pass;
	unsigned        flags;
} pass_t;

/**
* Wrapper for running opt_osr() as an ir_graph pass.
*/
static int pass_wrapper(ir_graph *irg, void *context)
{
	pass_t *pass = (pass_t*)context;
	opt_osr(irg, pass->flags);
	return 0;
}

ir_graph_pass_t *opt_osr_pass(const char *name, unsigned flags)
{
	pass_t *pass = XMALLOCZ(pass_t);

	pass->flags = flags;
	return def_graph_pass_constructor(
		&pass->pass, name ? name : "osr", pass_wrapper);
}
