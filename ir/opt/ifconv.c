/**
 * If conversion.
 * Make Mux nodes from Conds where it its possible.
 * @author Sebastian Hack
 * @date 4.2.2005
 */

#include <stdlib.h>
#include <string.h>
#include <alloca.h>

#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irdom_t.h"

#include "ifconv.h"
#include "irflag_t.h"

#include "debug.h"
#include "obst.h"
#include "set.h"
#include "bitset.h"
#include "bitfiddle.h"

#define MAX_DEPTH 4

/*
 * Mux optimization routines.
 */

#if 0
static ir_node *local_optimize_mux(ir_node *mux)
{
	int i, n;
	ir_node *res = mux;
  ir_node *sel = get_Mux_sel(mux);
	ir_node *cmp = skip_Proj(sel);

	/* Optimize the children  */
	for(i = 1, n = get_irn_arity(mux); i < n; ++i) {
		ir_node *operand = get_irn_n(mux, i);
		if(get_irn_op(operand) == op_Mux)
			optimize_mux(operand);
	}

	/* If we have no cmp above the mux, get out. */
	if(is_Proj(sel) && get_irn_mode(sel) == mode_b && get_irn_opcode(cmp) == iro_Cmp) {

		pn_Cmp cc = get_Proj_proj(sel);
		ir_mode *mode = get_irn_mode(mux);
		ir_node *block = get_nodes_block(n);
		ir_node *cmp_left = get_Cmp_left(cmp);
		ir_node *cmp_right = get_Cmp_right(cmp);
		ir_node *mux_true = get_Mux_true(mux);
		ir_node *mux_false = get_Mux_false(mux);

		/*
		 * Check for comparisons with signed integers.
		 */
		if(mode_is_int(mode) 					/* We need an integral mode */
				&& mode_is_signed(mode)   /* which is signed */
				&& cc == Lt) {						/* and have to compare for < */

			/*
			 * Mux(x:T < 0, -1, 0) -> Shrs(x, sizeof_bits(T) - 1)
			 * Conditions:
			 * T must be signed.
			 */
			if(classify_Const(cmp_right) == CNST_NULL
				&& classify_Const(mux_true) == CNST_ALL_ONE
				&& classify_Const(mux_false) == CNST_NULL) {

				ir_mode *u_mode = find_unsigned_mode(mode);

				res = new_r_Shrs(current_ir_graph, block, cmp_left,
						new_r_Const_long(current_ir_graph, block, u_mode,
							get_mode_size_bits(mode) - 1),
						mode);
			}

			/*
			 * Mux(0 < x:T, 1, 0) -> Shr(-x, sizeof_bits(T) - 1)
			 * Conditions:
			 * T must be signed.
			 */
			else if(classify_Const(cmp_left) == CNST_NULL
				&& classify_Const(mux_true) == CNST_ONE
				&& classify_Const(mux_false) == CNST_NULL) {

				ir_mode *u_mode = find_unsigned_mode(mode);

				res = new_r_Shr(current_ir_graph, block,

						/* -x goes to 0 - x in Firm (cmp_left is 0, see the if) */
						new_r_Sub(current_ir_graph, block, cmp_left, cmp_right, mode),

						/* This is sizeof_bits(T) - 1 */
						new_r_Const_long(current_ir_graph, block, u_mode,
							get_mode_size_bits(mode) - 1),
						mode);
			}
		}
	}

	return res;
}
#endif

/**
 * check, if a node is const and return its tarval or
 * return a default tarval.
 * @param cnst The node whose tarval to get.
 * @param or The alternative tarval, if the node is no Const.
 * @return The tarval of @p cnst, if the node is Const, @p otherwise.
 */
static tarval *get_value_or(ir_node *cnst, tarval *or)
{
	return get_irn_op(cnst) == op_Const ? get_Const_tarval(cnst) : or;
}


/**
 * Try to optimize nested muxes into a dis- or conjunction
 * of two muxes.
 * @param mux The parent mux, which has muxes as operands.
 * @return The replacement node for this mux. If the optimization is
 * successful, this might be an And or Or node, if not, its the mux
 * himself.
 */
static ir_node *optimize_mux_chain(ir_node *mux)
{
	int i;
	ir_node *res;
	ir_node *ops[2];
	ir_mode *mode = get_irn_mode(mux);
	tarval *null;
	tarval *minus_one;

	/*
	 * If we have no mux, or its mode is not integer, we
	 * can return.
	 */
	if(get_irn_op(mux) != op_Mux || !mode_is_int(mode))
		return mux;

	res = mux;
	null = get_tarval_null(mode);
	minus_one = tarval_sub(null, get_tarval_one(mode));

	ops[0] = get_Mux_false(mux);
	ops[1] = get_Mux_true(mux);

	for(i = 0; i < 2; ++i) {
		ir_node *a, *b, *d;
		tarval *tva, *tvb, *tvd;
		ir_node *child_mux;

		/*
		 * A mux operand at the first position can be factored
		 * out, if the operands fulfill several conditions:
		 *
		 * mux(c1, mux(c2, a, b), d)
		 *
		 * This can be made into:
		 * 1) mux(c1, 0, d) | mux(c2, a, b)
		 *    if a | d == d and b | d == d
		 *
		 * 2) mux(c1, -1, d) & mux(c2, a, b)
		 *    if a & d == d and a & b == b
		 */
		if(get_irn_op(ops[i]) == op_Mux) {

			child_mux = ops[i];
			a = get_Mux_false(child_mux);
			b = get_Mux_true(child_mux);
			d = ops[1 - i];

			/* Try the or stuff */
			tva = get_value_or(a, minus_one);
			tvb = get_value_or(b, minus_one);
			tvd = get_value_or(d, null);

			if(tarval_cmp(tarval_or(tva, tvd), tvd) == pn_Cmp_Eq
					&& tarval_cmp(tarval_or(tvb, tvd), tvd) == pn_Cmp_Eq) {

				ops[i] = new_Const(mode, null);
				res = new_r_Or(current_ir_graph, get_nodes_block(mux),
						mux, child_mux, mode);
				break;
			}

			/* If the or didn't go, try the and stuff */
			tva = get_value_or(a, null);
			tvb = get_value_or(b, null);
			tvd = get_value_or(d, minus_one);

			if(tarval_cmp(tarval_and(tva, tvd), tvd) == pn_Cmp_Eq
					&& tarval_cmp(tarval_and(tvb, tvd), tvd) == pn_Cmp_Eq) {

				ops[i] = new_Const(mode, minus_one);
				res = new_r_And(current_ir_graph, get_nodes_block(mux),
						mux, child_mux, mode);
				break;
			}
		}
	}

	/* recursively optimize nested muxes. */
	set_irn_n(mux, 1, optimize_mux_chain(ops[0]));
	set_irn_n(mux, 2, optimize_mux_chain(ops[1]));

	return res;
}


/***********************************************************
 * The If conversion itself.
 ***********************************************************/

/**
 * Default options.
 */
static opt_if_conv_info_t default_info = {
	4
};

/** The debugging module. */
static firm_dbg_module_t *dbg;

/**
 * A simple check for sde effects upton an opcode of a ir node.
 * @param irn The ir node to check,
 * @return 1 if the opcode itself may produce side effects, 0 if not.
 */
static INLINE int has_side_effects(const ir_node *irn)
{
	opcode opc = get_irn_opcode(irn);

	if(opc == iro_Cmp)
		return 0;

	return !mode_is_datab(get_irn_mode(irn));
}

/**
 * Decdies, if a given expression and its subexpressions
 * (to certain, also given extent) can be moved to a block.
 * @param expr The expression to examine.
 * @param block The block where the expression should go.
 * @param depth The current depth, passed recursively. Use 0 for
 * non-recursive calls.
 * @param max_depth The maximum depth to which the expression should be
 * examined.
 */
static int _can_move_to(ir_node *expr, ir_node *dest_block, int depth, int max_depth)
{
	int i, n;
	int res = 1;
	ir_node *expr_block = get_nodes_block(expr);


	/*
	 * If we are forced to look too deep into the expression,
	 * treat it like it could not be moved.
	 */
	if(depth >= max_depth) {
		res = 0;
		goto end;
	}

	/*
	 * If the block of the expression dominates the specified
	 * destination block, it does not matter if the expression
	 * has side effects or anything else. It is executed on each
	 * path the destination block is reached.
	 */
	if(block_dominates(expr_block, dest_block))
		goto end;

	/*
	 * We cannot move phis!
	 */
	if(is_Phi(expr)) {
		res = 0;
		goto end;
	}

	/*
	 * This should be superflous and could be converted into a assertion.
	 * The destination block _must_ dominate the block of the expression,
	 * else the expression could be used without its definition.
	 */
	if(!block_dominates(dest_block, expr_block)) {
		res = 0;
		goto end;
	}

	/*
	 * Surely, if the expression does not have a data mode, it is not
	 * movable. Perhaps onw should also test the floating property of
	 * the opcode/node.
	 */
	if(has_side_effects(expr)) {
		res = 0;
		goto end;
	}

	/*
	 * If the node looks alright so far, look at its operands and
	 * check them out. If one of them cannot be moved, this one
	 * cannot be moved either.
	 */
	for(i = 0, n = get_irn_arity(expr); i < n; ++i) {
		ir_node *op = get_irn_n(expr, i);
		int new_depth = is_Proj(op) ? depth : depth + 1;
		if(!_can_move_to(op, dest_block, new_depth, max_depth)) {
			res = 0;
			goto end;
		}
	}

end:
	DBG((dbg, LEVEL_5, "\t\t\t%Dcan move to %n: %d\n", depth, expr, res));

	return res;
}

/**
 * Convenience function for _can_move_to.
 * Checks, if an expression can be moved to another block. The check can
 * be limited to a expression depth meaning if we need to crawl in
 * deeper into an expression than a given threshold to examine if
 * it can be moved, the expression is rejected and the test returns
 * false.
 * @param expr The expression to check for.
 * @param dest_block The destination block you want @p expr to be.
 * @param max_depth The maximum depth @p expr should be investigated.
 * @return 1, if the expression can be moved to the destination block,
 * 0 if not.
 */
static INLINE int can_move_to(ir_node *expr, ir_node *dest_block, int max_depth)
{
	return _can_move_to(expr, dest_block, 0, max_depth);
}

static void move_to(ir_node *expr, ir_node *dest_block)
{
	int i, n;
	ir_node *expr_block = get_nodes_block(expr);

	/*
	 * If we reached the dominator, we are done.
	 * We will never put code through the dominator
	 */
	if(block_dominates(expr_block, dest_block))
		return;

	for(i = 0, n = get_irn_arity(expr); i < n; ++i)
		move_to(get_irn_n(expr, i), dest_block);

	set_nodes_block(expr, dest_block);
}

static ir_node *common_idom(ir_node *b1, ir_node *b2)
{
	if(block_dominates(b1, b2))
		return b1;
	else if(block_dominates(b2, b1))
		return b2;
	else {
		ir_node *p;

		for(p = b1; !block_dominates(p, b2); p = get_Block_idom(p));
		return p;
	}
}

/**
 * Information about a cond node.
 */
typedef struct _cond_t {
	ir_node *cond;					/**< The cond node. */
	ir_node *mux;						/**< The mux node, that will be generated for this cond. */
	struct list_head list;	/**< List head which is used for queueing this cond
														into the cond bunch it belongs to. */
	unsigned in_list : 1;
	struct _cond_t *link;
	long visited_nr;

	/**
	 * Information about the both 'branches'
	 * (true and false), the cond creates.
	 */
	struct {
		int pos;						/**< Number of the predecessor of the
													phi block by which this branch is
													reached. It is -1, if this branch is
													only reached through another cond. */

		ir_node *masked_by;	/**< If this cond's branch is only reached
													through another cond, we store this
													cond ir_node here. */
	} cases[2];
} cond_t;

static INLINE cond_t *get_cond(ir_node *irn, set *cond_set)
{
	cond_t templ;

	templ.cond = irn;
	return set_find(cond_set, &templ, sizeof(templ), HASH_PTR(templ.cond));
}


typedef void (cond_walker_t)(cond_t *cond, void *env);

static void _walk_conds(cond_t *cond, cond_walker_t *pre, cond_walker_t *post,
		long visited_nr, set *cond_set, void *env)
{
	int i;

	if(cond->visited_nr >= visited_nr)
		return;

	cond->visited_nr = visited_nr;

	if(pre)
		pre(cond, env);

	for(i = 0; i < 2; ++i) {
		cond_t *c = get_cond(cond->cases[i].masked_by, cond_set);

		if(c)
			_walk_conds(c, pre, post, visited_nr, cond_set, env);
	}

	if(post)
		post(cond, env);
}

static void walk_conds(cond_t *cond, cond_walker_t *pre, cond_walker_t *post,
		set *cond_set, void *env)
{
	static long visited_nr = 0;

	_walk_conds(cond, pre, post, ++visited_nr, cond_set, env);
}

static void link_conds(cond_t *cond, void *env)
{
	cond_t **ptr = (cond_t **) env;

	cond->link = *ptr;
	*ptr = cond;
}

/**
 * Compare two conds for use in a firm set.
 * Two cond_t's are equal, if they designate the same cond node.
 * @param a A cond_t.
 * @param b Another one.
 * @param size Not used.
 * @return 0 (!) if they are equal, != 0 otherwise.
 */
static int cond_cmp(const void *a, const void *b, size_t size)
{
	const cond_t *x = a;
	const cond_t *y = b;
	return x->cond != y->cond;
}

/**
 * Information about conds which can be made to muxes.
 * Instances of this struct are attached to the link field of
 * blocks in which phis are located.
 */
typedef struct _cond_info_t {
	struct list_head list;			/**< Used to list all of these structs per class. */

	struct list_head roots;			/**< A list of non-depending conds. Two conds are
																independent, if yot can not reach the one from the
																other (all conds in this list have to dominate the
																block this struct is attached to. */

	set *cond_set;							/**< A set of all dominating reachable conds. */
} cond_info_t;

/**
 * @see find_conds.
 */
static void _find_conds(ir_node *irn, ir_node *base_block, long visited_nr,
		ir_node *dominator, ir_node *masked_by, int pos, int depth, cond_info_t *ci)
{
	ir_node *block;

	block = get_nodes_block(irn);

	if(block_dominates(dominator, block)) {
		ir_node *cond = NULL;
		int i, n;

		/* check, if we're on a ProjX
		 *
		 * Further, the ProjX/Cond block must dominate the base block
		 * (the block with the phi in it), otherwise, the Cond
		 * is not affecting the phi so that a mux can be inserted.
		 */
		if(is_Proj(irn) && get_irn_mode(irn) == mode_X) {

			int proj = get_Proj_proj(irn);
			cond = get_Proj_pred(irn);

			/* Check, if the pred of the proj is a Cond
			 * with a Projb as selector.
			 */
			if(get_irn_opcode(cond) == iro_Cond
					&& get_irn_mode(get_Cond_selector(cond)) == mode_b) {

				cond_t *res, c;

				memset(&c, 0, sizeof(c));
				c.cond = cond;
				INIT_LIST_HEAD(&c.list);
				c.cases[0].pos = -1;
				c.cases[1].pos = -1;

				/* get or insert the cond info into the set. */
				res = set_insert(ci->cond_set, &c, sizeof(c), HASH_PTR(cond));

				if(!res->in_list) {
					res->in_list = 1;
					list_add(&res->list, &ci->roots);
				}

				/*
				 * Link it to the cond ir_node. We need that later, since
				 * one cond masks the other we want to retreive the cond_t
				 * data from the masking cond ir_node.
				 */
				set_irn_link(cond, res);

				/*
				 * Set masked by (either NULL or another cond node.
				 * If this cond is truly masked by another one, set
				 * the position of the actually investigated branch
				 * to -1. Since the cond is masked by another one,
				 * there could be more ways from the start block
				 * to this branch, so we choose -1.
				 */
				res->cases[proj].masked_by = masked_by;
				if(!masked_by)
					res->cases[proj].pos = pos;

				/*
				 * Since the masked_by nodes masks a cond, remove it from the
				 * root list of the conf trees.
				 */
				else {
					cond_t *m = get_cond(masked_by, ci->cond_set);
					struct list_head *list = &m->list;

					/*
					 * If this cond was not removed before,
					 * remove it now from the list.
					 */
					if(!list_empty(list))
						list_del_init(list);
				}

				DBG((dbg, LEVEL_5, "%{firm:indent}found cond %n (%s branch) "
							"for pos %d in block %n reached by %n\n",
							depth, cond, get_Proj_proj(irn) ? "true" : "false", pos, block, masked_by));
			}

			/*
			 * We set cond to NULL if the cond was an switch cond, since it is
			 * passed (as the masked_by argument) to recursive calls to this
			 * function. We do not consider switch conds as masking conds for
			 * other conds.
			 */
			else
				cond = NULL;
		}

		if(get_Block_block_visited(block) < visited_nr) {

			set_Block_block_visited(block, visited_nr);

			/* Search recursively from this cond. */
			for(i = 0, n = get_irn_arity(block); i < n; ++i) {
				ir_node *pred = get_irn_n(block, i);

				/*
				 * If the depth is 0 (the first recursion), we set the pos to
				 * the current viewed predecessor, else we adopt the position
				 * as given by the caller. We also increase the depth for the
				 * recursively called functions.
				 */
				_find_conds(pred, base_block, visited_nr, dominator, cond, pos, depth + 1, ci);
			}
		}
	}
}


/**
 * A convenience function for _find_conds.
 * It sets some parameters needed for recursion to appropriate start
 * values. Always use this function.
 * @param irn The node to start looking for conds from. This might
 * 	be the phi node we are investigating.
 * @param conds The set to record the found conds in.
 */
static INLINE void find_conds(ir_node *irn, cond_info_t *ci)
{
	int i, n;
	long visited_nr;
	ir_node *block = get_nodes_block(irn);

	inc_irg_block_visited(current_ir_graph);
	visited_nr = get_irg_block_visited(current_ir_graph);

	for(i = 0, n = get_irn_arity(block); i < n; ++i) {
		ir_node *pred = get_irn_n(block, i);
		ir_node *pred_block = get_nodes_block(pred);
		ir_node *dom = get_Block_idom(pred_block);

		/*
		 * If the pred_block is the start block, its idom is NULL
		 * so we treat the block itself as its immediate dominator.
		 */
		if(dom == NULL)
			dom = pred_block;

		_find_conds(pred, pred_block, visited_nr, dom, NULL, i, 0, ci);
	}
}


/**
 * Make the mux for a given cond.
 * @param phi The phi node which shall be replaced by a mux.
 * @param dom The block where the muxes shall be placed.
 * @param cond The cond information.
 * @return The mux node made for this cond.
 */
static ir_node *make_mux_on_demand(ir_node *phi, ir_node *dom, cond_t *cond, set *cond_set)
{
	int i;
	ir_node *projb = get_Cond_selector(cond->cond);
	ir_node *operands[2];

	operands[0] = NULL;
	operands[1] = NULL;
	cond->mux = NULL;

	for(i = 0; i < 2; ++i) {

		/*
		 * If this cond branch is masked by another cond, make the mux
		 * for that cond first, since the mux for this cond takes
		 * it as an operand.
		 */
		if(cond->cases[i].masked_by) {
			cond_t templ;
			cond_t *masking_cond;

			templ.cond = cond->cases[i].masked_by;
			masking_cond = set_find(cond_set, &templ, sizeof(templ), HASH_PTR(templ.cond));

			operands[i] = make_mux_on_demand(phi, dom, masking_cond, cond_set);
		}

		/*
		 * If this cond branch is not masked by another cond, take
		 * the corresponding phi operand as an operand to the mux.
		 */
		else {
			if(cond->cases[i].pos >= 0)
				operands[i] = get_irn_n(phi, cond->cases[i].pos);
		}
	}

	/*
	 * Move the operands to the dominator block if the cond
	 * made sense. Some conds found are not suitable for making a mux
	 * out of them, since one of their branches cannot be reached from
	 * the phi block. In that case we do not make a mux and return NULL.
	 */
	if(operands[0] && operands[1]) {
		move_to(operands[0], dom);
		move_to(operands[1], dom);
		move_to(projb, dom);

		/* Make the mux. */
		cond->mux = new_r_Mux(current_ir_graph, dom, projb,
				operands[0], operands[1], get_irn_mode(operands[0]));
	}

	return cond->mux;
}

typedef struct _phi_info_t {
	struct list_head list;
	cond_info_t *cond_info;
	ir_node *irn;
} phi_info_t;


/**
 * Examine a phi node if it can be replaced by some muxes.
 * @param irn A phi node.
 * @param info Parameters for the if conversion algorithm.
 */
static void check_out_phi(phi_info_t *phi_info, opt_if_conv_info_t *info)
{
	int max_depth = info->max_depth;
	int i, n;
	ir_node *irn = phi_info->irn;
	ir_node *block, *nw;
	cond_info_t *cond_info = phi_info->cond_info;
	cond_t *cond;
	int arity;

	set *cond_set = cond_info->cond_set;
	bitset_t *positions;

	block = get_nodes_block(irn);
	arity = get_irn_arity(irn);

	assert(is_Phi(irn));
	assert(get_irn_arity(irn) == get_irn_arity(block));
	assert(arity > 0);

	positions = bitset_alloca(arity);

	DBG((dbg, LEVEL_5, "phi candidate: %n\n", irn));

	list_for_each_entry(cond_t, cond, &cond_info->roots, list) {
		int cannot_move = 0;
		ir_node *cidom = get_nodes_block(cond->cond);

		cond_t *p, *head = NULL;

		DBG((dbg, LEVEL_5, "\tcond root: %n\n", cond->cond));

		/* clear the position array. */
		bitset_clear_all(positions);

		/*
		 * Link all conds which are in the subtree of
		 * the current cond in the list together.
		 */
		walk_conds(cond, link_conds, NULL, cond_set, &head);

		for(p = head, n = 0; p; p = p->link)
			cidom = common_idom(cidom, get_nodes_block(p->cond));

		DBG((dbg, LEVEL_5, "\tcommon idom: %n\n", cidom));

		for(p = head, n = 0; p && !cannot_move; p = p->link) {

			if(!can_move_to(get_Cond_selector(p->cond), cidom, max_depth)) {
				DBG((dbg, LEVEL_5, "\tcannot move selector of %n\n", p->cond));
				cannot_move = 1;
				break;
			}

			for(i = 0; i < 2; ++i) {
				int pos = p->cases[i].pos;

				if(pos != -1) {
					bitset_set(positions, pos);

					if(!can_move_to(get_irn_n(irn, pos), cidom, max_depth)) {
						cannot_move = 1;
						DBG((dbg, LEVEL_5, "\tcannot move phi operand %d\n", pos));
						break;
					}

					DBG((dbg, LEVEL_5, "\tcan move phi operand %d\n", pos));
				}
			}
		}

		/*
		 * If all operands and the cond condition can be moved to
		 * the common immediate dominator, move them there, make a
		 * mux and associate the corresponding phi operands with
		 * the mux.
		 */
		if(!cannot_move) {
			ir_node *mux = make_mux_on_demand(irn, cidom, cond, cond_info->cond_set);

			/* If a mux could be made, associate the phi operands with it. */
			DBG((dbg, LEVEL_5, "\tassociating:\n"));
			if(mux) {
				unsigned long elm;
				bitset_foreach(positions, elm) {
					DBG((dbg, LEVEL_5, "\t\t%d\n", positions[i]));
					set_irn_n(irn, (int) elm, mux);
				}
			}
		}
	}

	/*
	 * optimize the phi away. This can anable further runs of this
	 * function. Look at _can_move. phis cannot be moved there.
	 */
	nw = optimize_in_place_2(irn);
	if(nw != irn)
		exchange(irn, nw);
}

typedef struct _cond_walk_info_t {
	struct obstack *obst;
	struct list_head cond_info_head;
	struct list_head phi_head;
} cond_walk_info_t;


static void annotate_cond_info_pre(ir_node *irn, void *data)
{
	set_irn_link(irn, NULL);
}

static void annotate_cond_info_post(ir_node *irn, void *data)
{
	cond_walk_info_t *cwi = data;

	/*
	 * Check, if the node is a phi
	 * we then compute a set of conds which are reachable from this
	 * phi's block up to its dominator.
	 * The set is attached to the blocks link field.
	 */
	if(is_Phi(irn) && mode_is_datab(get_irn_mode(irn))) {
		ir_node *block = get_nodes_block(irn);

		cond_info_t *ci = get_irn_link(block);

		/* If the set is not yet computed, do it now. */
		if(!ci) {
			ci = obstack_alloc(cwi->obst, sizeof(*ci));
			ci->cond_set = new_set(cond_cmp, log2_ceil(get_irn_arity(block)));
			INIT_LIST_HEAD(&ci->roots);
			INIT_LIST_HEAD(&ci->list);

			/*
			 * Add this cond info to the list of all cond infos
			 * in this graph. This is just done to free the
			 * set easier afterwards (we save an irg_walk_graph).
			 */
			list_add(&cwi->cond_info_head, &ci->list);

			DBG((dbg, LEVEL_5, "searching conds at %n\n", irn));

			/*
			 * Fill the set with conds we find on the way from
			 * the block to its dominator.
			 */
			find_conds(irn, ci);

			/*
			 * If there where no suitable conds, delete the set
			 * immediately and reset the set pointer to NULL
			 */
			if(set_count(ci->cond_set) == 0) {
				del_set(ci->cond_set);
				ci->cond_set = NULL;
				obstack_free(cwi->obst, ci);
			}
		}

		else
			DBG((dbg, LEVEL_5, "conds already computed for %n\n", irn));

		set_irn_link(block, ci);

		if(ci) {
			phi_info_t *pi = obstack_alloc(cwi->obst, sizeof(*pi));
			pi->irn = irn;
			pi->cond_info = ci;
			INIT_LIST_HEAD(&pi->list);
			list_add(&pi->list, &cwi->phi_head);
		}

	}
}

#if 0
/**
 * Free the sets which are put at some blocks.
 */
static void free_sets(ir_node *irn, void *data)
{
	if(is_Block(irn) && get_irn_link(irn)) {
		set *conds = get_irn_link(irn);
		del_set(conds);
	}
}
#endif

void opt_if_conv(ir_graph *irg, opt_if_conv_info_t *params)
{
	struct obstack obst;
	phi_info_t *phi_info;
	cond_info_t *cond_info;
	cond_walk_info_t cwi;

	opt_if_conv_info_t *p = params ? params : &default_info;

	if(!get_opt_if_conversion())
		return;

	obstack_init(&obst);

	cwi.obst = &obst;
	INIT_LIST_HEAD(&cwi.cond_info_head);
	INIT_LIST_HEAD(&cwi.phi_head);

	/* Init the debug stuff. */
	dbg = firm_dbg_register("firm.opt.ifconv");
	firm_dbg_set_mask(dbg, 0);

	/* Ensure, that the dominators are computed. */
	compute_doms(irg);

	DBG((dbg, LEVEL_4, "if conversion for irg %s(%p)\n",
				get_entity_name(get_irg_entity(irg)), irg));

	/*
	 * Collect information about the conds pu the phis on an obstack.
	 * It is important that phi nodes which are 'higher' (with a
	 * lower dfs pre order) are in front of the obstack. Since they are
	 * possibly turned in to muxes this can enable the optimization
	 * of 'lower' ones.
	 */
	irg_walk_graph(irg, annotate_cond_info_pre, annotate_cond_info_post, &cwi);

	/* Process each suitable phi found. */
	list_for_each_entry(phi_info_t, phi_info, &cwi.phi_head, list) {
		DBG((dbg, LEVEL_4, "phi node %n\n", phi_info->irn));
		check_out_phi(phi_info, p);
	}

	list_for_each_entry(cond_info_t, cond_info, &cwi.cond_info_head, list) {
		del_set(cond_info->cond_set);
	}

	obstack_free(&obst, NULL);
}
