#include <assert.h>
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode.h"
#include "tv.h"

typedef struct cond_pair {
	ir_node *cmp_lo;
	ir_node *cmp_hi;
	pn_Cmp   pnc_lo;
	pn_Cmp   pnc_hi;
	ir_node *proj_lo;
	ir_node *proj_hi;
	tarval  *tv_lo;
	tarval  *tv_hi;
} cond_pair;

static int find_cond_pair(ir_node *const l, ir_node *const r, cond_pair *const res)
{
	if (is_Proj(l) && is_Proj(r)) {
		ir_node *const lo = get_Proj_pred(l);
		ir_node *const ro = get_Proj_pred(r);

		if (is_Cmp(lo) && is_Cmp(ro)) {
			ir_node *const lol = get_Cmp_left(lo);
			ir_node *const lor = get_Cmp_right(lo);
			ir_node *const rol = get_Cmp_left(ro);
			ir_node *const ror = get_Cmp_right(ro);

			/* TODO float */
			/* The constants shall be unequal.  Local optimisations handle the equal
			 * case */
			if (lol == rol && mode_is_int(get_irn_mode(lol)) && lor != ror && is_Const(lor) && is_Const(ror)) {
				tarval *const tv_l  = get_Const_tarval(lor);
				tarval *const tv_r  = get_Const_tarval(ror);
				pn_Cmp  const pnc_l = get_Proj_proj(l);
				pn_Cmp  const pnc_r = get_Proj_proj(r);
				pn_Cmp  const rel   = tarval_cmp(tv_l, tv_r);

				assert(rel != pn_Cmp_Eq);

				if (rel == pn_Cmp_Lt) {
					res->cmp_lo  = lo;
					res->cmp_hi  = ro;
					res->pnc_lo  = pnc_l;
					res->pnc_hi  = pnc_r;
					res->proj_lo = l;
					res->proj_hi = r;
					res->tv_lo   = tv_l;
					res->tv_hi   = tv_r;
				} else {
					assert(rel == pn_Cmp_Gt);
					res->cmp_lo  = ro;
					res->cmp_hi  = lo;
					res->pnc_lo  = pnc_r;
					res->pnc_hi  = pnc_l;
					res->proj_lo = r;
					res->proj_hi = l;
					res->tv_lo   = tv_r;
					res->tv_hi   = tv_l;
				}
				return 1;
			}
		}
	}
	return 0;
}

static void bool_and(ir_node *const n)
{
	ir_node *const l = get_And_left(n);
	ir_node *const r = get_And_right(n);
	cond_pair      res;
	if (find_cond_pair(l, r, &res)) {
		ir_node *const cmp_lo  = res.cmp_lo;
		ir_node *const cmp_hi  = res.cmp_hi;
		pn_Cmp   const pnc_lo  = res.pnc_lo;
		pn_Cmp   const pnc_hi  = res.pnc_hi;
		ir_node *const proj_lo = res.proj_lo;
		ir_node *const proj_hi = res.proj_hi;
		tarval  *const tv_lo   = res.tv_lo;
		tarval  *const tv_hi   = res.tv_hi;

		if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
				(pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
			/* x <|<=|== lo | x ==|>=|> hi -> false */
			ir_node *const t = new_Const(mode_b, tarval_b_false);
			exchange(n, t);
		} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
							 (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Ne)) {
			/* x <|<=|== lo && x <|<=|!= hi -> x <|<=|== lo */
			exchange(n, proj_lo);
		} else if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Ne) &&
							 (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
			/* x >=|>|!= lo || x ==|>=|> hi -> x ==|>=|> hi */
			exchange(n, proj_hi);
		} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo))) { /* lo + 1 == hi */
			if (pnc_lo == pn_Cmp_Ge && pnc_hi == pn_Cmp_Lt) {
				/* x >= c || x < c + 1 -> x == c */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_lo);
				ir_node  *const p = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Eq);
				exchange(n, p);
			} else if (pnc_lo == pn_Cmp_Gt) {
				if (pnc_hi == pn_Cmp_Ne) {
					/* x > c || x != c + 1 -> x > c + 1 */
					ir_graph *const irg   = current_ir_graph;
					ir_node  *const block = get_nodes_block(cmp_hi);
					ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Gt);
					exchange(n, p);
				} else if (pnc_hi == pn_Cmp_Lt) {
					/* x > c || x < c + 1 -> false */
					ir_node *const t = new_Const(mode_b, tarval_b_false);
					exchange(n, t);
				} else if (pnc_hi == pn_Cmp_Le) {
					/* x > c || x <= c + 1 -> x != c + 1 */
					ir_graph *const irg   = current_ir_graph;
					ir_node  *const block = get_nodes_block(cmp_hi);
					ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Eq);
					exchange(n, p);
				}
			} else if (pnc_lo == pn_Cmp_Ne && pnc_hi == pn_Cmp_Lt) {
				/* x != c || c < c + 1 -> x < c */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_lo);
				ir_node  *const p     = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Lt);
				exchange(n, p);
			}
		}
	}
}

static void bool_or(ir_node *const n)
{
	ir_node *const l = get_Or_left(n);
	ir_node *const r = get_Or_right(n);
	cond_pair      res;
	if (find_cond_pair(l, r, &res)) {
		ir_node *const cmp_lo  = res.cmp_lo;
		ir_node *const cmp_hi  = res.cmp_hi;
		pn_Cmp   const pnc_lo  = res.pnc_lo;
		pn_Cmp   const pnc_hi  = res.pnc_hi;
		ir_node *const proj_lo = res.proj_lo;
		ir_node *const proj_hi = res.proj_hi;
		tarval  *const tv_lo   = res.tv_lo;
		tarval  *const tv_hi   = res.tv_hi;

		if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Ne) &&
				(pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Ne)) {
			/* x >=|>|!= lo | x <|<=|!= hi -> true */
			ir_node *const t = new_Const(mode_b, tarval_b_true);
			exchange(n, t);
		} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
							 (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Ne)) {
			/* x <|<=|== lo || x <|<=|!= hi -> x <|<=|!= hi */
			exchange(n, proj_hi);
		} else if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Ne) &&
							 (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
			/* x >=|>|!= lo || x ==|>=|> hi -> x >=|>|!= lo */
			exchange(n, proj_lo);
		} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo))) { /* lo + 1 == hi */
			if (pnc_lo == pn_Cmp_Lt && pnc_hi == pn_Cmp_Ge) {
				/* x < c || x >= c + 1 -> x != c */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_lo);
				ir_node  *const p = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Ne);
				exchange(n, p);
			} else if (pnc_lo == pn_Cmp_Le) {
				if (pnc_hi == pn_Cmp_Eq) {
					/* x <= c || x == c + 1 -> x <= c + 1 */
					ir_graph *const irg   = current_ir_graph;
					ir_node  *const block = get_nodes_block(cmp_hi);
					ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Le);
					exchange(n, p);
				} else if (pnc_hi == pn_Cmp_Ge) {
					/* x <= c || x >= c + 1 -> true */
					ir_node *const t = new_Const(mode_b, tarval_b_true);
					exchange(n, t);
				} else if (pnc_hi == pn_Cmp_Gt) {
					/* x <= c || x > c + 1 -> x != c + 1 */
					ir_graph *const irg   = current_ir_graph;
					ir_node  *const block = get_nodes_block(cmp_hi);
					ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Ne);
					exchange(n, p);
				}
			} else if (pnc_lo == pn_Cmp_Eq && pnc_hi == pn_Cmp_Ge) {
				/* x == c || c >= c + 1 -> x >= c */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_lo);
				ir_node  *const p     = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Ge);
				exchange(n, p);
			}
		}
	}
}

static void bool_walk(ir_node *const n, void *const env)
{
	(void)env;

	if (get_irn_mode(n) != mode_b) return;

	if (is_And(n)) {
		bool_and(n);
	} else if (is_Or(n)) {
		bool_or(n);
	}
}

void opt_bool(ir_graph *const irg)
{
	irg_walk_graph(irg, NULL, bool_walk, NULL);
}
