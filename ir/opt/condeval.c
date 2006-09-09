#include <assert.h>
#include <alloca.h>
#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "irgwalk.h"
#include "irnode.h"
#include "tv.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

static void cond_eval(ir_node* block, void* env)
{
	int n_block = get_Block_n_cfgpreds(block);
	int i;

	for (i = 0; i < n_block; i++) {
		ir_node* pred;
		ir_node* projx;
		ir_node* cond;
		ir_node* cmp;
		ir_node* left;
		ir_node* right;
		ir_node* cond_block;
		ir_node* phi;
		ir_node* cnst;
		pn_Cmp pnc;
		int n_phi;
		int j;

		pred = get_Block_cfgpred(block, i);
		if (!is_Proj(pred)) continue;
		projx = pred;

		pred = get_Proj_pred(projx);
		if (!is_Cond(pred)) continue;
		cond = pred;

		pred = get_Cond_selector(cond);
		assert(is_Proj(pred));
		// TODO handle switches
		if (get_irn_mode(pred) != mode_b) continue;
		pnc = get_Proj_proj(pred);

		cmp = get_Proj_pred(pred);
		assert(is_Cmp(cmp));

		left  = get_Cmp_left(cmp);
		right = get_Cmp_right(cmp);

		// TODO handle Const-Const and Phi-Phi
		if (is_Phi(left)) {
			if (!is_Const(right)) continue;
			phi  = left;
			cnst = right;
		} else if (is_Phi(right)) {
			if (!is_Const(left)) continue;
			phi  = right;
			cnst = left;
			pnc = get_inversed_pnc(pnc);
		} else {
			continue;
		}

		cond_block = get_nodes_block(cond);
		if (get_nodes_block(phi) != cond_block) continue;

		if (get_Proj_proj(projx) == 0) pnc = get_negated_pnc(pnc, get_irn_mode(cnst));

		n_phi = get_Phi_n_preds(phi);
		for (j = 0; j < n_phi; j++) {
			tarval* tv_phi;
			tarval* tv_cnst;
			ir_node** ins;
			int k;

			pred = get_Phi_pred(phi, j);
			// TODO handle Phi cascades
			if (!is_Const(pred)) continue;

			tv_phi  = get_Const_tarval(pred);
			tv_cnst = get_Const_tarval(cnst);

			switch (tarval_cmp(tv_phi, tv_cnst)) {
				case pn_Cmp_Lt:
					if (pnc != pn_Cmp_Lt &&
							pnc != pn_Cmp_Le &&
							pnc != pn_Cmp_Lg) {
						continue;
					}
					break;

				case pn_Cmp_Eq:
					if (pnc != pn_Cmp_Le &&
							pnc != pn_Cmp_Ge &&
							pnc != pn_Cmp_Eq) {
						continue;
					}
					break;

				case pn_Cmp_Gt:
					if (pnc != pn_Cmp_Gt &&
							pnc != pn_Cmp_Ge &&
							pnc != pn_Cmp_Lg) {
						continue;
					}
					break;

				default: continue;
			}

			DB((
				dbg, LEVEL_1,
				"> Found condition evaluation candidate %+F->%+F predecessor %d\n",
				block, cond_block, j
			));

#if 0 // TODO repair data flow and dominance
			NEW_ARR_A(ir_node*, ins, n_block + 1);
			for (k = 0; k < n_block; k++) ins[k] = get_Block_cfgpred(block, k);
			ins[k] = get_Block_cfgpred(cond_block, j);
			set_irn_in(block, n_block + 1, ins);

			set_Block_cfgpred(cond_block, j, new_Bad());
			set_Phi_pred(phi, j, new_Bad());
#endif
		}
	}
}


void opt_cond_eval(ir_graph* irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.condeval");

	DB((dbg, LEVEL_1, "===> Performing condition evaluation on %+F\n", irg));

	irg_block_walk_graph(irg, NULL, cond_eval, NULL);
}
