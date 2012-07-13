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
 * @brief       Common functions for chordal register allocation.
 * @author      Sebastian Hack
 * @date        08.12.2004
 */
#include "config.h"

#include "bechordal_common.h"

#include "debug.h"

#include "iredges.h"
#include "bitset.h"

#include "bechordal.h"
#include "bechordal_t.h"
#include "beirgmod.h"
#include "beinsn_t.h"
#include "besched.h"
#include "bestatevent.h"
#include "benode.h"
#include "bemodule.h"
#include "belive.h"
#include "belive_t.h"
#include "fourcc.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* Make a fourcc for border checking. */
#define BORDER_FOURCC   FOURCC('B', 'O', 'R', 'D')

int has_reg_class(const be_chordal_env_t *env, const ir_node *irn)
{
	return arch_irn_consider_in_reg_alloc(env->cls, irn);
}

static inline border_t *border_add(be_chordal_env_t *env, struct list_head *head,
			ir_node *irn, unsigned step, unsigned pressure,
			unsigned is_def, unsigned is_real)
{
	border_t *b;

	if (!is_def) {
		border_t *def;

		b = OALLOC(env->obst, border_t);

		/* also allocate the def and tie it to the use. */
		def = OALLOCZ(env->obst, border_t);
		b->other_end = def;
		def->other_end = b;

		/*
		 * Set the link field of the irn to the def.
		 * This strongly relies on the fact, that the use is always
		 * made before the def.
		 */
		set_irn_link(irn, def);

		DEBUG_ONLY(b->magic = BORDER_FOURCC;)
		DEBUG_ONLY(def->magic = BORDER_FOURCC;)
	} else {
		/*
		 * If the def is encountered, the use was made and so was the
		 * the def node (see the code above). It was placed into the
		 * link field of the irn, so we can get it there.
		 */
		b = (border_t*)get_irn_link(irn);

		DEBUG_ONLY(assert(b && b->magic == BORDER_FOURCC && "Illegal border encountered");)
	}

	b->pressure = pressure;
	b->is_def = is_def;
	b->is_real = is_real;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	DBG((dbg, LEVEL_5, "\t\t%s adding %+F, step: %d\n", is_def ? "def" : "use", irn, step));


	return b;
}

void create_borders(ir_node *block, void *env_ptr)
{
/* Convenience macro for a def */
#define border_def(irn, step, real) \
	border_add(env, head, irn, step, pressure--, 1, real)

/* Convenience macro for a use */
#define border_use(irn, step, real) \
	border_add(env, head, irn, step, ++pressure, 0, real)

	be_chordal_env_t *env  = (be_chordal_env_t*)env_ptr;
	bitset_t         *live = bitset_malloc(get_irg_last_idx(env->irg));
	be_lv_t          *lv   = be_get_irg_liveness(env->irg);

	int i, n;
	size_t elm;
	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;

	bitset_clear_all(live);

	/* Set up the border list in the block info */
	head = OALLOC(env->obst, struct list_head);
	INIT_LIST_HEAD(head);
	assert(pmap_get(struct list_head, env->border_heads, block) == NULL);
	pmap_insert(env->border_heads, block, head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are necessary to build up real intervals.
	 */
	be_lv_foreach(lv, block, be_lv_state_end, i) {
		ir_node *irn = be_lv_get_irn(lv, block, i);
		if (has_reg_class(env, irn)) {
			DBG((dbg, LEVEL_3, "\tMaking live: %+F/%d\n", irn, get_irn_idx(irn)));
			bitset_set(live, get_irn_idx(irn));
			border_use(irn, step, 0);
		}
	}
	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DBG((dbg, LEVEL_1, "\tinsn: %+F, pressure: %d\n", irn, pressure));
		DBG((dbg, LEVEL_2, "\tlive: %B\n", live));

		if (get_irn_mode(irn) == mode_T) {
			foreach_out_edge(irn, edge) {
				ir_node *proj = get_edge_src_irn(edge);

				/*
				 * If the node defines some value, which can put into a
				 * register of the current class, make a border for it.
				 */
				if (has_reg_class(env, proj)) {
					int nr = get_irn_idx(proj);

					bitset_clear(live, nr);
					border_def(proj, step, 1);
				}
			}
		} else {
			/*
			 * If the node defines some value, which can put into a
			 * register of the current class, make a border for it.
			 */
			if (has_reg_class(env, irn)) {
				int nr = get_irn_idx(irn);

				bitset_clear(live, nr);
				border_def(irn, step, 1);
			}
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if (!is_Phi(irn)) {
			for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if (has_reg_class(env, op)) {
					int nr = get_irn_idx(op);
					const char *msg = "-";

					if (!bitset_is_set(live, nr)) {
						border_use(op, step, 1);
						bitset_set(live, nr);
						msg = "X";
					}

					DBG((dbg, LEVEL_4, "\t\t%s pos: %d, use: %+F\n", msg, i, op));
				}
			}
		}
		++step;
	}

	bitset_foreach(live, elm) {
		ir_node *irn = get_idx_irn(env->irg, elm);
		if (be_is_live_in(lv, block, irn))
			border_def(irn, step, 0);
	}

	bitset_free(live);
}


be_insn_t *chordal_scan_insn(be_chordal_env_t *env, ir_node *irn)
{
	be_insn_env_t ie;

	ie.allocatable_regs = env->allocatable_regs;
	ie.obst             = env->obst;
	ie.cls              = env->cls;
	return be_scan_insn(&ie, irn);
}

ir_node *pre_process_constraints(be_chordal_env_t *env, be_insn_t **the_insn)
{
	be_insn_t *insn       = *the_insn;
	ir_node   *perm       = NULL;
	bitset_t  *out_constr = bitset_alloca(env->cls->n_regs);
	int        i;

	assert(insn->has_constraints && "only do this for constrained nodes");

	/*
	 * Collect all registers that occur in output constraints.
	 * This is necessary, since if the insn has one of these as an input constraint
	 * and the corresponding operand interferes with the insn, the operand must
	 * be copied.
	 */
	for (i = 0; i < insn->use_start; ++i) {
		be_operand_t *op = &insn->ops[i];
		if (op->has_constraints)
			bitset_or(out_constr, op->regs);
	}

	/*
	 * Make the Perm, recompute liveness and re-scan the insn since the
	 * in operands are now the Projs of the Perm.
	 */
	perm = insert_Perm_after(env->irg, env->cls, sched_prev(insn->irn));

	/* Registers are propagated by insert_Perm_after(). Clean them here! */
	if (perm == NULL)
		return NULL;

	be_stat_ev("constr_perm", get_irn_arity(perm));
	foreach_out_edge(perm, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		arch_set_irn_register(proj, NULL);
	}

	/*
	 * We also have to re-build the insn since the input operands are now the Projs of
	 * the Perm. Recomputing liveness is also a good idea if a Perm is inserted, since
	 * the live sets may change.
	 */
	obstack_free(env->obst, insn);
	*the_insn = insn = chordal_scan_insn(env, insn->irn);

	/*
	 * Copy the input constraints of the insn to the Perm as output
	 * constraints. Succeeding phases (coalescing) will need that.
	 */
	for (i = insn->use_start; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];
		ir_node *proj = op->carrier;
		/*
		 * Note that the predecessor must not be a Proj of the Perm,
		 * since ignore-nodes are not Perm'ed.
		 */
		if (op->has_constraints &&  is_Proj(proj) && get_Proj_pred(proj) == perm) {
			be_set_constr_out(perm, get_Proj_proj(proj), op->req);
		}
	}

	return perm;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal_common)
void be_init_chordal_common(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.chordal_common");
}
