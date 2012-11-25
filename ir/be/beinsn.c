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
 * @brief       A data structure to treat nodes and node-proj collections uniformly.
 * @author      Sebastian Hack
 */
#include "config.h"

#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iredges.h"

#include "bechordal_t.h"
#include "besched.h"
#include "beinsn_t.h"
#include "beabi.h"
#include "raw_bitset.h"

be_insn_t *be_scan_insn(be_chordal_env_t const *const env, ir_node *const irn)
{
	struct obstack *obst = env->obst;
	be_operand_t o;
	int i, n;

	be_insn_t *insn = OALLOCZ(obst, be_insn_t);

	bool has_constraints = false;

	insn->irn = irn;
	if (get_irn_mode(irn) == mode_T) {
		ir_node *p;

		/* This instruction might create more than one def. These are handled
		   by Proj's, find them. */
		foreach_out_edge(irn, edge) {
			p = get_edge_src_irn(edge);

			/* did not work if the result is a ProjT. This should NOT happen
			   in the backend, but check it for now. */
			assert(get_irn_mode(p) != mode_T);

			if (arch_irn_consider_in_reg_alloc(env->cls, p)) {
				/* found a def: create a new operand */
				arch_register_req_t const *const req = arch_get_irn_register_req(p);
				if (arch_register_req_is(req, limited)) {
					o.regs          = req->limited;
					has_constraints = true;
				} else {
					o.regs           = env->allocatable_regs->data;
					has_constraints |= req->width > 1;
				}
				o.carrier         = p;
				o.partner         = NULL;
				obstack_grow(obst, &o, sizeof(o));
				insn->n_ops++;
			}
		}
	} else if (arch_irn_consider_in_reg_alloc(env->cls, irn)) {
		/* only one def, create one operand */
		arch_register_req_t const *const req = arch_get_irn_register_req(irn);
		if (arch_register_req_is(req, limited)) {
			o.regs          = req->limited;
			has_constraints = true;
		} else {
			o.regs           = env->allocatable_regs->data;
			has_constraints |= req->width > 1;
		}
		o.carrier = irn;
		o.partner = NULL;
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
	}

	insn->use_start = insn->n_ops;

	/* now collect the uses for this node */
	for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if (arch_irn_consider_in_reg_alloc(env->cls, op)) {
			/* found a register use, create an operand */
			arch_register_req_t const *const req = arch_get_irn_register_req_in(irn, i);
			if (arch_register_req_is(req, limited)) {
				o.regs          = req->limited;
				has_constraints = true;
			} else {
				o.regs = env->allocatable_regs->data;
			}
			o.carrier = op;
			o.partner = NULL;
			obstack_grow(obst, &o, sizeof(o));
			insn->n_ops++;
		}
	}

	if (!has_constraints)
		return NULL;

	insn->ops = (be_operand_t*)obstack_finish(obst);
	return insn;
}
