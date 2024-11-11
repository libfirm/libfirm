/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       A data structure to treat nodes and node-proj collections uniformly.
 * @author      Sebastian Hack
 */
#include "beinsn_t.h"

#include "bearch.h"
#include "bechordal_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"

be_insn_t *be_scan_insn(be_chordal_env_t *const env, ir_node *const irn)
{
	struct obstack              *const obst = &env->obst;
	const arch_register_class_t *const cls = env->cls;

	be_insn_t *const insn = OALLOCZ(obst, be_insn_t);
	insn->irn = irn;
	be_operand_t o;
	bool has_constraints = false;
	be_foreach_definition(irn, cls, p, req,
		/* found a def: create a new operand */
		if (req->limited != NULL) {
			o.regs          = req->limited;
			has_constraints = true;
		} else {
			o.regs           = env->allocatable_regs->data;
			has_constraints |= req->width > 1;
		}
		o.carrier = p;
		o.partner = NULL;
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
	);
	insn->use_start = insn->n_ops;

	/* now collect the uses for this node */
	be_foreach_use(irn, cls, in_req, op, op_req,
		/* found a register use, create an operand */
		if (in_req->limited != NULL) {
			o.regs          = in_req->limited;
			has_constraints = true;
		} else {
			o.regs = env->allocatable_regs->data;
		}
		o.carrier = op;
		o.partner = NULL;
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
	);

	if (!has_constraints)
		return NULL;

	insn->ops = (be_operand_t*)obstack_finish(obst);
	return insn;
}
