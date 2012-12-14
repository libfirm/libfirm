/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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
#include "beirg.h"
#include "beirgmod.h"
#include "beinsn_t.h"
#include "besched.h"
#include "statev_t.h"
#include "benode.h"
#include "bemodule.h"
#include "belive.h"
#include "belive_t.h"
#include "fourcc.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* Make a fourcc for border checking. */
#define BORDER_FOURCC   FOURCC('B', 'O', 'R', 'D')

static inline border_t *border_add(be_chordal_env_t *env, struct list_head *head,
			ir_node *irn, unsigned step, unsigned pressure,
			unsigned is_def, unsigned is_real)
{
	border_t *b;

	if (!is_def) {
		border_t *def;

		b = OALLOC(&env->obst, border_t);

		/* also allocate the def and tie it to the use. */
		def = OALLOCZ(&env->obst, border_t);
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

	be_chordal_env_t *const env = (be_chordal_env_t*)env_ptr;

	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;

	/* Set up the border list in the block info */
	head = OALLOC(&env->obst, struct list_head);
	INIT_LIST_HEAD(head);
	assert(pmap_get(struct list_head, env->border_heads, block) == NULL);
	pmap_insert(env->border_heads, block, head);

	ir_nodeset_t live;
	ir_nodeset_init(&live);

	be_lv_t *const lv = be_get_irg_liveness(env->irg);

	/*
	 * Make final uses of all values live out of the block.
	 * They are necessary to build up real intervals.
	 */
	be_lv_foreach_cls(lv, block, be_lv_state_end, env->cls, irn) {
		DB((dbg, LEVEL_3, "\tMaking live: %+F\n", irn));
		ir_nodeset_insert(&live, irn);
		border_use(irn, step, 0);
	}
	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DB((dbg, LEVEL_1, "\tinsn: %+F, pressure: %d\n", irn, pressure));

		be_foreach_definition(irn, env->cls, def, req,
			/*
			 * If the node defines some value, which can put into a
			 * register of the current class, make a border for it.
			 */
			ir_nodeset_remove(&live, def);
			border_def(def, step, 1);
		);

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if (!is_Phi(irn)) {
			be_foreach_use(irn, env->cls, in_req_, op, op_req_,
				const char *msg = "-";

				if (ir_nodeset_insert(&live, op)) {
					border_use(op, step, 1);
					msg = "X";
				}

				DB((dbg, LEVEL_4, "\t\t%s pos: %d, use: %+F\n", msg, i_, op));
			);
		}
		++step;
	}

	foreach_ir_nodeset(&live, irn, iter) {
		assert(be_is_live_in(lv, block, irn));
		border_def(irn, step, 0);
	}

	ir_nodeset_destroy(&live);
}

ir_node *pre_process_constraints(be_chordal_env_t *env, be_insn_t **the_insn)
{
	be_insn_t *insn = *the_insn;

	/*
	 * Make the Perm, recompute liveness and re-scan the insn since the
	 * in operands are now the Projs of the Perm.
	 */
	ir_node *const irn  = insn->irn;
	ir_node *const perm = insert_Perm_before(env->irg, env->cls, irn);

	/* Registers are propagated by insert_Perm_before(). Clean them here! */
	if (perm == NULL)
		return NULL;

	stat_ev_int("constr_perm", get_irn_arity(perm));

	/*
	 * We also have to re-build the insn since the input operands are now the Projs of
	 * the Perm. Recomputing liveness is also a good idea if a Perm is inserted, since
	 * the live sets may change.
	 */
	obstack_free(&env->obst, insn);
	*the_insn = insn = be_scan_insn(env, irn);

	/* Copy the input constraints of the irn to the Perm as output
	 * constraints. Succeeding phases (coalescing) will need that. */
	for (int i = 0, n = get_irn_arity(irn); i != n; ++i) {
		ir_node *const proj = get_irn_n(irn, i);
		/* Note that the predecessor is not necessarily a Proj of the Perm,
		 * since ignore-nodes are not Perm'ed. */
		if (!is_Proj(proj) || get_Proj_pred(proj) != perm)
			continue;
		/* FIXME: Only setting the constraints, when the register requirement is
		 * limited, is a hack.  It will break when multiple differently constrained
		 * inputs use the same value. */
		arch_register_req_t const *const req = arch_get_irn_register_req_in(irn, i);
		if (!arch_register_req_is(req, limited))
			continue;
		be_set_constr_out(perm, get_Proj_proj(proj), req);
	}

	return perm;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal_common)
void be_init_chordal_common(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.chordal_common");
}
