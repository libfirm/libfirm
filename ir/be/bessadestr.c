/**
 * Author:      Daniel Grund
 * Date:		25.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Performs SSA-Destruction.
 */

#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "irnode.h"
#include "iredges_t.h"
#include "bechordal_t.h"
#include "bearch.h"

#define get_reg(irn) arch_get_irn_register(chordal_env->arch_env, irn, arch_pos_make_out(0))
#define set_reg(irn, reg) arch_set_irn_register(chordal_env->arch_env, irn, arch_pos_make_out(0), reg)

/**
 * Maps blocks to perm nodes inserted during phi destruction.
 */
typedef struct _block2perm_t {
	ir_node *block, *perm;
} block2perm_t;

static int set_cmp_b2p(const void *x, const void *y, size_t size) {
	const block2perm_t *b1 = x;
	const block2perm_t *b2 = y;
	return b1->block != b2->block;
}

/**
 * Adjusts the register allocation for the phi-operands
 * by inserting perm nodes, if necessary.
 * @param phi The phi node to adjust operands for
 */
static void adjust_arguments(be_chordal_env_t *chordal_env, const ir_node *phi) {
	int i, max;
	ir_node *arg, *perm, *proj;
	const arch_register_t *phi_reg, *arg_reg, *proj_reg;
	const ir_edge_t *edge;
	set *b2p;
	block2perm_t find, *found;

	assert(is_Phi(phi) && "Can only handle phi-destruction :)");

	b2p = chordal_env->data;
	phi_reg = get_reg(phi);
	/* all arguments of the phi */
	for(i=0, max=get_irn_arity(phi); i<max; ++i) {
		arg = get_irn_n(phi, i);
		arg_reg = get_reg(arg);
		/* if registers don't match ...*/
		if (phi_reg != arg_reg) {
			/* iff needed insert perm node */
			find.block = get_nodes_block(arg);
			find.perm = NULL;
			found = set_insert(b2p, &find, sizeof(find), HASH_PTR(find.block));
			if (!found->perm)
				found->perm = insert_perm(TODO);

			/* now we have the perm in the predecessor block */
			perm = found->perm;
			/* adjust assigned registers for the projs */
			foreach_out_edge(perm, edge) {
				proj = get_edge_src_irn(edge);
				proj_reg = get_reg(proj);
				if (proj_reg == arg_reg)
					set_reg(proj, phi_reg);
				else if (proj_reg == phi_reg)
					set_reg(proj, arg_reg);
			}
		}
	}
}

static void checker(be_chordal_env_t *chordal_env) {
	pmap_entry *pme;
	int i, max;

	/* iterate over all blocks */
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;

		/* iterate over all ops in the block */
		list_for_each_entry_reverse(border_t, curr, head, list)
			if (curr->is_def && curr->is_real && is_Phi(curr->irn)) {
				const arch_register_t *phi_reg, *arg_reg;
				phi_reg = get_reg(curr->irn);
				/* iterate over all args of phi */
				for(i=0, max=get_irn_arity(phi); i<max; ++i) {
					arg_reg = get_reg(get_irn_n(curr->irn, i));
					assert(phi_reg == arg_reg && "WTF? You can do it better!?");
				}
			}
	}
}

void be_ssa_destruction(be_chordal_env_t *chordal_env) {
	pmap_entry *pme;
	set *b2p;

	b2p = new_set(set_cmp_b2p, 32);
	chordal_env->data = b2p;
	/* iterate over all blocks */
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;

		/* iterate over all ops in the block
		 * BETTER: phis are the first ops, so stop iteration on first non-phi-op */
		list_for_each_entry_reverse(border_t, curr, head, list)
			if (curr->is_def && curr->is_real && is_Phi(curr->irn))
				adjust_arguments(chordal_env, curr->irn);
	}
	del_set(b2p);
	checker(chordal_env);
}
