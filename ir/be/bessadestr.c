/**
 * Author:      Daniel Grund
 * Date:		25.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Performs SSA-Destruction.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "irnode.h"
#include "iredges_t.h"
#include "irdump.h"
#include "irprintf.h"

#include "be_t.h"
#include "beutil.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "benode_t.h"
#include "besched_t.h"

static firm_dbg_module_t *dbg = NULL;
#define DEBUG_LVL SET_LEVEL_2


#define get_reg(irn) arch_get_irn_register(chordal_env->arch_env, irn, 0)
#define set_reg(irn, reg) arch_set_irn_register(chordal_env->arch_env, irn, 0, reg)

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

#define is_Branch(irn)          (arch_irn_classify(arch_env, irn) == arch_irn_class_branch)
#define is_Perm(irn)            (arch_irn_classify(arch_env, irn) == arch_irn_class_perm)
#define get_reg_cls(irn)        (arch_get_irn_reg_class(arch_env, irn, arch_pos_make_out(0)))
#define is_curr_reg_class(irn)  (get_reg_cls(p) == chordal_env->cls)

static ir_node *get_or_insert_perm(be_main_session_env_t *session, be_chordal_env_t *chordal_env, ir_node *block) {
	block2perm_t find, *found;
	ir_node *p;
	set *b2p = chordal_env->data;
	const arch_env_t *arch_env = chordal_env->arch_env;


	/* iff needed insert perm node */
	DBG((dbg, LEVEL_1, "    Getting perm in %+F\n", block));

	/* .if the perm is in the pset return it */
	find.block = block;
	find.perm = NULL;
	found = set_insert(b2p, &find, sizeof(find), HASH_PTR(find.block));
	if (found->perm) {
		DBG((dbg, LEVEL_1, "      found it %+F in map\n", found->perm));
		return found->perm;
	}

	/* .else look for a perm of right register class in the schedule */
	p = sched_last(find.block);
	while (!is_Block(p) && (is_Branch(p) || (is_Perm(p) && !is_curr_reg_class(p))))
		p = sched_prev(p);

	/* if we haven't found a perm of the right register class create a new one */
	if (! (is_Perm(p) && is_curr_reg_class(p))) {
		DBG((dbg, LEVEL_1, "      insert it after %+F\n", p));
		p = insert_Perm_after(session, chordal_env->cls, p);
	}

	/* insert perm into pset and return it*/
	found->perm = p;
	return p;
}

#define is_pinned(irn) (get_irn_link(irn))
#define get_pinning_block(irn) ((ir_node *)get_irn_link(irn))
#define pin_irn(irn, lock) (set_irn_link(irn, lock))

/**
 * Adjusts the register allocation for the phi-operands
 * by inserting perm nodes, if necessary.
 * @param phi The phi node to adjust operands for
 */
static void adjust_phi_arguments(be_main_session_env_t *session, be_chordal_env_t *chordal_env, ir_node *phi) {
	int i, max;
	ir_node *arg, *phi_block, *arg_block;
	arch_env_t *arch_env = session->main_env->arch_env;
	const arch_register_t *phi_reg, *arg_reg;
	const arch_register_class_t *cls;

	assert(is_Phi(phi) && "Can only handle phi-destruction :)");
	DBG((dbg, LEVEL_1, "  for %+F\n", phi));

	cls = arch_get_irn_reg_class(session->main_env->arch_env, phi, arch_pos_make_out(0));
	phi_block = get_nodes_block(phi);
	phi_reg = get_reg(phi);

	/* process all arguments of the phi */
	for(i=0, max=get_irn_arity(phi); i<max; ++i) {
		ir_node *perm;

		arg = get_irn_n(phi, i);
		arg_block = get_nodes_block(arg);
		arg_reg = get_reg(arg);
		perm = get_Proj_pred(arg);
		assert(is_Perm(perm));

		DBG((dbg, LEVEL_1, "    arg %+F has perm %+F\n", arg, perm));
		/* if registers don't match ...*/
		if (phi_reg != arg_reg) {
			DBG((dbg, LEVEL_1, "      regs don't match %d %d\n", phi_reg, arg_reg));

			/* First check if there is another phi in the same block
			 * having arg at the same pos in its arg-list and the same color as arg */
			if (!is_pinned(arg)) {
				DBG((dbg, LEVEL_1, "      arg is not pinned\n"));
				ir_node *other_phi = phi;
				while ((other_phi = get_irn_link(other_phi)) != phi) {
					assert(is_Phi(other_phi) && get_nodes_block(phi) == get_nodes_block(other_phi) && "link fields are screwed up");
					if (get_irn_n(other_phi, i) == arg && get_reg(other_phi) == arg_reg) {
						DBG((dbg, LEVEL_1, "      other phi pinned the argument\n"));
						pin_irn(arg, phi_block);
					}
				}
			}

			/* If arg is pinned, another phi set the color of arg and pinned it.
			 * So this phi can't change the color again and a duplicate must be inserted.
			 *
			 * If arg interferes with phi, one can never set the same color for both
			 * Hence, a duplicate must be inserted */
			if (is_pinned(arg) || nodes_interfere(chordal_env, phi, arg)) {
				ir_node *dupl, *tmp;
				assert(get_pinning_block(arg) == phi_block && "If arg is pinned it must be due to a phi in the same block");

				dupl = new_Copy(session->main_env->node_factory, cls, session->irg, arg_block, arg);
				set_irn_n(phi, i, dupl);
				set_reg(dupl, phi_reg);
				DBG((dbg, LEVEL_1, "      inserting dupl %+F\n", dupl));

				/* Add dupl to schedule */
				tmp = sched_next(perm);
				while (is_Proj(tmp) && sched_has_next(tmp))
					tmp = sched_next(tmp);
				sched_add_after(tmp, dupl);

				/* Add dupl to chained list of duplicates. Ptrs starting at the Perm */
				tmp = perm;
				while (get_irn_link(tmp))
					tmp = get_irn_link(tmp);
				set_irn_link(tmp, dupl);
				set_irn_link(dupl, NULL);

				/* now the arg is the dupl */
				arg = dupl;
			} else {
				/* Arg is not pinned. So set its color to the color of the phi.
				 * If the phi color is used by another proj of this perm
				 * one must NOT swap the colors. Proof: Critical edges removed,
				 * livein(PhiBl) = liveout(ArgBl), if all phis are processed then
				 * every color is used exactly once.
				 */
				DBG((dbg, LEVEL_1, "      just set color\n"));
				set_reg(arg, phi_reg);
			}
		}

		/* Now the color of the arg (arg may be a dupl now) and the phi-result are equal.
		 * Pin it, so everyone knows and it never gets changed again.
		 * An arg never is a phi, because perms were inserted. So the link field is free */
		DBG((dbg, LEVEL_1, "      arg has correct color (now), so pin it\n"));
		pin_irn(arg, phi_block);
	}
}


static void insert_all_perms(be_main_session_env_t *session, be_chordal_env_t *chordal_env) {
	pmap_entry *pme;
	int i, max;
	ir_node *first_phi, *recent_phi;

	DBG((dbg, LEVEL_1, "Placing perms...\n"));

	/* place perms in cf-preds of phis */
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;

		first_phi = NULL;
		/* iterate over the first ops in the block until a non-phi is reached */
		list_for_each_entry(border_t, curr, head, list) {
			ir_node *phi = curr->irn;

			if (curr->is_def && curr->is_real && is_Phi(phi)) {
				set_irn_link(phi, NULL);
				/* chain of phis in a block */
				if (first_phi == NULL)
					first_phi = phi;
				else
					set_irn_link(recent_phi, phi);
				recent_phi = phi;

				/* insert perms */
				DBG((dbg, LEVEL_1, "  for %+F\n", phi));
				for(i=0, max=get_irn_arity(phi); i<max; ++i) {
					ir_node *perm;

					perm = get_or_insert_perm(session, chordal_env, get_Block_cfgpred_block(get_nodes_block(phi), i));
					DBG((dbg, LEVEL_1, "    %+F in block %N\n", perm, get_Block_cfgpred_block(get_nodes_block(phi), i)));
					set_irn_link(perm, NULL);
				}
			}
		}
		if (first_phi)
			set_irn_link(recent_phi, first_phi);
	}
}

static void	set_regs_or_place_dupls(be_main_session_env_t *session, be_chordal_env_t *chordal_env) {
	pmap_entry *pme;

	DBG((dbg, LEVEL_1, "Setting regs and placing dupls...\n"));

	/* iterate over all blocks and correct color of arguments*/
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;

		/* iterate over the first ops in the block until a non-phi is reached */
		list_for_each_entry(border_t, curr, head, list)
			if (curr->is_def && curr->is_real && is_Phi(curr->irn))
				adjust_phi_arguments(session, chordal_env, curr->irn);
	}
}

void be_ssa_destruction(be_main_session_env_t *session, be_chordal_env_t *chordal_env) {
	set *b2p;

	dbg = firm_dbg_register("ir.be.ssadestr");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	/* create a map for fast lookup of perms: block --> perm */
	b2p = new_set(set_cmp_b2p, 32);
	chordal_env->data = b2p;

	insert_all_perms(session, chordal_env);
	dump_ir_block_graph(session->irg, "-ssa_destr_perms_placed");

	set_regs_or_place_dupls(session, chordal_env);
	dump_ir_block_graph(session->irg, "-ssa_destr_regs_set");

	del_set(b2p);
}

void be_ssa_destruction_check(be_main_session_env_t *session, be_chordal_env_t *chordal_env) {
	pmap_entry *pme;
	int i, max;

	/* iterate over all blocks */
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;

		/* iterate over the first ops in the block */
		list_for_each_entry(border_t, curr, head, list)
			if (curr->is_def && curr->is_real && is_Phi(curr->irn)) {
				const arch_register_t *phi_reg, *arg_reg;
				ir_node *phi = curr->irn;

				phi_reg = get_reg(phi);
				/* iterate over all args of phi */
				for(i=0, max=get_irn_arity(phi); i<max; ++i) {
					ir_node *arg = get_irn_n(phi, i);
					arg_reg = get_reg(arg);
					if(phi_reg != arg_reg) {
						ir_printf("Registers of %+F and %+F differ: %s %s\n", phi, arg, phi_reg->name, arg_reg->name);
						assert(0 && "Registers of phi and arg differ\n");
					}
					if(!is_pinned(arg))
						ir_printf("Warning: Arg %+F not pinned\n", arg);
				}
			}
	}
}
