/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Performs SSA-Destruction.
 * @author      Daniel Grund
 * @date        25.05.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "irdump.h"
#include "irprintf.h"

#include "be_t.h"
#include "beutil.h"
#include "bechordal_t.h"
#include "bearch_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "besched_t.h"
#include "benodesets.h"
#include "bestatevent.h"
#include "beirg_t.h"
#include "beintlive_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define get_chordal_arch(ce) ((ce)->birg->main_env->arch_env)
#define get_reg(irn) arch_get_irn_register(get_chordal_arch(chordal_env), irn)
#define set_reg(irn, reg) arch_set_irn_register(get_chordal_arch(chordal_env), irn, reg)

#define is_Perm(irn)            (arch_irn_class_is(arch_env, irn, perm))
#define get_reg_cls(irn)        (arch_get_irn_reg_class(arch_env, irn, -1))
#define is_curr_reg_class(irn)  (get_reg_cls(p) == chordal_env->cls)

static void clear_link(ir_node *irn, void *data) {
	(void) data;
	set_irn_link(irn, NULL);
}

/**
 * For each block build a linked list of phis that
 *  - are in that block
 *  - have the current register class
 * The list is rooted at get_irn_link(BB).
 */
static void collect_phis_walker(ir_node *irn, void *data) {
	be_chordal_env_t *env = data;
	if (is_Phi(irn) && chordal_has_class(env, irn)) {
		ir_node *bl = get_nodes_block(irn);
		set_irn_link(irn, get_irn_link(bl));
		set_irn_link(bl, irn);
	}
}

/**
 * This struct represents a Proj for a Perm.
 * It records the argument in the Perm and the corresponding Proj of the
 * Perm.
 */
typedef struct {
	ir_node *arg;  /**< The phi argument to make the Proj for. */
	int pos;       /**< The proj number the Proj will get.
									 This also denotes the position of @p arg
									 in the in array of the Perm. */
	ir_node *proj; /**< The proj created for @p arg. */
} perm_proj_t;

static int cmp_perm_proj(const void *a, const void *b, size_t n) {
	const perm_proj_t *p = a;
	const perm_proj_t *q = b;
	(void) n;

	return !(p->arg == q->arg);
}

typedef struct insert_all_perms_env_t {
	be_chordal_env_t *chordal_env;
	pmap *perm_map;
} insert_all_perms_env_t;

/**
 * Insert Perms in all predecessors of a block containing a phi
 */
static void insert_all_perms_walker(ir_node *bl, void *data) {
	insert_all_perms_env_t *env = data;
	be_chordal_env_t *chordal_env = env->chordal_env;
	pmap *perm_map = env->perm_map;
	ir_graph *irg = chordal_env->irg;
	be_lv_t *lv = chordal_env->birg->lv;
	int i, n;

	assert(is_Block(bl));

	/* If the link flag is NULL, this block has no phis. */
	if(!get_irn_link(bl))
		return;

	/* Look at all predecessors of the phi block */
	for(i = 0, n = get_irn_arity(bl); i < n; ++i) {
		ir_node *phi, *perm, *insert_after, **in;
		perm_proj_t *pp;
		set *arg_set     = new_set(cmp_perm_proj, chordal_env->cls->n_regs);
		ir_node *pred_bl = get_Block_cfgpred_block(bl, i);
		int n_projs      = 0;

		assert(!pmap_contains(perm_map, pred_bl) && "Already permed that block");

		/*
		 * Note that all phis in the list are in the same
		 * register class by construction.
		 */
		for(phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
			perm_proj_t templ;
			ir_node *arg     = get_irn_n(phi, i);
			unsigned hash    = nodeset_hash(arg);

			if (arch_irn_is(chordal_env->birg->main_env->arch_env, arg, ignore))
				continue;

			templ.arg  = arg;
			pp         = set_find(arg_set, &templ, sizeof(templ), hash);

			/*
			 * If a proj_perm_t entry has not been made in the argument set,
			 * create one. The only restriction is, that the phi argument
			 * may not be live in at the current block, since this argument
			 * interferes with the phi and must thus not be member of a
			 * Perm. A copy will be inserted for this argument alter on.
			 */
			if(!pp && !be_is_live_in(lv, bl, arg)) {
				templ.pos = n_projs++;
				set_insert(arg_set, &templ, sizeof(templ), hash);
			}
		}


		if (n_projs) {
			/*
			 * Create a new Perm with the arguments just collected
			 * above in the arg_set and insert it into the schedule.
			 */
			in = xmalloc(n_projs * sizeof(in[0]));
			for(pp = set_first(arg_set); pp; pp = set_next(arg_set))
				in[pp->pos] = pp->arg;

			perm = be_new_Perm(chordal_env->cls, irg, pred_bl, n_projs, in);
			be_stat_ev("phi_perm", n_projs);

			insert_after = sched_skip(sched_last(pred_bl), 0, sched_skip_cf_predicator, chordal_env->birg->main_env->arch_env);
			sched_add_after(insert_after, perm);

			/*
			 * Make the Projs for the Perm and insert into schedule.
			 * Register allocation is copied from the former phi
			 * arguments to the projs (new phi arguments).
			 */
			insert_after = perm;
			for(pp = set_first(arg_set); pp; pp = set_next(arg_set)) {
				ir_node *proj = new_r_Proj(irg, pred_bl, perm, get_irn_mode(pp->arg), pp->pos);
				pp->proj = proj;
				assert(get_reg(pp->arg));
				set_reg(proj, get_reg(pp->arg));
#ifdef SCHEDULE_PROJS
				sched_add_after(insert_after, proj);
#endif
				insert_after = proj;
				DBG((dbg, LEVEL_2, "Copy register assignment %s from %+F to %+F\n", get_reg(pp->arg)->name, pp->arg, pp->proj));
			}

			/*
			 * Set the phi nodes to their new arguments: The Projs of the Perm
			 */
			for(phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
				perm_proj_t templ;

				templ.arg = get_irn_n(phi, i);
				pp        = set_find(arg_set, &templ, sizeof(templ), nodeset_hash(templ.arg));

				/* If not found, it was an interfering argument */
				if (pp) {
					set_irn_n(phi, i, pp->proj);
					be_liveness_introduce(lv, pp->proj);
				}
			}

			/* update the liveness of the Perm's operands. It might be changed. */
			{
				int i;
				for (i = 0; i < n_projs; ++i)
					be_liveness_update(lv, in[i]);
			}
			free(in);

			/* register in perm map */
			pmap_insert(perm_map, pred_bl, perm);
		}

		del_set(arg_set);
	}
}

#define is_pinned(irn) (get_irn_link(irn))
#define get_pinning_block(irn) ((ir_node *)get_irn_link(irn))
#define pin_irn(irn, lock) (set_irn_link(irn, lock))

/**
 * Adjusts the register allocation for the (new) phi-operands
 * and insert duplicates iff necessary.
 */
static void	set_regs_or_place_dupls_walker(ir_node *bl, void *data) {
	be_chordal_env_t *chordal_env = data;
	be_lv_t *lv = chordal_env->birg->lv;
	ir_node *phi;

	/* Consider all phis of this block */
	for (phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
		int i, max;
		ir_node *arg, *phi_block, *arg_block;
		const arch_register_t *phi_reg, *arg_reg;
		const arch_register_class_t *cls;

		assert(is_Phi(phi) && "Can only handle phi-destruction :)");

		phi_block = get_nodes_block(phi);
		phi_reg   = get_reg(phi);
		cls       = arch_get_irn_reg_class(get_chordal_arch(chordal_env), phi, -1);

		/* process all arguments of the phi */
		for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
			arg       = get_irn_n(phi, i);
			arg_block = get_Block_cfgpred_block(phi_block, i);
			arg_reg   = get_reg(arg);

			if (arch_irn_is(chordal_env->birg->main_env->arch_env, arg, ignore))
				continue;

			assert(arg_reg && "Register must be set while placing perms");

			DBG((dbg, LEVEL_1, "  for %+F(%s) -- %+F(%s)\n", phi, phi_reg->name, arg, arg_reg->name));

			if (values_interfere(chordal_env->birg, phi, arg)) {
				/*
					Insert a duplicate in arguments block,
					make it the new phi arg,
					set its register,
					insert it into schedule,
					pin it
				*/
				ir_node *dupl  = be_new_Copy(cls, chordal_env->irg, arg_block, arg);

				/* this is commented out because it will fail in case of unknown float */
#if 0
				ir_mode *m_phi = get_irn_mode(phi), *m_dupl = get_irn_mode(dupl);

				/*
					Conv signed <-> unsigned is killed on ia32
					check for: (both int OR both float) AND equal mode sizes
				*/
				assert(((mode_is_int(m_phi) && mode_is_int(m_dupl)) ||
					(mode_is_float(m_phi) && mode_is_float(m_dupl))) &&
					(get_mode_size_bits(m_phi) == get_mode_size_bits(m_dupl)));
#endif /* if 0 */

				set_irn_n(phi, i, dupl);
				set_reg(dupl, phi_reg);
				sched_add_after(sched_skip(sched_last(arg_block), 0, sched_skip_cf_predicator, chordal_env->birg->main_env->arch_env), dupl);
				pin_irn(dupl, phi_block);
				be_liveness_introduce(lv, dupl);
				be_liveness_update(lv, arg);
				DBG((dbg, LEVEL_1, "    they do interfere: insert %+F(%s)\n", dupl, get_reg(dupl)->name));
				continue; /* with next argument */
			}

			if (phi_reg == arg_reg) {
				/* Phi and arg have the same register, so pin and continue */
				pin_irn(arg, phi_block);
				DBG((dbg, LEVEL_1, "      arg has same reg: pin %+F(%s)\n", arg, get_reg(arg)->name));
				continue;
			}

			DBG((dbg, LEVEL_1, "    they do not interfere\n"));
			assert(is_Proj(arg));
			/*
				First check if there is an other phi
				- in the same block
				- having arg at the current pos in its arg-list
				- having the same color as arg

				If found, then pin the arg (for that phi)
			*/
			if (! is_pinned(arg)) {
				ir_node *other_phi;

				DBG((dbg, LEVEL_1, "      searching for phi with same arg having args register\n"));

				for(other_phi = get_irn_link(phi_block); other_phi; other_phi = get_irn_link(other_phi)) {

					assert(is_Phi(other_phi)                               &&
						get_nodes_block(phi) == get_nodes_block(other_phi) &&
						"link fields are screwed up");

					if (get_irn_n(other_phi, i) == arg && get_reg(other_phi) == arg_reg) {
						DBG((dbg, LEVEL_1, "        found %+F(%s)\n", other_phi, get_reg(other_phi)->name));
						pin_irn(arg, phi_block);
						break;
					}
				}
			}

			if (is_pinned(arg)) {
				/*
					Insert a duplicate of the original value in arguments block,
					make it the new phi arg,
					set its register,
					insert it into schedule,
					pin it
				*/
				ir_node *perm     = get_Proj_pred(arg);
				ir_node *dupl     = be_new_Copy(cls, chordal_env->irg, arg_block, arg);
				ir_node *ins;

				/* this is commented out because it will fail in case of unknown float */
#if 0
				ir_mode *m_phi    = get_irn_mode(phi);
				ir_mode *m_dupl   = get_irn_mode(dupl);

				/*
					Conv signed <-> unsigned is killed on ia32
					check for: (both int OR both float) AND equal mode sizes
				*/
				assert(((mode_is_int(m_phi) && mode_is_int(m_dupl)) ||
					(mode_is_float(m_phi) && mode_is_float(m_dupl))) &&
					(get_mode_size_bits(m_phi) == get_mode_size_bits(m_dupl)));
#endif /* if 0 */

				set_irn_n(phi, i, dupl);
				set_reg(dupl, phi_reg);
				/* skip the Perm's Projs and insert the copies behind. */
				for(ins = sched_next(perm); is_Proj(ins); ins = sched_next(ins));
				sched_add_before(ins, dupl);
				pin_irn(dupl, phi_block);
				be_liveness_introduce(lv, dupl);
				be_liveness_update(lv, arg);
				DBG((dbg, LEVEL_1, "      arg is pinned: insert %+F(%s)\n", dupl, get_reg(dupl)->name));
			} else {
				/*
					No other phi has the same color (else arg would have been pinned),
					so just set the register and pin
				*/
				set_reg(arg, phi_reg);
				pin_irn(arg, phi_block);
				DBG((dbg, LEVEL_1, "      arg is not pinned: so pin %+F(%s)\n", arg, get_reg(arg)->name));
			}
		}
	}
}

void be_ssa_destruction(be_chordal_env_t *chordal_env) {
	insert_all_perms_env_t insert_perms_env;
	pmap *perm_map = pmap_create();
	ir_graph *irg  = chordal_env->irg;
	be_lv_t *lv    = be_assure_liveness(chordal_env->birg);

	FIRM_DBG_REGISTER(dbg, "ir.be.ssadestr");

	be_liveness_invalidate(lv);
	be_liveness_assure_sets(lv);

	/* create a map for fast lookup of perms: block --> perm */
	irg_walk_graph(irg, clear_link, collect_phis_walker, chordal_env);

	DBG((dbg, LEVEL_1, "Placing perms...\n"));
	insert_perms_env.chordal_env = chordal_env;
	insert_perms_env.perm_map = perm_map;
	irg_block_walk_graph(irg, insert_all_perms_walker, NULL, &insert_perms_env);

	// Matze: really needed here?
	// Sebastian: Yes. the walker function uses interference.
	be_liveness_invalidate(lv);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_SSADESTR)
		be_dump(irg, "-ssa_destr_perms_placed", dump_ir_block_graph_sched);

	be_liveness_assure_chk(lv);

	DBG((dbg, LEVEL_1, "Setting regs and placing dupls...\n"));
	irg_block_walk_graph(irg, set_regs_or_place_dupls_walker, NULL, chordal_env);

	/* TODO: unfortunaltely updating doesn't work yet. */
	be_liveness_invalidate(lv);

	if (chordal_env->opts->dump_flags & BE_CH_DUMP_SSADESTR)
		be_dump(irg, "-ssa_destr_regs_set", dump_ir_block_graph_sched);

	pmap_destroy(perm_map);
}

static void ssa_destruction_check_walker(ir_node *bl, void *data) {
	be_chordal_env_t *chordal_env = data;
	ir_node *phi;
	int i, max;

	for (phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
		const arch_register_t *phi_reg, *arg_reg;

		phi_reg = get_reg(phi);
		/* iterate over all args of phi */
		for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
			ir_node *arg = get_irn_n(phi, i);

			if (arch_irn_is(chordal_env->birg->main_env->arch_env, arg, ignore))
				continue;

			arg_reg = get_reg(arg);

			if (phi_reg != arg_reg) {
				DBG((dbg, 0, "Error: Registers of %+F and %+F differ: %s %s\n", phi, arg, phi_reg->name, arg_reg->name));
				assert(0);
			}

			if (! is_pinned(arg)) {
				DBG((dbg, 0, "Warning: Phi argument %+F is not pinned.\n", arg));
				assert(0);
			}
		}
	}
}

void be_ssa_destruction_check(be_chordal_env_t *chordal_env) {
	irg_block_walk_graph(chordal_env->irg, ssa_destruction_check_walker, NULL, chordal_env);
}
