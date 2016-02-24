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
 * @brief       Performs SSA-Destruction.
 * @author      Daniel Grund
 * @date        25.05.2005
 */
#include "config.h"

#include "bessadestr.h"

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
#include "irtools.h"

#include "be_t.h"
#include "beutil.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "belive_t.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespillutil.h"
#include "statev_t.h"
#include "beirg.h"
#include "beintlive_t.h"
#include "bertg.h"

#include "lc_opts.h"

#include "gen_sparc_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg_icore = NULL;)
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static int build_icore_perms = 0;

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_BOOL("build_icore_perms", "build Perm nodes suited for iCore", &build_icore_perms),
	LC_OPT_LAST
};

static be_chordal_env_t *the_env = NULL;

static const char *get_reg_name(unsigned index)
{
	return arch_register_for_index(the_env->cls, index)->name;
}

static void impl_parallel_copy(ir_node *before, unsigned *parcopy, unsigned *n_used,
                               ir_node **phis, ir_node **phi_args, unsigned prednr,
                               unsigned opt_costs)
{
	ir_node        *block  = get_nodes_block(before);
	const unsigned  n_regs = the_env->cls->n_regs;
	be_lv_t        *lv     = be_get_irg_liveness(get_irn_irg(before));
	unsigned        restore_srcs[n_regs];
	unsigned        restore_dsts[n_regs];
	unsigned        num_restores = 0;

	decompose_rtg(parcopy, n_used, restore_srcs, restore_dsts, &num_restores, the_env->cls);

	/* Step 3: The remaining parallel copy must be suitable for a Perm. */
	unsigned perm_size = 0;
	ir_node *ins[n_regs];
	for (unsigned r = 0; r < n_regs; ++r) {
		unsigned src = parcopy[r];
		if (src != r) {
			assert(phi_args[src] != NULL);
			ins[perm_size++] = phi_args[src];
		}
	}

	ir_node *perm = NULL;
	if (perm_size > 0) {
		perm = be_new_Perm(the_env->cls, block, perm_size, ins);
		DB((dbg_icore, LEVEL_2, "Created %+F using permutation.\n", perm));
		sched_add_before(before, perm);
		unsigned input = 0;
		for (unsigned r = 0; r < n_regs; ++r) {
			unsigned src = parcopy[r];
			if (src != r) {
				ir_node *proj = new_r_Proj(perm, get_irn_mode(ins[input]), input);

				const arch_register_t *reg = arch_register_for_index(the_env->cls, r);
				arch_set_irn_register(proj, reg);

				assert(n_used[src] == 1);
				if (parcopy[src] == src) {
					DB((dbg_icore, LEVEL_2, "Perm: Freeing register %s of value %+F\n", get_reg_name(src), ins[input]));
					be_liveness_update(lv, phi_args[src]);
				}

				ir_node *phi = phis[r];
				assert(phi != NULL);
				set_irn_n(phi, prednr, proj);
				phi_args[r] = proj;

				be_liveness_introduce(lv, proj);
				be_liveness_update(lv, ins[input]);

				++input;
			}
		}
		be_liveness_introduce(lv, perm);

		for (unsigned r = 0; r < n_regs; ++r)
			parcopy[r] = r;
	}

#ifndef NDEBUG
	/* now we should only have fixpoints left */
	for (unsigned r = 0; r < n_regs; ++r) {
		assert(parcopy[r] == r);
	}
#endif

	if (num_restores > 0) {
		/* Step 4: Place restore movs. */
		DB((dbg_icore, LEVEL_2, "Placing restore movs.\n"));
		for (unsigned i = 0; i < num_restores; ++i) {
			unsigned src_reg = restore_srcs[i];
			unsigned dst_reg = restore_dsts[i];
			ir_node *src     = phi_args[src_reg];
			ir_node *copy    = be_new_Copy(block, src);
			sched_add_before(before, copy);

			DB((dbg_icore, LEVEL_2, "Inserted restore copy %+F %s -> %s\n", copy, get_reg_name(src_reg), get_reg_name(dst_reg)));
			const arch_register_t *reg = arch_register_for_index(the_env->cls, dst_reg);
			arch_set_irn_register(copy, reg);

			ir_node *phi = phis[dst_reg];
			assert(phi != NULL);
			set_irn_n(phi, prednr, copy);
			phi_args[src_reg] = copy;

			be_liveness_introduce(lv, copy);
			be_liveness_update(lv, src);
		}
		DB((dbg_icore, LEVEL_2, "Finished placing restore movs.\n"));
	}

#ifdef DEBUG_libfirm
	/* Emit statistics. */
	stat_ev_ctx_push_fmt("rtg_cflow", "%ld", get_irn_node_nr(block));
	stat_ev_int("rtg_cflow_opt_costs", opt_costs);
	stat_ev_int("rtg_cflow_num_copies", num_restores);
	if (perm != NULL) {
		stat_ev_int("rtg_cflow_created_perm", get_irn_node_nr(perm));
	}
	stat_ev_ctx_pop("rtg_cflow");
#endif
}

unsigned find_costs_general(unsigned *rtg, unsigned *numUsed, unsigned numRegs, bool dump);

static void analyze_parallel_copies_walker(ir_node *block, void *data)
{
	be_chordal_env_t *chordal_env = (be_chordal_env_t*) data;
	be_lv_t *lv = be_get_irg_liveness(chordal_env->irg);

	assert(is_Block(block));

	if (!get_irn_link(block))
		return;

	the_env = chordal_env;

	const unsigned n_regs = chordal_env->cls->n_regs;

	for (int i = 0; i < get_irn_arity(block); ++i) {
		unsigned parcopy[n_regs];
		unsigned n_used[n_regs];
		unsigned keep_val[n_regs];
		bool     part_of_rtg[n_regs];
		ir_node *phi_args[n_regs];
		ir_node *phis[n_regs];

		for (unsigned i = 0; i < n_regs; ++i) {
			parcopy[i] = i;
		}
		memset(n_used, 0, n_regs * sizeof(unsigned));
		memset(keep_val, 0, n_regs * sizeof(unsigned));
		memset(part_of_rtg, false, n_regs * sizeof(bool));
		memset(phi_args, 0, n_regs * sizeof(ir_node*));
		memset(phis, 0, n_regs * sizeof(ir_node*));

		// Always keep %g0.
		keep_val[REG_GP_G0] = 1;

		for (ir_node *phi = (ir_node *) get_irn_link(block); phi != NULL;
		     phi = (ir_node *) get_irn_link(phi)) {

			const arch_register_t *phi_reg = arch_get_irn_register(phi);
			ir_node               *arg     = get_irn_n(phi, i);
			const arch_register_t *arg_reg = arch_get_irn_register(arg);

			if (phi_reg == arg_reg
				|| (arg_reg->type & arch_register_type_joker)
				|| (arg_reg->type & arch_register_type_virtual)) {

				keep_val[arg_reg->index] = 1;
				continue;
			}

			assert(parcopy[phi_reg->index] == phi_reg->index);
			parcopy[phi_reg->index] = arg_reg->index;
			DB((dbg_icore, LEVEL_2, "copy %s -> %s\n", arg_reg->name, phi_reg->name));
			++n_used[arg_reg->index];
			part_of_rtg[arg_reg->index] = true;
			part_of_rtg[phi_reg->index] = true;

			if (be_is_live_in(lv, block, arg))
				keep_val[arg_reg->index] = 1;

			assert(phis[phi_reg->index] == NULL);
			phis[phi_reg->index] = phi;
			if (phi_args[arg_reg->index] != NULL)
				assert(phi_args[arg_reg->index] == arg);
			phi_args[arg_reg->index] = arg;
		}

		unsigned num_rtg_nodes = 0;
		for (unsigned i = 0; i < n_regs; ++i) {
			n_used[i] += keep_val[i];
			if (part_of_rtg[i])
				++num_rtg_nodes;
		}

		if (num_rtg_nodes > 0) {
			ir_node *pred   = get_Block_cfgpred_block(block, i);
			ir_node *before = be_get_end_of_block_insertion_point(pred);
#ifdef DEBUG_libfirm
			DB((dbg_icore, LEVEL_2, "RTG has %u nodes.\n", num_rtg_nodes));
			DB((dbg_icore, LEVEL_2, "copies for %+F:\n", pred));
			print_parcopy(parcopy, n_used, chordal_env->cls, dbg_icore);
			stat_ev_int("bessadestr_num_rtg_nodes", num_rtg_nodes);
#endif

			unsigned opt_costs;
			{
				unsigned raw_rtg[n_regs];
				unsigned raw_n_used[n_regs];
				for (unsigned i = 0; i < n_regs; ++i) {
					if (parcopy[i] != i)
						raw_rtg[i] = parcopy[i];
					else if (keep_val[i])
						raw_rtg[i] = i;
					else
						raw_rtg[i] = n_regs;

					raw_n_used[i] = n_used[i];
				}
				opt_costs = find_costs_general(raw_rtg, raw_n_used, n_regs, false);
			}

			impl_parallel_copy(before, parcopy, n_used, phis, phi_args, i, opt_costs);
			DB((dbg_icore, LEVEL_2, "\n"));
		}
	}
}

static void clear_link(ir_node *irn, void *data)
{
	(void) data;
	set_irn_link(irn, NULL);
}

/**
 * For each block build a linked list of phis that
 *  - are in that block
 *  - have the current register class
 * The list is rooted at get_irn_link(BB).
 */
static void collect_phis_walker(ir_node *irn, void *data)
{
	be_chordal_env_t *env = (be_chordal_env_t*)data;
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

static int cmp_perm_proj(const void *a, const void *b, size_t n)
{
	const perm_proj_t *p = (const perm_proj_t*)a;
	const perm_proj_t *q = (const perm_proj_t*)b;
	(void) n;

	return !(p->arg == q->arg);
}

/**
 * Insert Perms in all predecessors of a block containing a phi
 */
static void insert_all_perms_walker(ir_node *bl, void *data)
{
	be_chordal_env_t *const chordal_env = (be_chordal_env_t*)data;
	be_lv_t *lv = be_get_irg_liveness(chordal_env->irg);
	int i, n;

	assert(is_Block(bl));

	/* If the link flag is NULL, this block has no phis. */
	if (!get_irn_link(bl))
		return;

	/* Look at all predecessors of the phi block */
	for (i = 0, n = get_irn_arity(bl); i < n; ++i) {
		ir_node *phi, *perm, *insert_after, **in;
		set *arg_set     = new_set(cmp_perm_proj, chordal_env->cls->n_regs);
		ir_node *pred_bl = get_Block_cfgpred_block(bl, i);
		int n_projs      = 0;

		/*
		 * Note that all phis in the list are in the same
		 * register class by construction.
		 */
		for (phi = (ir_node*)get_irn_link(bl); phi != NULL;
		     phi = (ir_node*)get_irn_link(phi)) {
			ir_node                   *arg = get_irn_n(phi, i);
			unsigned                   hash;
			perm_proj_t                templ;

			hash = hash_irn(arg);
			templ.arg  = arg;
			perm_proj_t *const pp = set_find(perm_proj_t, arg_set, &templ, sizeof(templ), hash);

			/*
			 * If a proj_perm_t entry has not been made in the argument set,
			 * create one. The only restriction is, that the phi argument
			 * may not be live in at the current block, since this argument
			 * interferes with the phi and must thus not be member of a
			 * Perm. A copy will be inserted for this argument later on.
			 */
			if (!pp && !be_is_live_in(lv, bl, arg)) {
				templ.pos = n_projs++;
				(void)set_insert(perm_proj_t, arg_set, &templ, sizeof(templ), hash);
			}
		}


		if (n_projs) {
			/*
			 * Create a new Perm with the arguments just collected
			 * above in the arg_set and insert it into the schedule.
			 */
			in = XMALLOCN(ir_node*, n_projs);
			foreach_set(arg_set, perm_proj_t, pp) {
				in[pp->pos] = pp->arg;
			}

			perm = be_new_Perm(chordal_env->cls, pred_bl, n_projs, in);
			DEBUG_ONLY(stat_ev_int("phi_perm", n_projs));

			insert_after = pred_bl;
			do {
				insert_after = sched_prev(insert_after);
			} while (is_cfop(insert_after));
			sched_add_after(insert_after, perm);

			/*
			 * Make the Projs for the Perm and insert into schedule.
			 * Register allocation is copied from the former phi
			 * arguments to the projs (new phi arguments).
			 */
			foreach_set(arg_set, perm_proj_t, pp) {
				ir_node *proj = new_r_Proj(perm, get_irn_mode(pp->arg), pp->pos);
				pp->proj = proj;
				assert(arch_get_irn_register(pp->arg));
				arch_set_irn_register(proj, arch_get_irn_register(pp->arg));
				DBG((dbg, LEVEL_2, "Copy register assignment %s from %+F to %+F\n", arch_get_irn_register(pp->arg)->name, pp->arg, pp->proj));
			}

			/*
			 * Set the phi nodes to their new arguments: The Projs of the Perm
			 */
			for (phi = (ir_node*)get_irn_link(bl); phi != NULL;
			     phi = (ir_node*)get_irn_link(phi)) {
				perm_proj_t templ;

				templ.arg = get_irn_n(phi, i);
				perm_proj_t *const pp = set_find(perm_proj_t, arg_set, &templ, sizeof(templ), hash_irn(templ.arg));

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
static void set_regs_or_place_dupls_walker(ir_node *bl, void *data)
{
	be_chordal_env_t *chordal_env = (be_chordal_env_t*)data;
	be_lv_t *lv = be_get_irg_liveness(chordal_env->irg);
	ir_node *phi;

	/* Consider all phis of this block */
	for (phi = (ir_node*)get_irn_link(bl); phi != NULL;
	     phi = (ir_node*)get_irn_link(phi)) {
		ir_node                     *phi_block = get_nodes_block(phi);
		const arch_register_t       *phi_reg   = arch_get_irn_register(phi);
		int                          max;
		int                          i;

		assert(is_Phi(phi) && "Can only handle phi-destruction :)");

		/* process all arguments of the phi */
		for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
			ir_node                   *arg = get_irn_n(phi, i);
			const arch_register_t     *arg_reg;
			ir_node                   *arg_block;

			arg_block = get_Block_cfgpred_block(phi_block, i);
			arg_reg   = arch_get_irn_register(arg);

			assert(arg_reg && "Register must be set while placing perms");

			DBG((dbg, LEVEL_1, "  for %+F(%s) -- %+F(%s)\n", phi, phi_reg->name, arg, arg_reg->name));

			if (phi_reg == arg_reg
					|| (arg_reg->type & arch_register_type_joker)
					|| (arg_reg->type & arch_register_type_virtual)) {
				/* Phi and arg have the same register, so pin and continue */
				pin_irn(arg, phi_block);
				DBG((dbg, LEVEL_1, "      arg has same reg: pin %+F(%s)\n", arg, arch_get_irn_register(arg)->name));
				continue;
			}

			if (be_values_interfere(lv, phi, arg)) {
				ir_node *schedpoint;

				/*
					Insert a duplicate in arguments block,
					make it the new phi arg,
					set its register,
					insert it into schedule,
					pin it
				*/
				ir_node *dupl = be_new_Copy(arg_block, arg);
				DEBUG_ONLY(stat_ev_int("bessadestr_copies", 1));

				set_irn_n(phi, i, dupl);
				arch_set_irn_register(dupl, phi_reg);
				schedpoint = arg_block;
				do {
					schedpoint = sched_prev(schedpoint);
				} while (is_cfop(schedpoint));
				sched_add_after(schedpoint, dupl);
				pin_irn(dupl, phi_block);
				be_liveness_introduce(lv, dupl);
				be_liveness_update(lv, arg);
				DBG((dbg, LEVEL_1, "    they do interfere: insert %+F(%s)\n", dupl, arch_get_irn_register(dupl)->name));
				continue; /* with next argument */
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

				for (other_phi = (ir_node*)get_irn_link(phi_block);
				     other_phi != NULL;
				     other_phi = (ir_node*)get_irn_link(other_phi)) {

					assert(is_Phi(other_phi)                               &&
						get_nodes_block(phi) == get_nodes_block(other_phi) &&
						"link fields are screwed up");

					if (get_irn_n(other_phi, i) == arg && arch_get_irn_register(other_phi) == arg_reg) {
						DBG((dbg, LEVEL_1, "        found %+F(%s)\n", other_phi, arch_get_irn_register(other_phi)->name));
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
				ir_node *perm = get_Proj_pred(arg);
				ir_node *dupl = be_new_Copy(arg_block, arg);
				ir_node *ins;

				DEBUG_ONLY(stat_ev_int("bessadestr_copies", 1));
				set_irn_n(phi, i, dupl);
				arch_set_irn_register(dupl, phi_reg);
				/* skip the Perm's Projs and insert the copies behind. */
				for (ins = sched_next(perm); is_Proj(ins); ins = sched_next(ins)) {
				}
				sched_add_before(ins, dupl);
				pin_irn(dupl, phi_block);
				be_liveness_introduce(lv, dupl);
				be_liveness_update(lv, arg);
				DBG((dbg, LEVEL_1, "      arg is pinned: insert %+F(%s)\n", dupl, arch_get_irn_register(dupl)->name));
			} else {
				/*
					No other phi has the same color (else arg would have been pinned),
					so just set the register and pin
				*/
				arch_set_irn_register(arg, phi_reg);
				pin_irn(arg, phi_block);
				DBG((dbg, LEVEL_1, "      arg is not pinned: so pin %+F(%s)\n", arg, arch_get_irn_register(arg)->name));
			}
		}
	}
}

void be_ssa_destruction(be_chordal_env_t *chordal_env)
{
	ir_graph *irg = chordal_env->irg;

	FIRM_DBG_REGISTER(dbg, "ir.be.ssadestr");
	FIRM_DBG_REGISTER(dbg_icore, "ir.be.ssadestr.icore");

	be_invalidate_live_sets(irg);

	/* create a map for fast lookup of perms: block --> perm */
	irg_walk_graph(irg, clear_link, collect_phis_walker, chordal_env);

	if (build_icore_perms) {
		be_assure_live_chk(irg);

		DBG((dbg, LEVEL_1, "Analyzing parallel copies...\n"));
		irg_block_walk_graph(irg, analyze_parallel_copies_walker, NULL, chordal_env);

		be_invalidate_live_chk(irg);

		if (chordal_env->opts->dump_flags & BE_CH_DUMP_SSADESTR)
			dump_ir_graph(irg, "ssa_destr_icore_perms_placed");
	} else {
		DBG((dbg, LEVEL_1, "Placing perms...\n"));
		irg_block_walk_graph(irg, insert_all_perms_walker, NULL, chordal_env);

		if (chordal_env->opts->dump_flags & BE_CH_DUMP_SSADESTR)
			dump_ir_graph(irg, "ssa_destr_perms_placed");

		be_assure_live_chk(irg);

		DBG((dbg, LEVEL_1, "Setting regs and placing dupls...\n"));
		irg_block_walk_graph(irg, set_regs_or_place_dupls_walker, NULL, chordal_env);

		/* unfortunately updating doesn't work yet. */
		be_invalidate_live_chk(irg);

		if (chordal_env->opts->dump_flags & BE_CH_DUMP_SSADESTR)
			dump_ir_graph(irg, "ssa_destr_regs_set");
	}
}

static void ssa_destruction_check_walker(ir_node *bl, void *data)
{
	ir_node *phi;
	int i, max;
	(void)data;

	for (phi = (ir_node*)get_irn_link(bl); phi != NULL;
	     phi = (ir_node*)get_irn_link(phi)) {
		const arch_register_t *phi_reg, *arg_reg;

		phi_reg = arch_get_irn_register(phi);
		/* iterate over all args of phi */
		for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
			ir_node                   *arg = get_irn_n(phi, i);
			const arch_register_req_t *req = arch_get_irn_register_req(arg);

			if (req->type & arch_register_req_type_ignore)
				continue;

			arg_reg = arch_get_irn_register(arg);

			if (phi_reg != arg_reg) {
				DBG((dbg, 0, "Error: Registers of %+F and %+F differ: %s %s\n", phi, arg, phi_reg->name, arg_reg->name));
				assert(0);
			}

			if (!build_icore_perms && !is_pinned(arg)) {
				DBG((dbg, 0, "Warning: Phi argument %+F is not pinned.\n", arg));
				assert(0);
			}
		}
	}
}

void be_ssa_destruction_check(be_chordal_env_t *chordal_env)
{
	irg_block_walk_graph(chordal_env->irg, ssa_destruction_check_walker, NULL, NULL);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_ssa_destruction)
void be_init_ssa_destruction(void)
{
	lc_opt_entry_t *be_grp         = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ssadestr_group = lc_opt_get_grp(be_grp, "ssadestr");
	lc_opt_add_table(ssadestr_group, options);
}
