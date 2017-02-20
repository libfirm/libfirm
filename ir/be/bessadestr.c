/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Performs SSA destruction.
 * @author      Daniel Grund, Manuel Mohr
 * @date        25.05.2005
 */
#include "bessadestr.h"

#include "be_types.h"
#include "bearch.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "besched.h"
#include "bespillutil.h"
#include "bitset.h"
#include "debug.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "statev_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* We represent a parallel copy/register transfer graph as follows.  As usual,
 * nodes are registers and edges are move operations.
 * - We exploit the fact that each node has at most one incoming edge and
 *   save the reverse graph: parcopy[n] contains the node index of the
 *   *source* node of n.
 * - We mark nodes that do not have an incoming edge with parcopy[n] == n_regs.
 * - Self-loops are explicitely represented as parcopy[n] == n.
 * - Additionally, we maintain a second array n_used where n_used[n] gives us
 *   the number of *outgoing* edges of node n.
 */
static void impl_parcopy(const arch_register_class_t *cls,
                         ir_node *before, unsigned *parcopy, unsigned *n_used,
                         ir_node **phis, ir_node **phi_args, unsigned pred_nr)
{
	ir_node        *block        = get_nodes_block(before);
	ir_graph       *irg          = get_irn_irg(block);
	be_lv_t        *lv           = be_get_irg_liveness(irg);
	const unsigned  n_regs       = cls->n_regs;
	unsigned        num_restores = 0;
	unsigned        restore_srcs[n_regs];
	unsigned        restore_dsts[n_regs];

	/* Step 1: Search register transfer graph for nodes with > 1 outgoing edge.
	 * Goal: Establish invariant that each node has <= 1 outgoing edges by
	 *       recording restore copies (and inserting them later). */
	for (unsigned to_reg = 0; to_reg < n_regs; ++to_reg) {
		const unsigned from_reg    = parcopy[to_reg];
		unsigned       from_n_used = n_used[from_reg];

		if (from_reg == n_regs)
			continue;

		/* Decide if the current edge should be kept or not.
		 * We keep an edge if it is a self-loop or if it is the last outgoing
		 * edge of a node with multiple outgoing edges.
		 * TODO: More clever heuristics which edge to keep? */
		if (from_reg == to_reg || from_n_used == 1) {
			/* Keep from_reg -> to_reg and cut every other edge. */

			for (unsigned dst = 0; dst < n_regs; ++dst) {
				if (dst != to_reg && parcopy[dst] == from_reg) {
					/* Note: As we insert restore copies *after* the Perm node,
					 * we record to_reg as the source of the copy. */
					restore_srcs[num_restores] = to_reg;
					restore_dsts[num_restores] = dst;
					++num_restores;

					/* Mark as done. */
					parcopy[dst] = n_regs;
					if (dst > to_reg) {
						--from_n_used;
					}
				}
			}

			assert(from_n_used == 1);
			n_used[from_reg] = 1;
		} else {
			--n_used[from_reg];
		}
	}

#ifndef NDEBUG
	/* Check invariant. */
	for (unsigned dst = 0; dst < n_regs; ++dst) {
		const unsigned src = parcopy[dst];
		assert(src == n_regs || n_used[src] == 1);
	}
#endif

	/* Step 2: Build Perm node and Proj node(s). */
	unsigned  perm_size = 0;
	ir_node  *perm_ins[n_regs];
	for (unsigned dst = 0; dst < n_regs; ++dst) {
		const unsigned src = parcopy[dst];
		if (src != n_regs && src != dst) {
			assert(phi_args[src] != NULL);
			perm_ins[perm_size++] = phi_args[src];
		}
	}

	if (perm_size > 0) {
		ir_node *const perm = be_new_Perm(block, perm_size, perm_ins);
		sched_add_before(before, perm);

		unsigned i = 0;
		for (unsigned dst = 0; dst < n_regs; ++dst) {
			const unsigned src = parcopy[dst];
			if (src == n_regs || src == dst)
				continue;

			arch_register_t const *const reg  = arch_register_for_index(cls, dst);
			ir_node               *const proj = be_new_Proj_reg(perm, i, reg);

			ir_node *phi = phis[dst];
			set_irn_n(phi, pred_nr, proj);
			phi_args[dst] = proj;

			if (lv->sets_valid) {
				be_liveness_introduce(lv, proj);
				be_liveness_update(lv, perm_ins[i]);
			}

			++i;
		}
	}

	/* Step 3: Place restore copies. */
	for (unsigned i = 0; i < num_restores; ++i) {
		const unsigned  src_reg = restore_srcs[i];
		const unsigned  dst_reg = restore_dsts[i];

		ir_node               *const src  = phi_args[src_reg];
		arch_register_t const *const reg  = arch_register_for_index(cls, dst_reg);
		ir_node               *const copy = be_new_Copy_before_reg(src, before, reg);

		ir_node *phi = phis[dst_reg];
		set_irn_n(phi, pred_nr, copy);
		phi_args[src_reg] = copy;

		if (lv->sets_valid) {
			be_liveness_introduce(lv, copy);
			be_liveness_update(lv, src);
		}
	}
}

static void insert_shuffle_code_walker(ir_node *block, void *data)
{
	if (!is_Phi(sched_first(block)))
		return;

	const arch_register_class_t *cls    = (const arch_register_class_t*)data;
	ir_graph                    *irg    = get_irn_irg(block);
	be_lv_t                     *lv     = be_get_irg_liveness(irg);
	const unsigned               n_regs = cls->n_regs;

	for (int pred_nr = 0; pred_nr < get_irn_arity(block); ++pred_nr) {
		unsigned  parcopy [n_regs];
		unsigned  n_used  [n_regs];
		ir_node  *phis    [n_regs];
		ir_node  *phi_args[n_regs];
		bitset_t *keep_val = bitset_alloca(n_regs);

		memset(n_used,   0, n_regs * sizeof(n_used[0]));
		memset(phis,     0, n_regs * sizeof(phis[0]));
		memset(phi_args, 0, n_regs * sizeof(phi_args[0]));
		for (unsigned i = 0; i < n_regs; ++i) {
			parcopy[i] = n_regs;
		}

		bool need_perm = false;
		sched_foreach_phi(block, phi) {
			if (!arch_irn_consider_in_reg_alloc(cls, phi))
				continue;

			const unsigned  phi_reg_idx = arch_get_irn_register(phi)->index;
			ir_node        *arg         = get_irn_n(phi, pred_nr);
			const unsigned  arg_reg_idx = arch_get_irn_register(arg)->index;

			assert(parcopy[phi_reg_idx] == n_regs);
			parcopy[phi_reg_idx] = arg_reg_idx;
			++n_used[arg_reg_idx];

			if (phi_reg_idx != arg_reg_idx)
				need_perm = true;

			/* If arg is live, we must keep it in its current register.
			 * This is done by adding a self-loop to the register transfer
			 * graph.
			 * Because we may see the same arg multiple times, we use a bitset
			 * instead of directly incrementing n_used.
			 */
			if (be_is_live_in(lv, block, arg)) {
				assert(parcopy[arg_reg_idx] == n_regs || parcopy[arg_reg_idx] == arg_reg_idx);
				parcopy[arg_reg_idx] = arg_reg_idx;
				bitset_set(keep_val, arg_reg_idx);
			}

			assert(phis[phi_reg_idx] == NULL);
			phis[phi_reg_idx] = phi;
			assert(phi_args[arg_reg_idx] == NULL || phi_args[arg_reg_idx] == arg);
			phi_args[arg_reg_idx] = arg;
		}

		if (need_perm) {
			for (unsigned i = 0; i < n_regs; ++i)
				n_used[i] += bitset_is_set(keep_val, i);

			ir_node *pred   = get_Block_cfgpred_block(block, pred_nr);
			ir_node *before = be_get_end_of_block_insertion_point(pred);
			impl_parcopy(cls, before, parcopy, n_used, phis, phi_args, pred_nr);
		}
	}
}

void be_ssa_destruction(ir_graph *irg, const arch_register_class_t *cls)
{
	FIRM_DBG_REGISTER(dbg, "ir.be.ssadestr");

	be_invalidate_live_sets(irg);
	be_assure_live_chk(irg);

	irg_block_walk_graph(irg, insert_shuffle_code_walker, NULL, (void*)cls);

	/* unfortunately updating doesn't work yet. */
	be_invalidate_live_chk(irg);
}
