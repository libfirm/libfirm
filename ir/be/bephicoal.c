/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdlib.h>

#include "bitset.h"
#include "debug.h"
#include "bechordal.h"
#include "belive.h"

#include "bera_t.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"

#define DEBUG_LVL 1

#define MAX_PHI_CLS_SIZE (1<<(sizeof(unsigned char)*8)) /* possible colors added should fit into unsigned char */
#define MAX_COLORS 16


static firm_dbg_module_t *dbgphi = NULL;


typedef enum _live_status_t {
	livein = 1,
	liveout = 2
} live_status_t;


typedef struct _phi_unit_t {
	unsigned char count;
	unsigned char phi_count;
	ir_node **members;			/* [0..phi_count-1] are phi nodes. [phi_count..count-1] the rest/arguments of the phis */
	bitset_t **used_cols;
	int *tgt_colors;			/* [i] is the color to set for members[i]. [i] == -1 means dont change anything for members[i]*/
	live_status_t *live_info;	/* [i] says how/where members[i] is live */
} phi_unit_t;


static pset *done;


static INLINE void set_color(ir_node *irn, int col) {
	if (!pset_find_ptr(done, irn)) {
		set_irn_color(irn, col);
		pset_insert_ptr(done, irn);
	}
}


static void coalesce(phi_unit_t *pu) {
	int i;
	if (pu->phi_count != 1) {
		DBG((dbgphi, 1, "Dropped: phicount\n"));
		return;
	}
#if 0

	done = pset_new_ptr(64);

	for (i = 0; i < pu->count; ++i)
		if (pu->live_info[i] & livein) {
			DBG((dbgphi, 1, "Dropped: live-in\n"));
			return;
		}

	for (i = 0; i < pu->count; ++i) {
		block_list_t *bl;

		if (pu->tgt_colors[i] == -1)
			continue;

		/* TODO: if we move ahead to swapping (not just allocating new colors) this is wrong */
		set_color(pu->members[i], pu->tgt_colors[i]);
		if (pu->live_info[i] & liveout) {
			bl = get_dominated_dfs(get_nodes_block(pu->members[i]));
		}
	}

	del_pset(done);

#endif
}


static void ana_1_phi(phi_unit_t *pu) {
	ir_node *phi, *phi_blk;
	int i, o, n, col, best_color, size, best_size;
	ir_node **cand, **max_set, **best_max_set;

	cand = malloc(pu->count * sizeof(ir_node*));
	max_set = malloc(pu->count * sizeof(ir_node*));
	best_max_set = malloc(pu->count * sizeof(ir_node*));

	phi = pu->members[0];
	phi_blk = get_nodes_block(phi);


	/* fill live_info */
	DBG((dbgphi, 1, "\tLiveness info\n"));
	/* first the phi */
	if (is_live_out(phi_blk, phi)) {
		pu->live_info[0] = liveout;
		DBG((dbgphi, 1, "\t\t%n lives out\n", phi));
	}

	/* then all args */
	for (i = 0, n = get_irn_arity(phi); i < n; ++i) {
		int midx;
        ir_node *arg, *block_ith_pred;

        arg = get_irn_n(phi, i);
		block_ith_pred = get_nodes_block(get_irn_n(phi_blk, i));

		/* find the arg in the members array */
		midx = -1;
		for (o = 0; o < pu->count; ++o)
			if (pu->members[o] == arg) {
				midx = o;
				break;
			}
		assert(midx != -1 && "All args have to be in the members!\n");

		if (is_live_in(block_ith_pred, arg)) {
			pu->live_info[midx] |= livein;
			DBG((dbgphi, 1, "\t\t%n lives in\n", arg));
		}

		if (is_live_out(block_ith_pred, arg)) {
			pu->live_info[midx] |= liveout;
			DBG((dbgphi, 1, "\t\t%n lives out\n", arg));
		}
	}

	/* find best color */
	best_size = -1;
	for (col = 0; col < MAX_COLORS; ++col) { 		/* TODO: try phi color first */
		DBG((dbgphi, 1, "\tTesting colors %d\n", col));

		memset(cand, 0, pu->count * sizeof(ir_node*));
		memset(max_set, 0, pu->count * sizeof(ir_node*));

		/* BETTER: Alle die mit dem phi interferieren koennen eigentlich
		 * schon frueher raus, da unabhaengig von Farbe. */
		/* for this color get all potential candidates for a max indep. set */
		cand[0] = phi;
		size = 1;
		for (i = 1; i < pu->count; ++i)
			if ((!bitset_is_set(pu->used_cols[i], col) || get_irn_color(pu->members[i]) == col)
				&& !phi_ops_interfere(phi, pu->members[i])) {
				/* color is free or already used by the node
				 * and argument is not interfering with phi */
				cand[i] = pu->members[i];
				DBG((dbgphi, 1, "\t\tAdding candidate %n\n", cand[i]));
				size++;
			}
		if (size <= best_size) {
			/* If the candidate set is smaller the max indep. set wont be larger :) */
			continue;
		}

		/* determine the max indep. set */
		/* TODO: make this 'un-greedy' */
		size = 0;
		for (i = 0; i < pu->count; ++i) {
			int intf_det = 0;
			if (!cand[i])
				continue;

			for (o = 0; o < pu->count; ++o) {
				if (!max_set[o])
					continue;
				if (phi_ops_interfere(cand[i], max_set[o])) {
					DBG((dbgphi, 1, "\t\t\n"));
					intf_det = 1;
					break;
				}
			}

			if (!intf_det) {
				DBG((dbgphi, 1, "\t\tAdding to set %n\n", cand[i]));
				max_set[i] = cand[i];
				size++;
			}
		}


		DBG((dbgphi, 1, "\t\tColor %d resulted in a set size of %d\n", col, size));

		/* Did we find a better max set? */
		if (size > best_size) {
			void *tmp;

			best_color = col;
			best_size = size;

			tmp = best_max_set;
			best_max_set = max_set;
			max_set = tmp;
		}

		/* Is this a best possible set? */
		/* BETTER: Find a better lower bound than pu->count, considering interferences */
		if (best_size == pu->count)
			break;
	}
	DBG((dbgphi, 1, "Best color was %d. %d of %d copies needed.\n", best_color, pu->count-best_size, pu->count-1));


	/* now we have the best_max_set with its best_size
	 * so set the tgt_colors */
	for (i = 0; i < pu->count; ++i)
		if (best_max_set[i] && get_irn_color(best_max_set[i]) != best_color)
			pu->tgt_colors[i] = best_color;
		else
			pu->tgt_colors[i] = -1;

	free(cand);
	free(max_set);
	free(best_max_set);
}


static phi_unit_t *new_phi_unit(pset *pc) {
	phi_unit_t *pu;
	int i, o;
	ir_node *n;

	assert(pset_count(pc) <= MAX_PHI_CLS_SIZE && "Phi class too large!");

	pu = malloc(sizeof(phi_unit_t));
	pu->count = pset_count(pc);
	pu->members = malloc(pu->count * sizeof(*pu->members));
	pu->used_cols = malloc(pu->count * sizeof(bitset_t*));
	pu->tgt_colors = malloc(pu->count * sizeof(int));
	pu->live_info = calloc(pu->count, sizeof(ir_node*));


	/* fill the members array */
	DBG((dbgphi, 1, "Phi class:\n"));
	i = 0;
	o = pu->count-1;
	for (n = (ir_node *)pset_first(pc); n; n = (ir_node *)pset_next(pc)) {
		DBG((dbgphi, 1, "\t%n\n", n));
		if (is_Phi(n))
			pu->members[i++] = n;
		else
			pu->members[o--] = n;
	}
	pu->phi_count = i;
	DBG((dbgphi, 1, "\n"));


	/* fill used colors array */
	for (i = 0; i < pu->count; ++i)
		pu->used_cols[i] = get_ra_block_info(get_nodes_block(pu->members[i]))->used_colors;


	if (pu->phi_count == 1) {
		ana_1_phi(pu);
	} else {
		/* TODO */
		// ana_n_phi(pu);
	}

	return pu;
}


static void free_phi_unit(phi_unit_t *pu) {
	DBG((dbgphi, 1, "\n"));
	free(pu->members);
	free(pu->used_cols);
	free(pu->tgt_colors);
	free(pu->live_info);
	free(pu);
}


void be_phi_coalesce(pset *all_phi_classes) {
	pset *pc;
	phi_unit_t *pu;

	for (pc = (pset *)pset_first(all_phi_classes); pc; pc = (pset *)pset_next(all_phi_classes)) {
		pu = new_phi_unit(pc);
		coalesce(pu);
		free_phi_unit(pu);
	}
}


void be_phi_coal_init(void) {
	dbgphi = firm_dbg_register("Phi coalescing");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);
}
