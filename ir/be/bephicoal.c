/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdlib.h>

#include "obst.h"
#include "pset.h"
#include "bitset.h"
#include "debug.h"

#include "bechordal.h"
#include "belive.h"
#include "bera_t.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"

#define DEBUG_LVL SET_LEVEL_2

#define MAX_PHI_CLS_SIZE (1<<(sizeof(unsigned char)*8)) /* possible colors added should fit into unsigned char */
#define MAX_COLORS 16
#define CHANGE_IMPOSSIBLE 0
#define CHANGE_SAVE 1

#define DRYRUN 0
#define PERFORM 1

typedef struct _phi_unit_t {
	unsigned char count;
	unsigned char phi_count;

	/* 1 phi */
	ir_node **members;			/**< [0] is the phi node. [1..count-1] the arguments of the phi not interfering with it */
	int *colors;				/**< [i] is the color to set for members[i]. [i] == NO_COLOR means dont change anything for members[i]*/
	char *is_live_in;			/**< [i]==1 means members[i] is live-in in (all of) its cf-pred-blocks of the phi node */
} phi_unit_t;


static firm_dbg_module_t *dbgphi = NULL;

/**
 * Contains ir_nodes of phi-classes whose colors were reassigned during coalescing.
 * So one can perform a check not to switch them twice or more.
 */
static pset *pinned_nodes = NULL;

/**
 * Contains ir_nodes of phi-classes whose colors may change unlimited times.
 * These nodes are not optimizable, so there is no need to pin their color.
 */
static pset *free_nodes = NULL;


/* TODO: ask/check if additional ir_node-space is initialized with 0000 */
#define belongs_to_a_phi_class(n) (get_irn_phi_info(n)->phi)

#define is_color_free(bl,col) (!bitset_is_set(get_ra_block_info(bl)->used_colors, col))

/**
 * Sets the color of node @p n to @p col, but acts
 * pinning aware.
 */
static INLINE void set_color(ir_node *n, int col) {
	assert(!pset_find_ptr(pinned_nodes, n));
	set_irn_color(n, col);
	if (belongs_to_a_phi_class(n) && !pset_find_ptr(free_nodes, n))
		pset_insert_ptr(pinned_nodes, n);
}

/**
 * Tries to set the color of @p n to @p col. Performs recoloring of other nodes
 * as required to preserve correctness.
 * @param  n The node to set the color for
 * @param  col The color to set.
 * @param  dryrun If true no colors are actually set, only testing is performed.
 *                If false colors get set and it must be sure that no conflicts will occur.
 * @return If setting the color is impossible CHANGE_IMPOSSIBLE is returned
 *         else the number of nodes that need a (cascading) change is returned.
 */
static int color_irn(ir_node *n, int col, int perform) {
	ir_node *bl;
	int res = CHANGE_IMPOSSIBLE;
	DBG((dbgphi, LEVEL_2, "\t\t\t%n --> %d\n", n, col));
	assert(is_color(col));

	if (get_irn_color(n) == col) {
		DBG((dbgphi, LEVEL_2, "\t\t\t  Same\n"));
		/* insert it into pinned_nodes */
		if (perform)
			if (belongs_to_a_phi_class(n) && !pset_find_ptr(free_nodes, n))
				pset_insert_ptr(pinned_nodes, n);
		return CHANGE_SAVE;
	}

	if (pset_find_ptr(pinned_nodes, n)) {
		DBG((dbgphi, LEVEL_2, "\t\t\t  Pinned\n"));
		if (perform)
			assert(0 && "No prior check with dryrun or buggy");
		return CHANGE_IMPOSSIBLE;
	}

	bl = get_nodes_block(n);
	if (is_color_free(bl, col) && !is_live_out(bl, n)) {
		DBG((dbgphi, LEVEL_2, "\t\t\t  Free\n"));
		if (perform)
			set_color(n, col);
		return CHANGE_SAVE;
	}

	/* for now, in the aldi-version return impossible */
	DBG((dbgphi, LEVEL_2, "\t\t\t  Impossible\n"));
	return CHANGE_IMPOSSIBLE;

	return res;
}


/**
 * Tries to set as much members of a phi unit as possible to color @p col.
 * - Each change taken alone is guaranteed to be conflict free.
 * - _If_ all members are neither live-in nor live-out in their cf-pred-blocks
 *   _then_ all changes together can be applied conflict free.
 * - _If_ there is a member, which is live-in or live-out in its cf-pred-block
 *    of the phi node, it is possible that all changes together will conflict.
 * TODO: check above comment with swapping complete colors in mind
 * - TODO: Write sth. about dom-tree influence on this.
 */
static int try_colors(phi_unit_t *pu, int col, int b_size) {
	struct obstack ob;
	int i, o, cand_size, mis_size;
	ir_node **cand, **mis;

	obstack_init(&ob);

	/* first init pessimistically, so we can just return
	 * if we see there wont be a better result */
	mis_size = 0;
	for (i = 0; i < pu->count; ++i)
		pu->colors[i] = NO_COLOR;

	/* For all members check if color would be possible.
	 * Does not check if color is possible in combination with
	 * other members colors being set */
	cand_size = 0;
	for (i = 0; i < pu->count; ++i) {
		DBG((dbgphi, 1, "\t\t    Testing %n\n", pu->members[i]));
		if (color_irn(pu->members[i], col, DRYRUN) == CHANGE_SAVE) {
			DBG((dbgphi, 1, "\t\t\tAdding to cand\n"));
			obstack_ptr_grow(&ob, pu->members[i]);
			cand_size++;
		} else {
			DBG((dbgphi, 1, "\t\t\tImpossible\n"));
		}
		/* if color is not possible for the phi node then exit (phi comes first)*/
		if (cand_size == 0)
			goto ret;
	}
	cand = obstack_finish(&ob);

	/* shortcut: if cand is worse than best then mis wont be better. */
	if (cand_size < b_size)
		goto ret;

	/* now take the candidates cand and determine a max independent set
	 * with respect to edges representing live range interference */
	/* TODO: make this 'un-greedy' */
	DBG((dbgphi, 1, "\t\t    Max indep set\n"));
	for (i = 0; i < cand_size; ++i) {
		int intf_det = 0;
		for (o = 0; o < mis_size; ++o) {
			mis = (ir_node**) obstack_base(&ob);
			if (values_interfere(cand[i], mis[o])) {
				intf_det = 1;
				break;
			}
		}

		if (!intf_det) {
			DBG((dbgphi, 1, "\t\t\tAdding to mis %n\n", cand[i]));
			obstack_ptr_grow(&ob, cand[i]);
			mis_size++;
		}
	}
	mis = obstack_finish(&ob);

	/* Now set the colors of all nodes in the mis to col.
	 * HINT: Set the color of all nodes, even if one has the same color already */
	for (i = 0; i < pu->count; ++i)
		for (o = 0; o < mis_size; ++o)
			if (pu->members[i] == mis[o])
				pu->colors[i] = col;

ret:
	obstack_free(&ob, NULL);
	return mis_size;
}


/**
 * Sets the colors of members[i] to colors[i] as far as possible.
 * Each single change must be conflict free (checked by try_colors).
 * In some cases not all colors can be set.
 */
 /*TODO : BUGGY due to false reasoning with live-outs
  * 	  thus fallback to save variant with testing all before setting. */
static void set_colors(phi_unit_t *pu) {
	int i;

	for (i = 0; i < pu->count; ++i)
		if (pu->colors[i] != NO_COLOR) {
			if (color_irn(pu->members[i], pu->colors[i], DRYRUN) == CHANGE_SAVE) {
				DBG((dbgphi, 1, "\t\tSetting %n to %d\n", pu->members[i], pu->colors[i]));
				color_irn(pu->members[i], pu->colors[i], PERFORM);
			} else {
				DBG((dbgphi, 1, "\t\tConflict due to sth.: %n\n", pu->members[i]));
			}
		}
}


/**
 * Tries to re-allocate colors of this phi-class, to achieve a lower number of
 * copies placed during phi destruction. Optimized version. Works only for
 * phi-classes/phi-units with exactly 1 phi node, which is the case for approx.
 * 80% of all phi classes.
 */
static void coalesce_1_phi(phi_unit_t *pu) {
	int *b_colors, b_size, b_color;
	int i, size, col;

	/* init best search result */
	b_colors = malloc(pu->count * sizeof(*b_colors));
	b_size = 0;
	b_color = NO_COLOR;
	for (i = 0; i < pu->count; ++i)
		b_colors[i] = NO_COLOR;

	/* find optimum of all colors */
	for (col = MAX_COLORS-1; col >= 0; --col) {
		DBG((dbgphi, 1, "\tTrying color %d\n", col));
		size = try_colors(pu, col, b_size);

		/* did we find a better max ind. set? */
		if (size > b_size) {
			b_size = size;
			b_color = col;
			memcpy(b_colors, pu->colors, pu->count * sizeof(*b_colors));
			DBG((dbgphi, 1, "\t!! Better size: %d\n", b_size));
		}

		/* shortcut: if all members can be colored we are content */
		if (b_size == pu->count)
			break;
	}
	DBG((dbgphi, 1, "\tBest color: %d  Copies: %d/%d\n", b_color, pu->count-b_size, pu->count));
	if (b_color == NO_COLOR)
		goto ret;

	/* now apply the found optimum */
	memcpy(pu->colors, b_colors, pu->count * sizeof(*b_colors));
	set_colors(pu);

ret:
	free(b_colors);
}

/**
 * Tries to re-allocate colors of this phi-class, to achieve a lower number of
 * copies placed during phi destruction. General purpose version.
 */
static void coalesce_n_phi(phi_unit_t *pu) {
	DBG((dbgphi, 1, "\n"));
	/* TODO */
}

/**
 * Prepares a phi class for further processing as a phi unit.
 * @param pc The phi class to prepare.
 * @return A so called phi unit containing some prepared informations
 *         needed by the following coalescing phase.
 */
static phi_unit_t *new_phi_unit(pset *pc) {
	phi_unit_t *pu;
	ir_node *n, *phi = NULL;

	assert(pset_count(pc) <= MAX_PHI_CLS_SIZE && "Phi class too large!");

	/* get the phi count of this class */
	pu = malloc(sizeof(*pu));
	pu->phi_count = 0;
	for (n = pset_first(pc); n; n = pset_next(pc))
		if (is_Phi(n)) {
			phi = n;
			pu->phi_count++;
		}

	if (pu->phi_count == 1) {
		ir_node **tmp, *phi_block;
		int i, max;
		struct obstack ob;

		obstack_init(&ob);

		/* build member set not containing phi interferers */
		DBG((dbgphi, 1, "Phi-1 class:\n"));
		pu->count = 1; /*for the phi*/
		for (n = pset_first(pc); n; n = pset_next(pc)) {
			if (is_Phi(n))
				continue;
			if (!values_interfere(phi, n)) {
				DBG((dbgphi, 1, "\tAdding to members: %n\n", n));
				obstack_ptr_grow(&ob, n);
				pu->count++;
			} else {
				DBG((dbgphi, 1, "\tPhi interferer: %n\n", n));
				pset_insert_ptr(free_nodes, n);
			}
		}
		tmp = obstack_finish(&ob);
		pu->members = malloc(pu->count * sizeof(*pu->members));
		pu->members[0] = phi;
		memcpy(&pu->members[1], tmp, (pu->count-1) * sizeof(*tmp));

		/* init of colors array */
		pu->colors = malloc(pu->count * sizeof(*pu->colors));

		/* live-in analysis */
		/* HINT: It is possible that a node occurs twice as arg of a phi,
		 * one time being live-in, and another time not being live-in.
		 */
		pu->is_live_in = calloc(pu->count, sizeof(*pu->is_live_in));
		phi_block = get_nodes_block(phi);
		for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
			int midx, o;
	        ir_node *arg, *block_ith_pred;

	        arg = get_irn_n(phi, i);
			block_ith_pred = get_nodes_block(get_irn_n(phi_block, i));

			/* find the arg in the members array */
			midx = -1;
			for (o = 0; o < pu->count; ++o)
				if (pu->members[o] == arg) {
					midx = o;
					break;
				}
			if (midx == -1)
				continue;

			if (is_live_in(block_ith_pred, arg)) {
				pu->is_live_in[midx] |= 1;
				DBG((dbgphi, 1, "\t%n is live-in in %n\n", arg, block_ith_pred));
			}
		}

		obstack_free(&ob, NULL);
	} else {
		DBG((dbgphi, 1, "Phi-n class:\n"));
		/* TODO */
	}

	DBG((dbgphi, 1, "\n"));
	return pu;
}


/**
 * Deletes a phi unit
 */
static void free_phi_unit(phi_unit_t *pu) {
	DBG((dbgphi, 1, "\n"));
	if (pu->phi_count == 1) {
		free(pu->members);
		free(pu->colors);
		free(pu->is_live_in);
	} else {
		/* TODO */
	}
	free(pu);
}


void be_phi_coalesce(pset *all_phi_classes) {
	pset *pc;

	pinned_nodes = pset_new_ptr(256);
	free_nodes = pset_new_ptr(64);

	for (pc = pset_first(all_phi_classes); pc; pc = pset_next(all_phi_classes)) {
		phi_unit_t *pu = new_phi_unit(pc);
		if (pu->phi_count == 1)
			coalesce_1_phi(pu);
		else
			coalesce_n_phi(pu);
		free_phi_unit(pu);
	}

	del_pset(free_nodes);
	del_pset(pinned_nodes);
}


void be_phi_coal_init(void) {
	dbgphi = firm_dbg_register("Phi coalescing");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);
}
