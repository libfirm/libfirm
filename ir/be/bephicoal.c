/**
 * @author Daniel Grund
 * @date 04.01.2005
 */

#include <stdlib.h>

#include "obst.h"
#include "set.h"
#include "pset.h"
#include "bitset.h"
#include "debug.h"
#include "irouts.h"
#include "irdom.h"

#include "bechordal.h"
#include "belive.h"
#include "bera_t.h"
#include "bephicongr_t.h"
#include "bephicoal_t.h"

#define DEBUG_LVL SET_LEVEL_3
#define MAX_COLORS 16

/* some things for readable code */
#define CHANGE_SAVE NULL
#define CHANGE_IMPOSSIBLE (ir_node *)1
#define CHANGE_NYI (ir_node *)2
typedef enum _perform_t { dryrun = 0, perform = 1 } perform_t;
/* TODO: ask/check if additional ir_node-space is initialized with 0000 */
//#define belongs_to_a_phi_class(n) (get_irn_phi_info(n)->phi)
//#define is_color_free(bl,col) (!bitset_is_set(get_ra_block_info(bl)->used_colors, col))

typedef struct _phi_unit_t {
	unsigned char count;
	unsigned char phi_count;

	/* 1 phi */
	ir_node **members;			/**< [0] is the phi node. [1..count-1] the arguments of the phi not interfering with it */
	int *colors;				/**< [i] is the color to set for members[i]. [i] == NO_COLOR means dont change anything for members[i]*/
	/* TODO: perhaps unneccessary */
	char *is_live_in;			/**< [i]==1 means members[i] is live-in in (all of) its cf-pred-blocks of the phi node */
} phi_unit_t;


static firm_dbg_module_t *dbgphi = NULL;
int XXX_copies=0, XXX_count=0;


/**
 * Contains ir_nodes of phi-classes whose colors may change unlimited times.
 * These nodes are not optimizable, so there is no need to pin their color.
 */
static pset *free_nodes = NULL;

/**
 * Contains already optimized ir_nodes of phi-classes fully processed.
 * So one can perform a check not to switch them twice or more.
 */
static pset *pinned_global = NULL;

/**
 * Contains optimized ir_nodes of the phi-classes
 * currently optimized. So one can perform a check not to switch them
 * twice or more.
 */
static pset *pinned_local = NULL;

/**
 * Contains the hypothetic colors of currently processed phi unit
 */
static set *hyp_cols = NULL;

typedef struct _hyp_col_t {
	ir_node *irn;
	int color;
} hyp_col_t;

int set_cmp_hyp_col_t(const void *x, const void *y, size_t size) {
	return ((hyp_col_t *)x)->irn != ((hyp_col_t *)y)->irn;
}


/**
 * @return The hypothetic color of the irn if available.
 *         Otherwise the current color of it.
 */
static INLINE int get_hyp_color(ir_node *irn) {
	hyp_col_t hc;
	hyp_col_t *found;
	hc.irn = irn;
	found = set_find(hyp_cols, &hc, sizeof(hc), HASH_PTR(irn));
	if (found)
		return found->color;
	else
		return get_irn_color(irn);
}

/**
 * Sets the hypothetic color of an irn
 */
static INLINE void set_hyp_color(ir_node *irn, int color) {
	hyp_col_t hc;
	hyp_col_t *found;
	hc.irn = irn;
	found = set_find(hyp_cols, &hc, sizeof(hc), HASH_PTR(irn));
	if (found)
		found->color = color;
	else {
		hc.color = color;
		set_insert(hyp_cols, &hc, sizeof(hc), HASH_PTR(irn));
	}
}



/**
 * Variable neede in _color_irn.
 * Not on stack, because never changed during recursion.
 */
static perform_t _color_irn_perform;

/**
 * Get all nodes which conflict with the color-setting of a node, because they
 * have the same color and are living together at some point in time.
 * @param irn The ir_node to find conflicting nodes for
 * @param color The color the conflicting nodes must have
 * @param bl  The root-block of the dom-sub-tree to start the search from.
 * @param exc An exceptional node which is never added to the result set of conflicting nodes
 * @param res An obstack to grow the resulting nodes on.
 */
static void get_conflicting_nodes(const ir_node *irn, int color, ir_node *bl, const ir_node *exc, struct obstack *res) {
	struct obstack q;
	int in, out;

	/* setup the queue */
	obstack_init(&q);
	obstack_ptr_grow(&q, bl);
	in = 1;
	out = 0;

	/* process the queue */
	while (out < in) {
		ir_node *curr_bl, *sub_bl;
		int i, max;

		curr_bl = ((ir_node **)obstack_base(&q))[out++];

		/* Add to the result all nodes in the block which live in target color
		 * and interfere with the irn */
		for (i = 0, max = get_irn_n_outs(curr_bl); i < max; ++i) {
			ir_node *n = get_irn_out(curr_bl, i);
			int n_col;
			if (!is_allocatable_irn(n))
				continue;
			if (_color_irn_perform == perform)
				n_col = get_irn_color(n);
			else
				n_col = get_hyp_color(n);

			if (n != exc && get_hyp_color(n) == color && phi_ops_interfere(irn, n))
				obstack_ptr_grow(res, n);
		}

		/* If irn lives out check i-dominated blocks where the irn lives in */
		/* Fill the queue */
		if (is_live_out(curr_bl, irn)) {
			dominates_for_each(curr_bl, sub_bl)
				if (is_live_in(sub_bl, irn)) {
					obstack_ptr_grow(&q, sub_bl);
					in++;
				}
		}
	}

	obstack_free(&q, NULL);
}


/**
 * Tries to set the color of @p n to @p col. Performs recoloring of other nodes
 * as required to preserve correctness. Recursive.
 * @param  irn The node to set the color for
 * @param  col The color to set.
 * @param  trigger The irn that caused the wish to change the color of the irn
 * @return CHANGE_SAVE iff setting the color is possible.
 *         CHANGE_IMPOSSIBLE iff conflicts with reg-constraintsis occured.
 *         Else the conflicting ir_node is returned.
 *
 * ASSUMPTION: Assumes that a life range of a single value can't be spilt into
 * 			   several smaller intervals where other values can live in between.
 */
static ir_node *_color_irn(ir_node *irn, int col, const ir_node *trigger) {
	ir_node *res;
	struct obstack confl_ob;
	ir_node **confl, *cn;
	ir_node *irn_bl;
	int i, irn_col;
	if (_color_irn_perform == perform)
		irn_col = get_irn_color(irn);
	else
		irn_col = get_hyp_color(irn);

	obstack_init(&confl_ob);

	if (irn_col == col)
		goto ret_save;

	if (pset_find_ptr(pinned_global, irn)) {
		DBG((dbgphi, LEVEL_3, "\t\t\t%n \t~~> %n := %d: Pinned other\n", trigger, irn, col));
		res = irn;
		goto ret_confl;
	}

	if (pset_find_ptr(pinned_local, irn)) {
		DBG((dbgphi, LEVEL_3, "\t\t\t%n \t~~> %n := %d: Pinned current\n", trigger, irn, col));
		res = irn;
		goto ret_confl;
	}

	/* process all nodes which would conflict with this change */
	irn_bl = get_nodes_block(irn);
	get_conflicting_nodes(irn, col, irn_bl, trigger, &confl_ob);
	obstack_ptr_grow(&confl_ob, NULL);
	confl = (ir_node **) obstack_finish(&confl_ob);

	for (i = 0, cn = confl[0]; cn; cn = confl[++i]) {
		ir_node *sub_res;

		/* for now don't let the changes spread in root-direction of the dom-tree */
		if (is_live_in(irn_bl, cn)) {
			DBG((dbgphi, LEVEL_3, "\t\t\t%n \t~~> %n := %d: NYI %n\n", trigger, irn, col, cn));
			res = CHANGE_NYI;
			goto ret_confl;
		}

		/* try to color the conflicting node cn with the color of the irn itself */
		DBG((dbgphi, LEVEL_3, "\t\t\t%n \t~~> %n := %d: Subcheck\n", trigger, irn, col));
		sub_res = _color_irn(cn, irn_col, irn);
		if (sub_res != CHANGE_SAVE) {
			res = sub_res;
			goto ret_confl;
		}
	}
	/* if we arrive here all sub changes can be applied,
	 * so it is save to change this irn */

ret_save:
	DBG((dbgphi, LEVEL_2, "\t\t\t%n \t~~> %n := %d: Save\n", trigger, irn, col));
	obstack_free(&confl_ob, NULL);
	if (_color_irn_perform == perform)
		set_irn_color(irn, col);
	else
		set_hyp_color(irn, col);
	return CHANGE_SAVE;

ret_confl:
	DBG((dbgphi, LEVEL_2, "\t\t\t%n \t~~> %n := %d: Conflict\n", trigger, irn, col));
	obstack_free(&confl_ob, NULL);
	assert(!_color_irn_perform && "When applying changes these must be save, but if you reach here they aren't!");
	return res;
}


static ir_node *color_irn(ir_node *irn, int col, perform_t do_what) {
	ir_node *res;

	_color_irn_perform = do_what;
	res = _color_irn(irn, col, irn);

	if (res == CHANGE_SAVE && !pset_find_ptr(free_nodes, irn)) {
		if (do_what == perform) {
			DBG((dbgphi, LEVEL_2, "\t\t\t\t\t @G %n\n", irn));
			pset_insert_ptr(pinned_global, irn);
		} else {
			DBG((dbgphi, LEVEL_2, "\t\t\t\t\t @L %n\n", irn));
			pset_insert_ptr(pinned_local, irn);
		}
	}
	return res;
}


/**
 * Tries to set as much members of a phi unit as possible to color @p col.
 * - Each change taken alone is guaranteed to be conflict free.
 * - _If_ all members are neither live-in nor live-out in their cf-pred-blocks
 *   _then_ all changes together can be applied conflict free.
 * - _If_ there is a member, which is live-in or live-out in its cf-pred-block
 *    of the phi node, it is possible that all changes together will conflict.
 * TODO: Check above comment with swapping complete colors in mind
 * TODO: Write sth. about dom-tree influence on this.
 */
static int try_colors(phi_unit_t *pu, int col, int b_size) {
	struct obstack ob;
	int i, o, cand_size, mis_size;
	ir_node **cand, **mis;

	obstack_init(&ob);
	pinned_local = pset_new_ptr(8);
	hyp_cols = new_set(set_cmp_hyp_col_t, 8);

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
		if (color_irn(pu->members[i], col, dryrun) == CHANGE_SAVE) {
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
			if (phi_ops_interfere(cand[i], mis[o])) {
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
	del_set(hyp_cols);
	del_pset(pinned_local);
	obstack_free(&ob, NULL);
	return mis_size;
}


/**
 * Sets the colors of members[i] to colors[i].
 * All changes togehter must be conflict free.
 */
static void set_colors(phi_unit_t *pu) {
	int i;

	for (i = 0; i < pu->count; ++i)
		if (pu->colors[i] != NO_COLOR)
			color_irn(pu->members[i], pu->colors[i], perform);
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

	b_colors = malloc(pu->count * sizeof(*b_colors));

	/* init best search result */
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
	XXX_copies += pu->count-b_size;
	XXX_count += pu->count;
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
			if (!phi_ops_interfere(phi, n)) {
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

	pinned_global = pset_new_ptr(256);
	free_nodes = pset_new_ptr(64);

	for (pc = pset_first(all_phi_classes); pc; pc = pset_next(all_phi_classes)) {
		phi_unit_t *pu = new_phi_unit(pc);
		if (pu->phi_count == 1)
			coalesce_1_phi(pu);
		else
			coalesce_n_phi(pu);
		free_phi_unit(pu);
	}

	DBG((dbgphi, 1, "Copies: %d / %d\n", XXX_copies, XXX_count));

	del_pset(free_nodes);
	del_pset(pinned_global);
}


void be_phi_coal_init(void) {
	dbgphi = firm_dbg_register("Phi coalescing");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);
}
