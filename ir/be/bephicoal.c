/**
 * @author Daniel Grund
 * @date 04.01.2005
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "obst.h"
#include "set.h"
#include "pset.h"
#include "bitset.h"
#include "debug.h"
#include "irouts.h"
#include "irdom.h"

#include "bechordal.h"
#include "belive_t.h"
#include "bera_t.h"
#include "phiclass_t.h"
#include "bephicoal_t.h"

#define DEBUG_LVL 0 //SET_LEVEL_2
#define MAX_COLORS 32

#define INITIAL_SLOTS_PINNED_GLOBAL 256
#define INITIAL_SLOTS_CHANGED_NODES 32

/* some things for readable code */
#define CHANGE_SAVE NULL
#define CHANGE_IMPOSSIBLE (ir_node *)1
#define CHANGE_NYI (ir_node *)2
#define is_conflicting_node(n) (((int)n) > 2)

/**
 * Models conflicts between nodes. These may be life range conflicts or
 * pinning conflicts, which may occur while changing colors
 */
typedef struct _conflict_t {
	ir_node *n1, *n2;
} conflict_t;

/**
 * If an irn is changed, the changes first get stored in a node_stat_t,
 * to allow undo of changes in case of conflicts.
 */
typedef struct _node_stat_t {
	ir_node *irn;
	int color;
	int undo_color;
	char status;		/**< Bit 0: pinned, Bit 1: removed */
} node_stat_t;

#define _set_pinned(nodestat)    nodestat->status |= 1
#define _set_removed(nodestat)   nodestat->status |= 2
#define _clear_pinned(nodestat)  nodestat->status &= 255 ^ 1
#define _clear_removed(nodestat) nodestat->status &= 255 ^ 2
#define _is_pinned(nodestat)     (nodestat->status & 1)
#define _is_removed(nodestat)    (nodestat->status & 2)

/**
 * Central data structure. Contains infos needed during coalescing of the
 * corresponding phi class.
 */
typedef struct _phi_unit_t {
	unsigned char node_count;			/**< size of the nodes-array */
	unsigned char conflict_count;		/**< size of the conflicts-array */
	unsigned char conflict_count_org;	/**< initial size of the conflicts-array */
	ir_node **nodes;					/**< [0] is the phi node. [1..node_count-1] the arguments of the phi not interfering with it */
	conflict_t *conflicts;				/**< pairs of conflicting ir_nodes. */
	set *changed_nodes;					/**< contains node_stat_t's. */
} phi_unit_t;

static firm_dbg_module_t *dbgphi = NULL;

/**
 * Contains already optimized ir_nodes of phi-units fully processed.
 * So one can perform a check not to switch them twice or more.
 */
static pset *pinned_global = NULL;

static int set_cmp_node_stat_t(const void *x, const void *y, size_t size) {
	return ((node_stat_t *)x)->irn != ((node_stat_t *)y)->irn;
}

/**
 * Finds a node status entry of a node if existent.
 */
static INLINE node_stat_t *pu_find_node(phi_unit_t *pu, ir_node *irn) {
	node_stat_t find;
	find.irn = irn;
	return set_find(pu->changed_nodes, &find, sizeof(find), HASH_PTR(irn));
}

/**
 * Finds a node status entry of a node if existent. Otherwise it will return
 * an initialized new entry for this node.
 */
static INLINE node_stat_t *pu_find_or_insert_node(phi_unit_t *pu, ir_node *irn) {
	node_stat_t find;
	find.irn = irn;
	find.color = NO_COLOR;
	find.undo_color = NO_COLOR;
	find.status = 0;
	return set_insert(pu->changed_nodes, &find, sizeof(find), HASH_PTR(irn));
}

/**
 * @return The virtual color of a node, if set before, else just the real color.
 */
static INLINE int pu_get_new_color(phi_unit_t *pu, ir_node *irn) {
	node_stat_t *found = pu_find_node(pu, irn);
	if (found)
		return found->color;
	else
		return get_irn_color(irn);
}

/**
 * Sets the virtual color of a node.
 */
static INLINE void pu_set_new_color(phi_unit_t *pu, ir_node *irn, int color) {
	node_stat_t *found = pu_find_or_insert_node(pu, irn);
	found->undo_color = found->color;
	found->color = color;
	DBG((dbgphi, LEVEL_4, "%n %d\n", irn, color));
}

/**
 * Sets the virtual color of a node to the color it had,
 * before the last call to pu_set_new_color
 */
static INLINE void pu_undo_color(phi_unit_t *pu, ir_node *irn) {
	node_stat_t *ns = pu_find_node(pu, irn);
	assert(ns && "Nodes whose colors are undone must be in pu->changed_nodes");
	ns->color = ns->undo_color;
	DBG((dbgphi, LEVEL_3, "\t\tUndo: col(%n) := %d\n", irn, ns->undo_color));
}

/**
 * Checks if a node is removed from consideration respectively building
 * a maximum independent set.
 */
static INLINE int pu_is_node_removed(phi_unit_t *pu, ir_node *irn) {
	node_stat_t *found = pu_find_node(pu, irn);
	if (found)
		return _is_removed(found);
	else
		return 0;
}

/**
 * Removes a node from the base set, out of which a maximum independet
 * set gets build from.
 */
static INLINE void pu_remove_node(phi_unit_t *pu, ir_node *irn) {
	node_stat_t *found = pu_find_or_insert_node(pu, irn);
	_set_removed(found);
	DBG((dbgphi, LEVEL_4, "%n\n", irn));
}

/**
 * Checks if a node is local pinned; i.e. it belongs to the same phi unit and
 * has been optimized before the current processed one.
 */
static INLINE int pu_is_node_pinned(phi_unit_t *pu, ir_node *irn) {
	node_stat_t *found = pu_find_node(pu, irn);
	if (found)
		return _is_pinned(found);
	else
		return 0;
}

/**
 * Local-pins a node, so optimizations of further nodes of the same phi unit
 * can handle situations in which a color change would undo prior optimizations.
 */
static INLINE void pu_pin_node(phi_unit_t *pu, ir_node *irn) {
	node_stat_t *found = pu_find_or_insert_node(pu, irn);
	_set_pinned(found);
	DBG((dbgphi, LEVEL_4, "%n\n", irn));
}

/**
 * If a local pinned conflict occurs, a new edge in the conflict graph is added.
 * The next maximum independent set build, will regard it.
 */
static INLINE void pu_add_conflict(phi_unit_t *pu, ir_node *n1, ir_node *n2) {
	int count = pu->conflict_count;

	DBG((dbgphi, LEVEL_3, "\t    %n -- %n\n", n1, n2));
	assert(count != 255 && "Too much conflicts. Can hold max 255 entries");
	if ((count & 15) == 0)
		pu->conflicts = realloc(pu->conflicts, (count + 16)*sizeof(*pu->conflicts));

	if ((int)n1 < (int)n2) {
		pu->conflicts[count].n1 = n1;
		pu->conflicts[count].n2 = n2;
	} else {
		pu->conflicts[count].n1 = n2;
		pu->conflicts[count].n2 = n1;
	}

	pu->conflict_count++;
}

/**
 * Checks if two nodes are in a conflict.
 */
static INLINE int pu_are_conflicting(phi_unit_t *pu, ir_node *n1, ir_node *n2) {
	const ir_node *o1, *o2;
	int i;

	if ((int)n1 < (int)n2) {
		o1 = n1;
		o2 = n2;
	} else {
		o1 = n2;
		o2 = n1;
	}

	for (i = 0; i < pu->conflict_count; ++i)
		if (pu->conflicts[i].n1 == o1 && pu->conflicts[i].n2 == o2)
			return 1;
	return 0;
}

/**
 * Checks if a node is a member of a phi unit.
 * Other nodes should not be pinned global.
 */
static INLINE int pu_is_global_pinnable(phi_unit_t *pu, ir_node *irn) {
	int i;
	for (i = 0; i < pu->node_count; ++i)
		if (pu->nodes[i] == irn)
			return 1;
	return 0;
}

/**
 * Determines a maximum independent set with respect to the conflict edges
 * in pu->conflicts and the nodes beeing all non-removed nodes of pu->nodes.
 * TODO: make this 'un-greedy'
 * ATTENTION: be aware that phi nodes find their way into the set. For 1 phi
 * 			  in greedy version this is no prob, cause it comes first at [0].
 */
static int pu_get_mis(phi_unit_t *pu, struct obstack *res) {
	int i, o, size = 0;
	ir_node **mis;

	DBG((dbgphi, LEVEL_2, "\t    Max indep set:\n"));
	for (i = 0; i < pu->node_count; ++i) {
		int intf_det = 0;
		if (pu_is_node_removed(pu, pu->nodes[i]))
			continue;
		mis = (ir_node**) obstack_base(res);
		for (o = 0; o < size; ++o)
			if (pu_are_conflicting(pu, pu->nodes[i], mis[o])) {
				intf_det = 1;
				break;
			}

		if (!intf_det) {
			DBG((dbgphi, LEVEL_2, "\t\t%n\n", pu->nodes[i]));
			obstack_ptr_grow(res, pu->nodes[i]);
			size++;
		}
	}
	return size;
}

/**
 * Performs virtual re-coloring of node @p n to color @p col. Virtual colors of
 * other nodes are changed too, as required to preserve correctness. Function is
 * aware of local and global pinning. Recursive.
 * @param  irn The node to set the color for
 * @param  col The color to set.
 * @param  trigger The irn that caused the wish to change the color of the irn
 * @param  changed_nodes An obstack on which all ir_nodes get growed on, which are changed
 * @return CHANGE_SAVE iff setting the color is possible, with all transiteve effects.
 *         CHANGE_IMPOSSIBLE iff conflicts with reg-constraintsis occured.
 *         CHANGE_NYI iff an unhandled situation occurs.
 *         Else the first conflicting ir_node encountered is returned.
 *
 * ASSUMPTION: Assumes that a life range of a single value can't be spilt into
 * 			   several smaller intervals where other values can live in between.
 *             This should be true in SSA.
 */
static ir_node *_pu_color_irn(phi_unit_t *pu, ir_node *irn, int col, const ir_node *trigger, struct obstack *changed_nodes) {
	ir_node *res;
	struct obstack confl_ob;
	ir_node **confl, *cn;
	int i, irn_col;

	DBG((dbgphi, LEVEL_3, "\t\t%n \tcaused col(%n) \t%2d --> %2d\n", trigger, irn, pu_get_new_color(pu, irn), col));
	obstack_init(&confl_ob);
	irn_col = pu_get_new_color(pu, irn);

	if (irn_col == col)
		goto ret_save;
	if (pset_find_ptr(pinned_global, irn) || pu_is_node_pinned(pu, irn)) {
		res = irn;
		goto ret_confl;
	}

	/* get all nodes which would conflict with this change */
	{
		struct obstack q;
		int in, out;
		ir_node *irn_bl;

		irn_bl = get_nodes_block(irn);

		/* first check for a conflicting node which is 'living in' the irns block */
		{
			ir_node *n;
			pset *live_ins = get_live_in(irn_bl);
			for (n = pset_first(live_ins); n; n = pset_next(live_ins))
				if (is_allocatable_irn(n) && n != trigger && pu_get_new_color(pu, n) == col && phi_ops_interfere(irn, n)) {
					DBG((dbgphi, LEVEL_4, "\t\t    %n\ttroubles\n", n));
					obstack_ptr_grow(&confl_ob, n);
					pset_break(live_ins);
					break;
				}
		}

		/* setup the queue of blocks. */
		obstack_init(&q);
		obstack_ptr_grow(&q, irn_bl);
		in = 1;
		out = 0;

		/* process the queue. The code below checks for every block dominated
		 * by the irns one, and in which the irn is live, if there are
		 * conflicting nodes */
		while (out < in) {
			ir_node *curr_bl, *sub_bl;
			int i, max;

			curr_bl = ((ir_node **)obstack_base(&q))[out++];

			/* Add to the result all nodes in the block, which have
			 * the target color and interfere with the irn */
			for (i = 0, max = get_irn_n_outs(curr_bl); i < max; ++i) {
				ir_node *n = get_irn_out(curr_bl, i);
				if (is_allocatable_irn(n) && n != trigger && pu_get_new_color(pu, n) == col && phi_ops_interfere(irn, n)) {
					DBG((dbgphi, LEVEL_4, "\t\t    %n\ttroubles\n", n));
					obstack_ptr_grow(&confl_ob, n);
				}
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
		obstack_ptr_grow(&confl_ob, NULL);
		confl = (ir_node **) obstack_finish(&confl_ob);
	}

	/* process all nodes which would conflict with this change */
	for (i = 0, cn = confl[0]; cn; cn = confl[++i]) {
		ir_node *sub_res;

		/* try to color the conflicting node cn with the color of the irn itself */
		sub_res = _pu_color_irn(pu, cn, irn_col, irn, changed_nodes);
		if (sub_res != CHANGE_SAVE) {
			res = sub_res;
			goto ret_confl;
		}
	}
	/* if we arrive here all sub changes can be applied, so it's save to change this irn */

ret_save:
	DBG((dbgphi, LEVEL_3, "\t\t%n save\n", irn));
	obstack_free(&confl_ob, NULL);
	pu_set_new_color(pu, irn, col);
	obstack_ptr_grow(changed_nodes, irn);
	return CHANGE_SAVE;

ret_confl:
	DBG((dbgphi, LEVEL_3, "\t\t%n conflicting\n", irn));
	obstack_free(&confl_ob, NULL);
	return res;
}

static ir_node *pu_color_irn(phi_unit_t *pu, ir_node *irn, int col) {
	ir_node *res;
	struct obstack ob_undo;

	obstack_init(&ob_undo);
	res = _pu_color_irn(pu, irn, col, irn, &ob_undo);

	if (res != CHANGE_SAVE) { /* undo virtual changes caused by the last call */
		int i;
		ir_node *undo_node, **undo_nodes;

		obstack_ptr_grow(&ob_undo, NULL);
		undo_nodes = obstack_finish(&ob_undo);
		for (i = 0, undo_node = undo_nodes[0]; undo_node; undo_node = undo_nodes[++i])
			pu_undo_color(pu, undo_node);
	}

	obstack_free(&ob_undo, NULL);
	return res;
}

/**
 * Tries to set as much members of a phi unit as possible to color @p col.
 * All changes taken together are guaranteed to be conflict free.
 */
static int pu_try_color(phi_unit_t *pu, int col, int b_size) {
	struct obstack ob_mis;
	int i, redo, mis_size;
	ir_node **mis;

	obstack_init(&ob_mis);
	redo = 1;
	while (redo) {
		redo = 0;
		/* get a max independent set regarding current conflicts */
		mis_size = pu_get_mis(pu, &ob_mis);
		mis = obstack_finish(&ob_mis);

		/* shortcut: if mis size is worse than best, then mis won't be better. */
		if (mis_size < b_size)
			goto ret;

		/* check if its possible to set the color for all members of the maximum set*/
		for (i = 0; i < mis_size; ++i) {
			ir_node *test_node, *confl_node;

			test_node = mis[i];
			DBG((dbgphi, LEVEL_2, "\t    Testing %n\n", test_node));
			confl_node = pu_color_irn(pu, test_node, col);

			if (confl_node == CHANGE_SAVE) {
				DBG((dbgphi, LEVEL_2, "\t    Save\n"));
				pu_pin_node(pu, test_node);
			} else if (confl_node == CHANGE_NYI) {
				DBG((dbgphi, 0, "\t    NYI\n"));
			} else if (confl_node == CHANGE_IMPOSSIBLE) {
				/* TODO: this may happen due to reg constraints --> remove from set ?? */
			} else {
				DBG((dbgphi, LEVEL_2, "\t    Conflicting\n"));
				assert(is_conflicting_node(confl_node));

				if (pu_is_node_pinned(pu, confl_node)) {
					/* changing test_node would change back a node of current phi unit */
					pu_add_conflict(pu, confl_node, test_node);
					redo = 1;
				}
				if (pset_find_ptr(pinned_global, confl_node)) {
					/* changing test_node would change back a node of a prior phi unit */
					pu_remove_node(pu, test_node);
					redo = 1;
				}
			}

			if (confl_node != CHANGE_SAVE) {
				/* shortcut: color not possible for phi node (phi comes first) ==> exit */
				if (i == 0) {
					mis_size = 0;
					goto ret;
				}
				/* break iteration over current mis, because it will change */
				break;
			}
		}
		obstack_free(&ob_mis, mis);
	}

ret:
	obstack_free(&ob_mis, NULL);
	return mis_size;
}

/**
 * Tries to re-allocate colors of nodes in this phi unit, to achieve a lower
 * number of copy instructions placed during phi destruction. Optimized version.
 * Works only for phi-classes/phi-units with exactly 1 phi node, which is the
 * case for approximately 80% of all phi classes. All other phi classes are
 * reduced to this case.
 */
static void pu_coal_1_phi(phi_unit_t *pu) {
	int size, col, b_size, b_color;
	set *b_changes;

	/* init best search result */
	b_changes = NULL;
	b_size = 0;
	b_color = NO_COLOR;

	/* find optimum of all colors */
	for (col = MAX_COLORS-1; col >= 0; --col) {
		DBG((dbgphi, 1, "\tTrying color %d\n", col));
		size = pu_try_color(pu, col, b_size);

		/* did we find a better max ind. set? */
		if (size > b_size) {
			DBG((dbgphi, 1, "\tBetter size: %d\n", size));
			if (b_changes)
				del_set(b_changes);
			b_changes = pu->changed_nodes;
			b_size = size;
			b_color = col;
		} else {
			del_set(pu->changed_nodes);
		}

		/* reset the phi unit to original state for next color */
		pu->changed_nodes = new_set(set_cmp_node_stat_t, INITIAL_SLOTS_CHANGED_NODES);
		pu->conflict_count = pu->conflict_count_org;

		/* shortcut: if all members can be colored we are (very) happy */
		if (b_size == pu->node_count)
			break;
	}

	/* now apply the found optimum */
	if (b_changes) {
		node_stat_t *ns;
		DBG((dbgphi, 1, "\tBest color: %d  Copies: %d/%d\n", b_color, pu->node_count-b_size, pu->node_count-1));
		for (ns = set_first(b_changes); ns; ns = set_next(b_changes)) {
			/* NO_COLOR is possible, if we had an undo; so the irn stays in the
			 * pu->changed_nodes with new color set to NO_COLOR. */
			if (ns->color != NO_COLOR) {
				DBG((dbgphi, 1, "\t    color(%n) := %d\n", ns->irn, ns->color));
				set_irn_color(ns->irn, ns->color);
				if (pu_is_global_pinnable(pu, ns->irn) && ns->color == pu_get_new_color(pu, pu->nodes[0]))
					pset_insert_ptr(pinned_global, ns->irn);
			}
		}
		free(b_changes);
	} else {
		DBG((dbgphi, 1, "\tBest color: none\n"));
	}
}


/**
 * Prepares a phi class for further processing as one or more phi units.
 * Calls the worker-functions for all units.
 * @param pc The phi class to process.
 * @param root_phi In case of recursive call this is the phi node not beeing
 * 				   an argument in the phi1unit.
 * 				   Else this has to be NULL.
 */
static void coal_phi_class(pset *pc, ir_node *root_phi) {
	int phi_count = 0;
	ir_node *n, *phi = NULL;

	/* unfortunately there _can_ be >1 phi nodes in a phi1unit,
	 * so we have an if... */
	if (root_phi) {
		phi = root_phi;
		phi_count = 1;
	} else {
		/* get the phi count of this class. May result in phi_count == 1 */
		for (n = pset_first(pc); n; n = pset_next(pc))
			if (is_Phi(n)) {
				phi = n;
				phi_count++;
			}
	}

	/* the 'simple' case */
	if (phi_count == 1) {
		phi_unit_t *pu;
		ir_node **tmp;
		struct obstack ob;
		int i, o;

		obstack_init(&ob);

		DBG((dbgphi, 1, "\tPhi-1 unit:\n"));
		pu = calloc(1, sizeof(*pu));

		/* build member set not containing phi interferers */
		DBG((dbgphi, 1, "\t    %n\n", phi));
		obstack_ptr_grow(&ob, phi);
		pu->node_count = 1;

		for (n = pset_first(pc); n; n = pset_next(pc)) {
			if (n == phi)
				continue;
			if (!phi_ops_interfere(phi, n)) {
				DBG((dbgphi, 1, "\t    %n\n", n));
				obstack_ptr_grow(&ob, n);
				pu->node_count++;
			} else {
				DBG((dbgphi, 1, "\t    %n \tdropped\n", n));
			}
		}
		tmp = obstack_finish(&ob);
		pu->nodes = malloc(pu->node_count * sizeof(*pu->nodes));
		memcpy(&pu->nodes[0], tmp, pu->node_count * sizeof(*tmp));
		obstack_free(&ob, NULL);

		/* init conlict graph to life range interference */
		DBG((dbgphi, 1, "\tInitial conflicts:\n"));
		for (i = 0; i < pu->node_count; ++i)
			for (o = i+1; o < pu->node_count; ++o)
				if (phi_ops_interfere(pu->nodes[i], pu->nodes[o]))
					pu_add_conflict(pu, pu->nodes[i], pu->nodes[o]);
		pu->conflict_count_org = pu->conflict_count;

		/* init changed nodes */
		pu->changed_nodes = new_set(set_cmp_node_stat_t, INITIAL_SLOTS_CHANGED_NODES);

		pu_coal_1_phi(pu);

		free(pu->nodes);
		free(pu->changed_nodes);
		if (pu->conflicts)
			free(pu->conflicts);
	} else {	/* the 'not so easy' case */
		DBG((dbgphi, 1, "\tPhi-n unit:\n"));

		/* copy pc into big_pc... */
		pset *copy = pset_new_ptr(32);
		for (n = pset_first(pc); n; n = pset_next(pc)) {
			DBG((dbgphi, 1, "\t    %n\n", n));
			pset_insert_ptr(copy, n);
		}

		/* ... because we want to build small 'connected graphs' and
		 * delete their members from the copy */
		while (pset_count(copy) > 0) {
			/* build all connected sets from the copy */
			int last = 0, first = 0;
			ir_node **queue = calloc(pset_count(copy), sizeof(*queue));

			/* pick some node out of copy, place into queue */
			n = pset_first(copy);
			pset_break(copy);
			pset_remove_ptr(copy, n);
			queue[last++] = n;

			DBG((dbgphi, 1, "\tConnected:\n"));
			pset *connected = pset_new_ptr(8);
			while (first < last) {
				/* pick n out of the queue into connected set */
				n = queue[first++];
				pset_insert_ptr(connected, n);
				DBG((dbgphi, 1, "\t    %n\n", n));


				/* check if pre/successors are 'connected' with n */
				{
					ir_node *other;
					int i;
					/* insert all args of n, which are in the phi class to the queue */
					for(i=0; i < get_irn_arity(n); ++i) {
						other = get_irn_n(n, i);
						if (pset_find_ptr(copy, other) && !values_interfere(n, other)) {
							queue[last++] = other;
							pset_remove_ptr(copy, other);
						}
					}
					/* same for outs of n */
					for(i=0; i < get_irn_n_outs(n); ++i) {
						other = get_irn_out(n, i);
						if (pset_find_ptr(copy, other) && !values_interfere(n, other)) {
							queue[last++] = other;
							pset_remove_ptr(copy, other);
						}
					}
				}
			}

			/* Now we have a "connected graph" build from copy==pc.
			 * Remove 1-phi-units from the connected set for
			 * passing to optimizer */
			while (pset_count(connected) > 0) {
				pset *phi1unit;
				ir_node *phi = NULL;
				int i;
				/* search a phi node */
				for (n = pset_first(connected); n; n = pset_next(connected))
					if (is_Phi(n)) {
						phi = n;
						break;
					}
				pset_break(connected);

				/* if there are only non-phi nodes left quit */
				if (!phi)
					break;

				/* Build a 1-phi-unit with this phi */
				DBG((dbgphi, 1, "\t    Phi-1-unit:\n"));
				phi1unit = pset_new_ptr(8);
				pset_insert_ptr(phi1unit, phi);
				pset_remove_ptr(connected, phi);
				DBG((dbgphi, 1, "\t\t%n\n", phi));
				/* insert all arguments of phi, which are in the connected set
				 * to the 1-phi-unit */
				for(i=0; i < get_irn_arity(phi); ++i) {
					ir_node *arg = get_irn_n(phi, i);
					if (pset_find_ptr(connected, arg)) {
						DBG((dbgphi, 1, "\t\t%n\n", arg));
						pset_insert_ptr(phi1unit, arg);
						pset_remove_ptr(connected, arg);
					}
				}

				/* finally the call for coalescing the 1-phi-unit */
				if (pset_count(phi1unit) > 1) /* ==1 can happen if the connected set contains only a single phi node */
					coal_phi_class(phi1unit, phi);

				del_pset(phi1unit);
			}
			del_pset(connected);
			free(queue);
		}
		del_pset(copy);
	}
}


void be_phi_coalesce(pset *all_phi_classes) {
	pset *pc;

	pinned_global = pset_new_ptr(INITIAL_SLOTS_PINNED_GLOBAL);

	for (pc = pset_first(all_phi_classes); pc; pc = pset_next(all_phi_classes))
		coal_phi_class(pc, NULL);

	del_pset(pinned_global);
}


void be_phi_coal_init(void) {
	dbgphi = firm_dbg_register("ir.be.phicoal");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);
}
