/**
 * Author:      Daniel Grund
 * Date:		12.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.

 * Heuristic for minimizing copies using a queue which holds 'qnodes' not yet
 * examined. A qnode has a 'target color', nodes out of the opt unit and
 * a 'conflict graph'. 'Conflict graph' = "Interference graph' + 'conflict edges'
 * A 'max indep set' is determined form these. We try to color this mis using a
 * color-exchanging mechanism. Occuring conflicts are modeled with 'conflict edges'
 * and the qnode is reinserted in the queue. The first qnode colored without
 * conflicts is the best one.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "xmalloc.h"
#include "becopyopt.h"
#include "becopystat.h"

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define SLOTS_PINNED_GLOBAL 256
#define SLOTS_CONFLICTS 8
#define SLOTS_CHANGED_NODES 32

#define MIN(a,b) ((a<b)?(a):(b))
#define list_entry_queue(lh) list_entry(lh, qnode_t, queue)
#define HASH_CONFLICT(c) (HASH_PTR(c.n1) ^ HASH_PTR(c.n2))

/**
 * Modeling additional conflicts between nodes. NOT live range interference
 */
typedef struct _conflict_t {
	const ir_node *n1, *n2;
} conflict_t;

/**
 * If an irn is changed, the changes first get stored in a node_stat_t,
 * to allow undo of changes (=drop new data) in case of conflicts.
 */
typedef struct _node_stat_t {
	ir_node *irn;
	int new_color;
	int pinned_local :1;
} node_stat_t;

/**
 * Represents a node in the optimization queue.
 */
typedef struct _qnode_t {
	struct list_head queue;		/**< chaining of unit_t->queue */
	const unit_t *ou;			/**< the opt unit this qnode belongs to */
	int color;					/**< target color */
	set *conflicts;				/**< contains conflict_t's. All internal conflicts */
	int mis_size;				/**< number of nodes in the mis. */
	ir_node **mis;				/**< the nodes of unit_t->nodes[] being part of the max independent set */
	set *changed_nodes;			/**< contains node_stat_t's. */
} qnode_t;

pset *pinned_global;			/**< optimized nodes should not be altered any more */

static int set_cmp_conflict_t(const void *x, const void *y, size_t size) {
	const conflict_t *xx = x;
	const conflict_t *yy = y;
	return ! (xx->n1 == yy->n1 && xx->n2 == yy->n2);
}

/**
 * If a local pinned conflict occurs, a new edge in the conflict graph is added.
 * The next maximum independent set build, will regard it.
 */
static INLINE void qnode_add_conflict(const qnode_t *qn, const ir_node *n1, const ir_node *n2) {
	conflict_t c;
	DBG((dbg, LEVEL_4, "\t      %n -- %n\n", n1, n2));

	if ((int)n1 < (int)n2) {
		c.n1 = n1;
		c.n2 = n2;
	} else {
		c.n1 = n2;
		c.n2 = n1;
	}
	set_insert(qn->conflicts, &c, sizeof(c), HASH_CONFLICT(c));
}

/**
 * Checks if two nodes are in a conflict.
 */
static INLINE int qnode_are_conflicting(const qnode_t *qn, const ir_node *n1, const ir_node *n2) {
	conflict_t c;
	/* search for live range interference */
	if (n1!=n2 && nodes_interfere(qn->ou->co->chordal_env, n1, n2))
		return 1;
	/* search for recoloring conflicts */
	if ((int)n1 < (int)n2) {
		c.n1 = n1;
		c.n2 = n2;
	} else {
		c.n1 = n2;
		c.n2 = n1;
	}
	return (int) set_find(qn->conflicts, &c, sizeof(c), HASH_CONFLICT(c));
}

static int set_cmp_node_stat_t(const void *x, const void *y, size_t size) {
	return ((node_stat_t *)x)->irn != ((node_stat_t *)y)->irn;
}

/**
 * Finds a node status entry of a node if existent. Otherwise return NULL
 */
static INLINE node_stat_t *qnode_find_node(const qnode_t *qn, ir_node *irn) {
	node_stat_t find;
	find.irn = irn;
	return set_find(qn->changed_nodes, &find, sizeof(find), HASH_PTR(irn));
}

/**
 * Finds a node status entry of a node if existent. Otherwise it will return
 * an initialized new entry for this node.
 */
static INLINE node_stat_t *qnode_find_or_insert_node(const qnode_t *qn, ir_node *irn) {
	node_stat_t find;
	find.irn = irn;
	find.new_color = NO_COLOR;
	find.pinned_local = 0;
	return set_insert(qn->changed_nodes, &find, sizeof(find), HASH_PTR(irn));
}

/**
 * Returns the virtual color of a node if set before, else returns the real color.
 */
static INLINE int qnode_get_new_color(const qnode_t *qn, ir_node *irn) {
	node_stat_t *found = qnode_find_node(qn, irn);
	if (found)
		return found->new_color;
	else
		return get_irn_col(qn->ou->co, irn);
}

/**
 * Sets the virtual color of a node.
 */
static INLINE void qnode_set_new_color(const qnode_t *qn, ir_node *irn, int color) {
	node_stat_t *found = qnode_find_or_insert_node(qn, irn);
	found->new_color = color;
}

/**
 * Checks if a node is local pinned. A node is local pinned, iff it belongs
 * to the same optimization unit and has been optimized before the current
 * processed node.
 */
static INLINE int qnode_is_pinned_local(const qnode_t *qn, ir_node *irn) {
	node_stat_t *found = qnode_find_node(qn, irn);
	if (found)
		return found->pinned_local;
	else
		return 0;
}

/**
 * Local-pins a node, so optimizations of further nodes of the same opt unit
 * can handle situations in which a color change would undo prior optimizations.
 */
static INLINE void qnode_pin_local(const qnode_t *qn, ir_node *irn) {
	node_stat_t *found = qnode_find_or_insert_node(qn, irn);
	found->pinned_local = 1;
}

/**
 * Possible return values of qnode_color_irn()
 */
#define CHANGE_SAVE NULL
#define CHANGE_IMPOSSIBLE (ir_node *)1
#define is_conflicting_node(n) (((int)n) > 1)

/**
 * Performs virtual re-coloring of node @p n to color @p col. Virtual colors of
 * other nodes are changed too, as required to preserve correctness. Function is
 * aware of local and global pinning. Recursive.
 * @param  irn The node to set the color for
 * @param  col The color to set
 * @param  trigger The irn that caused the wish to change the color of the irn
 * @return CHANGE_SAVE iff setting the color is possible, with all transitive effects.
 *         CHANGE_IMPOSSIBLE iff conflicts with reg-constraintsis occured.
 *         Else the first conflicting ir_node encountered is returned.
 *
 * ASSUMPTION: Assumes that a life range of a single value can't be split into
 * 			   several smaller intervals where other values can live in between.
 *             This should be true in SSA.
 */
static ir_node *qnode_color_irn(const qnode_t *qn, ir_node *irn, int col, const ir_node *trigger) {
	ir_node *res;
	struct obstack confl_ob;
	ir_node **confl, *cn;
	int i, irn_col;
	const be_chordal_env_t *chordal_env = qn->ou->co->chordal_env;
	const arch_env_t *arch_env = chordal_env->arch_env;
	const arch_register_class_t *cls = chordal_env->cls;

	DBG((dbg, LEVEL_3, "\t      %n \tcaused col(%n) \t%2d --> %2d\n", trigger, irn, qnode_get_new_color(qn, irn), col));
	obstack_init(&confl_ob);
	irn_col = qnode_get_new_color(qn, irn);

	if (irn_col == col)
		goto ret_save;
	if (pset_find_ptr(pinned_global, irn) || qnode_is_pinned_local(qn, irn)) {
		res = irn;
		goto ret_confl;
	}
	if (!arch_reg_is_allocatable(arch_env,
								 irn,
								 arch_pos_make_out(0),
								 arch_register_for_index(cls, col)))
		goto ret_imposs;

	/* get all nodes which would conflict with this change */
	{
		struct obstack q;
		int in, out;
		ir_node *irn_bl;

		irn_bl = get_nodes_block(irn);

		/* first check for a conflicting node which is 'living in' the irns block */
		{
			ir_node *n;
			pset *live_ins = put_live_in(irn_bl, pset_new_ptr_default());
			for (n = pset_first(live_ins); n; n = pset_next(live_ins))
				if (arch_irn_has_reg_class(arch_env, n, arch_pos_make_out(0), cls)
            && n != trigger && qnode_get_new_color(qn, n) == col
            && nodes_interfere(chordal_env, irn, n)) {

					DBG((dbg, LEVEL_4, "\t        %n\ttroubles\n", n));
					obstack_ptr_grow(&confl_ob, n);
					pset_break(live_ins);
					break;
			}
            del_pset(live_ins);
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
				if (arch_irn_has_reg_class(arch_env, n, arch_pos_make_out(0), cls)
            && n != trigger && qnode_get_new_color(qn, n) == col
            && nodes_interfere(chordal_env, irn, n)) {

					DBG((dbg, LEVEL_4, "\t        %n\ttroubles\n", n));
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
		sub_res = qnode_color_irn(qn, cn, irn_col, irn);
		if (sub_res != CHANGE_SAVE) {
			res = sub_res;
			goto ret_confl;
		}
	}
	/* if we arrive here all sub changes can be applied, so it's save to change this irn */

ret_save:
	DBG((dbg, LEVEL_3, "\t      %n save\n", irn));
	obstack_free(&confl_ob, NULL);
	qnode_set_new_color(qn, irn, col);
	return CHANGE_SAVE;

ret_imposs:
	DBG((dbg, LEVEL_3, "\t      %n impossible\n", irn));
	obstack_free(&confl_ob, NULL);
	return CHANGE_IMPOSSIBLE;

ret_confl:
	DBG((dbg, LEVEL_3, "\t      %n conflicting\n", irn));
	obstack_free(&confl_ob, NULL);
	return res;
}

/**
 * Tries to set the colors for all members of this queue node;
 * to the target color qn->color
 * @returns 1 iff all members colors could be set
 *          0 else
 */
static int qnode_try_color(const qnode_t *qn) {
	int i;
	for (i=0; i<qn->mis_size; ++i) {
		ir_node *test_node, *confl_node;

		test_node = qn->mis[i];
		DBG((dbg, LEVEL_3, "\t    Testing %n\n", test_node));
		confl_node = qnode_color_irn(qn, test_node, qn->color, test_node);

		if (confl_node == CHANGE_SAVE) {
			DBG((dbg, LEVEL_3, "\t    Save --> pin local\n"));
			qnode_pin_local(qn, test_node);
		} else if (confl_node == CHANGE_IMPOSSIBLE) {
			DBG((dbg, LEVEL_3, "\t    Impossible --> remove from qnode\n"));
			qnode_add_conflict(qn, test_node, test_node);
		} else {
			if (qnode_is_pinned_local(qn, confl_node)) {
				/* changing test_node would change back a node of current ou */
				DBG((dbg, LEVEL_3, "\t    Conflicting local --> add conflict\n"));
				qnode_add_conflict(qn, confl_node, test_node);
			}
			if (pset_find_ptr(pinned_global, confl_node)) {
				/* changing test_node would change back a node of a prior ou */
				DBG((dbg, LEVEL_3, "\t    Conflicting global --> remove from qnode\n"));
				qnode_add_conflict(qn, test_node, test_node);
			}
		}

		if (confl_node != CHANGE_SAVE)
			return 0;
	}
	return 1;
}

/**
 * Determines a maximum independent set with respect to the interference and
 * conflict edges of all nodes in a qnode.
 */
static INLINE void qnode_max_ind_set(qnode_t *qn, const unit_t *ou) {
	int all_size, curr_size, i, o;
	int *which;
	ir_node **curr, **all = alloca(ou->node_count * sizeof(*all));

	/* all contains all nodes not removed in this qn */
	all_size = 0;
	for (i=0; i<ou->node_count; ++i)
		if (!qnode_are_conflicting(qn, ou->nodes[i], ou->nodes[i]))
			all[all_size++] = ou->nodes[i];

	/* which[i] says which element to take out of all[] and put into curr[i] */
	which = alloca(all_size*sizeof(*which));
	for (curr_size=0; curr_size<all_size; ++curr_size)
		which[curr_size] = curr_size;

	/* stores the currently examined set */
	curr = alloca(all_size*sizeof(*curr));

	while (1) { /* this loop will terminate because at least a single node will be a max indep. set */
		/* build current set */
		for (i=0; i<curr_size; ++i)
			curr[i] = all[which[all_size-curr_size+i]];

		/* check current set */
		for (i=0; i<curr_size; ++i)
			for (o=i+1; o<curr_size; ++o)
				if (qnode_are_conflicting(qn, curr[i], curr[o]))
					goto conflict_found;

		/* We had no conflict. This is the max indep. set */
		qn->mis_size = curr_size;
		for (i=0; i<curr_size; ++i)
			qn->mis[i] = curr[i];
		return;

conflict_found:
		/* We had a conflict. Generate next set */
		if (which[all_size-curr_size+1] == all_size-curr_size+1) {
			curr_size--;
			for (i=0; i<curr_size; ++i)
				which[all_size-curr_size+i] = i;
		} else {
			int redo = 1;
			while (redo) {
				int pos = all_size;
				do {
					pos--;
				} while (!(which[pos] = (which[pos]+1) % all_size));

				for (i=pos+1; i<all_size; ++i)
					which[i] = MIN(which[i-1]+1, all_size-1);

				redo = 0;
				for (i=all_size-curr_size; i<all_size-1; ++i)
					if (which[i]>=which[i+1]) {
						redo = 1;
						break;
					}
			}
		}
	}
}

/**
 * Creates a new qnode
 */
static INLINE qnode_t *new_qnode(const unit_t *ou, int color) {
	qnode_t *qn = xmalloc(sizeof(*qn));
	qn->ou = ou;
	qn->color = color;
	qn->mis = malloc(ou->node_count * sizeof(*qn->mis));
	qn->conflicts = new_set(set_cmp_conflict_t, SLOTS_CONFLICTS);
	qn->changed_nodes = new_set(set_cmp_node_stat_t, SLOTS_CHANGED_NODES);
	return qn;
}

/**
 * Frees space used by a queue node
 */
static INLINE void free_qnode(qnode_t *qn) {
	del_set(qn->conflicts);
	del_set(qn->changed_nodes);
	xfree(qn->mis);
	xfree(qn);
}

/**
 * Inserts a qnode in the sorted queue of the optimization unit. Queue is
 * ordered by field 'size' (the size of the mis) in decreasing order.
 */
static INLINE void ou_insert_qnode(unit_t *ou, qnode_t *qn) {
	struct list_head *lh;

	if (qnode_are_conflicting(qn, ou->nodes[0], ou->nodes[0])) {
		/* root node is not in qnode */
		free_qnode(qn);
		return;
	}

	qnode_max_ind_set(qn, ou);
	/* do the insertion */
	DBG((dbg, LEVEL_4, "\t  Insert qnode color %d with size %d\n", qn->color, qn->mis_size));
	lh = &ou->queue;
	while (lh->next != &ou->queue) {
		qnode_t *curr = list_entry_queue(lh->next);
		if (curr->mis_size <= qn->mis_size)
			break;
		lh = lh->next;
	}
	list_add(&qn->queue, lh);
}

/**
 * Tries to re-allocate colors of nodes in this opt unit, to achieve a lower
 * number of copy instructions placed during SSA-destruction and lowering.
 * Works only for opt units with exactly 1 root node, which is the
 * case for approximately 80% of all phi classes and all register constrained
 * nodes. (All other phi classes are reduced to this case.)
 */
static void ou_optimize(unit_t *ou) {
	int i;
	qnode_t *curr, *tmp;
	bitset_t *pos_regs = bitset_alloca(ou->co->chordal_env->cls->n_regs);

	DBG((dbg, LEVEL_1, "\tOptimizing unit:\n"));
	for (i=0; i<ou->node_count; ++i)
		DBG((dbg, LEVEL_1, "\t %n\n", ou->nodes[i]));

	/* init queue */
	INIT_LIST_HEAD(&ou->queue);
	arch_get_allocatable_regs(ou->co->chordal_env->arch_env, ou->nodes[0], arch_pos_make_out(0), ou->co->chordal_env->cls, pos_regs);
	bitset_foreach(pos_regs, i)
		ou_insert_qnode(ou, new_qnode(ou, i));

	/* search best */
	while (!list_empty(&ou->queue)) {
		/* get head of queue */
		curr = list_entry_queue(ou->queue.next);
		list_del(&curr->queue);
		DBG((dbg, LEVEL_2, "\t  Examine qnode color %d with size %d\n", curr->color, curr->mis_size));

		/* try */
		if (qnode_try_color(curr))
			break;
		/* no success, so re-insert */
		del_set(curr->changed_nodes);
		curr->changed_nodes = new_set(set_cmp_node_stat_t, SLOTS_CHANGED_NODES);
		ou_insert_qnode(ou, curr);
	}

	/* apply the best found qnode */
	if (curr->mis_size >= 2) {
		node_stat_t *ns;

		DBG((dbg, LEVEL_1, "\t  Best color: %d  Copies: %d/%d\n", curr->color, ou->interf+ou->node_count-curr->mis_size, ou->interf+ou->node_count-1));
		/* globally pin root and eventually others */
		pset_insert_ptr(pinned_global, ou->nodes[0]);
		for (i=1; i<ou->node_count; ++i) {
			ir_node *irn = ou->nodes[i];
			int nc = qnode_get_new_color(curr, irn);
			if (nc != NO_COLOR && nc == qnode_get_new_color(curr, ou->nodes[0]))
				pset_insert_ptr(pinned_global, irn);
		}

		/* set color of all changed nodes */
		for (ns = set_first(curr->changed_nodes); ns; ns = set_next(curr->changed_nodes)) {
			/* NO_COLOR is possible, if we had an undo */
			if (ns->new_color != NO_COLOR) {
				DBG((dbg, LEVEL_2, "\t    color(%n) := %d\n", ns->irn, ns->new_color));
				set_irn_col(ou->co, ns->irn, ns->new_color);
			}
		}
	}

	/* free best qnode (curr) and queue */
	free_qnode(curr);
	list_for_each_entry_safe(qnode_t, curr, tmp, &ou->queue, queue)
		free_qnode(curr);
}

void co_heur_opt(copy_opt_t *co) {
	unit_t *curr;
	dbg = firm_dbg_register("ir.be.copyoptheur");
	firm_dbg_set_mask(dbg, DEBUG_LVL);
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, DEBUG_LVL_HEUR);
	else
		firm_dbg_set_mask(dbg, DEBUG_LVL);

	pinned_global = pset_new_ptr(SLOTS_PINNED_GLOBAL);
	list_for_each_entry(unit_t, curr, &co->units, units)
		if (curr->node_count > 1)
			ou_optimize(curr);

	del_pset(pinned_global);
}
