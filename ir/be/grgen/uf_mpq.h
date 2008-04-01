#ifndef _EXT_GRS_UF_MPQ_H_
#define _EXT_GRS_UF_MPQ_H_

/*
 * Project:     libFIRM/extension module/GRS-matcher
 * File name:   ext/grs/uf_mpq.h
 * Purpose:     provides a union find structure (uf) (disjoint sets)
 *              and a meldable priority queue implementation (mpq)
 *              for pattern edges (note: in case of the uf edges are
 *              represented by their edge ids. This is because a
 *              disjoint integer set implementation is used.)
 * 				@note The uf-concept used in this impl allows
 *				      only elems greater than zero, but there are
 * 				      elems greater or EQUAL zero supported!
 * Author:      Veit Batz
 * Created:		7. June 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdlib.h>


#include "array.h"
#include "common_t.h"
#include "action_t.h"




#undef MAX
#define MAX(a,b) ( (a)>=(b) ? (a) : (b) )



typedef double uf_value_t;

static int *uf_parent;
static uf_value_t *uf_value;
static uf_value_t *uf_update;

static int uf_current_size = 1000;
static int uf_max_size = 0;
static int uf_already_used = 0;

/** initialize the union find structure */
void ext_grs_uf_init(int max_elem) {

	int max_size = max_elem + 1;

	if (uf_already_used == 0) {
		uf_current_size = MAX(max_size, 1000);
		uf_parent = NEW_ARR_F(int, uf_current_size);
		uf_value = NEW_ARR_F(uf_value_t, uf_current_size);
		uf_update = NEW_ARR_F(uf_value_t, uf_current_size);
		uf_already_used = 1;
	}

	if (max_size > uf_current_size) {
		uf_current_size = max_size;
		ARR_RESIZE(*uf_parent, uf_parent, uf_current_size);
		ARR_RESIZE(*uf_value, uf_value, uf_current_size);
		ARR_RESIZE(*uf_update, uf_update, uf_current_size);
	}

	memset(uf_parent, 0, max_size * sizeof(*uf_parent));
	memset(uf_value, 0, max_size * sizeof(*uf_value));
	memset(uf_update, 0, max_size * sizeof(*uf_update));

	uf_max_size = max_size;
}

/** find the value of the given elem */
uf_value_t ext_grs_uf_find_value(int elem) {

	int old_parent = uf_parent[elem];
	int new_root;
	uf_value_t eff_parent_update = 0;
	uf_value_t new_update = 0;

	/* check wether the given elem is in range */
	assert (elem < uf_max_size && "union find elem out of bounds");

	/* if elem is a root... */
	if (old_parent <= 0) {
		/* ...simply return the elems effective value */
		return uf_value[elem] + uf_update[elem];
	}

	/* if the old parent is a root... */
	if (uf_parent[old_parent] <= 0) {
		/* ...simply return the elems effective value */
		return uf_value[elem] + uf_update[elem] + uf_update[old_parent];
	}

	/* otherwise get effective parental update recursively */
	eff_parent_update = ext_grs_uf_find_value(old_parent) - uf_value[old_parent];

	/* do path compression: */
	/* let the old parents parent be the new parent, which must be the
	 * current root (by returning from recursion this has been set up by
	 * the recusrsive ascend to the root) */
	new_root = uf_parent[old_parent];
	uf_parent[elem] = new_root;

	/* adapt the update vaule */
	new_update = uf_update[elem] + eff_parent_update - uf_update[new_root];
	uf_update[elem] = new_update;

	/* return the effective value of the elem */
	return uf_value[elem] + new_update;
}

/** find the root of the set <code>elem</code> belongs to */
int ext_grs_uf_find(int elem) {

	/* check wether the given elem is in range */
	assert (elem < uf_max_size && "union find elem out of bounds");

	/* if elem is a root... */
	if (uf_parent[elem] <= 0) {
		/* ...simply return the elems effective value */
		return elem;
	}

	/* otherwise call find_value() for the elem (which performs path
	 * compression for the elem) */
	ext_grs_uf_find_value(elem);

	/* after path compression the parrent of elem has to be a root */
	assert(uf_parent[uf_parent[elem]] <= 0 && "after path compression the parent should be a root");
	return uf_parent[elem];
}

/** unite the sets given by the roots <code>root1</code> and <code>root2</code> */
int ext_grs_uf_unite(int root1, int root2) {

	/* check wether the given elements are roots */
	assert (uf_parent[root1] <= 0 && uf_parent[root2] <= 0 && "not a root element");

	/* check wether the given elem is in range */
	assert (root1 < uf_max_size && root2 < uf_max_size &&
		"union find elem out of bounds");

	/* the less deeper set is connected to the more deeper set */
	if (uf_parent[root1] < uf_parent[root2]) {
		uf_parent[root2] = root1;
		uf_update[root2] -= uf_update[root1];
		return root1;
	}

	uf_parent[root1] = root2;
	uf_update[root1] -= uf_update[root2];
	if (uf_parent[root1] == uf_parent[root2])
		uf_parent[root2]--;
	return root2;
}






/* Folgende Funktion FALSCH????? */


/** add the given value <code>a</code> to the values of all
 *  elements of the set given by <code>root</code> */
void ext_grs_uf_change_value(int root, uf_value_t a) {

	/* check wether the given elem is in range */
	assert (root < uf_max_size && "union find elem out of bounds");

	assert (uf_parent[root] <= 0 && "not a root element");
	uf_update[root] += a;
}










/* the meldable priority queue implementation */




/**
 * iterate over a list starting with <code>pos</code> up to the lists end
 * */
#define list_for_each_from(pos, head) \
	for ( ; pos != (head); pos = pos->next )

#define GET_ACTUAL_EDGE_COST(e) \
	((e)->cost - ext_grs_uf_find_value((e)->arg->aiid + 1))



/** a priority queue of edges */
typedef lc_list_t ext_grs_mpq_t;





static struct obstack obst;
static int obst_already_initlzd = 0;


/** initialize the meldable priority queue inplementation */
void ext_grs_mpq_init(void)
{
	if (! obst_already_initlzd) {
		obstack_init(&obst);
	}
	else {
		obstack_free(&obst, NULL);
		obstack_init(&obst);
	}
	obst_already_initlzd = 1;
}

/** create a new empty priority queue */
ext_grs_mpq_t *ext_grs_mpq_create(void)
{
	ext_grs_mpq_t *mpq = obstack_alloc(&obst, sizeof(*mpq));

	assert(obst_already_initlzd = 1 && "mpq structure not initialized yet");


	LC_INIT_LIST_HEAD(mpq);
	return mpq;
}

/** insert the given elem in the given priority queue */
void ext_grs_mpq_insert(ext_grs_mpq_t *pq, ext_grs_edge_t *e)
{
	lc_list_t *pos;
	ext_grs_edge_t *edge;

	assert (pq != NULL && "NULL pointer is not a valid priority queue");

	if (lc_list_empty(pq)) {
		lc_list_add(& e->mpq_list, pq);
		return;
	}

	/* go through pq untill the appropriate position is reached */
	lc_list_for_each(pos, pq) {
		edge = lc_list_entry(pos, ext_grs_edge_t, mpq_list);
		if (GET_ACTUAL_EDGE_COST(e) <= GET_ACTUAL_EDGE_COST(edge)) {
			lc_list_add_tail( & e->mpq_list, & edge->mpq_list );
			return;
		}
	}

	lc_list_add( & e->mpq_list, & edge->mpq_list );

}

/** get the an item with a minimal prio or NULL if <code>pq</code> is empty */
ext_grs_edge_t *ext_grs_mpq_find_min(ext_grs_mpq_t *pq)
{
	assert (pq != NULL && "NULL pointer is not a valid priority queue");

	/* return NULL if pq is empty */
	if (lc_list_empty(pq)) return NULL;

	/* return first item otherwise */
	return lc_list_entry(pq->next, ext_grs_edge_t, mpq_list);
}

/** Remove and return an item with a minimal prio, if <code>pq</code> is empty
 *  return NULL.
 *  @note	The member <code>list</code> of the returned item
 *			is in an undefined state. */
ext_grs_edge_t *ext_grs_mpq_delete_min(ext_grs_mpq_t *pq)
{
	ext_grs_edge_t *first_edge;

	assert (pq != NULL && "NULL pointer is not a vaild priority queue");

	/* return NULL if pq is empty */
	if (lc_list_empty(pq)) return NULL;

	/* otherwise remove and return the first item */
	first_edge = lc_list_entry(pq->next, ext_grs_edge_t, mpq_list);
	lc_list_del(& first_edge->mpq_list);
	return first_edge;
}

void ext_grs_mpq_dump(ext_grs_mpq_t *pq);

/** Meld two item disjoint priority queues <code>pq1</code> and <code>pq2</code>.
 *  ATTENTION: The given priority queues will be <b>destroyed!</b> */
ext_grs_mpq_t *ext_grs_mpq_meld(ext_grs_mpq_t *pq1, ext_grs_mpq_t *pq2)
{
	lc_list_t *pos1, *pos2, *n;
	ext_grs_edge_t *e1, *e2;
	int last = 1; /* tells wether that pq1 has been iteratied till the end and never breaked */

	assert (pq1 != NULL && pq2 != NULL && "NULL pointer is not a vaild priority queue");
	assert (pq1 != pq2 && "cannot meld an pq with itself");

#ifdef EXT_GRS_DEBUG
	printf("pq1:\n");
	ext_grs_mpq_dump(pq1);
	printf("pq2:\n");
	ext_grs_mpq_dump(pq2);
#endif


	/* if pq1 is empty return pq2 */
	if (lc_list_empty(pq1)) return pq2;

	/* insert the items of pq2 into pq1 (at appropriate positions) */
	pos1 = pq1->next; /* start walking pq1 from beginning */
	lc_list_for_each_safe (pos2, n, pq2) {
		e2 = lc_list_entry(pos2, ext_grs_edge_t, mpq_list);
		list_for_each_from (pos1, pq1) {
			e1 = lc_list_entry(pos1, ext_grs_edge_t, mpq_list);
			if (GET_ACTUAL_EDGE_COST(e2) <= GET_ACTUAL_EDGE_COST(e1)) {
				last = 0;
				break;
			}
		}

		if (last) {
			/* pq1 has been iterated till the end */
			/* insert item2 before the curent position in pq1 */
			lc_list_move(& e2->mpq_list, & e1->mpq_list);
			e1 = e2;
		}
		else {
			/* insert item2 before the curent position in pq1 */
			lc_list_move_tail(& e2->mpq_list, & e1->mpq_list);
			/* continue in pq1 with the current item */
			pos1 = & e1->mpq_list;
			/* maybe next time the end of pq1 is reached */
			last = 1;
		}
	}

#ifdef EXT_GRS_DEBUG
	printf("Result queue:\n");
	ext_grs_mpq_dump(pq1);
	printf("\n");
#endif

	return pq1;
}


/** Remove the given item from the given priority queue.
 *  @note The list of the deleted item is in an undifind state afterwards. */
void ext_grs_mpq_delete(ext_grs_edge_t *e)
{
	/* remove the item from its priority queue */
	lc_list_del(& e->mpq_list);
}


void ext_grs_mpq_dump(ext_grs_mpq_t *pq) {
	lc_list_t *pos;

	printf("{\n");
	lc_list_for_each(pos, pq) {
		ext_grs_edge_t *edge = lc_list_entry(pos, ext_grs_edge_t, mpq_list);
		printf("\t(name, cost, actual cost) =  (%s, %lf, %lf)\n",
			edge->name, edge->cost, GET_ACTUAL_EDGE_COST(edge));
	}
	printf("}\n");
}



#endif /*_UF_MPQ_H_*/
