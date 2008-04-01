#ifndef _ANA_VS_T_H_
#define _ANA_VS_T_H_
/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/ana_vs_t.c
 * Purpose:     internal interfaces of a v-structure (vs) statistic
 * 				based graph analyzer (see [D"orr95],[Batz05b])
 * Author:      Veit Batz
 * Created:		30. July 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file	ext/grs/ana_vs_t.h
 * @brief	internal interfaces of a v-structure (vs) statistic
 * 			based graph analyzer (see [D"orr95],[Batz05b])
 */

#include <stdlib.h>
#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif
#include "common_t.h"
#include "analyze_t.h"
#include "ana_vs.h"






/* an entry of a vs statistics which is represented by as pset */
typedef struct _ext_grs_vs_counter_t {
	ir_opcode o1, o2;
	modecode m1, m2;
	ext_grs_direction_t dir;
	double counter; /* accumulated exponent of multoplicity */
	int occurence; /* number of occurences of a node with such an edge */
} ext_grs_vs_counter_t;


typedef struct _ext_grs_vs_stat_t {
	ir_graph *irg;
	lc_pset *stat;
	struct obstack obst;
} ext_grs_vs_stat_t;

typedef struct {
	/* maps ir_graphs to vs statistics of the ir_graphs */
	lc_pset *ana_results;
	/* the global vs statistics */
	ext_grs_vs_stat_t global_vs_stat;
} ext_grs_ana_vs_private_t;




#ifndef _ext_grs_VS_HASH
#define _ext_grs_VS_HASH(c) \
	HASH_PTR(\
		(void *)(\
			((unsigned)_ext_grs_op_map[(c)->o1]) ^ \
			((unsigned)_ext_grs_op_map[(c)->o2]) ^ \
			((unsigned)_ext_grs_mode_map[(c)->m1]) ^ \
			((unsigned)_ext_grs_mode_map[(c)->m2]) ^ \
			((unsigned)(c)->dir << 5) \
		)\
	)
#endif

/* compare function for a pset representing a vs statistics */
static int _vs_cmp_func(const void *x1, const void *x2)
{
	ext_grs_vs_counter_t *c1, *c2;
	c1 = (ext_grs_vs_counter_t *) x1;
	c2 = (ext_grs_vs_counter_t *) x2;

	/* if all entries are equal: return 0, else non-zero */
	return (c1->o1 - c2->o1) | (c1->m1 - c2->m1) |
	       (c1->o2 - c2->o2) | (c1->m2 - c2->m2) |
	       (c1->dir - c2->dir) ;
}

static int _stat_cmp_func(const void *x1, const void *x2)
{
	ext_grs_vs_stat_t *s1, *s2;
	s1 = (ext_grs_vs_stat_t *) x1;
	s2 = (ext_grs_vs_stat_t *) x2;

	/* if irg entries are equal: return 0, else non-zero */
	return  s1->irg - s2->irg;
}




/* get the pset representing the given irgs vs statistics */
static ext_grs_vs_stat_t
*_get_irg_vs_stat(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	unsigned hash;
	ext_grs_vs_stat_t dummy_stat, *stat;

	lc_pset *res_set = ((ext_grs_ana_vs_private_t *)alz->data)->ana_results;

	hash = HASH_PTR(irg);

	dummy_stat.irg = irg;

	/* lookup for the pset representing the irgs vs statitic */
	stat = (ext_grs_vs_stat_t *)
		lc_pset_find(res_set, &dummy_stat, HASH_PTR(irg));

	/* if no such statistics present, create one */
	if (stat == NULL) {

		stat = malloc(sizeof(*stat));
		assert (stat != NULL && "could not allocate missing irg statistics");
		memset(stat, 0, sizeof(*stat));

		/* set it up */
		stat->irg = irg;
		stat->stat = lc_pset_new(_vs_cmp_func, 256);
		obstack_init(& stat->obst);

		/* store stat in pset */
		lc_pset_insert(res_set, stat, HASH_PTR(irg));
	}
	return stat;
}





/* add an onount to the given vs statistic */
void static _add_n_v_structures(
	ext_grs_vs_stat_t *vs_stat,
	ir_opcode o1, modecode m1, ir_opcode o2, modecode m2,
	ext_grs_direction_t dir, double x)
{
	unsigned hash;
	ext_grs_vs_counter_t *count, *dummy_count;
/*
	printf("o1,m1,o2,m2,d: %s,%s,%s,%s,%d: %lf\n",
			get_op_name(_ext_grs_op_map[o1]),
			get_mode_name(_ext_grs_mode_map[m1]),
			get_op_name(_ext_grs_op_map[o2]),
			get_mode_name(_ext_grs_mode_map[m2]),
			dir,
			x
		);
 */
	/* alloc a dummy stat entry on the stack */
	dummy_count = alloca(sizeof(*dummy_count));

	dummy_count->o1 = o1;
	dummy_count->o2 = o2;
	dummy_count->m1 = m1;
	dummy_count->m2 = m2;
	dummy_count->dir = dir;

	hash = _ext_grs_VS_HASH(dummy_count);
	count = lc_pset_find(vs_stat->stat, dummy_count, hash);

	/* if entry already present, add counter */
	if (count != NULL) {
		count->counter += x;
		return;
	}

	/* there's no stat entry present, so create one */
	dummy_count = obstack_alloc(& vs_stat->obst, sizeof(*count));
	dummy_count->o1 = o1;
	dummy_count->o2 = o2;
	dummy_count->m1 = m1;
	dummy_count->m2 = m2;
	dummy_count->dir = dir;
	dummy_count->counter = x;
	dummy_count->occurence = 0;
	lc_pset_insert(vs_stat->stat, dummy_count, hash);
}

/* subtract an onount from the given vs statistic */
void static _sub_n_v_structures(
	ext_grs_vs_stat_t *vs_stat,
	ir_opcode o1, modecode m1, ir_opcode o2, modecode m2,
	ext_grs_direction_t dir, double x)
{
	unsigned hash;
	ext_grs_vs_counter_t *count, *dummy_count;

	/* alloc stat entry on the stat objects obstack */
	dummy_count = (ext_grs_vs_counter_t *)
		obstack_alloc(& vs_stat->obst, sizeof(*count));

	dummy_count->o1 = o1;
	dummy_count->o2 = o2;
	dummy_count->m1 = m1;
	dummy_count->m2 = m2;
	dummy_count->dir = dir;

	hash = _ext_grs_VS_HASH(dummy_count);
	count = lc_pset_find(vs_stat->stat, dummy_count, hash);

	/* if entry already present, add counter */
	if (count != NULL) {
		count->counter -= x;
		obstack_free(& vs_stat->obst, dummy_count);
		return;
	}

	/* if entry not present, store dummy_count as a new one */
	dummy_count->counter = -x;
	dummy_count->occurence = 0;
	lc_pset_insert(vs_stat->stat, dummy_count, hash);
}

/* get the current multiplicity of the given vs */
double static _get_n_v_structures(
	ext_grs_vs_stat_t *vs_stat, ir_opcode o1, modecode m1, ir_opcode o2, modecode m2,
	ext_grs_direction_t dir)
{
	unsigned hash;

	ext_grs_vs_counter_t *count, dummy_count;
	dummy_count.o1 = o1;
	dummy_count.o2 = o2;
	dummy_count.m1 = m1;
	dummy_count.m2 = m2;
	dummy_count.dir = dir;

	hash = _ext_grs_VS_HASH(&dummy_count);

	count = lc_pset_find(vs_stat->stat, &dummy_count, hash);

	/* if entry already present return counter */
	if (count != NULL) {
		return count->counter;
	}

	/* otherwise return 0 */
	return (double)0;
}

void static INLINE _inc_n_occurence(ext_grs_vs_stat_t *vs_stat,
	ir_opcode o1, modecode m1, ir_opcode o2, modecode m2,	ext_grs_direction_t dir)
{
	unsigned hash;
	ext_grs_vs_counter_t *count, *dummy_count;

	/* alloc stat entry on the stat objects obstack */
	dummy_count = (ext_grs_vs_counter_t *)
		obstack_alloc(& vs_stat->obst, sizeof(*dummy_count));

	dummy_count->o1 = o1;
	dummy_count->o2 = o2;
	dummy_count->m1 = m1;
	dummy_count->m2 = m2;
	dummy_count->dir = dir;

	hash = _ext_grs_VS_HASH(dummy_count);
	count = lc_pset_find(vs_stat->stat, dummy_count, hash);

	/* if entry already present, add counter */
	if (count != NULL) {
		count->occurence++;
		obstack_free(& vs_stat->obst, dummy_count);
		return;
	}

	/* if entry not present, store dummy_count as a new one */
	dummy_count->counter = (double)0;
	dummy_count->occurence = 1;
	lc_pset_insert(vs_stat->stat, dummy_count, hash);
}

void static INLINE _add_n_occurence(ext_grs_vs_stat_t *vs_stat,
	ir_opcode o1, modecode m1, ir_opcode o2, modecode m2,	ext_grs_direction_t dir, int add)
{
	unsigned hash;
	ext_grs_vs_counter_t *count, *dummy_count;

	/* alloc stat entry on the stat objects obstack */
	dummy_count = (ext_grs_vs_counter_t *)
		obstack_alloc(& vs_stat->obst, sizeof(*count));

	dummy_count->o1 = o1;
	dummy_count->o2 = o2;
	dummy_count->m1 = m1;
	dummy_count->m2 = m2;
	dummy_count->dir = dir;

	hash = _ext_grs_VS_HASH(dummy_count);
	count = lc_pset_find(vs_stat->stat, dummy_count, hash);

	/* if entry already present, add counter */
	if (count != NULL) {
		count->occurence += add;
		obstack_free(& vs_stat->obst, dummy_count);
		return;
	}

	/* if entry not present, store dummy_count as a new one */
	dummy_count->counter = (double)0;
	dummy_count->occurence = 1;
	lc_pset_insert(vs_stat->stat, dummy_count, hash);
}


int static _get_n_occurence(ext_grs_vs_stat_t *vs_stat,
	ir_opcode o1, modecode m1, ir_opcode o2, modecode m2,	ext_grs_direction_t dir)
{
	unsigned hash;

	ext_grs_vs_counter_t *count, dummy_count;
	dummy_count.o1 = o1;
	dummy_count.o2 = o2;
	dummy_count.m1 = m1;
	dummy_count.m2 = m2;
	dummy_count.dir = dir;

	hash = _ext_grs_VS_HASH(&dummy_count);

	count = lc_pset_find(vs_stat->stat, &dummy_count, hash);

	/* if entry already present return counter */
	if (count != NULL) {
		return count->occurence;
	}

	/* otherwise return 0 */
	return 0;
}


#endif /*_ANA_VS_T_H_*/
