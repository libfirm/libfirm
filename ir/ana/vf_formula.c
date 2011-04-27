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
 * @brief   Boolean formulas for VFirm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include <string.h>
#include "vf_formula.h"
#include "obstack.h"
#include "irnode_t.h"
#include <assert.h>
#include <alloca.h>

typedef struct obstack obstack;

/* TODO: some form of garbage collections could be nice here. */

struct vf_info {
	obstack obst;
};

typedef struct vf_literal {
	ir_node *irn;
	char     value;
} vf_literal;

struct vf_clause {
	vf_literal *literals; /* Sorted by irn. */
	int         count;
};

typedef enum vf_clause_rel {
	vf_clause_rel_lhs_implies_rhs,
	vf_clause_rel_rhs_implies_lhs,
	vf_clause_rel_equal,
	vf_clause_rel_unrelated
} vf_clause_rel;

#define COUNT_TRUE  -1
#define COUNT_FALSE -2

/* Perform a binary search to determine the index in the clauses literal array
 * to insert a literal for the given irn at. If there is already a literal with
 * that irn, its index is returned. If not, the bitwise complement of the found
 * insertion position is returned. */
static int vf_literal_search(vf_clause clause, ir_node *irn)
{
	int low  = 0;
	int high = clause.count - 1;

	while (low <= high) {
		int      mid    = low + (high - low) / 2;
		ir_node *ir_mid = clause.literals[mid].irn;

		if      (ir_mid == irn) return mid;
		else if (ir_mid  < irn) low  = mid + 1;
		else                    high = mid - 1;
	}

	return ~low;
}

/* A shortcut for memory copy on the literals. */
#define LCPY(src, src_index, dst, dst_index, length) \
	memcpy( \
		&(dst).literals[dst_index], \
		&(src).literals[src_index], \
		(length) * sizeof(*((dst).literals)) \
	)

/* Remove a literal from the given clause and return a changed one. */
static vf_clause vf_clause_remove(vf_info *vfi, vf_clause clause, int index)
{
	vf_clause new_clause;
	assert((index >= 0) && (index < clause.count));
	assert(clause.count > 1); /* Don't allow empty clauses. */

	new_clause.count    = clause.count - 1;
	new_clause.literals = OALLOCN(&vfi->obst, vf_literal, new_clause.count);

	LCPY(clause, 0,         new_clause, 0,     index);
	LCPY(clause, index + 1, new_clause, index, clause.count - (index + 1));

	return new_clause;
}

/* Insert a literal into the given clause and return a changed one. */
static vf_clause vf_clause_insert(vf_info *vfi, vf_clause clause, int index,
                                  vf_literal literal)
{
	vf_clause new_clause;
	assert((index >= 0) && (index <= clause.count));

	new_clause.count    = clause.count + 1;
	new_clause.literals = OALLOCN(&vfi->obst, vf_literal, new_clause.count);
	new_clause.literals[index] = literal;

	LCPY(clause, 0,     new_clause, 0,         index);
	LCPY(clause, index, new_clause, index + 1, clause.count - index);

	return new_clause;
}

#undef LCPY

/* Insert the given literal at the proper position and write the result to
 * the given location. Returns true on success and false when the specified
 * literal contradicts the clause (yielding false). */
static int vf_clause_insert_literal(vf_info *vfi, vf_clause clause,
                                     vf_literal literal, vf_clause *result)
{
	int index = vf_literal_search(clause, literal.irn);

	if (index >= 0) {

		if (clause.literals[index].value == literal.value) {
			*result = clause;
			return 1;
		}

		return 0;
	}

	*result = vf_clause_insert(vfi, clause, ~index, literal);
	return 1;
}

/* Inserts a whole clause into another one. The behaviour is the same as
 * vf_clause_insert_literal. */
static int vf_clause_insert_clause(vf_info *vfi, vf_clause lhs,
                                    vf_clause rhs, vf_clause *result)
{
	int i;

	*result = lhs;
	for (i = 0; i < rhs.count; i++) {

		if (!vf_clause_insert_literal(vfi, *result, rhs.literals[i], result)) {
			return 0;
		}
	}

	return 1;
}

static vf_clause_rel vf_clause_compare(vf_clause lhs, vf_clause rhs)
{
    int i = 0, j = 0;
    assert((lhs.count > 0) && (rhs.count > 0));

    /* Remaining literals to test. If the value is negative, it has been
     * proven that it can't be implied by the other cond. */
    int lhs_rem = lhs.count;
    int rhs_rem = rhs.count;

    /* Lhs needs at least as many literals as rhs, to imply it. The same is
     * true for the opposite direction. */
    if (lhs.count < rhs.count) rhs_rem = -1;
    if (rhs.count < lhs.count) lhs_rem = -1;

    /* We can't exclude both possibilities yet. */
    assert((lhs_rem >= 0) || (rhs_rem >= 0));

    /* Compare literals in the clauses to find a contradiction. */
    while ((i < lhs.count) && (j < rhs.count)) {
    	vf_literal lhs_lit = lhs.literals[i];
    	vf_literal rhs_lit = rhs.literals[j];

    	/* If literals match, reduce the counter on both sides. */
    	if (lhs_lit.irn == rhs_lit.irn) {
    		/* But if they contradict, we are done. */
    		if (lhs_lit.value != rhs_lit.value) {
    			return vf_clause_rel_unrelated;
    		}

    		lhs_rem--;
    		rhs_rem--;

    		/* And advance to the next literals. */
    		i++; j++;
    		continue;
    	}

    	/* The literals are sorted. So if the current lhs literal is bigger
    	 * than the rhs literal, the rhs literal is not in lhs. That means,
    	 * that lhs can't imply rhs. The same argument holds true the other
    	 * way around. */

    	if (lhs_lit.irn > rhs_lit.irn) { rhs_rem = -1; j++; }
    	else                           { lhs_rem = -1; i++; }

    	/* Stop if no implication is possible. */
    	if ((lhs_rem < 0) && (rhs_rem < 0)) {
    		return vf_clause_rel_unrelated;
    	}
    }

    /* Depending on the number of literals found for each clause, select a
     * return value. If all lhs literals were found in rhs, rhs implies lhs
     * and so on. */
    if ((lhs_rem == 0) && (rhs_rem == 0)) {
    	return vf_clause_rel_equal;
    }

    if (lhs_rem == 0) return vf_clause_rel_rhs_implies_lhs;
    if (rhs_rem == 0) return vf_clause_rel_lhs_implies_rhs;

    return vf_clause_rel_unrelated;
}

/* Determine if the given clauses can be combined (that is they differ in one
 * position only and contradict there). Returns the index where they contradict
 * or -1 if they can't be combined. */
static int vf_clause_combine(vf_clause lhs, vf_clause rhs)
{
	int i, res = -1;
	if (lhs.count != rhs.count) return -1;

	for (i = 0; i < lhs.count; i++) {
		if (lhs.literals[i].irn != rhs.literals[i].irn) return -1;

		/* Store the position and make sure it's the only one. */
		if (lhs.literals[i].value != rhs.literals[i].value) {
			if (res >= 0) return -1;
			res = i;
		}
	}

	return res;
}

/* Dump the given clause to the given file. */
static void vf_clause_dump(vf_clause clause, FILE *f)
{
	int i;

	for (i = 0; i < clause.count; i++) {
		if (i != 0) fprintf(f, " ");
		if (!clause.literals[i].value) fprintf(f, "!");
		fprintf(f, "%ld", get_irn_node_nr(clause.literals[i].irn));
	}

	if (clause.count == 0) {
		fprintf(f, "true");
	}
}

static vf_cond vf_clause_to_cond(vf_info *vfi, vf_clause clause)
{
	vf_cond cond;
	cond.count   = 1;
	cond.clauses = OALLOC(&vfi->obst, vf_clause);
	cond.clauses[0] = clause;
	return cond;
}

/* Or-combine a cond and a clause. (abc OR def) OR ghi = ? */
static vf_cond vf_cond_or_clause(vf_info *vfi, vf_cond cond,
                                 vf_clause clause)
{
	int      i, j, count;
	char    *drop;
	vf_cond  new_cond;

	/* Always take care when using cond parameters. */
	if (vf_cond_is_true(cond))  return vf_cond_get_true();
	if (vf_cond_is_false(cond)) return vf_clause_to_cond(vfi, clause);

	/* Try to combine clauses by applying distributivity first. */
	for (i = 0; i < cond.count; i++) {
        int index = vf_clause_combine(clause, cond.clauses[i]);
        if (index < 0) continue;

        /* A contradiction of two clauses with length 1 (a or !a). */
        if (clause.count == 1) return vf_cond_get_true();

        /* Remove the contradicted literal and retry. */
        return vf_cond_or_clause(vfi, cond,
        	vf_clause_remove(vfi, clause, index)
        );
    }

	drop  = alloca(clause.count * sizeof(char));
	count = 0;

	/* Keep only clauses that don't include "clause". Those that do, are too
	 * specific, to be kept and will be replaced by "clause". */
	for (i = 0; i < cond.count; i++) {
		vf_clause_rel relation = vf_clause_compare(cond.clauses[i], clause);
		drop[i] = 0;

		switch (relation) {
		case vf_clause_rel_equal:
			/* If the clause is already there, we are done. */
			return cond;
		case vf_clause_rel_lhs_implies_rhs:
			/* If lhs implies clause, we can throw it away. */
			drop[i] = 1;
			break;
		case vf_clause_rel_rhs_implies_lhs:
			/* If clause implies lhs, we don't need it. */
			return cond;
		default:
			break;
		}

		if (!drop[i]) count++;
    }

	/* Create the new cond with the selected clauses. */
	new_cond.count   = count + 1;
	new_cond.clauses = OALLOCN(&vfi->obst, vf_clause, new_cond.count);

	j = 0;
    for (i = 0; i < cond.count; i++) {
        if (!drop[i]) {
        	new_cond.clauses[j] = cond.clauses[i];
            j++;
        }
    }

    new_cond.clauses[j] = clause;

    return new_cond;
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

vf_cond vf_cond_get_false(void)
{
	static vf_clause clause;
	static vf_cond   cond = { &clause, COUNT_FALSE };
	return cond;
}

vf_cond vf_cond_get_true(void)
{
	static vf_clause clause;
	static vf_cond   cond = { &clause, COUNT_TRUE };
	return cond;
}

int vf_cond_is_true(vf_cond cond)
{
	return cond.count == COUNT_TRUE;
}

int vf_cond_is_false(vf_cond cond)
{
	return cond.count == COUNT_FALSE;
}

vf_cond vf_cond_new(vf_info *vfi, ir_node *irn, char value)
{
	vf_cond cond;
	cond.count   = 1;
	cond.clauses = OALLOC(&vfi->obst, vf_clause);

	/* Only add one clause. */
	cond.clauses[0].count    = 1;
	cond.clauses[0].literals = OALLOC(&vfi->obst, vf_literal);

	/* That contains the specified literal. */
	cond.clauses[0].literals[0].irn   = irn;
	cond.clauses[0].literals[0].value = value;

	return cond;
}

/* OR-combine two conds. (abc OR def) OR (ghi OR jkl) = ? */
vf_cond vf_cond_or(vf_info *vfi, vf_cond lhs, vf_cond rhs)
{
	vf_cond result = lhs; int i;

	/* Test for special cases. lhs is checked by vf_cond_or_clause. */
	if (vf_cond_is_true(rhs))  return vf_cond_get_true();
	if (vf_cond_is_false(rhs)) return lhs;

	for (i = 0; i < rhs.count; i++) {
		/* Or-combine all clauses of rhs with lhs. */
		result = vf_cond_or_clause(vfi, result, rhs.clauses[i]);
	}

	return result;
}

/* AND-combine two conds. (abc OR def) AND (ghi OR jkl) = ? */
vf_cond vf_cond_and(vf_info *vfi, vf_cond lhs, vf_cond rhs)
{
	vf_cond result = vf_cond_get_false();
	int i, j;

	/* Test for special cases. */
	if (vf_cond_is_false(lhs)) return vf_cond_get_false();
	if (vf_cond_is_false(rhs)) return vf_cond_get_false();
	if (vf_cond_is_true(lhs))  return rhs;
	if (vf_cond_is_true(rhs))  return lhs;

	for (i = 0; i < lhs.count; i++) {
		for (j = 0; j < rhs.count; j++) {
			vf_clause combined;

			/* AND-combine all permutations. */
			if (vf_clause_insert_clause(vfi, lhs.clauses[i], rhs.clauses[j],
			                            &combined)) {

				/* Only OR-combine with the current result if not false. */
				result = vf_cond_or_clause(vfi, result, combined);
			}
		}
	}

	return result;
}

int vf_cond_implies(vf_cond lhs, vf_cond rhs)
{
	int i;

	if (vf_cond_is_false(lhs)) return 1;
	if (vf_cond_is_false(rhs)) return 0;
	if (vf_cond_is_true(lhs))  return vf_cond_is_true(rhs);
	if (vf_cond_is_true(rhs))  return 1;

	assert(lhs.count == 1);

	/* Check if lhs implies any of the rhs clauses. */
	for (i = 0; i < rhs.count; i++) {

		vf_clause_rel relation = vf_clause_compare(
			lhs.clauses[0], rhs.clauses[i]
		);

		/* If it does, we are done. */
		if ((relation == vf_clause_rel_lhs_implies_rhs) ||
		    (relation == vf_clause_rel_equal)) {
			return 1;
		}
	}

	return 0;
}

/* Dump the given cond to the given file. */
void vf_cond_dump(vf_cond cond, FILE *f)
{
	if      (vf_cond_is_true(cond))  fprintf(f, "true");
	else if (vf_cond_is_false(cond)) fprintf(f, "false");
	else {
		int i;
		for (i = 0; i < cond.count; i++) {
			if (i != 0) fprintf(f, " | ");
			vf_clause_dump(cond.clauses[i], f);
		}
	}
}

int vf_cond_equals(vf_cond lhs, vf_cond rhs)
{
	int i;

	/* Handle special cases. */
	if (vf_cond_is_true(lhs))  return vf_cond_is_true(rhs);
	if (vf_cond_is_true(rhs))  return vf_cond_is_true(lhs);
	if (vf_cond_is_false(lhs)) return vf_cond_is_false(rhs);
	if (vf_cond_is_false(rhs)) return vf_cond_is_false(lhs);
	assert(vf_cond_is_clause(lhs) && vf_cond_is_clause(rhs));

	/* Compare clause lengths. */
	if (lhs.clauses[0].count != rhs.clauses[0].count) return 0;

	/* And then literals right to left. Use a hash for fast checks? */
	for (i = 0; i < lhs.clauses[0].count; i++) {
		vf_literal lhs_lit = lhs.clauses[0].literals[i];
		vf_literal rhs_lit = rhs.clauses[0].literals[i];

		if ((lhs_lit.irn != rhs_lit.irn) || (lhs_lit.value != rhs_lit.value)) {
			return 0;
		}
	}

	return 1;
}

int vf_cond_is_literal(vf_cond cond)
{
	return (cond.count == 1) && (cond.clauses[0].count == 1);
}

int vf_cond_is_clause(vf_cond cond)
{
	return (cond.count == 1);
}

ir_node *vf_cond_get_irn(vf_cond cond)
{
	assert(vf_cond_is_literal(cond));
	return cond.clauses[0].literals[0].irn;
}

int vf_cond_get_value(vf_cond cond)
{
	assert(vf_cond_is_literal(cond));
	return cond.clauses[0].literals[0].value;
}

vf_info *vf_init(void)
{
	vf_info *vfi = XMALLOC(vf_info);
	obstack_init(&vfi->obst);
	return vfi;
}

void vf_free(vf_info *vfi)
{
	obstack_free(&vfi->obst, NULL);
	xfree(vfi);
}
