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

#ifndef FIRM_ANA_VF_FORMULA_H
#define FIRM_ANA_VF_FORMULA_H

#include <stdio.h>
#include "firm_types.h"

/* Represents a boolean formula in disjunctive form. This is a tool for the
 * other VFirm analyses and not an analysis of itself. Represented formulas
 * have the form:
 *
 * cond    := '(' clause ')' ('OR' '(' clause ')')*
 * clause  := literal ('AND' literal)*
 * literal := irn | '!' irn
 *
 * This provides the means to work with those conditions in a high-level way
 * and allows AND- and OR-combining them, with immediate optimization. Note
 * that finding the minimal form of a logical formula is NP. The code here
 * tries to break down formulas, by applying two simplifications, whenever
 * possible:
 *
 * 1. abc OR a!bc = a(b OR !b)c = ac
 * 2. abc OR ab   = ab
 *
 * It's not ideal and surely O(scary), but let's hope it will be sufficient.
 * At least the whole condition code is isolated here and can be changed. */

typedef struct vf_info   vf_info;
typedef struct vf_clause vf_clause;

typedef struct vf_cond {
	vf_clause *clauses;
	int        count; /* Negative values are reserved. */
} vf_cond;

/** Initialize obstacks etc. */
vf_info *vf_init(void);

/** Free data again. */
void vf_free(vf_info *gci);

/** Get the true condition. */
vf_cond vf_cond_get_true(void);

/** Get the false condition. */
vf_cond vf_cond_get_false(void);

/** Create a simple condition for node irn. Either true or false (value). */
vf_cond vf_cond_new(vf_info *gci, ir_node *irn, char value);

/** Dump the given condition to the given file. */
void vf_cond_dump(vf_cond cond, FILE *f);

/** And-combine the given conds. */
vf_cond vf_cond_and(vf_info *gci, vf_cond lhs, vf_cond rhs);

/** Or-combine the given conds. */
vf_cond vf_cond_or(vf_info *gci, vf_cond lhs, vf_cond rhs);

/** Test if the given cond is true. */
int vf_cond_is_true(vf_cond cond);

/** Test if the given cond is false. */
int vf_cond_is_false(vf_cond cond);

/** Test if lhs implies rhs. Note that lhs can only have one clause. */
int vf_cond_implies(vf_cond lhs, vf_cond rhs);

/** Determine if the condition is a single literal. */
int vf_cond_is_literal(vf_cond cond);

/** Determine if the condition is a single clause. */
int vf_cond_is_clause(vf_cond cond);

/** Get the conditions irn. Must be a single literal. */
ir_node *vf_cond_get_irn(vf_cond cond);

/** Get the conditions value. Must be a single literal. */
int vf_cond_get_value(vf_cond cond);

/** Determines if the given conditions match. Only literal supported. */
int vf_cond_equals(vf_cond lhs, vf_cond rhs);

#endif
