/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   iropt --- optimizations of an ir node.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IROPT_H
#define FIRM_IR_IROPT_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup iroptimize
 * @defgroup iropt  Local Optimizations
 * @{
 */

/** If the expression referenced can be evaluated statically
 *  computed_value returns a tarval representing the result,
 *  tarval_bad if operation will throw an exception/produce undefined
 *  behaviour, otherwise tarval_unknown. */
FIRM_API ir_tarval *computed_value(const ir_node *n);

/** Applies all optimizations to n that are expressible as a pattern
 *  in Firm, i.e., they need not a walk of the graph.
 *  Returns a better node for n.  Does not free n -- other nodes could
 *  reference n.
 *
 *  An equivalent optimization is applied in the constructors defined in
 *  ircons.ch.  There n is freed if a better node could be found.
 */
FIRM_API ir_node *optimize_in_place(ir_node *n);

/**
 * checks whether 1 value is the negated other value
 */
FIRM_API int ir_is_negated_value(const ir_node *a, const ir_node *b);

/**
 * (conservatively) approximates all possible relations when comparing
 * the value @p left and @p right
 */
FIRM_API ir_relation ir_get_possible_cmp_relations(const ir_node *left,
                                                   const ir_node *right);

/**
 * enable/disable imprecise floatingpoint optimizations. These include rules
 * like (a - x) + x = a, or a-0=a. They are wrong in several corner cases but
 * may still be fine for some applications.
 */
FIRM_API void ir_allow_imprecise_float_transforms(int enable);

/** return 1 if imprecise float transformations are allowed. */
FIRM_API int ir_imprecise_float_transforms_allowed(void);

/** @} */

#include "end.h"

#endif
