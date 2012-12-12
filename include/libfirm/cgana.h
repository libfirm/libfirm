/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Intraprozedural analyses to estimate the call graph.
 * @author      Hubert Schmid
 * @date        09.06.2002
 * @brief
 *  Interprocedural analysis to estimate the calling relation.
 *
 *  This analysis computes all entities representing methods that
 *  can be called at a Call node.  Further it computes a set of
 *  methods that are 'free', i.e., their adress is handled by
 *  the program directly, or they are visible external.
 */
#ifndef FIRM_ANA_CGANA_H
#define FIRM_ANA_CGANA_H

#include "firm_types.h"
#include "begin.h"

/** @addtogroup callgraph
 * @{
 */

/** Analyses a rough estimation of the possible call graph.
 *
 *  Determines for each Call node the set of possibly called methods.
 *  Stores the result in the field 'callees' of the Call node.  If the
 *  address can not be analysed, e.g. because it is loaded from a
 *  variable, the array contains the unknown_entity. (See
 *  set_Call_callee()). cgana() returns the set of 'free' methods, i.e.,
 *  the methods that can be called from external or via function
 *  pointers.  This datastructure must be freed with 'xfree()' by the
 *  caller of cgana().
 *
 *  cgana() sets the callee_info_state of each graph and the program to
 *  consistent.
 *
 *  The algorithm implements roughly Static Class Hierarchy Analysis
 *  as described in "Optimization of Object-Oriented Programs Using
 *  Static Class Hierarchy Analysis" by Jeffrey Dean and David Grove
 *  and Craig Chambers.
 *
 *  Performs some optimizations possible by the analysed information:
 *  - Replace SymConst-name nodes by SymConst-entity nodes if possible.
 *  - Replace (Sel-method(Alloc)) by SymConst-entity.
 *  - Replaces Sel-method by SymConst-entity if the method is never overwritten.
 */
FIRM_API size_t cgana(ir_entity ***free_methods);

/**
 * Frees callee information.
 *
 * Sets callee_info_state of the graph passed to none.  Sets callee field
 * in all call nodes to NULL.  Else it happens that the field contains
 * pointers to other than firm arrays.
 */
FIRM_API void free_callee_info(ir_graph *irg);
/** Frees callee information for all graphs in the current program. */
FIRM_API void free_irp_callee_info(void);

/**
 * Optimizes the address expressions passed to call nodes.
 * Performs only the optimizations done by cgana.
 */
FIRM_API void opt_call_addrs(void);

/** @} */

#include "end.h"

#endif
