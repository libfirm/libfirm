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

#include <stddef.h>
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
 *  pointers.  This data structure must be freed with 'xfree()' by the
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
 *  - Replace (Sel-method(Alloc)) by Address.
 *  - Replaces Sel-method by Address if the method is never overwritten.
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

/** Sets, get and remove the callee information for a Call node.
 *
 *  The callee information lists all method entities that can be called
 *  from this node.  If the address expression can not be analyzed fully,
 *  e.g., as entities can be called that are not in the compilation unit,
 *  the array contains the unknown_entity.  The array contains only entities
 *  with peculiarity_existent, but with all kinds of visibility.  The entities
 *  not necessarily contain an irg.
 *
 *  The array is only accessible if callee information is valid.  See flag
 *  in graph.
 *
 *  The memory allocated for the array is managed automatically, i.e., it must
 *  not be freed if the Call node is removed from the graph.
 *
 *  @param node A Call node.
 */
FIRM_API int cg_call_has_callees(const ir_node *node);
/** Returns the number of callees of Call node @p node. */
FIRM_API size_t cg_get_call_n_callees(const ir_node *node);
/** Returns callee number @p pos of Call node @p node. */
FIRM_API ir_entity *cg_get_call_callee(const ir_node *node, size_t pos);

/** Sets the full callee array.
 *
 *  The passed array is copied. */
FIRM_API void cg_set_call_callee_arr(ir_node *node, size_t n, ir_entity **arr);
/** Frees callee array of call node @p node */
FIRM_API void cg_remove_call_callee_arr(ir_node *node);

/** @} */

#include "end.h"

#endif
