/*
 * Project:     libFIRM
 * File name:   ir/ana/cgana.h
 * Purpose:     Intraprozedural analyses to estimate the call graph.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universit‰t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * Intraprozedurale Analyse zur Absch‰tzung der Aufrulrelation. Es
 * wird eine Menge von freien Methoden und anschlieﬂend die an den
 * Call-Operationen aufrufbaren Methoden bestimmt.
 *
 */

#ifndef _CGANA_H_
#define _CGANA_H_

#include "entity.h"

/* Methoden sind "frei", wenn ihr Funktionszeiger (potentiell)
 * "explizit" bekannt ist, d.h.:
 *
 * - die Methode ist von aussen sichtbar (external_visible).
 *
 * - ihr Funktionszeiger ist "frei", d.h. der Funktionszeiger wurde
 *   nicht ausschliesslich an den entsprechenden Eingang eines
 *   Call-Knotens weitergegeben, sondern z.B. in den Speicher
 *   geschrieben, als Parameter uebergeben, ...
 *
 * Die main-Methode ist immer in der Menge enthalten.
 *
 * Die Links an den "ir_node"s werden geloescht. */

/** Analyses a rough estimation of the possible call graph.
 *
 *  Determines for each Call node the set of possibly called methods.
 *  Stores the result in the field 'callees' of the Call node.  If the
 *  address can not be analysed, e.g. because it is loaded from a
 *  variable, the array contains NULL. @@@ the array should contain a
 *  special entity 'unknown'. (See "set_Call_callee"). cgana returns
 *  the set of 'free' methods, i.e., the methods that can be called
 *  from external or via function pointers.  This datastructure must
 *  be freed with 'free()' by the caller of cgana.
 *
 *  cgana sets the callee_info_state of each graph to consistent.
 *
 *  The algorithm implements roughly Static Class Hierarchy Analysis
 *  as described in "Optimization of Object-Oriented Programs Using
 *  Static Class Hierarchy Analysis" by Jeffrey Dean and David Grove
 *  and Craig Chambers.
 *
 *  Performs some optimizations possible by the analysed information:
 *    - Replace SymConst-name nodes by SymConst-entity nodes if possible.
 *    - Replace (Sel-method(Alloc)) by SymConst-entity.
 *    - Replaces Sel nodes by Bad if there is no implementation for the
 *         selected entity.  (@@@ was genau meint unreachable?)
 *    - Replaces Sel-method by SymConst-entity if the method is never overwritten.
 *    - Replaces Calls by Tuple containing Bads if callee array is empty
 *         (there is no implementation to call)
 *
 *  Leaves Bad control predecessors in the graph!
 */
void cgana(int *len, entity ***free_methods);

/** Free callee information.
 *
 *  Sets callee_info_state of the graph passed to none.  Sets callee field
 *  in all call nodes to NULL.  Else it happens that the field contains
 *  pointers to other than firm arrays.
 */
void free_callee_info(ir_graph *irg);

/* Optimize the address expressions passed to call nodes.
 * Performs only the optimizations done by cgana. */
void opt_call_addrs(void);


#endif /* _CGANA_H_ */
