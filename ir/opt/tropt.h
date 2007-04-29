/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Perform optimizations of the type representation.
 * @date    20.4.2005
 * @author  Goetz Lindenmaier
 * @version $Id$
 */
#ifndef FIRM_OPT_TROPT_H
#define FIRM_OPT_TROPT_H

#include "firm_types.h"

/** This is the type for a method, that returns a pointer type to
 *  tp.  This is needed in the normalization. */
typedef ir_type *(*gen_pointer_type_to_func)(ir_type *tp);

/**  Insert Casts so that class type casts conform exactly with the type hierarchy.
 *
 *  Formulated in Java, this achieves the following:
 *
 *  For a class hierarchy
 *    class A {}
 *    class B extends A {}
 *    class C extends B {}
 *  we transforms a cast
 *    (A)new C()
 *  to
 *    (A)((B)new C()).
 *
 *  The algorithm works for Casts with class types, but also for Casts
 *  with all pointer types that point (over several indirections,
 *  i.e. ***A) to a class type.  Normalizes all graphs.  Computes type
 *  information (@see irtypeinfo.h) if not available.
 *  Invalidates trout information as new casts are generated.
 *
 *  @param gppt_fct A function that returns a pointer type that points
 *    to the type given as argument.  If this parameter is NULL, a default
 *    function is used that either uses trout information or performs a O(n)
 *    search to find an existing pointer type.  If it can not find a type,
 *    generates a pointer type with mode_P_mach and suffix "cc_ptr_tp".
 */
void normalize_irp_class_casts(gen_pointer_type_to_func gppt_fct);


/**  Insert Casts so that class type casts conform exactly with the type hierarchy
 *   in given graph.
 *
 *   For more details see normalize_irp_class_casts().
 *
 *  This transformation requires that type information is computed. @see irtypeinfo.h.
 */
void normalize_irg_class_casts(ir_graph *irg, gen_pointer_type_to_func gppt_fct);


/** Optimize casting between class types.
 *
 *    class A { m(); }
 *    class B extends A { }
 *    class C extends B {}
 *  Performs the following transformations:
 *    C c = (C)(B)(A)(B)new C()  --> C c = (C)(B)newC() --> C c = new C()
 *    (Optimizing downcasts as A a = (A)(B)(new A()) --> A a = new A() can
 *     be suppressed by setting the flag opt_suppress_downcast_optimization.
 *     Downcasting A to B might cause an exception.  It is not clear
 *     whether this is modeled by the Firm Cast node, as it has no exception
 *     outputs.);
 *  If there is inh_m() that overwrites m() in B:
 *    ((A) new B()).m()  --> (new B()).inh_m()
 *  Phi((A)x, (A)y)  --> (A) Phi (x, y)  if (A) is an upcast.
 *
 *  Computes type information if not available. @see irtypeinfo.h.
 *  Typeinformation is valid after optimization.
 *  Invalidates trout information.
 */
void optimize_class_casts(void);

#endif /* FIRM_OPT_TROPT_H */
