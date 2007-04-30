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
 * @brief   Optimization of function calls.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_OPT_FUNCCALL_H
#define FIRM_OPT_FUNCCALL_H

/**
 * Optimize function calls by handling const functions.
 *
 * This optimization first detects all "const functions", i.e.,
 * IR graphs that neither read nor write memory (and hence did
 * not create exceptions, as these use memory in Firm).
 *
 * The result of calls to such functions depends only on its
 * arguments, hence those calls are no more pinned.
 *
 * This is a rather strong criteria, so do not expect that a
 * lot of functions will be found. Moreover, all of them might
 * already be inlined if inlining is activated.
 * Anyway, it might be good for handling builtin's or pseudo-graphs,
 * even if the later read/write memory (but we know how).
 *
 * This optimizations read the irg_const_function property of
 * entities and and sets the irg_const_function property of
 * graphs.
 *
 * If callee information is valid, we also optimize polymorphic Calls.
 *
 * @param force_run  if non-zero, an optimization run is started even
 *                   if no const function graph was detected.
 *                   Else calls are only optimized if at least one
 *                   const function graph was detected.
 *
 * If the fontend created external entities with the irg_const_function
 * property set, the force_run parameter should be set, else
 * should be unset.
 *
 * @note This optimization destroys the link fields of nodes.
 */
void optimize_funccalls(int force_run);

#endif /* FIRM_OPT_FUNCCALL_H */
