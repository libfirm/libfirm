/*
 * Project:     libFIRM
 * File name:   ir/opt/ldstopt.h
 * Purpose:     optimization of real function calls
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file funccall.h
 *
 * Optimization of real function calls.
 *
 * @author Michael Beck
 */
#ifndef _FUNCCALL_H_
#define _FUNCCALL_H_

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
 * @param force_run  if set, an optimization run is startet even
 *                   if no const function graph was detected.
 *                   Else calls are only optimized if at least one
 *                   const function graph was detected.
 *
 * If the fontend created external entities with irg_const_function
 * property set, the force_run parameter should be set, else
 * should be unset.
 */
void optimize_funccalls(int force_run);

#endif /* _FUNCCALL_H_ */
