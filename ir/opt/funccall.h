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
 * Optimize function calls by handling real functions.
 *
 * This optimization first detects all "real fucntions", ie
 * IR graphs that neither read nor write memory (and hence did
 * not create exceptions, as these use memory in Firm).
 *
 * The result of calls to such functions depends only on its
 * arguments, hence those calls are not anymore pinned.
 *
 * This is a rather strong criteria, so do not expect that a
 * lot of functions will be found. Moreover, all of them might
 * already be inlined if inlining is activated.
 * Anyway, it might be good for handling builtin's or pseudo-graphs,
 * even if the later read/write memory (but we know how).
 */
void optimize_funccalls(void);

#endif /* _FUNCCALL_H_ */
