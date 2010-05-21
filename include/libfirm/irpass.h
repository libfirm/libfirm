/*
 * Copyright (C) 1995-2009 University of Karlsruhe.  All right reserved.
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
 * @brief     Manager for transformation passes.
 * @author    Michael Beck
 * @version   $Id$
 */
#ifndef FIRM_IR_PASS_H
#define FIRM_IR_PASS_H

#include "firm_types.h"
#include "begin.h"

/**
 * Creates a new ir_graph pass manager.
 *
 * @param name        the name of the manager
 * @param verify_all  if non-zero, all passes of this manager will be verified
 * @param dump_all    if non-zero, all passes results will be dumped
 *
 * @return the newly created manager
 */
FIRM_API ir_graph_pass_manager_t *new_graph_pass_mgr(const char *name,
                                                     int verify_all,
                                                     int dump_all);

/**
 * Add an ir_graph pass to a graph pass manager.
 *
 * @param mgr   the ir_graph pass manager
 * @param pass  the pass to add
 */
FIRM_API void ir_graph_pass_mgr_add(ir_graph_pass_manager_t *mgr,
                                    ir_graph_pass_t *pass);

/**
 * Run all passes of an ir_graph pass manager.
 *
 * @param mgr   the manager
 *
 * @return 0 if all passes return 0, else 1
 */
FIRM_API int ir_graph_pass_mgr_run(ir_graph_pass_manager_t *mgr);

/**
 * Terminate an ir_graph pass manager and all owned passes.
 *
 * @param mgr   the manager
 */
FIRM_API void term_graph_pass_mgr(ir_graph_pass_manager_t *mgr);

/**
 * Creates a new ir_prog pass manager.
 *
 * @param name        the name of the manager
 * @param verify_all  if non-zero, all passes of this manager will be verified
 * @param dump_all    if non-zero, all passes results will be dumped
 *
 * @return  the newly created manager
 */
FIRM_API ir_prog_pass_manager_t *new_prog_pass_mgr(const char *name,
                                                   int verify_all,
                                                   int dump_all);

/**
 * Add an ir_prog pass to an ir_prog pass manager.
 *
 * @param mgr   the ir_prog pass manager
 * @param pass  the pass to add
 */
FIRM_API void ir_prog_pass_mgr_add(ir_prog_pass_manager_t *mgr,
                                   ir_prog_pass_t *pass);

/**
 * Add an ir_graph_pass_manager as a pass to an ir_prog pass manager.
 *
 * @param mgr        the ir_prog pass manager
 * @param graph_mgr  the ir_graph pass manager to be added
 */
FIRM_API void ir_prog_pass_mgr_add_graph_mgr(ir_prog_pass_manager_t *mgr,
                                            ir_graph_pass_manager_t *graph_mgr);

/**
 * Add an ir_graph_pass as a pass to an ir_prog pass manager.
 *
 * @param mgr   the ir_prog pass manager
 * @param pass  the ir_graph pass to be added
 */
FIRM_API void ir_prog_pass_mgr_add_graph_pass(ir_prog_pass_manager_t *mgr,
                                              ir_graph_pass_t *pass);

/**
 * Run all passes of an ir_prog pass manager.
 *
 * @param mgr   the manager
 *
 * @return 0 if all passes return 0, else 1
 */
FIRM_API int ir_prog_pass_mgr_run(ir_prog_pass_manager_t *mgr);

/**
 * Terminate an ir_prog pass manager and all owned passes.
 *
 * @param mgr   the manager
 */
FIRM_API void term_prog_pass_mgr(ir_prog_pass_manager_t *mgr);

/**
 * Set the run index for an irgraph pass manager.
 *
 * @param mgr      the manager
 * @param run_idx  the index for the first pass of this manager
 */
FIRM_API void ir_graph_pass_mgr_set_run_idx(
	ir_graph_pass_manager_t *mgr, unsigned run_idx);

/**
 * Creates an ir_graph pass for running void function(ir_graph *irg).
 * Uses the default verifier and dumper.
 * The pass returns always 0.
 *
 * @param name      the name of this pass
 * @param function  the function to run
 *
 * @return  the newly created ir_graph pass
 */
FIRM_API ir_graph_pass_t *def_graph_pass(
	const char *name, void (*function)(ir_graph *irg));

/**
 * Creates an ir_graph pass for running int function(ir_graph *irg).
 * Uses the default verifier and dumper.
 * The pass returns the return value of function.
 *
 * @param name      the name of this pass
 * @param function  the function to run
 *
 * @return  the newly created ir_graph pass
 */
FIRM_API ir_graph_pass_t *def_graph_pass_ret(
	const char *name, int (*function)(ir_graph *irg));

/**
 * Creates an ir_graph pass for running int function(ir_graph *irg).
 * Uses the default verifier and dumper.
 * The pass returns the return value of function.
 *
 * @param memory    if non-NULL, an already allocated ir_graph_pass_t
 * @param name      the name of this pass
 * @param function  the function to run
 *
 * @return  the newly created ir_graph pass
 */
FIRM_API ir_graph_pass_t *def_graph_pass_constructor(
	ir_graph_pass_t *memory,
	const char *name, int (*function)(ir_graph *irg, void *context));

/**
 * Set the run_parallel property of a graph pass.
 * If the flag is set to non-zero, the pass can be executed
 * parallel on all graphs of a ir_prog.
 *
 * @param pass  the pass
 * @param flag  new flag setting
 */
FIRM_API void ir_graph_pass_set_parallel(ir_graph_pass_t *pass, int flag);

/**
 * Creates an ir_prog pass for running void function().
 * Uses the default verifier and dumper.
 * The pass returns always 0.
 *
 * @param name      the name of this pass
 * @param function  the function to run
 *
 * @return  the newly created ir_graph pass
 */
FIRM_API ir_prog_pass_t *def_prog_pass(
	const char *name, void (*function)(void));

/**
 * Creates an ir_prog pass for running void function().
 * Uses the default verifier and dumper.
 * The pass returns always 0.
 *
 * @param memory    if non-NULL, an already allocated ir_prog_pass_t
 * @param name      the name of this pass
 * @param function  the function to run
 *
 * @return  the newly created ir_prog pass
 */
FIRM_API ir_prog_pass_t *def_prog_pass_constructor(
	ir_prog_pass_t *memory,
	const char *name, int (*function)(ir_prog *irp, void *context));

/**
 * Create a pass that calls some function.
 * This pass calls the given function, but has no dump nor verify.
 *
 * @param name      the name of this pass
 * @param function  the function to run
 * @param context   context parameter
 *
 * @return  the newly created ir_prog pass
 */
FIRM_API ir_prog_pass_t *call_function_pass(
	const char *name, void (*function)(void *context), void *context);

/**
 * Set the run index for an irprog pass manager.
 *
 * @param mgr      the manager
 * @param run_idx  the index for the first pass of this manager
 */
FIRM_API void ir_prog_pass_mgr_set_run_idx(
	ir_prog_pass_manager_t *mgr, unsigned run_idx);

#include "end.h"

#endif
