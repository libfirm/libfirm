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
 * @brief     Manager for transformation passes.
 * @author    Michael Beck
 * @version   $Id: $
 */
#ifndef FIRM_IR_PASS_H
#define FIRM_IR_PASS_H

#include "firm_types.h"

/**
 * Creates a new ir_graph pass manager.
 *
 * @param name        the name of the manager
 * @param verify_all  if non-zero, all passes of this manager will be verified
 * @param dump_all    if non-zero, all passes results will be dumped
 *
 * @return the newly created manager
 */
ir_graph_pass_manager_t *new_graph_pass_mgr(
	const char *name, int verify_all, int dump_all);

/**
 * Add an ir_graph pass to a graph pass manager.
 *
 * @param mgr   the ir_graph pass manager
 * @param pass  the pass to add
 */
void ir_graph_pass_mgr_add(ir_graph_pass_manager_t *mgr, ir_graph_pass_t *pass);

/**
 * Run all passes of an ir_graph pass manager.
 *
 * @param mgr   the manager
 *
 * @return 0 if all passes return 0, else 1
 */
int ir_graph_pass_mgr_run(ir_graph_pass_manager_t *mgr);

/**
 * Terminate an ir_graph pass manager and all owned passes.
 *
 * @param mgr   the manager
 */
void term_graph_pass_mgr(ir_graph_pass_manager_t *mgr);

/**
 * Creates a new ir_prog pass manager.
 *
 * @param name        the name of the manager
 * @param verify_all  if non-zero, all passes of this manager will be verified
 * @param dump_all    if non-zero, all passes results will be dumped
 *
 * @return  the newly created manager
 */
ir_prog_pass_manager_t *new_prog_pass_mgr(
	const char *name, int verify_all, int dump_all);

/**
 * Add an ir_prog pass to an ir_prog pass manager.
 *
 * @param mgr   the ir_prog pass manager
 * @param pass  the pass to add
 */
void ir_prog_pass_mgr_add(ir_prog_pass_manager_t *mgr, ir_prog_pass_t *pass);

/**
 * Add an ir_graph_pass_manager as a pass to an ir_prog pass manager.
 *
 * @param mgr        the ir_prog pass manager
 * @param graph_mgr  the ir_graph pass manager to be added
 */
void ir_prog_pass_mgr_add_graph_mgr(
	ir_prog_pass_manager_t *mgr, ir_graph_pass_manager_t *graph_mgr);

/**
 * Add an ir_graph_pass as a pass to an ir_prog pass manager.
 *
 * @param mgr   the ir_prog pass manager
 * @param pass  the ir_graph pass to be added
 */
void ir_prog_pass_mgr_add_graph_pass(
	ir_prog_pass_manager_t *mgr, ir_graph_pass_t *pass);

/**
 * Run all passes of an ir_prog pass manager.
 *
 * @param mgr   the manager
 *
 * @return 0 if all passes return 0, else 1
 */
int ir_prog_pass_mgr_run(ir_prog_pass_manager_t *mgr);

/**
 * Terminate an ir_prog pass manager and all owned passes.
 *
 * @param mgr   the manager
 */
void term_prog_pass_mgr(ir_prog_pass_manager_t *mgr);

#endif
