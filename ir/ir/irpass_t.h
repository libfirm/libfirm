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
#ifndef FIRM_IR_PASS_T_H
#define FIRM_IR_PASS_T_H

/**
 * @file
 * @brief     Manager for optimization passes. Private Header
 * @author    Michael Beck
 * @version   $Id: $
 */
#include "firm_types.h"
#include "firm_common.h"
#include "adt/list.h"
#include "irpass.h"

/**
 * Pass function on an ir_graph.
 */
typedef int (*RUN_ON_IRG_FUNC)(ir_graph *irg, void *ctx);

/**
 * Dump function on an ir_graph.
 */
typedef void (*DUMP_ON_IRG_FUNC)(ir_graph *irg, void *ctx, unsigned idx);

/**
 * Pass function on an ir_prog.
 */
typedef int (*RUN_ON_IRPROG_FUNC)(ir_prog *prog, void *ctx);

/**
 * Dump function on an ir_prog.
 */
typedef void (*DUMP_ON_IRPROG_FUNC)(ir_prog *irg, void *ctx, unsigned idx);

/**
 * Init/Term function on an pass.
 */
typedef void (*INIT_TERM_FUNC)(void *ctx);

/**
 * A graph pass.
 */
struct ir_graph_pass_t {
	/** The firm kind. */
	firm_kind          kind;

	/** This function is run on every graph on an ir_prog. */
	RUN_ON_IRG_FUNC    run_on_irg;
	RUN_ON_IRG_FUNC    verify_irg;
	DUMP_ON_IRG_FUNC   dump_irg;
	/** This function is called if this pass is added. */
	INIT_TERM_FUNC     add_to_mgr;
	/** This function is called if this pass is removed. */
	INIT_TERM_FUNC     rem_from_mgr;

	/** context parameter for this pass */
	void               *context;

	/** The name of the pass. */
	const char         *name;

	/** Links all passes. */
	list_head          list;

	unsigned verify:1;     /**< Set if this pass should be verified. */
	unsigned dump:1;       /**< Set if this pass should be dumped. */
};

/**
 * A irprog pass.
 */
struct ir_prog_pass_t {
	/** The firm kind. */
	firm_kind           kind;

	/** This function is on an ir_prog. */
	RUN_ON_IRPROG_FUNC  run_on_irprog;
	RUN_ON_IRPROG_FUNC  verify_irprog;
	DUMP_ON_IRPROG_FUNC dump_irprog;
	/** This function is called if this pass is added. */
	INIT_TERM_FUNC      add_to_mgr;
	/** This function is called if this pass is removed. */
	INIT_TERM_FUNC      rem_from_mgr;

	/** context parameter for this pass */
	void                *context;

	/** The name of the pass. */
	const char          *name;

	/** Links all passes */
	list_head           list;

	unsigned verify:1;     /**< Set if this pass should be verified. */
	unsigned dump:1;       /**< Set if this pass should be dumped. */
};

/**
 * A graph pass manager.
 */
struct ir_graph_pass_manager_t {
	firm_kind  kind;           /**< The firm kind. */
	list_head  passes;         /**< The list of passes. */
	unsigned   n_passes;       /**< Number of added passes. */
	const char *name;          /**< the name of the manager. */
	unsigned   run_idx;        /**< The run number for the first pass of this manager. */
	unsigned   verify_all:1;   /**< Set if every pass should be verified. */
	unsigned   dump_all:1;     /**< Set if every pass should be dumped. */
};

/**
 * A irprog pass manager.
 */
struct ir_prog_pass_manager_t {
	firm_kind  kind;           /**< The firm kind. */
	list_head  passes;         /**< The list of passes. */
	unsigned   n_passes;       /**< Number of added passes. */
	const char *name;          /**< the name of the manager. */
	unsigned   run_idx;        /**< The run number for the first pass of this manager. */
	unsigned   verify_all:1;   /**< Set if every pass should be verified. */
	unsigned   dump_all:1;     /**< Set if every pass should be dumped. */
};

#endif
