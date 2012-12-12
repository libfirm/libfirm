/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */
#ifndef FIRM_IR_PASS_T_H
#define FIRM_IR_PASS_T_H

/**
 * @file
 * @brief     Manager for optimization passes. Private Header
 * @author    Michael Beck
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
 * An ir_sgraph pass.
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

	unsigned run_parallel:1;       /**< if set this pass can run parallel on all graphs. */
};

/**
 * An ir_prog pass.
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

	unsigned is_wrapper:1; /**< set if this is a wrapper pass. */
};

/**
 * An ir_graph pass manager.
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
 * An ir_prog pass manager.
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

/**
 * Ensure that no verifier is run an ir_prog pass.
 */
int ir_prog_no_verify(ir_prog *prog, void *ctx);

/**
 * Ensure that no dumper is run from an ir_prog pass.
 */
void ir_prog_no_dump(ir_prog *prog, void *ctx, unsigned idx);

#endif
