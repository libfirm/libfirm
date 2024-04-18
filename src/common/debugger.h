/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Helper function for integrated debug support
 * @author    Michael Beck
 * @date      2005
 */
#ifndef FIRM_DEBUG_DEBUGGER_H
#define FIRM_DEBUG_DEBUGGER_H

#include "firm_types.h"

/** Break into the debugger. */
void firm_debug_break(void);

/**
 * High level function to use from debugger interface
 *
 * Supported commands:
 *  .create nr    break if node nr was created
 *  .help         list all commands
 */
void firm_break(const char *cmd);

/** Creates the debugger tables. */
void firm_init_debugger(void);

void firm_finish_debugger(void);

/**
 * @defgroup external_debug    helper functions for debuggers
 *
 * @{
 */

/**
 * Return the content of the debug text buffer.
 *
 * To be called from the debugger.
 */
const char *firm_debug_text(void);

/**
 * A gdb helper function to print tarvals.
 */
const char *gdb_tarval_helper(void *tv_object);

/**
 * A gdb helper to print all (new-style-) out edges of a node
 */
const char *gdb_out_edge_helper(const ir_node *node);

/**
 * High level function to use from debugger interface
 *
 * See show_commands() for supported commands.
 */
void firm_debug(const char *cmd);

/**
 * @}
 */

#endif
