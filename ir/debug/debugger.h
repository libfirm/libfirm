/*
 * Project:     libFIRM
 * File name:   ir/debug/debugger.h
 * Purpose:     Helper function for integerated debug support
 * Author:      Michael Beck
 * Modified by:
 * Created:     2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef __DEBUGGER_H__
#define __DEBUGGER_H__

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

#endif /* __DEBUGGER_H__ */
