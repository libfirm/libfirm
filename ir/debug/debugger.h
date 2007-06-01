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
 * @brief     Helper function for integrated debug support
 * @author    Michael Beck
 * @date      2005
 * @version   $Id$
 */
#ifndef FIRM_DEBUG_DEBUGGER_H
#define FIRM_DEBUG_DEBUGGER_H

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

#endif
