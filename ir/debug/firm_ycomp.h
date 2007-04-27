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
 * @brief     Connect firm to ycomp
 * @author    Christian Wuerdig
 * @date      16.11.2006
 * @version   $Id$
 */
#ifndef FIRM_DEBUG_FIRM_YCOMP_H
#define FIRM_DEBUG_FIRM_YCOMP_H

#define FIRM_YCOMP_DEFAULT_HOST "localhost"
#define FIRM_YCOMP_DEFAULT_PORT 4242

/**
 * Establish connection to yComp and register debugger hooks.
 * @param host Hostname where yComp is running
 * @param port Port on which yComp is listening
 */
void firm_init_ycomp_debugger(const char *host, unsigned port);

/**
 * Close connection to yComp and unregister debugger hooks.
 */
void firm_finish_ycomp_debugger(void);

#endif
