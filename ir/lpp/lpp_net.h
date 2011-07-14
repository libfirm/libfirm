/*
 * Copyright (C) 2005-2011 University of Karlsruhe.  All right reserved.
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
 * @author  Sebastian Hack
 */
#ifndef LPP_LPP_NET_H
#define LPP_LPP_NET_H

#include "lpp.h"

char **lpp_get_solvers(const char *host);

void lpp_set_dbg(const char *host, int mask);

void lpp_solve_net(lpp_t *lpp, const char *host, const char *solver);

#endif
