/**
 * @file   lpp_net.h
 * @date   20.07.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _LPP_NET_H
#define _LPP_NET_H

#include "lpp.h"

char **lpp_get_solvers(const char *host);

void lpp_set_dbg(const char *host, int mask);

void lpp_solve_net(lpp_t *lpp, const char *host, const char *solver);

#endif /* _LPP_NET_H */
