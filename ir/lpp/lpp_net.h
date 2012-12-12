/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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
