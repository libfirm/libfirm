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
 * @brief       Statistic events.
 * @author      Sebastian Hack
 * @date        03.09.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BESTATEVENT_H
#define FIRM_BE_BESTATEVENT_H

#include <stdio.h>

#include "firm_types.h"

void be_stat_ev_push(const char **tags, int n_tags, FILE *f);
void be_stat_ev_pop(void);

void be_stat_ev(const char *ev, int value);
void be_stat_ev_l(const char *ev, long value);
void be_stat_ev_dbl(const char *ev, double value);
void be_stat_ev_ull(const char *ev, ulong64 value);

int be_stat_ev_is_active(void);

#endif /* FIRM_BE_BESTATEVENT_H */
