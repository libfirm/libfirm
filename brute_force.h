/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Brute force PBQP solver.
 * @date    02.12.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#ifndef KAPS_BRUTE_FORCE_H
#define KAPS_BRUTE_FORCE_H

#include "pbqp_t.h"

void solve_pbqp_brute_force(pbqp_t *pbqp);

#endif /* KAPS_BRUTE_FORCE_H */
