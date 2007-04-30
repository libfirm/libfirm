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
 * @brief       Performs SSA-Destruction.
 * @author      Daniel Grund
 * @date        25.05.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BESSADESTR_H
#define FIRM_BE_BESSADESTR_H

#include "bechordal.h"

/**
 * Performs SSA-Destruction. Arguments get adjusted, phi nodes just stay.
 */
void be_ssa_destruction(be_chordal_env_t *chordal_env);
void be_ssa_destruction_check(be_chordal_env_t *chordal_env);

#endif /* FIRM_BE_BESSADESTR_H */
