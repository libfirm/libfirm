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
 * Author:      Daniel Grund, Matthias Braun
 * Date:		20.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef BESPILLBELADY_H_
#define BESPILLBELADY_H_

#include "beirg.h"
#include "bearch.h"
#include "bespill.h"

/**
 * Same as be_spill_belady but reuses an existing spill environment.
 * This is usefull for "pre-spillers" that create some spills+reloads
 * but can't ensure that regpressure never exceeds the number of registers
 */
void be_spill_belady_spill_env(be_irg_t *birg, const arch_register_class_t *cls,
                               spill_env_t *spill_env);

#endif /*BESPILLBELADY_H_*/
