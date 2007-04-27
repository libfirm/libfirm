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

/*
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILLMORGAN_H_
#define BESPILLMORGAN_H_

#include "beirg.h"
#include "bearch.h"

void be_spill_morgan(be_irg_t *birg, const arch_register_class_t *cls);

#endif
