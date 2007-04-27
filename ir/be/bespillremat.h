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
 * @file   bespillremat.h
 * @date   2006-04-06
 * @author Adam M. Szalkowski
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef BESPILLREMAT_H_
#define BESPILLREMAT_H_
#include "bechordal.h"

void be_spill_remat(be_irg_t *birg, const arch_register_class_t *cls);

#endif /*BESPILLREMAT_H_*/
