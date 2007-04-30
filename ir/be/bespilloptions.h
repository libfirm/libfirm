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
 * @brief       Option handling for spiller.
 * @author      Matthias Braun
 * @date        12.10.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BESPILL_OPTIONS_H
#define FIRM_BE_BESPILL_OPTIONS_H

#include "bearch.h"
#include "beirg.h"

extern int be_coalesce_spill_slots;
extern int be_do_remats;

typedef struct be_spiller_t {
	void (*spill) (be_irg_t *birg, const arch_register_class_t *cls);
} be_spiller_t;
void be_register_spiller(const char *name, be_spiller_t *spiller);

void be_do_spill(be_irg_t *birg, const arch_register_class_t *cls);

#endif /* FIRM_BE_BESPILL_OPTIONS_H */
