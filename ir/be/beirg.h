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
 * @brief       Backend irg - a ir_graph with additional analysis information.
 * @author      Matthias Braun
 * @date        05.05.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BEIRG_H
#define FIRM_BE_BEIRG_H

#include "execfreq.h"
#include "belive.h"
#include "bedomfront.h"

typedef struct be_irg_t be_irg_t;

ir_graph *be_get_birg_irg(const be_irg_t *birg);

void be_assure_liveness(be_irg_t *birg);
void be_invalidate_liveness(be_irg_t *birg);
be_lv_t *be_get_birg_liveness(const be_irg_t *birg);

void be_assure_dom_front(be_irg_t *birg);
void be_invalidate_dom_front(be_irg_t *birg);
be_dom_front_info_t *be_get_birg_dom_front(const be_irg_t *birg);

const arch_env_t *be_get_birg_arch_env(const be_irg_t *birg);

ir_exec_freq *be_get_birg_exec_freq(const be_irg_t *birg);

/**
 * frees all memory allocated by birg structures (liveness, dom_front, ...).
 * The memory of the birg structure itself is not freed.
 */
void be_free_birg(be_irg_t *birg);

#endif /* FIRM_BE_BEIRG_H */
