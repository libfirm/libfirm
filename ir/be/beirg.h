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
 * @brief       Backend irg - a ir_graph with additional analysis information.
 * @author      Matthias Braun
 * @date        05.05.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BEIRG_H
#define FIRM_BE_BEIRG_H

#include "be.h"
#include "be_types.h"
#include "be_t.h"
#include "irtypes.h"

be_lv_t *be_assure_liveness(ir_graph *irg);

void be_assure_dom_front(ir_graph *irg);
void be_invalidate_dom_front(ir_graph *irg);

/**
 * frees all memory allocated by birg structures (liveness, dom_front, ...).
 * The memory of the birg structure itself is not freed.
 */
void be_free_birg(ir_graph *irg);

/**
 * An ir_graph with additional analysis data about this irg. Also includes some
 * backend structures
 */
typedef struct be_irg_t {
	ir_graph               *irg;
	be_main_env_t          *main_env;
	be_abi_irg_t           *abi;
	arch_code_generator_t  *cg;
	ir_exec_freq           *exec_freq;
	be_dom_front_info_t    *dom_front;
	be_lv_t                *lv;
	struct obstack          obst; /**< birg obstack (mainly used to keep
	                                   register constraints which we can't keep
	                                   in the irg obst, because it gets replace
	                                   during code selection) */
} be_irg_t;

static inline be_irg_t *be_birg_from_irg(const ir_graph *irg)
{
	return (be_irg_t*) irg->be_data;
}

static inline be_lv_t *be_get_irg_liveness(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->lv;
}

static inline ir_exec_freq *be_get_irg_exec_freq(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->exec_freq;
}

static inline be_dom_front_info_t *be_get_irg_dom_front(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->dom_front;
}

static inline be_abi_irg_t *be_get_irg_abi(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->abi;
}

static inline be_options_t *be_get_irg_options(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->main_env->options;
}

static inline arch_code_generator_t *be_get_irg_cg(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->cg;
}

/** deprecated */
static inline ir_graph *be_get_birg_irg(const be_irg_t *birg)
{
	return birg->irg;
}

static inline const arch_env_t *be_get_irg_arch_env(const ir_graph *irg)
{
	return be_birg_from_irg(irg)->main_env->arch_env;
}

static inline struct obstack *be_get_be_obst(const ir_graph *irg)
{
	be_irg_t *birg = be_birg_from_irg(irg);
	return &birg->obst;
}

#endif /* FIRM_BE_BEIRG_H */
