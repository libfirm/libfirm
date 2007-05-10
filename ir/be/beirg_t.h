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
#ifndef FIRM_BE_BEIRG_T_H
#define FIRM_BE_BEIRG_T_H

#include "beirg.h"
#include "be_t.h"
#include "irlivechk.h"

/**
 * An ir_graph with additional analysis data about this irg. Also includes some
 * backend structures
 */
struct be_irg_t {
	ir_graph               *irg;
	be_main_env_t          *main_env;
	be_abi_irg_t           *abi;
	arch_code_generator_t  *cg;
	ir_exec_freq           *exec_freq;
	be_dom_front_info_t    *dom_front;
	be_lv_t                *lv;
	lv_chk_t               *lv_chk;
};

static INLINE be_lv_t *
_be_get_birg_liveness(const be_irg_t *birg) {
	return birg->lv;
}

static INLINE lv_chk_t *
_be_get_birg_liveness_chk(const be_irg_t *birg) {
	return birg->lv_chk;
}

static INLINE ir_exec_freq *
_be_get_birg_exec_freq(const be_irg_t *birg) {
	return birg->exec_freq;
}

static INLINE be_dom_front_info_t *
_be_get_birg_dom_front(const be_irg_t *birg) {
	return birg->dom_front;
}

static INLINE ir_graph *
_be_get_birg_irg(const be_irg_t *birg) {
	return birg->irg;
}

static INLINE const arch_env_t *
_be_get_birg_arch_env(const be_irg_t *birg) {
	return birg->main_env->arch_env;
}

#define be_get_birg_exec_freq(birg)        _be_get_birg_exec_freq(birg)
#define be_get_birg_liveness(birg)         _be_get_birg_liveness(birg)
#define be_get_birg_liveness_chk(birg)     _be_get_birg_liveness_chk(birg)
#define be_get_birg_dom_front(birg)        _be_get_birg_dom_front(birg)
#define be_get_birg_irg(birg)              _be_get_birg_irg(birg)

#endif /* FIRM_BE_BEIRG_T_H */
