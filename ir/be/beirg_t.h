/**
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Backend irg - a ir_graph with additional analysis information
 */
#ifndef BEIRG_T_H_
#define BEIRG_T_H_

#include "beirg.h"

/**
 * An ir_graph with additional analysis data about this irg. Also includes some
 * backend structures
 */
struct _be_irg_t {
	ir_graph                      *irg;
	struct _be_main_env_t         *main_env;
	struct _be_abi_irg_t          *abi;
	struct _arch_code_generator_t *cg;
	ir_exec_freq                  *exec_freq;
	be_dom_front_info_t           *dom_front;
	be_lv_t                       *lv;
};

static INLINE be_lv_t *
_be_get_birg_liveness(const be_irg_t *birg) {
	return birg->lv;
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

#define be_get_birg_exec_freq(birg)        _be_get_birg_exec_freq(birg)
#define be_get_birg_liveness(birg)         _be_get_birg_liveness(birg)
#define be_get_birg_dom_front(birg)        _be_get_birg_dom_front(birg)
#define be_get_birg_irg(birg)              _be_get_birg_irg(birg)

#endif
