/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

#ifndef FIRM_ANA_IROUTS_T_H
#define FIRM_ANA_IROUTS_T_H

#include "irouts.h"

#define foreach_irn_out(irn, idx, succ) \
	for (bool succ##__b = true; succ##__b;) \
		for (ir_node const *const succ##__irn = (irn); succ##__b; succ##__b = false) \
			for (unsigned idx = 0, succ##__n = get_irn_n_outs(succ##__irn); succ##__b && idx != succ##__n; ++idx) \
				for (ir_node *const succ = (succ##__b = false, get_irn_out(succ##__irn, idx)); !succ##__b; succ##__b = true)

#define foreach_irn_out_r(irn, idx, succ) \
	for (bool succ##__b = true; succ##__b;) \
		for (ir_node const *const succ##__irn = (irn); succ##__b; succ##__b = false) \
			for (unsigned idx = get_irn_n_outs(succ##__irn); succ##__b && idx-- != 0;) \
				for (ir_node *const succ = (succ##__b = false, get_irn_out(succ##__irn, idx)); !succ##__b; succ##__b = true)

#endif
