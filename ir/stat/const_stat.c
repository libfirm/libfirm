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
 * @brief   Statistic functions for constant counting.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "firmstat_t.h"
#include "tv_t.h"

/**
 * calculated the dual logarithm of |value|
 */
static unsigned log2abs(long value) {
	unsigned res = 0;

	if (value < 0)
		value = -value;

	if (value > 0xFFFF) {
		res += 16;
		value >>= 16;
	}
	if (value > 0xFF) {
		res += 8;
		value >>= 8;
	}
	if (value > 0xF) {
		res += 4;
		value >>= 4;
	}
	if (value > 3) {
		res += 2;
		value >>= 2;
	}
	if (value > 1) {
		res += 1;
	}

	return res;
}

/**
 * classify the value of a float tarval
 */
static float_classify_t classify_float_value(tarval *tv) {
	ir_mode *mode = get_tarval_mode(tv);

	if (tv == get_mode_null(mode))
		return STAT_FC_1;
	else if (tv == get_mode_one(mode))
		return STAT_FC_1;

	return STAT_FC_OTHER;
}

/* return a human readable name for an float classification */
const char *stat_fc_name(float_classify_t classification) {
	switch (classification) {
	case STAT_FC_0:     return "0.0";
	case STAT_FC_1:     return "1.0";
	case STAT_FC_2:     return "2.0";
	case STAT_FC_0_5:   return "0.5";
	case STAT_FC_EXACT: return "exact";
	case STAT_FC_OTHER: return "other";
	default:            return "<UNKNOWN>";
	}
}

/* update info on Consts */
void stat_update_const(stat_info_t *status, ir_node *node, graph_entry_t *graph)
{
	ir_mode *mode = get_irn_mode(node);
	tarval *tv;
	unsigned bits;

	if (mode_is_int(mode)) {
		tv   = get_Const_tarval(node);

		/* FIXME: */
		if (! tarval_is_long(tv))
			return;

		bits = log2abs(get_tarval_long(tv));

		if (bits > ARR_SIZE(status->const_info.int_bits_count))
			bits = ARR_SIZE(status->const_info.int_bits_count);

		cnt_inc(&status->const_info.int_bits_count[bits]);
	} else if (mode_is_float(mode)) {
		tv = get_Const_tarval(node);

		cnt_inc(&status->const_info.floats[classify_float_value(tv)]);
	} else {
		/* something different */
		cnt_inc(&status->const_info.others);
	}
}

/* clears the const statistics for a new snapshot */
void stat_const_clear(stat_info_t *status) {
	int i;

	for (i = 0; i < ARR_SIZE(status->const_info.int_bits_count); ++i)
		cnt_clr(&status->const_info.int_bits_count[i]);

	for (i = 0; i < ARR_SIZE(status->const_info.floats); ++i)
		cnt_clr(&status->const_info.floats[i]);

	cnt_clr(&status->const_info.others);
}

/* initialize the Const statistic. */
void stat_init_const_cnt(stat_info_t *status) {
	/* currently nothing */
}
