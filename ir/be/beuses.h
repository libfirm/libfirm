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
 * @brief       Methods to compute when a value will be used again.
 * @author      Sebastian Hack, Matthias Braun
 * @date        27.06.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BEUSES_H
#define FIRM_BE_BEUSES_H

#include "firm_types.h"
#include "belive.h"

typedef struct _be_next_use_t {
	unsigned       time;
	int            outermost_loop;
	const ir_node *before;
} be_next_use_t;

#define USES_INFINITY  10000000
#define USES_PENDING   9999999

static INLINE int USES_IS_INFINITE(unsigned time)
{
	return time >= USES_INFINITY;
}

static INLINE int USES_IS_PENDING(unsigned time)
{
	return time == USES_PENDING;
}

typedef struct _be_uses_t be_uses_t;

be_next_use_t be_get_next_use(be_uses_t *uses, ir_node *from,
                         unsigned from_step, const ir_node *def,
                         int skip_from_uses);

be_uses_t *be_begin_uses(ir_graph *irg, const be_lv_t *lv);

void be_end_uses(be_uses_t *uses);

#endif /* FIRM_BE_BEUSES_H */
