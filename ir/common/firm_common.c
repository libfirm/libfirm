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
 * @author    Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "config.h"

#include "irgraph.h"
#include "irloop.h"
#include "tv.h"

/**
 * Ideally, this macro would check if size bytes could be read at
 * pointer p. No generic solution.
 */
#define POINTER_READ(p, size) (p)

/* returns the kind of the thing */
firm_kind get_kind(const void *firm_thing)
{
	return POINTER_READ(firm_thing, sizeof(firm_kind)) ? *(firm_kind *)firm_thing : k_BAD;
}
