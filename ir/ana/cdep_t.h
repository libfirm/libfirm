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
 * @brief   control dependence analysis
 * @author  Christoph Mallon
 * @version $Id$
 */
#ifndef FIRM_ANA_CDEP_T_H
#define FIRM_ANA_CDEP_T_H

#include "cdep.h"

/**
 * An entry in the control dependence list.
 */
struct ir_cdep {
	ir_node *node;  /**< A node on which the current block is control dependent on. */
	ir_cdep    *next;  /**< Link to the next one if any. */
};

static inline ir_node *_get_cdep_node(const ir_cdep *cdep)
{
	return skip_Id(cdep->node);
}

static inline ir_cdep *_get_cdep_next(const ir_cdep *cdep)
{
	return cdep->next;
}

#define get_cdep_node(cdep)     _get_cdep_node(cdep)
#define get_cdep_next(cdep)     _get_cdep_next(cdep)

#endif
