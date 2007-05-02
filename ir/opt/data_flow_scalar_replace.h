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
 * @brief   scalar replacement of compounds
 * @author  Beyhan Veliev
 * @version $Id$
 */
#ifndef FIRM_OPT_DATA_FLOW_SCALAR_REPLACE_H
#define FIRM_OPT_DATA_FLOW_SCALAR_REPLACE_H

#include "irgraph.h"

/**
 * Do the scalar replacement optimization.
 * Make a date flow analyze and split the
 * data flow edges.
 *
 * @param irg  the graph which should be optimized
 */
void data_flow_scalar_replacement_opt(ir_graph *irg);

#endif
