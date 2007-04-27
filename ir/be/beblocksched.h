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

/*
 * Block schedule calculator
 *
 * $Id$
 */
#ifndef _BEBLOCKSCHED_H
#define _BEBLOCKSCHED_H

#include "firm_config.h"

#include "obst.h"
#include "execfreq.h"
#include "irnode.h"
#include "irgraph.h"

ir_node **be_create_block_schedule(ir_graph *irg, ir_exec_freq *execfreqs);

#endif /* _BEBLOCKSCHED_H */
