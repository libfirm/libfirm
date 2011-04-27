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
 * @brief   Partition or  VFirm graphs for Firm construction.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_OPT_VF_DSTR_PARTITION_H
#define FIRM_OPT_VF_DSTR_PARTITION_H

#include "firm_types.h"
#include "vf_loop.h"

typedef struct vp_info vp_info;

/** Partition the graph in acyclic subgraphs with the given loop info. */
vp_info *vp_partition(ir_graph *irg);

/** Combine the subgraphs again. */
void vp_combine(vp_info *vpi);

/** Free the partition data again. */
void vp_free(vp_info *vpi);

/** Get the loop analyses information from the restore data. */
vl_info *vp_get_vl_info(vp_info *vpi);

#endif
