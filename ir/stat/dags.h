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
 * @brief   Statistics for Firm. DAG's in graphs.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_STAT_DAGS_H
#define FIRM_STAT_DAGS_H

#include "firmstat_t.h"
/*
 * count the DAG's size of a graph
 */
void count_dags_in_graph(graph_entry_t *global, graph_entry_t *graph);

#endif /* FIRM_STAT_DAGS_H */
