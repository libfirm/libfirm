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
 * @brief   HTML dumper for PBQP.
 * @date    03.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#ifndef KAPS_HTML_DUMPER_H
#define KAPS_HTML_DUMPER_H

#include "pbqp_t.h"

void pbqp_dump_input(pbqp *pbqp);

void pbqp_dump_graph(pbqp *pbqp);

void dump_simplifyedge(pbqp *pbqp, pbqp_edge *edge);

void dump_section(FILE *f, int level, const char *txt);

void dump_node(FILE *file, pbqp_node *node);
void dump_edge(FILE *file, pbqp_edge *edge);

#endif /* KAPS_HTML_DUMPER_H */
