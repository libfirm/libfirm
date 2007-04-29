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
 * @brief   Statistics for Firm. Dumping patterns.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_STAT_PATTERN_DMP_H
#define FIRM_STAT_PATTERN_DMP_H

#include "counter.h"

typedef struct _pattern_dumper_t pattern_dumper_t;

/**
 * Starts a new pattern.
 */
void pattern_dump_new_pattern(pattern_dumper_t *self, counter_t *cnt);

/**
 * Finish the current pattern.
 */
void pattern_dump_finish_pattern(pattern_dumper_t *self);

/**
 * Dumps a node.
 */
void pattern_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code, void *attr);

/**
 * Dump a ref.
 */
void pattern_dump_ref(pattern_dumper_t *self, unsigned id);

/**
 * Dump an edge.
 *
 * @param tgt       The target ID
 * @param src       The source ID
 * @param pos       The edge position
 * @param mode_code The mode_code of the edge
 */
void pattern_dump_edge(pattern_dumper_t *self, unsigned tgt, unsigned src, unsigned pos, unsigned mode_code);

/**
 * Start the children dumper.
 */
void pattern_start_children(pattern_dumper_t *self, unsigned id);

/**
 * Finish the children dumper.
 */
void pattern_finish_children(pattern_dumper_t *self, unsigned id);

/**
 * Finish the dumper, destroys the dumper object
 */
void pattern_end(pattern_dumper_t *self);

/**
 * Pattern dumper factory for text dumper.
 */
pattern_dumper_t *new_text_dumper(void);

/**
 * Pattern dumper factory for vcg dumper.
 *
 * @param vcg_name    name of the VCG file
 * @param max_pattern maximum number of pattern to be dumped
 */
pattern_dumper_t *new_vcg_dumper(const char *vcg_name, unsigned max_pattern);

#endif /* FIRM_STAT_PATTERN_DMP_H */
