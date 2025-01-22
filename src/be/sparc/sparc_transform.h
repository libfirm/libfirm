/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declaration for the transform function (code selection)
 * @author  Hannes Rapp, Matthias Braun
 */
#ifndef FIRM_BE_SPARC_SPARC_TRANSFORM_H
#define FIRM_BE_SPARC_SPARC_TRANSFORM_H

#include "firm_types.h"
#include <stdbool.h>

void sparc_init_transform(void);

void sparc_transform_graph(ir_graph *irg);

void sparc_init_asm_constraints(void);

ir_node *create_ldf(dbg_info *dbgi, ir_node *block, ir_node *ptr,
                    ir_node *mem, ir_mode *mode, ir_entity *entity,
                    long offset, bool is_frame_entity);

ir_node *create_stf(dbg_info *dbgi, ir_node *block, ir_node *value,
                    ir_node *ptr, ir_node *mem, ir_mode *mode,
                    ir_entity *entity, long offset,
                    bool is_frame_entity);

#endif
