/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declaration for the transform function (code selection)
 */
#ifndef FIRM_BE_AMD64_AMD64_TRANSFORM_H
#define FIRM_BE_AMD64_AMD64_TRANSFORM_H

void amd64_init_transform(void);

ir_node *amd64_new_spill(ir_node *value, ir_node *after);

ir_node *amd64_new_reload(ir_node *value, ir_node *spill, ir_node *before);

void amd64_transform_graph(ir_graph *irg);

#endif
