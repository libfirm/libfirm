/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declaration for the transform function (code selection)
 */
#ifndef FIRM_BE_amd64_amd64_TRANSFORM_H
#define FIRM_BE_amd64_amd64_TRANSFORM_H

void amd64_init_transform(void);

void amd64_transform_graph(ir_graph *irg);

#endif
