/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 */
#ifndef FIRM_ANA_TROUTS_T_H
#define FIRM_ANA_TROUTS_T_H

#include "trouts.h"

void add_type_pointertype_to(const ir_type *tp, ir_type *ptp);
void add_type_arraytype_of(const ir_type *tp, ir_type *atp);

#endif
