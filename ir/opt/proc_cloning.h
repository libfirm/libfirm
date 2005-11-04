/*
 * Project:     libFIRM
 * File name:   ir/clone functions./proc_cloning.h
 * Purpose:     procedure cloning
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef PROC_CLONING_H
#define PROC_CLONING_H

#include "firm_types.h"

/**
 * Do the procedure cloning. Evaluate a heuristic weight for every
 * call(..., Const, ...). If the weight is bigger than threshold,
 * clone the entity and fix the calls.
 *
 * @param threshold   the threshold for cloning
 */
void proc_cloning(float threshold);

#endif /* PROC_CLONING_H */
