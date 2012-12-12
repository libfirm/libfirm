/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Performs SSA-Destruction.
 * @author      Daniel Grund
 * @date        25.05.2005
 */
#ifndef FIRM_BE_BESSADESTR_H
#define FIRM_BE_BESSADESTR_H

#include "bechordal.h"

/**
 * Performs SSA-Destruction. Arguments get adjusted, phi nodes just stay.
 */
void be_ssa_destruction(be_chordal_env_t *chordal_env);
void be_ssa_destruction_check(be_chordal_env_t *chordal_env);

#endif /* FIRM_BE_BESSADESTR_H */
