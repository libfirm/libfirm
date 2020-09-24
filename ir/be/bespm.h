/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       SPM support
 * @author      Daniel Biester
 * @date        13.08.2020
 */
#ifndef FIRM_BE_SPM_H
#define FIRM_BE_SPM_H

void spm_calculate_dprg_info(void);

void spm_find_memory_allocation(void);

void spm_test_call(void);

#endif
