/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Memory disambiguator
 * @author   Michael Beck
 * @date     27.12.2006
 */
#ifndef FIRM_ANA_IRMEMORY_T_H
#define FIRM_ANA_IRMEMORY_T_H

/**
 * One-time inititialization of the memory< disambiguator.
 */
void firm_init_memory_disambiguator(void);

bool is_partly_volatile(ir_node *ptr);

#endif
