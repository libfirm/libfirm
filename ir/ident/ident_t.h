/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Hash table to store names -- private header.
 * @author   Goetz Lindenmaier
 */
#ifndef FIRM_IDENT_IDENT_T_H
#define FIRM_IDENT_IDENT_T_H

#include "ident.h"

/**
 * Initialize the ident module.
 */
void init_ident(void);

/**
 * Finishes the ident module, frees all entries.
 */
void finish_ident(void);

/** initializes the name mangling code */
void firm_init_mangle(void);

void firm_finish_mangle(void);

#endif
