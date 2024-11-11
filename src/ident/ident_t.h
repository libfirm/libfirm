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

#define get_id_str(x)   get_id_str_(x)

static inline const char *get_id_str_(ident *ident)
{
	return ident;
}

/**
 * Initialize the ident module.
 */
void init_ident(void);

/**
 * Finishes the ident module, frees all entries.
 */
void finish_ident(void);

#define NEW_IDENT(x) new_id_from_chars((x), sizeof(x) - 1)

#endif
