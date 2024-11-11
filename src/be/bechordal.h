/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Chordal register allocation.
 * @author      Sebastian Hack
 * @date        14.12.2004
 */
#ifndef FIRM_BE_BECHORDAL_H
#define FIRM_BE_BECHORDAL_H

typedef struct be_chordal_env_t     be_chordal_env_t;
typedef struct be_ra_chordal_opts_t be_ra_chordal_opts_t;
typedef struct border_t             border_t;

typedef struct be_ra_chordal_coloring_t {
	void (*allocate)(be_chordal_env_t *env);
} be_ra_chordal_coloring_t;

void be_register_chordal_coloring(const char *name, be_ra_chordal_coloring_t *coloring);

#endif
