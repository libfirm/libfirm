/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Universitaet Karlsruhe
 */

/**
 * @file
 * @author Daniel Grund
 */
#ifndef MPS_H
#define MPS_H

#include <stdio.h>
#include "lpp.h"

/**
 * Two styles of mps files
 *
 * s_mps_fixed: mps where spaces are allowed in identifiers
 *              and all things have a fixed column... :-0
 * s_mps_free:  mps where whitespace is a seperator :-)
 */
typedef enum {s_mps_fixed, s_mps_free} lpp_mps_style_t;

/**
 * Writes the description of a lp problem object (lpp)
 * to the stream out, using the specified style.
 */
void mps_write_mps(lpp_t *lpp, lpp_mps_style_t style, FILE *out);

/**
 * Writes the start values of a lp problem object (lpp)
 * to the stream out, using the specified style.
 */
void mps_write_mst(lpp_t *lpp, lpp_mps_style_t style, FILE *out);

#endif
