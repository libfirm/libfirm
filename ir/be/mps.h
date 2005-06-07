/**
 * Author:      Daniel Grund
 * Date:		02.06.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _MPS_H_
#define _MPS_H_

#include <stdio.h>
#include "lpp.h"

/**
 * Two styles of mps files
 *
 * s_mps_fixed: mps where spaces are allowed in identifiers
 * 				and all things have a fixed column... :-0
 * s_mps_free:  mps where whitespace is a seperator :-)
 */
typedef enum _style_t {s_mps_fixed, s_mps_free} style_t;

/**
 * Writes the description of a lp problem object (lpp)
 * to the stream out, using the specified style.
 */
void mps_write_mps(lpp_t *lpp, style_t style, FILE *out);

/**
 * Writes the start values of a lp problem object (lpp)
 * to the stream out, using the specified style.
 */
void mps_write_mst(lpp_t *lpp, style_t style, FILE *out);

#endif /*_MPS_H_*/
