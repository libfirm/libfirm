/*
 * Project:     libFIRM
 * File name:   ir/stat/pattern.h
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _PATTERN_H_
#define _PATTERN_H_

/**
 * @file pattern.h
 *
 * Statistics for libFirm, pattern history.
 */

/**
 * calculates the pattern history.
 *
 * @param irg    The IR-graph
 */
void stat_calc_pattern_history(ir_graph *irg);

/**
 * initialises the pattern history.
 *
 * @param enable  Enable flag.
 */
void stat_init_pattern_history(int enable);

/**
 * finishes the pattern history
 */
void stat_finish_pattern_history(void);

#endif /* _PATTERN_H_ */
