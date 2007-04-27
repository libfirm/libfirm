/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/*
 * Project:     libFIRM
 * File name:   ir/stat/pattern.h
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 */
#ifndef _PATTERN_H_
#define _PATTERN_H_

/**
 * @file pattern.h
 *
 * Statistics for libFirm, pattern history.
 */

/**
 * Calculates the pattern history.
 *
 * @param irg    The IR-graph
 */
void stat_calc_pattern_history(ir_graph *irg);

/**
 * Initializes the pattern history.
 *
 * @param enable  Enable flag.
 */
void stat_init_pattern_history(int enable);

/**
 * Finish the pattern history.
 */
void stat_finish_pattern_history(const char *fname);

#endif /* _PATTERN_H_ */
