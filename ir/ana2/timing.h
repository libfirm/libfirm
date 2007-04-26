/* -*- c -*- */

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

/**
 * @file
 * @brief    generic timing routines
 * @author   Florian
 * @date     Mon 18 Oct 2004
 * @version  $Id$
 */
#ifndef FIRM_ANA2_TIMING_H
#define FIRM_ANA2_TIMING_H

/*
   Data structures
 */
typedef struct timing_env timing_t;

/*
  Protos
*/
timing_t *start_timing (void);
int       end_timing   (timing_t*);

#endif


/*
  $Log$
  Revision 1.2  2004/12/21 15:52:23  beck
  moved struct timing_env to .c file, added config.h

  Revision 1.1  2004/10/29 18:55:52  liekweg
  (mostly) generic timimg


*/
