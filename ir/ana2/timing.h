/* -*- c -*- */

/*
 * Time-stamp: <26.10.2004 11:56:23h liekweg>
 * Project:     libFIRM
 * File name:   ir/ana2/timing.h
 * Purpose:     generic timing routines
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _TIMING_H_
#define _TIMING_H_

/*
   Data structures
 */
typedef struct timing_env timing_t;

/*
  Protos
*/
timing_t *start_timing (void);
int       end_timing   (timing_t*);

#endif /* def _TIMING_H_ */


/*
  $Log$
  Revision 1.2  2004/12/21 15:52:23  beck
  moved struct timing_env to .c file, added config.h

  Revision 1.1  2004/10/29 18:55:52  liekweg
  (mostly) generic timimg


*/
