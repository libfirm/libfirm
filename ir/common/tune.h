/*
 * Project:     libFIRM
 * File name:   ir/common/tune.h
 * Purpose:     Tune --- tunable parameters.
 * Author:      Markus Armbruster & Christian von Roques
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster & Christian von Roques
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _TUNE_H
#define _TUNE_H

/**
 @file tune.h

 Tunable parameters.

 @author Markus Armbruster & Christian von Roques

 From the jargon file:

   :tune: vt.  [from automotive or musical usage] To optimize a
      program or system for a particular environment, esp. by adjusting
      numerical parameters designed as {hook}s for tuning, e.g., by
      changing `#define' lines in C.  One may `tune for time'
      (fastest execution), `tune for space' (least memory use), or
      `tune for configuration' (most efficient use of hardware).  See
      {bum}, {hot spot}, {hand-hacking}.

 */


/** Suggested minimal block size for buffered I/O */
#define TUNE_SMALL_IOBUF_SIZE 512

/** Size of pdeq block cache */
#define TUNE_NSAVED_PDEQS 16


/* The following numbers are only estimates.  If the input exceeds
   them, the program dynamically enlarges data structures.  However,
   larger values generally mean slower startup.  */


/** Expected number of distinct identifiers */
#define TUNE_NIDENTS 1024

/** Expected number of classes */
#define TUNE_NCLASSES 128

/** Expected number of class types */
#define TUNE_NCTYPES 128

/** Expected number of routine, stream and stream object types */
#define TUNE_NMTYPES 512

/** Expected number of method families */
#define TUNE_NMFAMILIES (TUNE_NCTYPES * 16)

/** Expected number of Intermediate Representation nodes for a method */
#define TUNE_NIR_NODES 512

/** Expected number of distinct constant target values */
#define TUNE_NCONSTANTS	2048

/** notify configuration manager to prefetch sources */
#define TUNE_PREFETCH

#endif
