/*
 * Project:     libFIRM
 * File name:   ir/adt/fourcc.h
 * Purpose:     define the famous infame FOURCC macro.
 * Author:
 * Modified by:
 * Created:     02.01.2004
 * CVS-ID:      $Id$
 * Copyright:   (C) 2004 University of Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FOURCC_H
#define _FOURCC_H

#include "firm_config.h"

/* define an always readable fourcc code */
#ifdef WORDS_BIGENDIAN
#define FOURCC(a,b,c,d)         ((d) | ((c) << 8) | ((b) << 16) | ((a) << 24))
#else
#define FOURCC(a,b,c,d)         ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))
#endif

#endif /* _FOURCC_H */
