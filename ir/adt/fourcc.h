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


/* define an always readable fourcc code */
#define FOURCC(a,b,c,d)         ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))

#endif /* _FOURCC_H */
