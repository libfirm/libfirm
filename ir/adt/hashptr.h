/*
 * Project:     libFIRM
 * File name:   ir/adt/hashptr.h
 * Purpose:     Hash function for pointers
 * Author:      Michael Beck, Sebastian Hack
 * Modified by:
 * Created:     2004
 * CVS-ID:      $Id$
 * Copyright:   (C) 2004 University of Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef __HASHPTR_H__
#define __HASHPTR_H__

/**
 * hash a pointer value: Pointer addresses are mostly aligned to 4
 * or 8 bytes. So we remove the lowest 3 bits
 */
#define HASHPTR(ptr)    (((char *)ptr - (char *)0) >> 3)

#endif /* __HASHPTR_H__ */
