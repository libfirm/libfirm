/*
 * Project:     libFIRM
 * File name:   ir/ir/irprintf_t.h
 * Purpose:     A little printf understanding some firm types.
 * Author:      Sebastian Hack
 * Created:     29.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _IRPRINTF_T_H
#define _IRPRINTF_T_H

#include "firm_config.h"
#include "irprintf.h"

#ifdef DEBUG_libfirm

#define ir_debugf    ir_printf
#define ir_fdebugf   ir_fprintf

#else

static INLINE void ir_debugf(const char *fmt, ...)
{
}

static INLINE void ir_fdebugf(FILE *f, const char *fmt, ...)
{
}

#endif


#endif
