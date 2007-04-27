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
 * File name:   ir/ir/irprintf_t.h
 * Purpose:     A little printf understanding some firm types.
 * Author:      Sebastian Hack
 * Created:     29.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
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
