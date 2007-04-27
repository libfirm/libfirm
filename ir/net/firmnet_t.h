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
 * @file    libfirm/ir/net/firmnet_t.h
 * @brief   Some convenience macros for firmnet.c
 * @author  Christian Wuerdig, implementation copied from liblpp created by Sebastian Hack
 * @date    17.11.2006
 * @version $Id$
 */

#ifndef _FIRMNET_T_H_
#define _FIRMNET_T_H_

#include "firmnet.h"

#define BASIC_ERR_CHECK(expr,op,cond,fmt,last) \
{ \
	int res; \
	if((res = (expr)) op cond) { \
	fprintf(stderr, "%s(%d): %d = %s(%d): ", \
	__FILE__, __LINE__, res, #expr, cond); \
	lpp_print_err fmt; \
	fprintf(stderr, "\n"); \
	last; \
	} \
}

#define BASIC_ERRNO_CHECK(expr,op,cond,last) \
{ \
	int _basic_errno_check_res = (expr); \
	if(_basic_errno_check_res op cond) { \
	fprintf(stderr, "%s(%d): %d = %s(%d): %s\n", \
	__FILE__, __LINE__, _basic_errno_check_res, #expr, cond, strerror(errno)); \
	last; \
	} \
}

#define ERR_CHECK_RETURN(expr, op, cond, fmt, retval) \
	BASIC_ERR_CHECK(expr, op, cond, fmt, return retval)

#define ERRNO_CHECK_RETURN(expr, op, cond, retval) \
	BASIC_ERRNO_CHECK(expr, op, cond, return retval)

#define ERR_CHECK_RETURN_VOID(expr, op, cond, fmt) \
	BASIC_ERR_CHECK(expr, op, cond, fmt, return)

#define ERRNO_CHECK_RETURN_VOID(expr, op, cond) \
	BASIC_ERRNO_CHECK(expr, op, cond, return)

#define ERR_CHECK(expr, op, cond, fmt) \
	BASIC_ERR_CHECK(expr, op, cond, fmt, (void) 0)

#define ERRNO_CHECK(expr, op, cond) \
	BASIC_ERRNO_CHECK(expr, op, cond, (void) 0)

#endif /* _FIRMNET_T_H_ */
