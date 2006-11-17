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
