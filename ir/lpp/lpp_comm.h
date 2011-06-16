/**
 * @file   lpp_server.h
 * @date   19.07.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _LPP_COMM_H
#define _LPP_COMM_H

#include <stdio.h>
#include <stdarg.h>

#ifdef _MSC_VER

typedef size_t              ssize_t;
typedef unsigned __int16    uint16_t;
typedef unsigned __int32    uint32_t;

/* disable warning: 'foo' was declared deprecated, use 'bla' instead */
/* of course MS had to make 'bla' incompatible to 'foo', so a simple */
/* define will not work :-((( */
#pragma warning( disable : 4996 )

#else /* _MSC_VER */

#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <netinet/in.h>

#endif /* _MSC_VER */



#define LPP_PORT    2175
#define LPP_BUFSIZE (1 << 20)

enum {
#define LPP_CMD(x) LPP_CMD_ ## x,
#include "lpp_cmd.def"
#undef LPP_CMD
  LPP_CMD_LAST
};

#define BASIC_ERR_CHECK(expr,op,cond,fmt,last) \
{ \
  int res; \
  if((res = (expr)) op cond) { \
    fprintf(stderr, "%s(%u): %d = %s(%d): ", \
        __FILE__, (unsigned) __LINE__, res, #expr, cond); \
    lpp_print_err fmt; \
    fprintf(stderr, "\n"); \
    last; \
  } \
}

#define BASIC_ERRNO_CHECK(expr,op,cond,last) \
{ \
  int _basic_errno_check_res = (expr); \
  if(_basic_errno_check_res op cond) { \
    fprintf(stderr, "%s(%u): %d = %s(%d): %s\n", \
        __FILE__, (unsigned) __LINE__, _basic_errno_check_res, #expr, (int) cond, strerror(errno)); \
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

typedef struct _lpp_comm_t lpp_comm_t;

lpp_comm_t *lpp_comm_new(int fd, size_t buf_size);

int lpp_comm_fileno(const lpp_comm_t *comm);

ssize_t lpp_flush(lpp_comm_t *comm);

void lpp_comm_free(lpp_comm_t *comm);

void lpp_print_err(const char *fmt, ...);

void lpp_writel(lpp_comm_t *comm, uint32_t x);

void lpp_writed(lpp_comm_t *comm, double dbl);

void lpp_writes(lpp_comm_t *comm, const char *str);

uint32_t lpp_readl(lpp_comm_t *comm);

int lpp_read_cmd(lpp_comm_t *comm);

double lpp_readd(lpp_comm_t *comm);

char *lpp_reads(lpp_comm_t *comm);

char *lpp_readbuf(lpp_comm_t *comm, char *buf, size_t buflen);

int lpp_ack(lpp_comm_t *comm, char *buf, size_t buflen);

void lpp_send_res(lpp_comm_t *comm, int ok, const char *fmt, ...);

void lpp_send_ack(lpp_comm_t *comm);

const char *lpp_get_cmd_name(int cmd);

#endif /* _LPP_COMM_H */
