/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 */
#ifndef LPP_LPP_COMM_H
#define LPP_LPP_COMM_H

#include <stdio.h>
#include <stdint.h>

#define LPP_PORT    2175
#define LPP_BUFSIZE (1 << 20)

enum {
	LPP_CMD_BAD,
	LPP_CMD_OK,
	LPP_CMD_PROBLEM,
	LPP_CMD_SOLUTION,
	LPP_CMD_SOLVER,
	LPP_CMD_BYE,
	LPP_CMD_SOLVERS,
	LPP_CMD_SET_DEBUG,
	LPP_CMD_INFO,
	LPP_CMD_LAST
};

#define BASIC_ERR_CHECK(expr,op,cond,fmt,last) \
do { \
  int err_check_res; \
  if((err_check_res = (expr)) op cond) { \
    fprintf(stderr, "%s(%u): %d = %s(%d): ", \
        __FILE__, (unsigned) __LINE__, err_check_res, #expr, cond); \
    lpp_print_err fmt; \
    fprintf(stderr, "\n"); \
    last; \
  } \
} while(0)

#define BASIC_ERRNO_CHECK(expr,op,cond,last) \
do { \
  int _basic_errno_check_res = (expr); \
  if(_basic_errno_check_res op cond) { \
    fprintf(stderr, "%s(%u): %d = %s(%d): %s\n", \
        __FILE__, (unsigned) __LINE__, _basic_errno_check_res, #expr, (int) cond, strerror(errno)); \
    last; \
  } \
} while(0)

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

void lpp_flush(lpp_comm_t *comm);

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

#endif
