/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Universitaet Karlsruhe
 */

#ifndef LPP_SERVER_H
#define LPP_SERVER_H

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <netinet/in.h>

enum {
#define LPP_CMD(x) x,
#include "lpp_cmd.def"
#undef LPP_CMD
  LPP_CMD_LAST
};

#define BASIC_ERR_CHECK(expr,cond,fmt,last) \
  if((expr) cond) { \
    fprintf(stderr, "%s(%d): %s %s: ", __FILE__, __LINE__, #expr, #cond); \
    print_err fmt; \
    fprintf(stderr, "\n"); \
    last; \
  }

#define BASIC_ERRNO_CHECK(expr,cond,last) \
  if((expr) cond) { \
    fprintf(stderr, "%s(%d): %s %s: %s\n", \
        __FILE__, __LINE__, #expr, #cond, strerror(errno)); \
    last; \
  }

#define ERR_CHECK_RETURN(expr, cond, fmt, retval) \
  BASIC_ERR_CHECK(expr, cond, fmt, return retval)

#define ERRNO_CHECK_RETURN(expr, cond, retval) \
  BASIC_ERRNO_CHECK(expr, cond, return retval)

#define ERR_CHECK_RETURN_VOID(expr, cond, fmt) \
  BASIC_ERR_CHECK(expr, cond, fmt, return)

#define ERRNO_CHECK_RETURN_VOID(expr, cond) \
  BASIC_ERRNO_CHECK(expr, cond, return)

#define ERR_CHECK(expr, cond, fmt) \
  BASIC_ERR_CHECK(expr, cond, fmt, (void) 0)

#define ERRNO_CHECK(expr, cond) \
  BASIC_ERRNO_CHECK(expr, cond, (void) 0)

static void print_err(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
}

static  void writel(int fd, uint32_t x)
{
	x = htonl(x);
	ERRNO_CHECK(write(fd, &x, sizeof(x)), == -1);
}

static  void writed(int fd, double dbl)
{
	ERRNO_CHECK(write(fd, &dbl, sizeof(dbl)), == -1);
}

static  void writes(int fd, const char *str)
{
	size_t n = strlen(str);
	writel(fd, n);
	ERRNO_CHECK(write(fd, str, n), == -1);
}

static  uint32_t readl(int fd)
{
	uint32_t res;

	ERRNO_CHECK(read(fd, &res, sizeof(res)), == -1);
	return ntohl(res);
}

static  double readd(int fd)
{
	double res;
	ERRNO_CHECK(read(fd, &res, sizeof(res)), == -1);
	return res;
}

static  char *reads(int fd)
{
	size_t len = readl(fd);
	char *res = malloc(sizeof(char) * (len + 1));

	ERRNO_CHECK(read(fd, res, len), == -1);
	res[len] = '\0';
	return res;
}

static char *readbuf(int fd, size_t buflen, char *buf)
{
	char dummy[1024];
	size_t i;
	size_t n         = buflen - 1;
	size_t len       = readl(fd);
	size_t max_read  = MIN(n, len);
	size_t rest      = len - max_read;

	if(buflen > 0 && buf != NULL) {
		ERRNO_CHECK(read(fd, buf, max_read), == -1);
		buf[max_read] = '\0';
	}

	/* eat up data that didnt fit into the string */
	for(i = 0, n = rest / sizeof(dummy); i < n; ++i)
		read(fd, dummy, sizeof(dummy));

	if(rest % sizeof(dummy) > 0)
		read(fd, dummy, rest % sizeof(dummy));

	return buf;
}

static int ack(int fd, size_t buflen, char *buf)
{
	int res = 0;
	int cmd = readl(fd);

	switch(cmd) {
		case LPP_CMD_OK:
			res = 1;
			break;
		case LPP_CMD_BAD:
			readbuf(fd, buflen, buf);
		default:
			res = 0;
	}

	return res;
}

static void send_res(int fd, int ok, const char *fmt, ...)
{
	if(!ok) {
		char buf[1024];
		va_list args;

		va_start(args, fmt);
		vsnprintf(buf, sizeof(buf), fmt, args);
		va_end(args);

		writel(fd, LPP_CMD_BAD);
		writes(fd, buf);
	}

	else
		writel(fd, LPP_CMD_OK);
}

static void send_ack(int fd)
{
	send_res(fd, 1, "");
}

#endif
