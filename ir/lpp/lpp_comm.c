/**
 * @file   lpp_comm.c
 * @date   21.07.2005
 * @author Sebastian Hack
 *
 * Protocol stuff for lpp server
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock2.h>

#define vsnprintf _vsnprintf

#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#endif

#include "irtools.h"
#include "debug.h"

#include "lpp_comm.h"


/* undef to disable buffering socket i/o */
#define ENABLE_BUFFERING

/* undef to disable debugging */
#undef  ENABLE_DEBUGGING

struct _lpp_comm_t {
	int fd;
	size_t buf_size;
	char *w_pos;
	char *r_pos;
	char *r_max;
	char *w_buf;
	char *r_buf;
};

static firm_dbg_module_t *dbg = NULL;

/**
 * Try to read some bytes but block until a certain amount is read.
 * @param fd The file descriptor.
 * @param buf The buffer to read into.
 * @param try The amount of bytes to try to read.
 * @param at_least block until this many bytes are read.
 * @return The number of bytes read or -1 on error.
 */
static ssize_t secure_recv(int fd, void *buf, size_t try, size_t at_least)
{
  ssize_t res;
  size_t bytes_read = 0;
  char *data = buf;

  do {
    res = recv(fd, &data[bytes_read], try - bytes_read, 0);
    if(res <= 0) {
      if(res == 0 || errno != EAGAIN)
        return -1;
      continue;
    }

    bytes_read += res;

  } while(bytes_read < at_least);

  return bytes_read;
}

static ssize_t secure_send(int fd, const void *buf, size_t n)
{
  ssize_t res;
  size_t bytes_written = 0;
  const char *data = buf;

  do {
    res = send(fd, &data[bytes_written], n - bytes_written, 0);
    if(res < 0) {
      if(errno != EAGAIN)
        return -1;
      continue;
    }

    bytes_written += res;

  } while(bytes_written < n);

  return n;
}

#ifdef ENABLE_BUFFERING
ssize_t lpp_flush(lpp_comm_t *comm)
{
	ssize_t res = 0;
	if(comm->w_pos - comm->w_buf > 0) {
		DBG((dbg, LEVEL_1, "flushing %d bytes\n", comm->w_pos - comm->w_buf));
		res = secure_send(comm->fd, comm->w_buf, comm->w_pos - comm->w_buf);
		if(res < 0)
			return res;

		comm->w_pos = comm->w_buf;
	}
	return res;
}

static ssize_t lpp_write(lpp_comm_t *comm, const void *buf, size_t len)
{
	assert(comm->w_pos - comm->w_buf >= 0);

	DBG((dbg, LEVEL_1, "write of length %d\n", len));
	if(len > 0) {
		size_t free = (comm->w_buf + comm->buf_size) - comm->w_pos;
		size_t copy = MIN(free, len);
		size_t rest = len - copy;
		const char *pos   = buf;

		DBG((dbg, LEVEL_1, "\tfree = %d, copy = %d, rest = %d\n", free, copy, rest));
		if(copy > 0) {
			memcpy(comm->w_pos, pos, copy);
			comm->w_pos += copy;
			pos         += copy;
		}

		/*
		 * Not everything in buf fits into the buffer,
		 * so flush the buffer and write the rest.
		 */
		if(rest > 0) {
			size_t i;
			size_t n_direct = rest / comm->buf_size;
			size_t last_rest;

			if(lpp_flush(comm) < 0)
				return -1;

			for(i = 0; i < n_direct; ++i) {
				if(secure_send(comm->fd, pos, comm->buf_size) < 0)
					return -1;

				pos += comm->buf_size;
			}

			last_rest = ((const char *) buf + len) - pos;

			if(last_rest > 0) {
				assert(last_rest < comm->buf_size);
				assert(comm->w_pos == comm->w_buf);
				memcpy(comm->w_pos, pos, last_rest);
				comm->w_pos += last_rest;
			}
		}
	}

	return len;
}

static ssize_t lpp_read(lpp_comm_t *comm, void *buf, size_t len)
{
	DBG((dbg, LEVEL_1, "read of length %d\n", len));
	if(len > 0) {
		size_t left = comm->r_max - comm->r_pos;
		size_t copy = MIN(left, len);
		size_t rest = len - copy;
		char *pos   = buf;

		DBG((dbg, LEVEL_1, "\tleft = %d, copy = %d, rest = %d\n", left, copy, rest));
		if(copy > 0) {
			memcpy(pos, comm->r_pos, copy);
			pos         += copy;
			comm->r_pos += copy;
		}

		/* We want to read more than the buffer can provide. */
		if(rest > 0) {
			size_t bs = comm->buf_size;
			size_t n_direct = rest / comm->buf_size;
			size_t i;
			size_t last_rest;

			/*
			 * The buffer is now completely read, so
			 * reset the pointers.
			 */
			comm->r_pos = comm->r_buf;
			comm->r_max = comm->r_buf;

			for(i = 0; i < n_direct; ++i) {
				if(secure_recv(comm->fd, pos, bs, bs) < 0)
					return -1;

				pos += comm->buf_size;
			}

			last_rest = ((const char *) buf + len) - pos;

			if(last_rest > 0) {
				ssize_t bytes_read = 0;

				assert(last_rest < comm->buf_size);
				assert(comm->r_pos == comm->r_buf);

				bytes_read = secure_recv(comm->fd, comm->r_buf, bs, last_rest);
				if(bytes_read < 0)
					return -1;

				memcpy(pos, comm->r_buf, last_rest);
				comm->r_pos = comm->r_buf + last_rest;
				comm->r_max = comm->r_buf + bytes_read;
			}
		}
	}

	return len;
}

#else /* ENABLE_BUFFERING */
ssize_t lpp_flush(lpp_comm_t *comm)
{
	return 0;
}

static ssize_t lpp_write(lpp_comm_t *comm, const void *buf, size_t len)
{
	return secure_send(comm->fd, buf, len);
}

static ssize_t lpp_read(lpp_comm_t *comm, void *buf, size_t len)
{
	return secure_recv(comm->fd, buf, len, len);
}
#endif

lpp_comm_t *lpp_comm_new(int fd, size_t buf_size)
{
	lpp_comm_t *res = malloc(sizeof(res[0]));

	res->fd       = fd;
	res->w_buf    = malloc(buf_size);
	res->w_pos    = res->w_buf;
	res->r_buf    = malloc(buf_size);
	res->r_pos    = res->r_buf;
	res->r_max    = res->r_buf;
	res->buf_size = buf_size;

	return res;
}

int lpp_comm_fileno(const lpp_comm_t *comm)
{
	return comm->fd;
}

void lpp_comm_free(lpp_comm_t *comm)
{
	free(comm->w_buf);
	free(comm->r_buf);
	free(comm);
}

void lpp_print_err(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

void lpp_writel(lpp_comm_t *comm, uint32_t x)
{
  x = htonl(x);
  ERRNO_CHECK(lpp_write(comm, &x, sizeof(x)), !=, sizeof(x));
}

void lpp_writed(lpp_comm_t *comm, double dbl)
{
  ERRNO_CHECK(lpp_write(comm, &dbl, sizeof(dbl)), !=, sizeof(dbl));
}

void lpp_writes(lpp_comm_t *comm, const char *str)
{
  size_t n = strlen(str);
  lpp_writel(comm, n);
  ERRNO_CHECK(lpp_write(comm, str, n), !=, (ssize_t) n);
}

uint32_t lpp_readl(lpp_comm_t *comm)
{
  uint32_t res;

  ERRNO_CHECK(lpp_read(comm, &res, sizeof(res)), !=, sizeof(res));
  return ntohl(res);
}

int lpp_read_cmd(lpp_comm_t *comm)
{
	uint32_t res = 0;
	int retval;

	for(;;) {
		retval = recv(comm->fd, (char *)&res, sizeof(res), 0);
		if(retval < 0) {
			if(errno != EAGAIN)
				return -1;
		}

		else
			break;
	}

	return (int) ntohl(res);
}

double lpp_readd(lpp_comm_t *comm)
{
  double res;
  ERRNO_CHECK(lpp_read(comm, &res, sizeof(res)), !=, sizeof(res));
  return res;
}

char *lpp_reads(lpp_comm_t *comm)
{
  size_t len = lpp_readl(comm);
  char *res = malloc(sizeof(char) * (len + 1));

  ERRNO_CHECK(lpp_read(comm, res, len), !=, (ssize_t) len);
  res[len] = '\0';
  return res;
}

char *lpp_readbuf(lpp_comm_t *comm, char *buf, size_t buflen)
{
  char dummy[1024];
  size_t i;
  size_t n         = buflen - 1;
  size_t len       = lpp_readl(comm);
  size_t max_read  = n < len ? n : len;
  size_t rest      = len - max_read;

  if(buflen > 0 && buf != NULL) {
    ERRNO_CHECK(lpp_read(comm, buf, max_read), !=, (ssize_t) max_read);
    buf[max_read] = '\0';
  }
  else
    rest = len;

  /* eat up data that didnt fit into the string */
  for(i = 0, n = rest / sizeof(dummy); i < n; ++i)
    ERRNO_CHECK(lpp_read(comm, dummy, sizeof(dummy)), !=, sizeof(dummy));

  if(rest % sizeof(dummy) > 0)
    ERRNO_CHECK(lpp_read(comm, dummy, rest % sizeof(dummy)), !=,
	            (ssize_t) (rest % sizeof(dummy)) );

  return buf;
}

int lpp_ack(lpp_comm_t *comm, char *buf, size_t buflen)
{
  int res = 0;
  int cmd = lpp_readl(comm);

  switch(cmd) {
    case LPP_CMD_OK:
      res = 1;
      break;
    case LPP_CMD_BAD:
      lpp_readbuf(comm, buf, buflen);
    default:
      res = 0;
  }

  return res;
}

void lpp_send_res(lpp_comm_t *comm, int ok, const char *fmt, ...)
{
  if(!ok) {
    char buf[1024];
    va_list args;

    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    lpp_writel(comm, LPP_CMD_BAD);
    lpp_writes(comm, buf);
  }

  else
    lpp_writel(comm, LPP_CMD_OK);
}

void lpp_send_ack(lpp_comm_t *comm)
{
  lpp_send_res(comm, 1, "");
}

const char *lpp_get_cmd_name(int cmd)
{
  switch(cmd) {
#define LPP_CMD(x) case LPP_CMD_ ## x: return #x;
#include "lpp_cmd.def"
#undef LPP_CMD
  }

  return "<unknown>";
}
