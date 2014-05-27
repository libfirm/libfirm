/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   A client for an lpp solving server.
 * @author  Sebastian Hack
 */
#ifdef _WIN32
#include <winsock.h>
#include <io.h>

#else
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/wait.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>

/* solaris fix */
#ifndef INADDR_NONE
#define INADDR_NONE (in_addr_t)(-1)
#endif

#endif


#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "timing.h"

#include "lpp_net.h"
#include "lpp_t.h"
#include "lpp_comm.h"
#include "xmalloc.h"

#ifdef _WIN32
static int winsock_init(void)
{
	WORD wVersionRequested;
	WSADATA wsaData;
	int err;

	wVersionRequested = MAKEWORD( 2, 2 );

	err = WSAStartup( wVersionRequested, &wsaData );
	if ( err != 0 ) {
		/* Tell the user that we could not find a usable */
		/* WinSock DLL.                                  */
		return 0;
	}

	/* Confirm that the WinSock DLL supports 2.2.*/
	/* Note that if the DLL supports versions greater    */
	/* than 2.2 in addition to 2.2, it will still return */
	/* 2.2 in wVersion since that is the version we      */
	/* requested.                                        */

	if ( LOBYTE( wsaData.wVersion ) != 2 ||
			HIBYTE( wsaData.wVersion ) != 2 ) {
		/* Tell the user that we could not find a usable */
		/* WinSock DLL.                                  */
		WSACleanup( );
		return 0;
	}
	return 1;
}
#endif

static int connect_tcp(const char *host, uint16_t port)
{
	struct hostent     *phe;
	struct protoent    *ppe;
	struct sockaddr_in sin;
	int s;

#ifdef _WIN32
	winsock_init();
#endif

	memset(&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port   = htons(port);

	if ((phe = gethostbyname(host))) {
		memcpy(&sin.sin_addr, phe->h_addr_list[0], phe->h_length);
	} else if ((sin.sin_addr.s_addr = inet_addr(host)) == INADDR_NONE) {
		lpp_print_err("cannot get host entry for %s", host);
		return -1;
	}

	ppe = getprotobyname("tcp");
	ERRNO_CHECK_RETURN(s = socket(PF_INET, SOCK_STREAM, ppe->p_proto), <, 0, -1);
	ERRNO_CHECK_RETURN(connect(s, (struct sockaddr *) &sin, sizeof(sin)), <, 0, -1);

	return s;
}

char **lpp_get_solvers(const char *host)
{
	int fd, n;
	char **res = NULL;
	lpp_comm_t *comm;

	ERR_CHECK_RETURN(fd = connect_tcp(host, LPP_PORT), <, 0,
			("could not connect to %s", host), NULL);

	comm = lpp_comm_new(fd, LPP_BUFSIZE);

	lpp_writel(comm, LPP_CMD_SOLVERS);
	lpp_flush(comm);
	n = lpp_readl(comm);
	res = XMALLOCN(char*, n+1);
	res[n] = NULL;

	if(n > 0) {
		int i;
		for(i = 0; i < n; ++i)
			res[i] = lpp_reads(comm);
	}

	lpp_writel(comm, LPP_CMD_BYE);
	lpp_flush(comm);
	lpp_comm_free(comm);
	close(fd);
	return res;
}

void lpp_set_dbg(const char *host, int mask)
{
	int fd;
	lpp_comm_t *comm;

	ERR_CHECK_RETURN_VOID(fd = connect_tcp(host, LPP_PORT), <, 0, ("could not connect to %s", host));

	comm = lpp_comm_new(fd, LPP_BUFSIZE);

	lpp_writel(comm, LPP_CMD_SET_DEBUG);
	lpp_writel(comm, mask);
	lpp_flush(comm);
	lpp_writel(comm, LPP_CMD_BYE);
	lpp_flush(comm);
	lpp_comm_free(comm);
	close(fd);
}

void lpp_solve_net(lpp_t *lpp, const char *host, const char *solver)
{
	char buf[1024];
	int n, fd, ready;
	lpp_comm_t *comm;
	ir_timer_t *t_send, *t_recv;

	ERR_CHECK_RETURN_VOID(fd = connect_tcp(host, LPP_PORT), <, 0,
		("could not connect to %s", host));

	comm = lpp_comm_new(fd, LPP_BUFSIZE);

	/* Set the solver */
	lpp_writel(comm, LPP_CMD_SOLVER);
	lpp_writes(comm, solver);
	lpp_flush(comm);

	t_send = ir_timer_new();
	t_recv = ir_timer_new();

	ir_timer_start(t_send);
	lpp_writel(comm, LPP_CMD_PROBLEM);
	lpp_serialize(comm, lpp, 1);
	lpp_serialize_values(comm, lpp, lpp_value_start);
	lpp_flush(comm);
	ir_timer_stop(t_send);
	lpp->send_time = ir_timer_elapsed_usec(t_send);

	ready = 0;
	while (! ready) {
		int cmd = lpp_readl(comm);
		switch (cmd) {
			case LPP_CMD_SOLUTION:
				ir_timer_push(t_recv);
				lpp_deserialize_stats(comm, lpp);
				lpp_deserialize_values(comm, lpp, lpp_value_solution);
				ir_timer_stop(t_recv);
				lpp->recv_time = ir_timer_elapsed_usec(t_recv);
				ready = 1;
				break;
			case LPP_CMD_INFO:
				lpp_readbuf(comm, buf, sizeof(buf));
				buf[sizeof(buf) - 1] = '\0';

				if(lpp->log != NULL) {
					fputs(buf, lpp->log);
					n = strlen(buf);
					if(buf[n - 1] != '\n')
						putc('\n', lpp->log);
					fflush(lpp->log);
				}
				break;
			case LPP_CMD_BAD:
				fprintf(stderr, "solver process died unexpectedly\n");
				goto end;
			default:
				fprintf(stderr, "invalid command: %s(%d)\n", lpp_get_cmd_name(cmd), cmd);
				return;
		}
	}

	lpp_writel(comm, LPP_CMD_BYE);
	lpp_flush(comm);

end:
	lpp_comm_free(comm);
#ifdef _WIN32
	closesocket(fd);
#else
	close(fd);
#endif
}
