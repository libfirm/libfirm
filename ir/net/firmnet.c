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
 * @file
 * @brief   TCP/IP handling (Windows and Unix like systems)
 * @author  Christian Wuerdig, implementation copied from liblpp created by Sebastian Hack
 * @date    17.11.2006
 * @version $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "firmnet_t.h"

#ifdef _WIN32
static int winsock_init(void) {
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
#endif /* _WIN32 */

int firmnet_connect_tcp(const char *host, uint16_t port)
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

	if ((phe = gethostbyname(host)))
		memcpy(&sin.sin_addr, phe->h_addr, phe->h_length);
	else if((sin.sin_addr.s_addr = inet_addr(host)) == INADDR_NONE) {
		fprintf(stderr, "cannot get host entry for %s", host);
		return -1;
	}

	ppe = getprotobyname("tcp");
	ERRNO_CHECK_RETURN(s = socket(PF_INET, SOCK_STREAM, ppe->p_proto), <, 0, -1);
	ERRNO_CHECK_RETURN(connect(s, (struct sockaddr *) &sin, sizeof(sin)), <, 0, -1);

	return s;
}

void firmnet_close_socket(int fd) {
#ifdef _WIN32
	closesocket(fd);
#else /* _WIN32 */
	close(fd);
#endif /* _WIN32 */
}

/**
 * Send message of size @p n from buffer @p buf to file descriptor @p fd.
 * @param fd   The file descriptor, the message should be send to.
 * @param buf  The buffer containing the message
 * @param n    The length of the message.
 * @return Number of bytes written or -1 on failure.
 */
ssize_t firmnet_send(int fd, const void *buf, size_t n)
{
	ssize_t    res;
	size_t     bytes_written = 0;
	const char *data = buf;

	do {
		res = send(fd, &data[bytes_written], n - bytes_written, 0);
		if (res < 0) {
			if (errno != EAGAIN)
				return -1;
			continue;
		}

		bytes_written += res;

	} while (bytes_written < n);

	return n;
}

/**
 * Try to read some bytes but block until a certain amount is read.
 * @param fd The file descriptor.
 * @param buf The buffer to read into.
 * @param try The amount of bytes to try to read.
 * @param at_least block until this many bytes are read.
 * @return The number of bytes read or -1 on error.
 */
ssize_t firmnet_recv(int fd, void *buf, size_t try, size_t at_least)
{
	ssize_t res;
	size_t  bytes_read = 0;
	char   *data       = buf;

	do {
		res = recv(fd, &data[bytes_read], try - bytes_read, 0);
		if (res <= 0) {
			if (res == 0 || errno != EAGAIN)
				return -1;
			continue;
		}

		bytes_read += res;

	} while (bytes_read < at_least);

	return bytes_read;
}
