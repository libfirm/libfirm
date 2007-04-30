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
 * @brief   Interfaces for TCP/IP handling (Windows and Unix like systems)
 * @author  Christian Wuerdig, copied from liblpp created by Sebastian Hack
 * @date    17.11.2006
 * @version $Id$
 */

#ifndef FIRM_NET_FIRMNET_H
#define FIRM_NET_FIRMNET_H

#ifdef _WIN32
#include <winsock.h>
#include <io.h>

#else /* _WIN32 */
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/wait.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <unistd.h>
#endif /* _WIN32 */

#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _MSC_VER

typedef size_t				ssize_t;
typedef unsigned __int16	uint16_t;
typedef unsigned __int32	uint32_t;

#else /* _MSC_VER */

#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <netinet/in.h>

#endif /* _MSC_VER */

/**
 * Establishes a TCP/IP connection to @p host at port @p port.
 * @param host Hostname to connect to
 * @param port Port number
 * @return The file descriptor on success, -1 otherwise
 */
int firmnet_connect_tcp(const char *host, uint16_t port);

/**
 * Closes connection established on socket @p fd.
 * @param fd The file descriptor identifying the connection
 */
void firmnet_close_socket(int fd);

/**
 * Send message of size @p n from buffer @p buf to file descriptor @p fd.
 * @param fd   The file descriptor, the message should be send to.
 * @param buf  The buffer containing the message
 * @param n    The length of the message.
 * @return Number of bytes written or -1 on failure.
 */
ssize_t firmnet_send(int fd, const void *buf, size_t n);

/**
 * Try to read some bytes but block until a certain amount is read.
 * @param fd The file descriptor.
 * @param buf The buffer to read into.
 * @param try The amount of bytes to try to read.
 * @param at_least block until this many bytes are read.
 * @return The number of bytes read or -1 on error.
 */
ssize_t firmnet_recv(int fd, void *buf, size_t try, size_t at_least);

#endif
