/*
 * Project:     libFIRM
 * File name:   ir/debug/firm_ycomp.h
 * Purpose:     Connect firm to ycomp
 * Author:      Christian Wuerdig
 * Modified by:
 * Created:     16.11.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_YCOMP_H_
#define _FIRM_YCOMP_H_

#define FIRM_YCOMP_DEFAULT_HOST "localhost"
#define FIRM_YCOMP_DEFAULT_PORT 4242

/**
 * Establish connection to yComp and register debugger hooks.
 * @param host Hostname where yComp is running
 * @param port Port on which yComp is listening
 */
void firm_init_ycomp_debugger(const char *host, unsigned port);

/**
 * Close connection to yComp and unregister debugger hooks.
 */
void firm_finish_ycomp_debugger(void);

#endif /* _FIRM_YCOMP_H_ */
