/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file panic.h
 *
 * @author Martin Trapp, Christian Schaefer
 */

/* $Id$ */

# ifndef _PANIC_H_
# define _PANIC_H_

/**
 * Prints a panic message to stderr and exits.
 */
void panic (const char *fmt, ...);

# endif /*_PANIC_H_ */
