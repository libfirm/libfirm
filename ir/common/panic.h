/*
 * Project:     libFIRM
 * File name:   ir/common/panic.h
 * Purpose:
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file panic.h
 *
 * @author Martin Trapp, Christian Schaefer
 */


# ifndef _PANIC_H_
# define _PANIC_H_

/**
 * Prints a panic message to stderr and exits.
 */
void panic (const char *fmt, ...);

# endif /*_PANIC_H_ */
