/** vim: set sw=4 ts=4:
 * @file   beprofile.h
 * @date   2006-04-06
 * @author Adam M. Szalkowski
 * @cvs-id $Id$
 *
 * Code instrumentation and execution count profiling
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BEPROFILE_H_
#define _BEPROFILE_H_

/**
 * Instruments irgs with profile code.
 * @return The irg doing the profile initialization.
 */
ir_graph *be_profile_instrument(void);

void be_profile_read(void);

#endif /* _BEPROFILE_H_ */
