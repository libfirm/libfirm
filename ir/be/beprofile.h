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
 * Instruments irgs with profile code
 * @return The irg doing the profile initialization.
 */
ir_graph * be_profile_instrument(void);

/**
 * Reads the corresponding profile info file if it exists and returns a
 * profile info struct
 */
void be_profile_read(char * filename);

/**
 * Frees the profile info
 */
void be_profile_free();

/**
 * Get block execution count as determined be profiling
 */
unsigned int be_profile_get_block_execcount(const ir_node * block);

#endif /* _BEPROFILE_H_ */
