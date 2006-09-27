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

/** Additional flags for profiling */
enum profile_flags {
	profile_with_locations = 0x0001,   /**< create location table */
	profile_default        = 0         /**< default settings */
};

/**
 * Instruments irgs with profile code
 *
 * @param filename  The name of the output file for the profile information
 * @param flags     Additional flags
 *
 * @return The irg doing the profile initialization.
 */
ir_graph *be_profile_instrument(const char *filename, unsigned flags);

/**
 * Reads the corresponding profile info file if it exists and returns a
 * profile info struct
 * @param filename The name of the file containing profile information
 */
void be_profile_read(const char *filename);

/**
 * Frees the profile info
 */
void be_profile_free(void);

/**
 * Get block execution count as determined be profiling
 */
unsigned int be_profile_get_block_execcount(const ir_node * block);

/**
 * Initializes exec_freq structure for an irg based on profile data
 */
ir_exec_freq *be_create_execfreqs_from_profile(ir_graph *irg);

/**
 * Tells whether profile module has acquired data
 */
int be_profile_has_data(void);

#endif /* _BEPROFILE_H_ */
