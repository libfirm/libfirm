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
 * @brief       Code instrumentation and execution count profiling.
 * @author      Adam M. Szalkowski
 * @date        06.04.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BEPROFILE_H
#define FIRM_BE_BEPROFILE_H

#include "irgraph.h"
#include "irnode.h"

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

#endif /* FIRM_BE_BEPROFILE_H */
