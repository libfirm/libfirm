/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Code instrumentation and execution count profiling.
 * @author      Adam M. Szalkowski
 * @date        06.04.2006
 */
#ifndef FIRM_BE_BEPROFILE_H
#define FIRM_BE_BEPROFILE_H

#include <stdbool.h>
#include <stdint.h>

#include "firm_types.h"

/**
 * Instruments all irgs in the program with profile code.
 * The final code will have a counter for each basic block which is
 * incremented in that block. After the program has run the info is written
 * to @p filename.
 */
ir_graph *ir_profile_instrument(const char *filename);

/**
 * Reads the corresponding profile info file if it exists and returns a
 * profile info struct
 * @param filename The name of the file containing profile information
 */
bool ir_profile_read(const char *filename);

/**
 * Frees the profile info
 */
void ir_profile_free(void);

/**
 * Get block execution count as determined be profiling
 */
uint32_t ir_profile_get_block_execcount(const ir_node *block);

/**
 * Initializes exec_freq structure for an irg based on profile data
 */
void ir_create_execfreqs_from_profile(void);

#endif
