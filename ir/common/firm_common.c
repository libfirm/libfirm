/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author    Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "irloop.h"
#include "tv.h"

/**
 * Ideally, this macro would check if size bytes could be read at
 * pointer p. No generic solution.
 */
#define POINTER_READ(p, size) (p)

/* returns the kind of the thing */
firm_kind get_kind(const void *firm_thing)
{
	return POINTER_READ(firm_thing, sizeof(firm_kind)) ? *(firm_kind *)firm_thing : k_BAD;
}
