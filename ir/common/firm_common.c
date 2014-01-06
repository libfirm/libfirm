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

/* returns the kind of the thing */
firm_kind get_kind(const void *firm_thing)
{
	return *(firm_kind*)firm_thing;
}
