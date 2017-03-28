/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date   28.03.2017
 * @author Abel Lucas
 * @brief Adding support to bool in 2013 and older MSVC compilers.
 */
#ifndef FIRM_ADT_BOOL_H
#define FIRM_ADT_BOOL_H

#include "../begin.h"

#if defined(_MSC_VER) && _MSC_VER < 1900
# define bool	unsigned char
# define true	1
# define false	0
# define __bool_true_false_are_defined	1
#else
# include <stdbool.h>
#endif

#include "../end.h"

#endif
