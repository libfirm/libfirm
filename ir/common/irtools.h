/*
 * Project:     libFIRM
 * File name:   ir/ir/irtools.h
 * Purpose:     Some often needed tool-functions
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _IRTOOLS_H_
#define _IRTOOLS_H_

#include "firm_config.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
lc_opt_entry_t *firm_opt_get_root(void);
#endif


#include "irnode.h"

/**
 * convert an integer into pointer
 */
#define INT_TO_PTR(v)   ((void *)((char *)0 + (v)))

/**
 * convert a pointer into an integer
 */
#define PTR_TO_INT(v)   ((int)((char *)(v) - (char *)0))

/**
 * The famous clear_link() walker-function.
 * Do not implement it by yourself, use this one
 */
void firm_clear_link(ir_node *n, void *env);

#endif /* _IRTOOLS_H_ */
