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

#include "irnode.h"

/**
 * The famous clear_link() walker-function.
 * Do not implement it by yourself, use this one
 */
void firm_clear_link(ir_node *n, void *env);

#endif /* _IRTOOLS_H_ */
