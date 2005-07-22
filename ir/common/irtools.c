/*
 * Project:     libFIRM
 * File name:   ir/ir/irtools.c
 * Purpose:     Some often needed tool-functions
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdlib.h>
#include "irnode_t.h"
#include "irtools.h"

/* the famous clear_link implementation. */
void firm_clear_link(ir_node *n, void *env) {
  set_irn_link(n, NULL);
}
