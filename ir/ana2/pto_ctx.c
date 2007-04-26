/* -*- c -*- */

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
 * @brief   Manage context-sensitivity markers
 * @author  Florian
 * @date    Sat Nov 13 19:35:27 CET 2004
 * @version $Id$
 */
# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

#include <assert.h>

/*
 pto_ctx: Manage context-sensitivity markers
*/

# include "pto_ctx.h"
# include "pto_debug.h"
# include "pto_comp.h"
# include "ecg.h"

# include "irnode.h"
/* # include "xmalloc.h" */

/* Local Defines: */

/* Local Data Types: */

/* Local Variables: */
static ctx_info_t *curr_ctx = NULL;

/* Local Prototypes: */

/* ===================================================
   Local Implementation:
   =================================================== */


/* ===================================================
   Exported Implementation:
   =================================================== */
/* Find the appropriate ctx for the given call and the given graph */
/* ctx_info_t *find_ctx (ir_node *call, graph_info_t *ginfo, ctx_info_t *curr_ctx) */
int find_ctx_idx (ir_node *call, graph_info_t *ginfo, ctx_info_t *curr_ctx)
{
  int i;
  const int n_ctxs = ginfo->n_ctxs;

  for (i = 0; i < n_ctxs; i ++) {
    ctx_info_t *ctx = ginfo->ctxs [i];

    if ((ctx->enc == curr_ctx) && (ctx->call == call)) {
      return (i);
    }
  }

  fflush (stdout);
  assert (0 && "CTX not found");

  return (-1);
}

/* Get the current ctx */
ctx_info_t *get_curr_ctx (void)
{
  return (curr_ctx);
}

/* Set the current ctx to the given ctx.  Return the old value */
ctx_info_t *set_curr_ctx (ctx_info_t *ctx)
{
  ctx_info_t *old_ctx = curr_ctx;

  curr_ctx = ctx;

  return (old_ctx);
}


/*
  $Log$
  Revision 1.6  2005/12/05 12:19:54  beck
  added missing include <assert.h> (not anymore included in libFirm)

  Revision 1.5  2005/01/14 13:37:55  liekweg
  Insert purpose descr

  Revision 1.4  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.3  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.2  2004/11/20 21:21:35  liekweg
  Add pto_ctx_allocs

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
