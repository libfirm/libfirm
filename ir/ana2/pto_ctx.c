/* -*- c -*- */

/*
   Project:     libFIRM
   File name:   ir/ana/pto_ctx.c
   Purpose:     ...
   Author:      Florian
   Modified by:
   Created:     Sat Nov 13 19:35:27 CET 2004
   CVS-ID:      $Id$
   Copyright:   (c) 1999-2004 Universität Karlsruhe
   Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/

# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

/*
 pto_ctx: ...
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
  Revision 1.4  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.3  2004/11/24 14:53:55  liekweg
  Bugfixes

  Revision 1.2  2004/11/20 21:21:35  liekweg
  Add pto_ctx_allocs

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
