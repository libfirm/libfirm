/* -*- c -*- */

#ifndef _EGC_H_
#define _EGC_H_

# include "irgraph.h"
# include "irnode.h"

/*
  data
*/

typedef struct ctx_info
{
  ir_graph *graph;              /* The graph of the callR who created this ctx */
  /* (which is redundant, since it's always == get_irn_graph (call)  */
  ir_node *call;                /* The call through which this graph was called */
  struct ctx_info *enc;         /* The ctx in which our callR was called */
  int id;
} ctx_info_t;

typedef struct alloc_info
{
  ir_graph *graph;
  ir_node *alloc;
  type *tp;
  struct alloc_info *prev;
} alloc_info_t;

typedef struct callEd_info
{
  ir_graph *callEd;
  struct callEd_info *prev;
} callEd_info_t;

typedef struct call_info
{
  ir_node *call;
  callEd_info_t *callEds;
  struct call_info *prev;
} call_info_t;

typedef struct graph_info
{
  ir_graph *graph;
  call_info_t *calls;
  alloc_info_t *allocs;
  ctx_info_t **ctxs;
  int n_ctxs;
  int ecg_seen;
  int allocs_seen;
  struct graph_info *prev;
} graph_info_t;

typedef void graph_hnd_t  (graph_info_t*, void*);
typedef void alloc_hnd_t  (alloc_info_t*, void*);
typedef void call_hnd_t   (call_info_t*, void*);
typedef void callEd_hnd_t (callEd_info_t*, void*);

/* protos */
void ecg_print_ctx (ctx_info_t*, FILE *stream);

ctx_info_t *get_ctx (graph_info_t*, int);
ctx_info_t *get_main_ctx (void);

void ecg_iterate_graphs (graph_hnd_t*, void*);
void ecg_iterate_allocs (graph_info_t*, alloc_hnd_t*, void*);
void ecg_iterate_calls  (graph_info_t*, call_hnd_t*, void*);
void ecg_iterate_callEds  (call_info_t*, callEd_hnd_t*, void*);

graph_info_t *ecg_get_info (ir_graph*);
alloc_info_t *ecg_get_alloc_info (ir_graph*);
callEd_info_t *ecg_get_callEd_info (ir_node*);

void ecg_init (int);
void ecg_cleanup (void);
void ecg_report (void);
void ecg_ecg (void);

#endif /* defined _EGC_H_ */


/*
$Log$
Revision 1.3  2004/11/20 21:20:29  liekweg
Added iterator functions

Revision 1.2  2004/11/18 16:36:37  liekweg
Added unique ids for debugging, added access functions

Revision 1.1  2004/10/20 14:59:42  liekweg
Added ana2, added ecg and pto

Revision 1.3  2004/10/14 11:31:29  liekweg
SHUTUP_GCC

Revision 1.2  2004/10/12 11:02:03  liekweg
wtf?

Revision 1.1  2004/09/29 12:03:39  liekweg
Added ecg mod
 */
