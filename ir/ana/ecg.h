/* -*- c -*- */

#ifndef _EGC_H_
#define _EGC_H_

/*
  data
*/

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
  int ecg_seen;
  int allocs_seen;
} graph_info_t;

/* protos */
void ecg_init (int);
graph_info_t *ecg_get_info (ir_graph*);
alloc_info_t *ecg_get_alloc_info (ir_graph*);
void ecg_cleanup (void);
void ecg_report (void);
void ecg_ecg (void);

#endif /* defined _EGC_H_ */


/* Local Variables: */
/* mode: c */
/* c-basic-offset: 2 */
/* End: */

/*$Log$
 *Revision 1.2  2004/10/12 11:02:03  liekweg
 *wtf?
 *
/*Revision 1.1  2004/09/29 12:03:39  liekweg
/*Added ecg mod
/*
 */
