/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana/ecg.c
 * Purpose:     Extended Call Graph
 * Author:      Florian
 * Modified by:
 * Created:     14.09.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/**
   Erweiterter Aufrufgraph.
*/

#include "irnode.h"
#include "pmap.h"
/* #include "eset.h" */
#include "irgwalk.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "trvrfy.h"
#include "xmalloc.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

# define BUF_SIZE 1024

# include "ecg.h"
# include "typalise.h"
# include "lset.h"

/*
  le flag
*/
/* static int verbose     = 0; */
static int do_typalise = 0;

/*
  globals
*/

/* Ids for the ctxs */
static int ctx_id = 0;
ctx_info_t *main_ctx = NULL;

/* mapping from method graphs (callR) to method graphs (lset_ts of callEds) */
/* static pmap *calls; */
static pmap *graph_infos;

/* linked list of all graph_infos: */
static graph_info_t *graph_infos_list = NULL;

/* Counters for ecg_ecg and friends */
static long _graphs = 0;
static long _calls  = 0;
static long _allocs = 0;

static int _depth = 0;
static int _max_depth = 0;

static int _max_callEds = 0;
static entity* _max_callEds_callR = NULL;

/* Protos */
void set_main_ctx (ctx_info_t*);

/* ====================
   Alloc stuff
   ==================== */
static void append_alloc (graph_info_t *ginfo, ir_node *alloc, type *tp)
{
  alloc_info_t *ainfo = (alloc_info_t*) xmalloc (sizeof (alloc_info_t));

  ainfo->graph = ginfo->graph;
  ainfo->alloc = alloc;
  ainfo->tp    = tp;

  ainfo->prev = ginfo->allocs;
  ginfo->allocs = ainfo;
}


/* ====================
   CallEd stuff
   ==================== */
/**
   Append the given callEd to the given callEd info.
*/
static callEd_info_t *append_callEd_info (callEd_info_t *ced, ir_graph *callEd)
{
  callEd_info_t *nced = (callEd_info_t*) xmalloc (sizeof (sizeof (callEd_info_t)));

  nced->callEd = callEd;
  nced->prev = ced;

  return (nced);
}

/**
   Append all callEd methods of the given (call) node to the given graph_info.
*/
static void append_calls (graph_info_t *info, ir_node *call, lset_t *callEds)
{
  call_info_t *cinfo = (call_info_t*) xmalloc (sizeof (call_info_t));

  /* setup */
  cinfo->call = call;
  cinfo->prev = info->calls;
  info->calls = cinfo;
  cinfo->callEds = NULL;

  /* enter */
  ir_graph *callEd = lset_first (callEds);
  while (callEd) {
    cinfo->callEds = append_callEd_info (cinfo->callEds, callEd);

    callEd = lset_next (callEds);
  }
}

/**
   Append the (single) callEd to the given (call) node of the given graph_info.
*/
static void append_call (graph_info_t *info, ir_node *call, ir_graph *callEd)
{
  call_info_t *cinfo = (call_info_t*) xmalloc (sizeof (call_info_t));

  cinfo->call = call;
  cinfo->prev = info->calls;
  info->calls = cinfo;

  cinfo->callEds = append_callEd_info (cinfo->callEds, callEd);
}

/**
   Given a method, find the firm graph that implements that method.
   Return NULL for abstract and native methods.
*/
static ir_graph *_get_implementing_graph (entity *method)
{
  ir_graph *graph = NULL;

  /* What's up with the fenced out stuff in rta? */
  if (peculiarity_existent == get_entity_peculiarity (method)) {
    if (visibility_external_allocated == get_entity_visibility (method)) {
      /* Todo: native implementation */

      return (NULL);
    } else {
      graph = get_entity_irg (get_SymConst_entity (get_atomic_ent_value (method)));
      assert (graph && "no graph");

      return (graph);
    }
  } else if (0 && (peculiarity_description == get_entity_peculiarity (method))) {
    /* abstract --- can't find an implementation */
    graph = get_entity_irg (method);
    assert (!graph && "graph in abstract method");

    return (NULL);
  } else if ((peculiarity_description == get_entity_peculiarity (method)) ||
             (peculiarity_inherited == get_entity_peculiarity (method))) {
    /* search UPWARDS */
    int i;
    int n_over = get_entity_n_overwrites (method);

    assert (!graph);

    for (i = 0; (NULL == graph) && (i < n_over); i ++) {
      entity *over = get_entity_overwrites (method, i);

      graph = _get_implementing_graph (over);
    }
  } else {
    assert (0 && "invalid peculiarity");
  }


  return (graph);
}

/**
   Collect all graphs of 'method' in the given set.
*/
static void _collect_implementing_graphs (entity *method, lset_t *set)
{
  /* search DOWN-wards in clazz hierarchy */
  int i;
  int n_over = get_entity_n_overwrittenby (method);
  ir_graph *graph = get_entity_irg (method);

  if (NULL == graph) {
    graph = _get_implementing_graph (method);
  }

  if (graph) {
    lset_insert (set, graph);
  }

  for (i = 0; i < n_over; i ++) {
    entity *over = get_entity_overwrittenby (method, i);

    _collect_implementing_graphs (over, set);
  }
}


/**
   Collect all graphs that could possibly be executed when 'method' is called.
*/
static lset_t *get_implementing_graphs (entity *method, ir_node *select)
{
  lset_t *set = lset_create ();
  {
    ir_graph *impl = _get_implementing_graph (method);

    if (NULL != impl) {
      lset_insert (set, impl);
    } else {
      /* actually, abstract OR native */
    }
  }

  _collect_implementing_graphs (method, set);

  if (lset_empty (set)) {
    /* then it's a method which is only implemented natively, and we
       don' bother to analyse anything */
    return (set);
  }

  /* void *tmp = lset_first (set); */
  int n_graphs = lset_n_entries (set);

  /* typalise select_in */
  if (do_typalise) {
    ir_node *select_in = get_Sel_ptr (select);
    typalise_t *ta = typalise (select_in);
    assert (ta && "typalise failed (go figure)");

    /* const char *res = ta_name (ta); */

    /* fprintf (stdout, "typalyse res = %s\n", res); */

    if (1 != n_graphs) {
      set = filter_for_ta (set, ta);

      int n_filtered_graphs = lset_n_entries (set);

      /*
        fprintf (stdout, "%s: %02d %02d\n",
        __FUNCTION__,
        n_graphs,
        n_filtered_graphs,
        n_graphs - n_filtered_graphs);
      */
      n_graphs = n_filtered_graphs;
    }
  }

  if (n_graphs > _max_callEds) {
    _max_callEds = n_graphs;
    _max_callEds_callR = method;
  }


  if (visibility_external_allocated != get_entity_visibility (method)) {
    if (0 == n_graphs) {
      /* fprintf (stdout, "no graphs for method %s\n", get_entity_name (method)); */
      assert (n_graphs && "no graphs for method");
    }
  }

  return (set);
}

/**
   Action for the graph.
*/
static void ecg_calls_act (ir_node *node, void *env)
{
  opcode op = get_irn_opcode (node);
  graph_info_t *graph_info = (graph_info_t*) env;

  if (iro_Call == op) {         /* CALL */
    entity *ent = NULL;
    ir_node *ptr = get_Call_ptr (node);

    /* CALL SEL */
    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
      lset_t *graphs = get_implementing_graphs (ent, ptr);

      append_calls (graph_info, node, graphs);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind (ptr) == symconst_addr_ent) {
        ent = get_SymConst_entity (ptr);
        ir_graph *graph = get_entity_irg (ent);

        if (graph) {
          append_call (graph_info, node, graph);
        } else {
          /* it's an externally allocated thingy */
        }
      } else if (get_SymConst_kind (ptr) == symconst_addr_name) {
        /* If this SymConst refers to a method the method is external_visible
           and therefore must be considered live anyways. */
        if (get_SymConst_name (ptr) != new_id_from_str ("iro_Catch")) {
          assert (ent && "couldn't determine entity of call to symConst");
        }
      } else {
        /* other symconst. */
        assert (0 && "This SymConst can not be an address for a method call.");
      }

      /* STRANGE, no less ... */
    } else {
      DDMN (ptr);
      assert (0 && "Unexpected address expression");
    }
  } else if (iro_Alloc == op) {
    type *tp = get_Alloc_type (node);
    /* const char *name = get_type_name (tp); */

    append_alloc (graph_info, node, tp);

    /* fprintf (stdout, "NEW \"%s\"\n", name); */
  }
}

/**
   Collect called graphs for the given graph.
*/
static void ecg_fill_graph_calls (ir_graph *graph)
{
  graph_info_t *graph_info = (graph_info_t*) xmalloc (sizeof (graph_info_t));

  graph_info->graph = graph;
  graph_info->calls = NULL;
  graph_info->ecg_seen = 0;
  graph_info->ctxs = NULL;
  graph_info->n_ctxs = 0;

  /* link up into global list */
  graph_info->prev = graph_infos_list;
  graph_infos_list = graph_info;

  irg_walk_graph (graph, ecg_calls_act, NULL, graph_info);

  pmap_insert (graph_infos, graph, graph_info);
}

/**
   For each graph, collect called graphs, and enter them into calls.
*/
static void ecg_fill_calls (void)
{
  int i;

  for (i = 0; i < get_irp_n_irgs (); i++) {
    ir_graph *graph = get_irp_irg (i);

    ecg_fill_graph_calls (graph);
  }
}

/**
   Allocate a new ctx for the given graph and the given enclosing ctx.
*/
static ctx_info_t *new_ctx (ir_graph *graph, ir_node *call, ctx_info_t *enc)
{
  ctx_info_t *res = xmalloc (sizeof (ctx_info_t));

  res->graph = graph;
  res->call = call;
  res->enc = enc;
  res->id = ctx_id ++;

  return (res);
}


/**
   Fill in the ctxs parts of the graph_infos
*/
static void ecg_fill_ctxs_count (ir_graph *graph)
{
  graph_info_t *ginfo = ecg_get_info (graph);

  /* count how many ctxs we have per graph */
  if (0 == ginfo->ecg_seen) {

    ginfo->ecg_seen = 1;
    call_info_t *cinfo = ginfo->calls;

    while (NULL != cinfo) {
      callEd_info_t *ced = cinfo->callEds;

      while (NULL != ced) {
        ir_graph *callEd_graph = ced->callEd;

        /* first step: we have a new ctx */
        graph_info_t *callEd_info = ecg_get_info (callEd_graph);
        callEd_info->n_ctxs ++;

        /* Calling graph -> callEd_graph */
        ecg_fill_ctxs_count (callEd_graph);

        ced = ced->prev;
      } /* end forall callEds (call) */

      cinfo = cinfo->prev;
    } /* end forall (calls(graph)) */

    ginfo->ecg_seen = 0;
  }
}

static void ecg_fill_ctxs_alloc (void)
{
  /* allocate the memory needed for the ctxts: */
  graph_info_t *ginfo = graph_infos_list;

  while (NULL != ginfo) {
    ginfo->ctxs = (ctx_info_t **) xmalloc (ginfo->n_ctxs * sizeof (ctx_info_t*));

    /*
      fprintf (stdout, "graph of \"%s\": n_ctxs = %i\n",
      get_entity_name (get_irg_entity (ginfo->graph)), ginfo->n_ctxs);
    */
    ginfo->n_ctxs = 0;

    ginfo = ginfo->prev;
  }
}

/**
   Fill in the ctxs parts of the graph_infos
*/
static void ecg_fill_ctxs_write (ir_graph *graph, ctx_info_t *enc_ctx)
{
  graph_info_t *ginfo = ecg_get_info (graph);

  /* enter a new ctx for all callEds along the call edges of this graph */
  if (0 == ginfo->ecg_seen) {
    ginfo->ecg_seen = 1;
    call_info_t *cinfo = ginfo->calls;

    while (NULL != cinfo) {
      callEd_info_t *ced = cinfo->callEds;

      while (NULL != ced) {
        ctx_info_t *ctx = new_ctx (graph, cinfo->call, enc_ctx);

        ir_graph *callEd_graph = ced->callEd;

        /* write the ctx of this call into the callEd graph */
        graph_info_t *callEd_info = ecg_get_info (callEd_graph);

        callEd_info->ctxs [callEd_info->n_ctxs] = ctx;
        callEd_info->n_ctxs ++;

        /* Calling graph -> callEd_graph */
        ecg_fill_ctxs_write (callEd_graph, ctx);

        ced = ced->prev;
      } /* end forall callEds (call) */

      cinfo = cinfo->prev;
    } /* end forall (calls(graph)) */

    ginfo->ecg_seen = 0;
  }
}

/**
   Fill in the ctxs parts of the graph_infos
*/
static void ecg_fill_ctxs (void)
{
  ecg_fill_ctxs_count (get_irp_main_irg ());
  ecg_fill_ctxs_alloc ();

  ctx_info_t *main_ctx = new_ctx (get_irp_main_irg (), NULL, NULL);
  ir_graph *main_irg = get_irp_main_irg ();

  set_main_ctx (main_ctx);

  /* Grrr, have to add this ctx manually to main.ginfo ... */
  graph_info_t *ginfo = ecg_get_info (main_irg);
  ginfo->n_ctxs = 1;
  ginfo->ctxs = (ctx_info_t **) xmalloc (1 * sizeof (ctx_info_t*));
  ginfo->ctxs [0] = main_ctx;

  ecg_fill_ctxs_write (main_irg, main_ctx);
}

/* ====================
   CTX stuff
   ==================== */
/*
  Nicely print a ctx_info_t to the given output stream
*/
void ecg_print_ctx (ctx_info_t *ctx, FILE *stream)
{
  entity *ent = get_irg_ent (ctx->graph);
  ir_node *call = ctx->call;
  const char *ent_name = (char*) get_entity_name (ent);
  const char *own_name = (char*) get_type_name (get_entity_owner (ent));

  fprintf (stream, "CTX[%i](%s.%s->%s[%li])",
           ctx->id, own_name, ent_name,
           get_op_name (get_irn_op (call)),
           get_irn_node_nr (call));

  if (NULL != ctx->enc) {
    fprintf (stream, "->%i", ctx->enc->id);
  }

  fprintf (stream, "\n");
}

/*
  Get a ctx of the given graph info
*/
ctx_info_t *get_ctx (graph_info_t *ginfo, int ctx_idx)
{
  assert (ginfo->n_ctxs > ctx_idx);

  return (ginfo->ctxs [ctx_idx]);
}

/*
  Get the pseudo-ctx of 'main'
*/
ctx_info_t *get_main_ctx ()
{
  return (main_ctx);
}

/*
  Set the pseudo-ctx of 'main'
*/
void set_main_ctx (ctx_info_t *ctx)
{
  main_ctx = ctx;
}


/* ====================
   ECG stuff
   ==================== */

/* ====================
   Iterator stuff
   ==================== */
/*
   Iterate over all graphs
*/
void ecg_iterate_graphs (graph_hnd_t *hnd, void *env)
{
  graph_info_t *ginfo = graph_infos_list;

  while (NULL != ginfo) {
    hnd (ginfo, env);

    ginfo = ginfo->prev;
  }
}


/*
   Iterate of all allocs of a given graph info
*/
void ecg_iterate_allocs (graph_info_t *ginfo, alloc_hnd_t *hnd, void *env)
{
  alloc_info_t *ainfo = ginfo->allocs;

  while (NULL != ainfo) {
    hnd (ainfo, env);

    ainfo = ainfo->prev;
  }
}


/*
  Iterate over all calls of the given graph info
*/
void ecg_iterate_calls  (graph_info_t *ginfo, call_hnd_t *hnd, void *env)
{
  call_info_t *cinfo = ginfo->calls;

  while (NULL != cinfo) {
    hnd (cinfo, env);

    cinfo = cinfo->prev;
  }
}


/*
  Iterate over all callEds of the given call info
*/
void ecg_iterate_callEds  (call_info_t *cinfo, callEd_hnd_t *hnd, void *env)
{
  callEd_info_t *ced = cinfo->callEds;

  while (NULL != ced) {
    hnd (ced, env);

    ced = ced->prev;
  }
}


/*
  get the call infos for the given graph
*/
graph_info_t *ecg_get_info (ir_graph *graph)
{
  graph_info_t *ginfo = (graph_info_t*) pmap_get (graph_infos, graph);

  assert (ginfo && "no info for graph");

  return (ginfo);
}

/*
  Get the Alloc Infos for the given graph
*/
alloc_info_t *ecg_get_alloc_info (ir_graph *graph)
{
  graph_info_t *ginfo = ecg_get_info (graph);

  return (ginfo->allocs);
}

/*
  Get the Call Info for the given call
*/
callEd_info_t *ecg_get_callEd_info (ir_node *call)
{
  ir_graph *graph = get_irn_irg (call);
  graph_info_t *ginfo = ecg_get_info (graph);

  call_info_t *call_info = ginfo->calls;

  while (NULL != call_info) {
    if (call == call_info->call) {
      return (call_info->callEds);
    }

    call_info = call_info->prev;
  }

  return (NULL);
}


/**
   Dump the given graph and it's calls and it's calls callEds to the given file.
*/
static int ecg_ecg_graph (FILE *dot, ir_graph *graph)
{
  const char *name = get_irg_entity (graph) ?
    get_entity_name (get_irg_entity (graph)) : "noEntity";
  const char *color =
    (get_entity_stickyness
     (get_irg_entity (graph)) == stickyness_sticky) ?
    "red" : "lightyellow";

  /* graph_info_t *ginfo = (graph_info_t*) pmap_get (graph_infos, graph); */
  graph_info_t *ginfo = ecg_get_info (graph);

  if (0 != ginfo->ecg_seen) {
    fprintf (dot, "\t/* recursive call to \"%s\" (%d) */\n",
             name, (int) ginfo->ecg_seen);
# if 0
    fprintf (dot, "\t/* recursive call to \"%s\" (0x%08x) */\n",
             name, (int) graph);
# endif /* 0 */
    return (ginfo->ecg_seen);
  }

  assert (0L <= _graphs);

  const int graph_no = _graphs ++;
  ginfo->ecg_seen = graph_no;

  fprintf (dot, "\t/* Graph of \"%s.%s\" */\n",
           get_type_name (get_entity_owner (get_irg_entity (graph))),
           name);
  fprintf (dot, "\tgraph_%i [label=\"<HEAD>%s\\l%s\\l|<CTX>n_ctx = %i\\l\", color=\"%s\"];\n",
           graph_no,
           get_type_name (get_entity_owner (get_irg_entity (graph))),
           name,
           ginfo->n_ctxs,
           color);
  fprintf (dot, "\n");

  if (visibility_external_allocated ==
      get_entity_visibility (get_irg_entity (graph))) {
    fprintf (dot, "\t/* graph \"%s\" is external */\n", name);

    return (graph_no);
  }

  call_info_t *cinfo = ginfo->calls;
  while (NULL != cinfo) {
    ir_node *call = cinfo->call;
    callEd_info_t *ced = cinfo->callEds;
    const int call_no = _calls ++;
    const char *call_color = (NULL == ced->prev) ? "lightblue" : "blue3";

    fprintf (dot, "\t/* Call %li */\n", get_irn_node_nr (call));
    fprintf (dot, "\tcall_%i [label=\"call\\[%li\\]\", color=\"%s\", shape=\"ellipse\"];\n",
             call_no, get_irn_node_nr (call), call_color);
    fprintf (dot, "\tgraph_%i -> call_%i [color=\"black\"];\n", graph_no, call_no);

    while (NULL != ced) {
      ir_graph *callEd_graph = ced->callEd;
      const int callEd_no = ecg_ecg_graph (dot, callEd_graph);
      const char *callEd_name = get_irg_entity (callEd_graph) ?
        get_entity_name (get_irg_entity (callEd_graph)) : "noEntity";
      const char *direction = (callEd_no <= graph_no) ? "forward" : "forward";
      const char *callEd_color     = (callEd_no <= graph_no) ? "red" : "black";

      fprintf (dot, "\t/* Call from graph \"%s\" to graph \"%s\" */\n",
               name,
               callEd_name);
      /* Check for recursive calls */
      /* if (callEd_no > graph_no) */ { /* do recursive calls (for now) */
        fprintf (dot, "\tcall_%i -> graph_%i:HEAD [color=\"%s\", dir=\"%s\"];\n",
                 call_no, callEd_no, callEd_color, direction);
      }

      ced = ced->prev;
      /* ced = NULL; */
    } /* done all calEds (call) */

    cinfo = cinfo->prev;
  } /* done all calls (graph) */

  /* now the allocs */
  alloc_info_t *ainfo = ecg_get_alloc_info (graph);
  if (ainfo) {
    fprintf (dot, "\t/* now the allocs */\n");
  } else {
    fprintf (dot, "\t/* no allocs */\n");
  }

  while (NULL != ainfo) {
    ir_node *alloc = ainfo->alloc;
    const char *name = get_type_name (ainfo->tp);
    const char *color = "red1";

    _allocs ++;
    fprintf (dot, "\talloc_0x%08x_%i [label=\"%s\", color=\"%s\"];\n",
             (int) alloc, graph_no, name, color);

    fprintf (dot, "\tgraph_%i -> alloc_0x%08x_%i;\n",
             graph_no, (int) alloc, graph_no);

    ainfo = ainfo->prev;
  }

  if (0 == ginfo->allocs_seen) {
    ginfo->allocs_seen = 1;
  }

  /* write table of ctxs */
  {
    fprintf (dot, "\tctx_%i [label=\"<HEAD>", graph_no);

    int i;
    const int max_ctxs = 30;
    const int n_ctxs = (ginfo->n_ctxs > max_ctxs) ? max_ctxs : ginfo->n_ctxs;

    assert (ginfo->ctxs && "no ctx");
    for (i = 0; i < n_ctxs; i ++) {
      ctx_info_t *ctx_info = ginfo->ctxs [i];

      if (NULL != ctx_info->enc) {
        fprintf (dot, "ctx_info \\[%i\\] = ctx\\[%i\\-\\>%i\\]\\l",
                 i,
                 ctx_info->id,
                 ctx_info->enc->id);
      } else {
        fprintf (dot, "ctx_info \\[%i\\] = ctx\\[%i\\]\\l",
                 i, ctx_info->id);
      }

      if (i+1 != n_ctxs) {
        fprintf (dot, "|");
      }
    }

    if (0 < ginfo->n_ctxs - max_ctxs) {
      fprintf (dot, "(%i more)\\l", ginfo->n_ctxs - max_ctxs);
    }

    fprintf (dot, "\", color=\"green3\"];\n");

    fprintf (dot,
             "\tgraph_%i:CTX -> ctx_%i:HEAD [label=\"ctx\", dir=\"none\", style=\"dotted\"];\n",
             graph_no, graph_no);
  }

  fprintf (dot, "\t/* done with graph of \"%s\" */\n\n", name);

  fflush (dot);
  ginfo->ecg_seen = 0;

  return (graph_no);
}

/**
   Count how many nodes the ECG will have
*/
static char spaces [BUF_SIZE];

static void ecg_ecg_count (ir_graph *graph)
{
  graph_info_t *ginfo = (graph_info_t*) pmap_get (graph_infos, graph);

  if (0 != ginfo->ecg_seen) {
    return;
  }

  _depth ++;
  if (_depth > _max_depth) {
    _max_depth = _depth;

    /*
      fprintf (stdout, "_max_depth = %i\n", _max_depth);
      fprintf (stdout, "\tn_graphs: %i\n", _graphs);
    */
  }

  assert (0L <= _graphs);

  /*
    if (0 == (_graphs % 1000000)) {
    fprintf (stdout, "\tn_graphs: %i\n", _graphs);
    fprintf (stdout, "_depth = %i\n", _depth);
    }
  */

  const int graph_no = _graphs ++;
  ginfo->ecg_seen = graph_no;

  fprintf (stdout, "%sMethod \"%s.%s\"\n",
           spaces + BUF_SIZE - _depth,
           get_type_name (get_entity_owner (get_irg_entity (graph))),
           get_entity_name (get_irg_entity (graph)));

  call_info_t *cinfo = ginfo->calls;
  while (NULL != cinfo) {

    callEd_info_t *ced = cinfo->callEds;

    fprintf (stdout, "%sCall \"0x%08x\"\n",
             spaces + BUF_SIZE - _depth,
             (int) cinfo->call);

    while (NULL != ced) {
      ir_graph *callEd_graph = ced->callEd;

      fprintf (stdout, "%sCall Target \"%s.%s\"\n",
               spaces + BUF_SIZE - _depth,
               get_type_name (get_entity_owner (get_irg_entity (callEd_graph))),
               get_entity_name (get_irg_entity (callEd_graph)));

      ecg_ecg_count (callEd_graph);

      ced = ced->prev;
    } /* done all calEds (call) */
    cinfo = cinfo->prev;
  } /* done all calls (graph) */

  ginfo->ecg_seen = 0;
  _depth --;
}

/* ====================
   Public Interface
   ==================== */

/**
   Initialise our data structures.
*/
void ecg_init (int typalise)
{
  do_typalise = typalise;

  graph_infos = pmap_create ();

  ecg_fill_calls ();
  ecg_fill_ctxs ();

  ecg_ecg ();
}

/**
   Clean up our mess.
*/
void ecg_cleanup ()
{
  int i;

  for (i = 0; i < get_irp_n_irgs (); i++) {
    ir_graph *graph = get_irp_irg (i);

    graph_info_t *info = pmap_get (graph_infos, graph);
    call_info_t *cinfo = info->calls;

    while (NULL != cinfo) {
      cinfo->call = NULL;

      callEd_info_t *ced = cinfo->callEds;

      while (NULL != ced) {
        callEd_info_t *nced = ced->prev;
        free (ced);
        ced->prev = NULL;
        ced->callEd = NULL;
        ced = nced;
      }

      cinfo->callEds = NULL;

      free (cinfo);
      cinfo = cinfo->prev;
    }

    free (info);
    pmap_insert (graph_infos, graph, NULL);
  }


  pmap_destroy (graph_infos);

  /*  paranoia mode */
  graph_infos = NULL;
}

/**
   Show what we have found.
*/
void ecg_report ()
{
  int i;

  FILE *dot = fopen ("calls.dot", "w");

  fprintf (dot, "digraph \"calls\" {\n");
  fprintf (dot, "\tnode [shape = \"record\", style = \"filled\"];\n");
  fprintf (dot, "\tedge [color = \"black\"];\n");
  fprintf (dot, "\n");
  fprintf (dot, "\tsize = \"11, 7\";\n");
  fprintf (dot, "\trotate = \"90\";\n");
  fprintf (dot, "\tratio = \"fill\";\n");
  fprintf (dot, "\trankdir = \"LR\";\n");
  fprintf (dot, "\n");

  for (i = 0; i < get_irp_n_irgs (); i++) {
    ir_graph *graph = get_irp_irg (i);
    graph_info_t *info = (graph_info_t*) pmap_get (graph_infos, graph);

    const char *name = get_irg_entity (graph) ?
      get_entity_name (get_irg_entity (graph)) : "noEntity";

    const char *oname = get_type_name
      (get_entity_owner (get_irg_entity (graph)));

    const char *color =
      (get_entity_stickyness
       (get_irg_entity (graph)) == stickyness_sticky) ?
      "red3" : "lightyellow";

    fprintf (dot, "\t/* graph_0x%08x (\"%s\") */\n", (int) graph, name);
    fprintf (dot,
             "\tgraph_0x%08x [label=\"%s\\l%s\", color=\"%s\"];\n",
             (int) graph, oname, name, color);
    fprintf (dot, "\n");

    call_info_t *cinfo = info->calls;
    if (cinfo) {
      fprintf (dot, "\t/* now the calls */\n");
    } else {
      fprintf (dot, "\t/* no calls, nothing to see, move along! */\n");
    }

    while (NULL != cinfo) {
      ir_node *call = cinfo->call;

      fprintf (dot, "\t/* call_0x%08x */\n", (int) call);
      fprintf (dot, "\tcall_0x%08x [label=\"call\\[%li\\]\\l0x%08x\"];\n",
               (int) call, get_irn_node_nr (call), (int) call);
      fprintf (dot, "\tgraph_0x%08x -> call_0x%08x;\n",
               (int) graph, (int) call);

      callEd_info_t *ced = cinfo->callEds;
      while (NULL != ced) {
        fprintf (dot, "\tcall_0x%08x -> graph_0x%08x;\n",
                 (int) call, (int) ced->callEd);
        ced = ced->prev;
      }
      fprintf (dot, "\n");

      cinfo = cinfo->prev;
    }
    fprintf (dot, "\n");

    alloc_info_t *ainfo = info->allocs;
    if (ainfo) {
      fprintf (dot, "\t/* now the allocs */\n");
    } else {
      fprintf (dot, "\t/* no allocs */\n");
    }


    while (NULL != ainfo) {
      ir_node *alloc = ainfo->alloc;
      const char *name = get_type_name (ainfo->tp);
      const char *color = "green3";

      fprintf (dot, "\talloc_0x%08x [label=\"%s\", color=\"%s\"];\n",
               (int) alloc, name, color);
      fprintf (dot, "\tgraph_0x%08x -> alloc_0x%08x;\n",
               (int) graph, (int) alloc);

      ainfo = ainfo->prev;
    }
  }
  fprintf (dot, "}\n");

  /*
    fprintf (stdout, " max_callEds: %i\n", _max_callEds);
    fprintf (stdout, " max_callEds_callR: \"%s\"\n",
    get_entity_name (_max_callEds_callR));
  */
  fclose (dot);
}

/**
   Experimental:  Print the ecg
*/
void ecg_ecg ()
{
  _graphs = 0;
  _calls  = 0;

  ir_graph *main_graph = get_irp_main_irg ();

  /*
    memset (spaces, '.', BUF_SIZE);
    spaces [BUF_SIZE-1] = '\0';

    ecg_ecg_count (main_graph);
    fprintf (stdout, "n_graphs: %i\n", _graphs);
    fprintf (stdout, "max_depth = %i\n", _max_depth);
  */

  /* return; */

  _graphs = 0;
  _calls  = 0;

  FILE *dot = fopen ("ecg.dot", "w");

  fprintf (dot, "digraph \"ecg\" {\n");
  fprintf (dot, "\tnode [shape = \"record\", style = \"filled\"];\n");
  fprintf (dot, "\tedge [color = \"black\"];\n");
  fprintf (dot, "\n");
  fprintf (dot, "\tsize = \"11, 7\";\n");
  fprintf (dot, "\trotate = \"90\";\n");
  fprintf (dot, "\tratio = \"fill\";\n");
  fprintf (dot, "\trankdir = \"LR\";\n");
  fprintf (dot, "\n");

  /* ir_graph *main_graph = get_irp_main_irg (); */
  ecg_ecg_graph (dot, main_graph);

  fprintf (dot, "\t/* Grand Total: */\n");
  fprintf (dot, "\t/* calls:  %i */\n", (int) _calls);
  fprintf (dot, "\t/* graphs: %i */\n", (int) _graphs);
  fprintf (dot, "\t/* allocs: %i */\n", (int) _allocs);
  fprintf (dot, "\t/* (sales tax not included) */\n");

  fprintf (dot, "}\n");

  fclose (dot);
}



/*
  $Log$
  Revision 1.5  2004/11/20 21:20:29  liekweg
  Added iterator functions

  Revision 1.4  2004/11/18 16:36:37  liekweg
  Added unique ids for debugging, added access functions

  Revision 1.3  2004/11/04 14:54:44  liekweg
  Nicer Colors

  Revision 1.2  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise

  Revision 1.1  2004/10/20 14:59:41  liekweg
  Added ana2, added ecg and pto

  Revision 1.6  2004/10/18 12:47:19  liekweg
  minor fix

  Revision 1.5  2004/10/14 11:31:28  liekweg
  SHUTUP_GCC

  Revision 1.4  2004/10/12 11:02:01  liekweg
  wtf?

*/
