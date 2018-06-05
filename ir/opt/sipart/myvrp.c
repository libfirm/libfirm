/**
 * @file
 * @author  Andreas Seltenreich
 * @brief   Value range analysis.
 *
 **/

#include "debug.h"
#include "tv.h"
#include "irtypes.h"
#include "pdeq.h"
#include "irgwalk.h"
#include "iredges.h"

#include "adt/obst.h"
#include "myvrp.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

#define DB1(...) 	DB((dbg,LEVEL_1,__VA_ARGS__))
#define DB2(...) 	DB((dbg,LEVEL_2,__VA_ARGS__))
#define DB3(...) 	DB((dbg,LEVEL_3,__VA_ARGS__))

struct env {
  struct obstack *obst;
  pdeq *q;
};

struct vrpdata {
  ir_tarval *bottom;
  ir_tarval *top;
};

#define BOTTOM(n) ((struct vrpdata*) get_irn_link(n))->bottom
#define TOP(n) ((struct vrpdata*) get_irn_link(n))->top

static void new_range(ir_node *node, ir_tarval *bottom, ir_tarval *top, pdeq *q)
{
  if (BOTTOM(node) != bottom || TOP(node) != top) {
    BOTTOM(node) = bottom;
    TOP(node) = top;
    foreach_out_edge(node, edge) {
      ir_node *work = get_edge_src_irn(edge);
      DB2("queueing %+F\n", work);
      pdeq_putr(q, work);
    }
  } else {
    DB3("no change on %+F\n", node);
  }
}

static irg_walk_func init_nodedata;
static void init_nodedata(ir_node *node, void *data)
{
  struct env *env =data;
  struct vrpdata *vrpdata = OALLOC(env->obst, struct vrpdata);
  vrpdata->top = vrpdata->bottom = tarval_unknown;

  set_irn_link(node, vrpdata);

  switch (get_irn_opcode(node)) {
  case iro_Const:
    new_range(node, get_Const_tarval(node), get_Const_tarval(node), env->q);
    break;
  default:
    if (mode_is_data(get_irn_mode(node)))
      new_range(node,
		get_mode_min(get_irn_mode(node)),
		get_mode_max(get_irn_mode(node)),
		env->q);
  }
}

static void myvrp_transfer(ir_node *node, struct env *env)
{
  ir_mode *mode = get_irn_mode(node);


  switch (get_irn_opcode(node)) {

  case iro_Conv: {
    ir_node *pred = get_Conv_op(node);
    new_range(node,
	      tarval_convert_to(BOTTOM(pred), mode),
	      tarval_convert_to(TOP(pred), mode),
	      env->q);
    break;
  }

  case iro_Add: {
    ir_node *left = get_Add_left(node);
    ir_node *right = get_Add_right(node);
    new_range(node,
	      tarval_add(BOTTOM(left), BOTTOM(right)),
	      tarval_add(TOP(left), TOP(right)),
	      env->q);
    break;
  }

  case iro_Sub: {    
    ir_node *left = get_Sub_left(node);
    ir_node *right = get_Sub_right(node);
    new_range(node,
	      tarval_sub(BOTTOM(left), TOP(right), mode),
	      tarval_sub(TOP(left), BOTTOM(right), mode),
	      env->q);
    
    break;
  }  

  }
}

void myvrp_analyze(ir_graph *irg, struct obstack *client_obst)
{
  FIRM_DBG_REGISTER(dbg, "firm.ana.myvrp");

  struct env env;
  env.obst = client_obst;
  env.q = new_pdeq();
  assure_edges(irg);

  /* tarval_int_overflow_mode_t rem; */
  tarval_set_integer_overflow_mode(TV_OVERFLOW_SATURATE);

  irg_walk_graph(irg, init_nodedata, 0, &env);

  while (!pdeq_empty(env.q)) {
    ir_node *n = pdeq_getl(env.q);
    myvrp_transfer(n, &env);
  }

  tarval_set_integer_overflow_mode(TV_OVERFLOW_WRAP);

  del_pdeq(env.q);
}

