#include <assert.h>
#include <firm.h>
#include <irnodemap.h>
#include <stdint.h>

#include "irnode_t.h"
#include "sitools.h"
#include "factor.h"
#include "contraction.h"

#define ir_printf(...) (void)0

irg_walk_func uncontract_walker;
void uncontract_walker(ir_node *node, void *data) {
  (void) data;

  if (is_Contraction(node)) {
    ir_node *graph = get_Contraction_graph(node);
    assert(graph);
    exchange(node, graph);
  }
}

static void walk_contraction_1(ir_node *node, ir_node *boundary, irg_walk_func *cb, void *data) {
  if (is_Contraction(node)) {
    return;
  }

  foreach_irn_in(boundary, n, input) {
    if (node == input) {
      return;
    }
  }

  cb(node, data);

  foreach_irn_in(node, n, pred) {
    if (!is_Block(node))
    walk_contraction_1(pred, boundary, cb, data);
  }
}

void walk_contraction(ir_node *node, irg_walk_func *cb, void *data) {
  assert(is_Contraction(node));
  walk_contraction_1(get_Contraction_graph(node), node, cb, data);
}

static irg_walk_func hide_contractions_walker;
static void hide_contractions_walker(ir_node *node, void * env)
{
  ir_nodemap *map = env;

  foreach_irn_in(node, n, pred) {
    if (is_Contraction(pred)) {
      ir_node *graph = get_Contraction_graph(pred);
      set_irn_n(node, n, graph);
      ir_nodemap_insert_fast(map, pred, graph);
    }
  }
}

ir_nodemap *hide_contractions(ir_graph *irg) {
  ir_nodemap *map = XMALLOC(ir_nodemap);
  ir_nodemap_init(map, irg);

  irg_walk_graph(irg, hide_contractions_walker, 0, map);
  return map;
}

static irg_walk_func unhide_contractions_walker;

static void unhide_contractions_walker(ir_node *node, void *env)
{
   ir_nodemap *map = env;

   foreach_irn_in(node, n, pred)
     {
       ir_node *contraction = ir_nodemap_get(ir_node, map, pred);
       if (contraction)
	 set_irn_n(node, n, contraction);
     }
}

void unhide_contractions(ir_graph *irg, ir_nodemap *handle) {
  irg_walk_graph(irg, unhide_contractions_walker, 0, handle);
}

static irg_walk_func add_contraction_keepalive;
static void add_contraction_keepalive(ir_node *node, void *env) {
  ir_node *end = env;
  if (is_Contraction(node)) {
    ir_node *graph = get_Contraction_graph(node);
    ir_printf("keepalive %+F for %+F added\n", graph, node);
    assert(get_irn_irg(graph) == get_irn_irg(node));
    assert(get_irn_irg(graph) == get_irn_irg(end));
    add_End_keepalive(end, graph);
  }
}

void add_contraction_keepalives(ir_graph *irg) {
  irg_walk_graph(irg, 0, add_contraction_keepalive, get_irg_end(irg));
}


static irg_walk_func remove_contraction_keepalive;
static void remove_contraction_keepalive(ir_node *node, void *env) {
  ir_node *end = env;
  if (is_Contraction(node)) {
    ir_node *graph = get_Contraction_graph(node);
    assert(graph);
    ir_printf("keepalive %+F for %+F removed\n", graph, node);
    remove_End_keepalive(end, graph);
  }
}

void remove_contraction_keepalives(ir_graph *irg) {
  irg_walk_graph(irg, 0, remove_contraction_keepalive, get_irg_end(irg));
}

static irg_walk_func fix_contraction_after_irg_copy;
static void fix_contraction_after_irg_copy(ir_node *node, void *env) {
  (void) env;
  if (is_Contraction(node)) {
    ir_node *graph = get_Contraction_graph(node);
    assert(graph);
    graph = get_irn_link(graph);
    assert(graph);
    set_Contraction_graph(node, graph);
  }
}

void fix_contractions_after_irg_copy(ir_graph *irg) {
  irg_walk_graph(irg, fix_contraction_after_irg_copy, 0, 0);
}

static irg_walk_func count_contractions_walk;
static void count_contractions_walk(ir_node *node, void *env) {
  int *count = env;
  if (is_Contraction(node)) {
    (*count)++;
  }
}

int count_contractions(ir_graph *irg)
{
  int result = 0;
  irg_walk_graph(irg, count_contractions_walk, 0, &result);
  ir_printf("contractions: %d\n", result);
  return result;
}

ir_graph *create_contracted_irg_copy(ir_graph *irg) {
  int nkeep = get_End_n_keepalives(get_irg_end(irg));
  int ncontractions = count_contractions(irg);
  assert(count_contractions(irg) == ncontractions);
  assert(count_contractions(irg) == ncontractions);
  add_contraction_keepalives(irg);
  assert(count_contractions(irg) == ncontractions);
  ir_graph *result = create_irg_copy(irg);
  assert(count_contractions(irg) == ncontractions);
  int ncopycontractions = count_contractions(result);
  remove_contraction_keepalives(irg);
  assert(count_contractions(irg) == ncontractions);
  assert(get_End_n_keepalives(get_irg_end(irg)) == nkeep);
  fix_contractions_after_irg_copy(result);
  assert(count_contractions(result) ==  ncopycontractions);
  remove_contraction_keepalives(result);
  assert(count_contractions(result) ==  ncopycontractions);
  return result;
}

static void dump_contracted_node(FILE *out, const ir_node *contraction, const ir_node *node)
{
	foreach_irn_in(contraction, idx, input) {
		if (node == input) {
			fprintf(out, "$%d", idx);
			return;
		}
	}

	if (is_Const(node)) {
		ir_fprintf(out, "%F", get_Const_tarval(node));
		return;
	}

	fprintf(out, "%s(", get_irn_opname(node));
	int emit_comma = 0;
	foreach_irn_in(node, pidx, pred) {
		if (emit_comma++)
			fputc(',', out);
		dump_contracted_node(out, contraction, pred);
	}
	fputc(')', out);
}

void node_dump_Contraction(FILE *out, const ir_node *self, dump_reason_t reason) {
	ir_mode* mode;
	switch(reason) {
	case dump_node_opcode_txt:
		fprintf(out, "%s", get_irn_opname(self));
		break;

	case dump_node_mode_txt:
		mode = get_irn_mode(self);

		if (mode != NULL && mode != mode_BB && mode != mode_ANY && mode != mode_BAD && mode != mode_T )
			fprintf(out, "%s", get_mode_name(mode));
		break;

	case dump_node_nodeattr_txt:
		//keep empty - nodenr inserted automagically
		break;

	case dump_node_info_txt: {
		ir_node *graph = get_Contraction_graph(self);
		fputs("\ngraph: ", out);
		dump_contracted_node(out, self, graph);
		break;
	}
	}
}

