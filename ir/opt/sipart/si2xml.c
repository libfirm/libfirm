#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#include <assert.h>

#include <firm.h>
#include "error.h"
#include "irnode_t.h"
#include "si2xml.h"

#define ICORE_BC_WIDTH 32

/* indentation of elements in output */
#define DI "    "  /* data */
#define NI "  "  /* node */
#define EI "  "  /* edge */
#define GI ""   /* graph */
#define KI "  " /* key */

#define foreach_Call_param(irn, idx, pred) \
	for (bool pred##__b = true; pred##__b;) \
		for (ir_node const *const pred##__irn = (irn); pred##__b; pred##__b = false) \
			for (int idx = 0, pred##__n = get_Call_n_params(pred##__irn); pred##__b && idx != pred##__n; ++idx) \
				for (ir_node *const pred = (pred##__b = false, get_Call_param(pred##__irn, idx)); !pred##__b; pred##__b = true)

#define foreach_Return_res(irn, idx, pred) \
	for (bool pred##__b = true; pred##__b;) \
		for (ir_node const *const pred##__irn = (irn); pred##__b; pred##__b = false) \
			for (int idx = 0, pred##__n = get_Return_n_ress(pred##__irn); pred##__b && idx != pred##__n; ++idx) \
				for (ir_node *const pred = (pred##__b = false, get_Return_res(pred##__irn, idx)); !pred##__b; pred##__b = true)

static FILE *file;

static bool is_emitable_node(ir_node *node) {
	return is_Call(node) || is_Load(node) || is_Store(node)
		|| is_Return(node) || is_Start(node) || is_Sync(node);
}

static ir_node *get_source_node(ir_node *node) {
	if (is_emitable_node(node))
		return node;
	switch(get_irn_opcode(node)) {
	case iro_Proj:
		return get_source_node(get_Proj_pred(node));
	case iro_Slice:
		return get_source_node(get_Slice_pred(node));
	default:
		panic("cannot locate an emitable source node via op %s", get_id_str(get_irn_op(node)->name));
	}
}

static void emit_data_mode(ir_node *pred) {
	ir_mode *mode = get_irn_mode(pred);
	const char *name = get_id_str(get_mode_ident(mode));
	fprintf(file, DI "<data key=\"mode\">%s</data>\n", name);
	fprintf(file, DI "<data key=\"mode_is_data\">%d</data>\n", mode_is_data(mode) ? 1 : 0);
}

/* Emit sourceport and source for an edge targeted at PRED.  PRED
   might not be the actual target, but some intermediary Proj or Slice. */
static void emit_edge_attr(ir_node *pred) {
	ir_node *source = get_source_node(pred);
	ir_mode *mode = get_irn_mode(pred);

	if (!source) {
		fprintf(file, ">\n");
		return;
	}

	if (mode == mode_M) {
		fprintf(file, " source=\"%d\"", get_irn_idx(source));
		fprintf(file, ">\n");
		return;
	}

	switch(get_irn_opcode(source)) {
	case iro_Start: {
		assert(is_Proj(pred) && is_Proj(get_Proj_pred(pred))
			   && is_Start(get_Proj_pred(get_Proj_pred(pred))));
		fprintf(file, " source=\"%d-%ld\" sourceport=\"0\"",
				get_irn_idx(source), get_Proj_proj(pred));
		break;
	}
	case iro_Call:  {
		fprintf(file, " source=\"%d\"", get_irn_idx(source));
		assert(is_Proj(pred) && is_Proj(get_Proj_pred(pred))
			   && is_Call(get_Proj_pred(get_Proj_pred(pred))));
		fprintf(file, " sourceport=\"%ld\"", get_Proj_proj(pred));
		break;
	}
	case iro_Load: {
		fprintf(file, " source=\"%d\"", get_irn_idx(source));
		if (is_Slice(pred)) {
			fprintf(file, " sourceport=\"%ld\"", get_Slice_from(pred)/ICORE_BC_WIDTH);
		} else if (is_Proj(pred)) {
			fprintf(file, " sourceport=\"0\"");
		} else {
			panic("cannot determine source port for Load");
		}
		break;
	}
	default:
		panic("cannot determine source port for op %s", get_id_str(get_irn_op(source)->name));
	}

	fprintf(file, ">\n");
}

static void emit_firm_op(ir_node *node)
{
	fprintf(file, DI "<data key=\"firmop\">%s</data>\n",  get_irn_opname(node));
}

static void emit_mem_edge(ir_node *target, ir_node *pred)
{
	if (is_NoMem(pred)) return;
	
	ir_node *source = get_source_node(pred);
	
	if (is_Start(source))
		return;

	fprintf(file, EI "<edge target=\"%d\" source=\"%d\">\n",
			get_irn_idx(target), get_irn_idx(source));
	emit_data_mode(pred);
	fprintf(file, EI "</edge>\n");
}

irg_walk_func emit_nodes;
void emit_nodes(ir_node *node, void *data)
{
  (void) data;

  switch(get_irn_opcode(node)) {

  case iro_Load:
  {
	  fprintf(file, NI "<node id=\"%d\">\n", get_irn_idx(node));
      emit_firm_op(node);
      fprintf(file, DI "<data key=\"label\">%s</data>\n",  "LoadStore");
	  /* int port = 0; */
	  /* for (int width = get_mode_size_bits(get_irn_mode(node)); width > -1; width-=ICORE_BC_WIDTH) { */
	  /* 	  emit_output_port(port++); */
	  /* } */
	  fprintf(file, NI "</node>\n");
	  emit_mem_edge(node, get_Load_mem(node));
	  break;
  }
  case iro_Store:
  {
	  fprintf(file, NI "<node id=\"%d\">\n", get_irn_idx(node));
      emit_firm_op(node);
      fprintf(file, DI "<data key=\"label\">%s</data>\n",  "LoadStore");
	  fprintf(file, NI "</node>\n");

	  ir_node *pred = get_Store_value(node);
	  if (is_Pack(pred)) {
		  foreach_irn_in(pred, idx, ppred) {
			  fprintf(file, "<edge target=\"%d\" targetport=\"%d\"",
					  get_irn_idx(node), idx);
			  emit_edge_attr(ppred);
			  fputs(EI "</edge>\n", file);
		  }
	  } else {
		  fprintf(file, EI "<edge target=\"%d\" targetport=\"0\"", get_irn_idx(node));
		  emit_edge_attr(pred);
		  emit_data_mode(pred);
		  fputs(EI "</edge>\n", file);
	  }

	  emit_mem_edge(node, get_Store_mem(node));
	  break;
  }
  case iro_Call:
    {
      fprintf(file, NI "<node id=\"%d\">\n", get_irn_idx(node));
      emit_firm_op(node);
      ir_node *address = get_Call_ptr(node);
      assert(is_Address(address));
      ir_entity *ent = get_Address_entity(address);
      fprintf(file, DI "<data key=\"label\">%s</data>\n",      get_entity_ld_name(ent));
      fprintf(file, NI "</node>\n");

	  foreach_Call_param(node, idx, pred) {
		  fprintf(file, EI "<edge target=\"%d\" targetport=\"%d\"", get_irn_idx(node), idx);
		  emit_edge_attr(pred);
		  emit_data_mode(pred);
		  fputs(EI "</edge>\n", file);
	  }
    }
    break;
  case iro_Return: {
	  foreach_Return_res(node, idx, pred) {
		  if (get_irn_mode(pred) == mode_M) continue;
		  fprintf(file, NI "<node id=\"%d-%d\">\n", get_irn_idx(node), idx);
		  emit_firm_op(node);
		  fprintf(file, DI "<data key=\"label\">GPR_output_r%d</data>\n", idx);
		  fprintf(file, NI "</node>\n");

		  fprintf(file, EI "<edge target=\"%d-%d\" targetport=\"0\"", get_irn_idx(node), idx);
		  emit_edge_attr(pred);
		  emit_data_mode(pred);
		  fputs(EI "</edge>\n", file);
	  }
	  break;
  }
  case iro_Start: {
	  ir_entity *ent = get_irg_entity(get_irn_irg(node));
	  ir_type *method = get_entity_type(ent);
	  for (size_t i = 0; i < get_method_n_params(method); i++) {
		  fprintf(file, NI "<node id=\"%d-%d\">\n", get_irn_idx(node), (int)i);
		  emit_firm_op(node);
		  fprintf(file, DI "<data key=\"label\">GPR_input_r%d</data>\n", (int)i);
		  fprintf(file, NI "</node>\n");
	  }
	  break;
  }
  case iro_Sync: {
	  fprintf(file, NI "<node id=\"%d\">\n", get_irn_idx(node));
	  fputs(DI "<data key=\"label\">Sync</data>\n", file);
	  emit_firm_op(node);
	  fprintf(file, NI "</node>\n");
	  foreach_irn_in(node, idx, pred) {
		  emit_mem_edge(node, pred);
	  }
	  break;
  }
  }
}

irg_walk_func emit_edges;
void emit_edges(ir_node *node, void *data)
{
	(void)data;
  foreach_irn_in(node, idx, pred) {
    fprintf(file, EI "<edge source=\"%d\" target=\"%d\">\n", get_irn_idx(node), get_irn_idx(pred));
	emit_data_mode(pred);
	fprintf(file, EI "</edge>\n");
  }
}

void si2xml(FILE *output, ir_graph *irg)
{
	file = output;

  assert(get_entity_additional_properties(get_irg_entity(irg))
	 & mtp_special_instruction);

  setlocale(LC_ALL, "");
  fprintf(file, "<?xml version=\"1.0\" encoding=\"%s\"?>\n",
	  nl_langinfo(CODESET));

  fprintf(file,
	  "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n"
	  "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
	  "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns\n"
	  "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n");

  fprintf(file, GI "<graph id=\"%s\" edgedefault=\"directed\">\n", get_entity_ld_name(get_irg_entity(irg)));

  fprintf(file, KI "<key attr.name=\"label\" attr.type=\"string\" for=\"node\" id=\"label\"/>\n");
  fprintf(file, KI "<key attr.name=\"firmop\" attr.type=\"string\" for=\"node\" id=\"firmop\"/>\n");
  fprintf(file, KI "<key attr.name=\"mode\" attr.type=\"string\" for=\"edge\" id=\"mode\"/>\n");
  fprintf(file, KI "<key attr.name=\"mode_is_data\" attr.type=\"boolean\" for=\"edge\" id=\"mode_is_data\"/>\n");

  irg_walk_graph(irg, emit_nodes, 0, 0);
  fprintf(file, GI "</graph></graphml>\n");
}

