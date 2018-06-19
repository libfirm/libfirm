/**
 * @file
 * @brief    Generate VHDL output from suitable IRGs.
 * @author   Andreas Seltenreich
 *
 */

#include <firm.h>
#include <adt/obstack.h>
#include "firm2vhdl.h"

#include <assert.h>
#include <stdlib.h>

struct env {
  FILE *file;
};


#define fatal(x) do { \
    ir_fprintf(stderr, "%n %N: %s\n", node, node, x);	\
  exit(1); } while (0)

#define warn(x) do { \
    ir_fprintf(stderr, "%n %N: %s\n", node, node, x);	\
  } while (0)

void init_firm2vhdl(void)
{
  init_vhdl_modes();
}

static int is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
  return (get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode));
}

/* Generate identifier for entity input signals from an argument position.
   TODO: make this user-supplyable. */
const char *get_input_name(int argument_number)
{
  const char static *signals[] = {
    "CONTROL",
    "INPUT0",
    "INPUT1",
    "INPUT2",
    "INPUT3",
    0
  };
  assert(argument_number < sizeof(signals)/sizeof(signals[0]));
  return signals[argument_number];
}

/* Generate VHDL identifier for entity output signals from an return
   parameter position.  TODO: make this user-supplyable. */
const char *get_output_name(int argument_number)
{
  const char static *signals[] = {
    /* "status", */
    "OUTPUT0",
    "OUTPUT1",
    "OUTPUT2",
    "OUTPUT3",
    0
  };
  assert(argument_number < sizeof(signals)/sizeof(signals[0]));
  return signals[argument_number];
}


static void emit_variables(ir_node *node, void *data)
{
  struct env *env = data;
  ir_mode *mode = get_irn_mode(node);

  if (mode == mode_b) {
    ir_fprintf(env->file, "\tvariable node%N : boolean;\t-- %n \n", node, node);
    return;
  }

  if (!mode_is_data(mode))
    return;

  assert(get_mode_arithmetic(mode) == irma_twos_complement);

  ir_fprintf(env->file, "\tvariable node%N : ", node);

  fprintf(env->file, mode_is_signed(mode) ? "signed" : "unsigned");
  fprintf(env->file, "(%u downto 0)" SIGNAL_INITIALIZER ";", get_mode_size_bits(mode)-1);
  ir_fprintf(env->file, " -- %n \n", node);
}

static const char *format_relation(ir_relation rel)
{
  switch(rel) {
  case ir_relation_equal:
    return "=";
  case ir_relation_less:
    return "<";
  case ir_relation_greater:
    return ">";
  case ir_relation_less_equal:
    return "<=";
  case ir_relation_greater_equal:
    return ">=";
  case ir_relation_less_greater:
    return "/=";
  default:
  case ir_relation_less_equal_greater:
  case ir_relation_unordered:
  case ir_relation_unordered_equal:
  case ir_relation_unordered_less:
  case ir_relation_unordered_less_equal:
  case ir_relation_unordered_greater:
  case ir_relation_unordered_greater_equal:
  case ir_relation_unordered_less_greater:
  case ir_relation_true:
  case ir_relation_false:
    return "unsupported_relation";
  }
}

static void emit_process(ir_node *node, void *data)
{
  struct env *env = data;
  ir_mode *mode = get_irn_mode(node);

  /* if (!(mode_is_data(mode)||mode_is_num(mode))) */
  /*   return; */

  switch (get_irn_opcode(node)) {

  case iro_Proj: {
    if (is_arg_Proj(node)) {
      long proj = get_Proj_num(node);
      ir_fprintf(env->file, "node%N := %s(%s);\t-- %n\n",
		 node,
		 mode_is_signed(mode) ? "signed" : "unsigned",
		 get_input_name(proj), node);
    } else if (is_x_regular_Proj(node)) {
      if (!is_Start(get_Proj_pred(node)))
	fatal("unsupported Proj");
    }
  }
    break;

  case iro_Return: {
    for (int n = 0; n < get_Return_n_ress(node); n++) {
      ir_fprintf(env->file, "%s <= std_logic_vector(node%N);\t-- %n\n",
		 get_output_name(n), get_Return_res(node, n), node);
    }
  }
    break;

#define BINOP(op) \
  ir_fprintf(env->file, "node%N := node%N " #op " node%N;\t-- %n\n", \
	     node, get_binop_left(node), get_binop_right(node), node)

  case iro_Add: BINOP(+);   break;
  case iro_Sub: BINOP(-);   break;
  case iro_And: BINOP(and); break;
  case iro_Or:  BINOP(or);  break;
  case iro_Eor: BINOP(xor); break;

#undef BINOP

  case iro_Mul:
    // Convert to unsigned to get the correct MSB of the result.
    // (VHDL preserves the sign bit when resizing signed values)
    ir_fprintf(env->file, "node%N := %s(std_logic_vector(resize(unsigned(std_logic_vector(node%N * node%N)), %u)));\t-- %n\n",
	       node,
               mode_is_signed(mode) ? "signed" : "unsigned",
               get_binop_left(node), get_binop_right(node),
	       get_mode_size_bits(mode), node);
    break;
  case iro_Not:
    ir_fprintf(env->file, "node%N := not node%N;\t-- %n\n",
	       node, get_Not_op(node), node);
      break;

  case iro_Minus:
    ir_fprintf(env->file, "node%N := - node%N;\t-- %n\n",
	       node, get_Minus_op(node), node);
      break;

  case iro_PinnedConst:
    ir_fprintf(env->file, "node%N := ", node);
    fprintf(env->file, "to_%s(%ld, %u);",
	    mode_is_signed(mode) ? "signed" : "unsigned",
            get_tarval_long(get_PinnedConst_tarval(node)),
	    get_mode_size_bits(mode));
    ir_fprintf(env->file, "\t-- %n\n", node);
    break;

  case iro_Mux:
    ir_fprintf(env->file, "if node%N then node%N := node%N; else node%N := node%N; end if;\t-- %n\n",
               get_Mux_sel(node),
               node,
               get_Mux_true(node),
               node,
               get_Mux_false(node),
	       node);
    break;

  case iro_Cmp:
    ir_fprintf(env->file, "node%N := node%N %s node%N;\t-- %n\n",
	       node,
	       get_Cmp_left(node),
	       format_relation(get_Cmp_relation(node)),
	       get_Cmp_right(node),
	       node);
    break;

  case iro_Shl:
    assert(is_PinnedConst(get_Shl_right(node)));
    ir_fprintf(env->file, "node%N := shift_left(unsigned(node%N), %u);\t-- %n\n",
	       node,
	       get_Shl_left(node),
	       get_tarval_long(get_PinnedConst_tarval(get_Shl_right(node))),
	       node);
    break;


  case iro_Shr:
    assert(is_PinnedConst(get_Shr_right(node)));
    ir_fprintf(env->file, "node%N := %s(shift_right(unsigned(node%N), %u));\t-- %n\n",
	       node,
	       mode_is_signed(mode) ? "signed" : "",
	       get_Shr_left(node),
	       get_tarval_long(get_PinnedConst_tarval(get_Shr_right(node))),
	       node);
    break;

  case iro_Shrs:
    assert(is_PinnedConst(get_Shrs_right(node)));
    ir_fprintf(env->file, "node%N := %s(shift_right(signed(node%N), %u));\t-- %n\n",
	       node,
	       mode_is_signed(mode) ? "" : "unsigned",
	       get_Shrs_left(node),
	       get_tarval_long(get_PinnedConst_tarval(get_Shrs_right(node))),
	       node);
    break;

  case iro_Conv: {
    ir_node *pred = get_Conv_op(node);
    ir_mode *pred_mode = get_irn_mode(pred);

    if (is_downconv(pred_mode, mode) && mode_is_signed(pred_mode)) {
      /* IEEE.numeric_std transplants the sign bit on downconv from a
	 signed type.  Avoid this case to maintain C semantics. */
      ir_fprintf(env->file, "node%N := %s(resize(unsigned(node%N),%u));\t-- %n\n",
		 node,
		 (mode_is_signed(mode) ? "signed" : ""),
		 pred,
		 get_mode_size_bits(mode),
		 node);
    } else {
      ir_fprintf(env->file, "node%N := %s(resize(node%N,%u));\t-- %n\n",
		 node,
		 (mode_is_signed(mode) == mode_is_signed(pred_mode)) ? "" :
		 (mode_is_signed(mode) ? "signed" : "unsigned "),
		 pred,
		 get_mode_size_bits(mode),
		 node);
    }
  }
    break;

  case iro_Block: {
    /* assert((get_irg_start_block(get_irn_irg(node)) == node) */
    /* 	   ||(get_irg_end_block(get_irn_irg(node)) == node) */
    /* 	   ||(get_irg_start_block(get_irn_irg(node)) == */
    /* 	      get_nodes_block(get_Block_cfgpred(node,0)))); */
  }
    break;

  case iro_Start:
  case iro_End:
    break;

  default: {
    warn("no emission");
  }
  }
}

static const char *get_irg_name(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	return get_entity_ld_name(entity);
}

void irg2vhdl(FILE *output, ir_graph *irg)
{
  struct env env;
  env.file = output;

  fprintf(env.file, "architecture %s of test_atom is\n", get_irg_name(irg));
  fprintf(env.file, "begin\n");
  fprintf(env.file, "process (clk)\n", get_irg_name(irg));
  irg_walk_blkwise_graph(irg, 0, emit_variables, &env);
  fprintf(env.file, "begin\n");
  irg_walk_blkwise_graph(irg, 0, emit_process, &env);
  fprintf(env.file, "end process;\n");
  fprintf(env.file, "end %s;\n", get_irg_name(irg));
}
