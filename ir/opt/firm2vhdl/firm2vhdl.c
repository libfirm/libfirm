/**
 * @file
 * @brief    Generate VHDL output from suitable IRGs.
 * @author   Andreas Seltenreich
 *
 */

#include <firm.h>
#include <adt/obst.h>
#include <adt/obstack.h>
#include <adt/pmap.h>
#include "firm2vhdl.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

struct env {
	FILE *file;
};

struct block_env {
	FILE *file;
	ir_node *block;
	struct obstack variable_obst;
	struct obstack process_obst;
	pmap *phis_assigned;
	bool valid;
};


#define fatal(x)                                                  \
	do {                                                      \
		ir_fprintf(stderr, "%n %N: %s\n", node, node, x); \
		exit(1);                                          \
	} while (0)

#define warn(x)                                                   \
	do {                                                      \
		ir_fprintf(stderr, "%n %N: %s\n", node, node, x); \
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
const char *get_vhdl_output_name(int argument_number)
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


static void emit_phi_signals(ir_node *node, void *data)
{
	struct env *env = data;
	ir_mode *mode = get_irn_mode(node);

	if (is_Phi(node) && mode != mode_M) {
		ir_fprintf(env->file, "\tsignal phi%N : ", node);
		fprintf(env->file, mode_is_signed(mode) ? "signed" : "unsigned");
		fprintf(env->file, "(%u downto 0)" SIGNAL_INITIALIZER ";", get_mode_size_bits(mode)-1);
		ir_fprintf(env->file, "-- %+F \n", node);
	}
}

static void emit_exec_signals(ir_node *node, void *data)
{
	struct env *env = data;

	if (is_Block(node)) {
		ir_fprintf(env->file, "\tsignal exec%N : std_logic := '0';\n", node);
	}
}

static void emit_start(FILE *file, ir_graph *irg)
{
	fprintf(file, "\t\tif START = '1' then\n");
	ir_fprintf(file, "\t\t\texec%N <= '1';\n", get_irg_start_block(irg));
	fprintf(file, "\t\t\tREADY <= '0';\n");
	fprintf(file, "\t\tend if;\n");
}

static void emit_variable(ir_node *node, void *data)
{
	struct env *env = (struct env*)data;

	ir_mode *mode = get_irn_mode(node);

	if (mode == mode_b) {
		ir_fprintf(env->file, "\tvariable node%N : boolean;\t-- %+F \n", node, node);
		return;
	}

	if (!mode_is_data(mode))
		return;

	assert(get_mode_arithmetic(mode) == irma_twos_complement);

	ir_fprintf(env->file, "\tvariable node%N : ", node);
	   fprintf(env->file, mode_is_signed(mode) ? "signed" : "unsigned");
	   fprintf(env->file, "(%u downto 0)" SIGNAL_INITIALIZER ";", get_mode_size_bits(mode)-1);
	ir_fprintf(env->file, " -- %+F \n", node);
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

static void finish_block(struct block_env *env)
{
	if (env->valid) {
		ir_fprintf(env->file, "\n\t\tif exec%N = '1' then -- %+F\n", env->block, env->block);

		obstack_1grow(&env->variable_obst, '\0');
		char *variables = obstack_finish(&env->variable_obst);
		fprintf(env->file, "%s", variables);

		ir_fprintf(env->file, "\t\t\texec%N <= '0';\n", env->block);

		obstack_1grow(&env->process_obst, '\0');
		char *process = obstack_finish(&env->process_obst);
		fprintf(env->file, "%s", process);

		fprintf(env->file, "\t\tend if;\n");

		obstack_free(&env->variable_obst, NULL);
		obstack_free(&env->process_obst, NULL);
		pmap_destroy(env->phis_assigned);
	}
}

static void emit_process(ir_node *node, void *data)
{
	struct block_env *env = data;
	ir_mode *mode = get_irn_mode(node);

	// env may be invalid if we are only just visiting the start block
	assert(is_Block(node) || env->valid);

	struct obstack *obst = &env->process_obst;

	switch (get_irn_opcode(node)) {

	case iro_Proj: {
		if (is_arg_Proj(node)) {
			long proj = get_Proj_num(node);
			ir_obst_printf(obst, "\t\t\tnode%N := %s(%s);\t-- %+F\n",
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
			ir_obst_printf(obst, "\t\t\t%s <= std_logic_vector(node%N);\t-- %+F\n",
			               get_vhdl_output_name(n), get_Return_res(node, n), node);
			ir_obst_printf(obst, "\t\t\tREADY <= '1';\n");
		}
	}
		break;

#define BINOP(op) \
		ir_obst_printf(obst, "\t\t\tnode%N := node%N " #op " node%N;\t-- %+F\n", \
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
		ir_obst_printf(obst, "\t\t\tnode%N := %s(std_logic_vector(resize(unsigned(std_logic_vector(node%N * node%N)), %u)));\t-- %+F\n",
		               node,
		               mode_is_signed(mode) ? "signed" : "unsigned",
		               get_binop_left(node), get_binop_right(node),
		               get_mode_size_bits(mode), node);
		break;
	case iro_Not:
		ir_obst_printf(obst, "\t\t\tnode%N := not node%N;\t-- %+F\n",
		               node, get_Not_op(node), node);
		break;

	case iro_Minus:
		ir_obst_printf(obst, "\t\t\tnode%N := - node%N;\t-- %+F\n",
		               node, get_Minus_op(node), node);
		break;

	case iro_PinnedConst:
		ir_obst_printf(obst, "\t\t\tnode%N := ", node);
		obstack_printf(obst, "to_%s(%ld, %u);",
		               mode_is_signed(mode) ? "signed" : "unsigned",
		               get_tarval_long(get_PinnedConst_tarval(node)),
		               get_mode_size_bits(mode));
		ir_obst_printf(obst, "\t-- %+F\n", node);
		break;

	case iro_Mux:
		ir_obst_printf(obst, "\t\t\tif node%N then node%N := node%N; else node%N := node%N; end if;\t-- %+F\n",
		               get_Mux_sel(node),
		               node,
		               get_Mux_true(node),
		               node,
		               get_Mux_false(node),
		               node);
		break;

	case iro_Cmp:
		ir_obst_printf(obst, "\t\t\tnode%N := node%N %s node%N;\t-- %+F\n",
		               node,
		               get_Cmp_left(node),
		               format_relation(get_Cmp_relation(node)),
		               get_Cmp_right(node),
		               node);
		break;

	case iro_Shl:
		if (is_PinnedConst(get_Shl_right(node))) {
			ir_obst_printf(obst, "\t\t\tnode%N := shift_left(unsigned(node%N), %u);\t-- %+F\n",
			               node,
			               get_Shl_left(node),
			               get_tarval_long(get_PinnedConst_tarval(get_Shl_right(node))),
			               node);
		} else {
			ir_obst_printf(obst, "\t\t\tnode%N := barrel_left(unsigned(node%N), node%N);\t-- %+F\n",
			               node,
			               get_Shl_left(node),
			               get_Shl_right(node),
			               node);
		}
		break;


	case iro_Shr:
		if (is_PinnedConst(get_Shr_right(node))) {
			ir_obst_printf(obst, "\t\t\tnode%N := %s(shift_right(unsigned(node%N), %u));\t-- %+F\n",
			               node,
			               mode_is_signed(mode) ? "signed" : "",
			               get_Shr_left(node),
			               get_tarval_long(get_PinnedConst_tarval(get_Shr_right(node))),
			               node);
		} else {
			ir_obst_printf(obst, "\t\t\tnode%N := %s(barrel_unsigned_right(unsigned(node%N), node%N));\t-- %+F\n",
			               node,
			               mode_is_signed(mode) ? "signed" : "",
			               get_Shr_left(node),
			               get_Shr_right(node),
			               node);
		}
		break;

	case iro_Shrs:
		if (is_PinnedConst(get_Shrs_right(node))) {
			ir_obst_printf(obst, "\t\t\tnode%N := %s(shift_right(signed(node%N), %u));\t-- %+F\n",
			               node,
			               mode_is_signed(mode) ? "" : "unsigned",
			               get_Shrs_left(node),
			               get_tarval_long(get_PinnedConst_tarval(get_Shrs_right(node))),
			               node);
		} else {
			ir_obst_printf(obst, "\t\t\tnode%N := %s(barrel_signed_right(signed(node%N), node%N));\t-- %+F\n",
			               node,
			               mode_is_signed(mode) ? "" : "unsigned",
			               get_Shrs_left(node),
			               get_Shrs_right(node),
			               node);
		}
		break;

	case iro_Conv: {
		ir_node *pred = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (is_downconv(pred_mode, mode) && mode_is_signed(pred_mode)) {
			/* IEEE.numeric_std transplants the sign bit on downconv from a
			   signed type.  Avoid this case to maintain C semantics. */
			ir_obst_printf(obst, "\t\t\tnode%N := %s(resize(unsigned(node%N),%u));\t-- %+F\n",
			               node,
			               (mode_is_signed(mode) ? "signed" : ""),
			               pred,
			               get_mode_size_bits(mode),
			               node);
		} else {
			ir_obst_printf(obst, "\t\t\tnode%N := %s(resize(node%N,%u));\t-- %+F\n",
			               node,
			               (mode_is_signed(mode) == mode_is_signed(pred_mode)) ? "" :
			               (mode_is_signed(mode) ? "signed" : "unsigned "),
			               pred,
			               get_mode_size_bits(mode),
			               node);
		}
	}
		break;

	case iro_Phi: {
		if (get_irn_mode(node) != mode_M) {
			ir_obst_printf(obst, "\t\t\tnode%N := phi%N;\t-- %+F\n", node, node, node);
		}
	}
		break;

	case iro_Jmp: {
		assert(get_irn_n_outs(node) == 1);
		ir_node *next_block = get_irn_out(node, 0);
		ir_obst_printf(obst, "\t\t\texec%N <= '1';\n", next_block);
	}
		break;

	case iro_Cond: {
		ir_node *cmp = get_Cond_selector(node);
		ir_node *block_true = NULL;
		ir_node *block_false = NULL;

		for (unsigned i = 0; i < get_irn_n_outs(node); i++) {
			ir_node *proj = get_irn_out(node, i);
			unsigned pn = get_Proj_num(proj);

			assert(get_irn_n_outs(proj) == 1);
			ir_node *next_block = get_irn_out(proj, 0);

			if (pn == pn_Cond_true) {
				block_true = next_block;
			} else if (pn == pn_Cond_false) {
				block_false = next_block;
			} else {
				fatal("Unexpected Proj(Cond)");
			}
		}

		assert(block_true && block_false);

		ir_obst_printf(obst, "\t\t\tif node%N then -- %+F\n", cmp, node);
		ir_obst_printf(obst, "\t\t\t\texec%N <= '1';\n", block_true);
		ir_obst_printf(obst, "\t\t\telse\n");
		ir_obst_printf(obst, "\t\t\t\texec%N <= '1';\n", block_false);
		ir_obst_printf(obst, "\t\t\tend if;\n");
	}
		break;

	case iro_Block: {
		finish_block(env);

		/* Set up this block */
		env->block = node;
		obstack_init(&env->variable_obst);
		obstack_init(&env->process_obst);
		env->phis_assigned = pmap_create();
		env->valid = true;
	}

	case iro_Start:
	case iro_End:
		break;

	default: {
		fatal("no emission");
	}
	}

	// See if we need to set a Phi in a block ahead
	// Only do this for data modes, not b or X.
	if (mode_is_int(mode)) {
		for (unsigned i = 0; i < get_irn_n_outs(node); i++) {
			ir_node *user = get_irn_out(node, i);

			if (is_Phi(user)) {
				if (!pmap_contains(env->phis_assigned, user)) {
					pmap_insert(env->phis_assigned, user, node);
					ir_obst_printf(obst, "\t\t\tphi%N <= node%N;\n", user, node);
				}
				// We may see one Phi node multiple times, but only as a user
				// of the same value
				assert(pmap_get(ir_node, env->phis_assigned, user) == node);
			}
		}
	}
}

static const char *get_irg_name(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	return get_entity_ld_name(entity);
}

void generate_architecture(FILE *output, ir_graph *irg)
{
	struct env env;
	env.file = output;

	fprintf(env.file, "architecture %s of %s_ent is\n", get_irg_name(irg), get_irg_name(irg));
	irg_walk_graph(irg, 0, emit_phi_signals, &env);
	irg_walk_graph(irg, 0, emit_exec_signals, &env);
	fprintf(env.file,
	        "\n"
	        "\tfunction barrel_left(x : unsigned; n : unsigned) return unsigned is\n"
	        "\t\tvariable xvec : std_logic_vector(x'length-1 downto 0);\n"
	        "\t\tvariable nvec : std_logic_vector(4 downto 0);\n"
	        "\t\tvariable result : std_logic_vector(31 downto 0);\n"
	        "\tbegin\n"
	        "\t\txvec := std_logic_vector(x);\n"
	        "\t\tnvec := std_logic_vector(n(4 downto 0));\n"
	        "\t\tresult := xvec;\n"
	        "\t\tif nvec(4) = '1' then\n"
	        "\t\t\tresult := result(15 downto 0) & \"0000000000000000\";\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(3) = '1' then\n"
	        "\t\t\tresult := result(23 downto 0) & \"00000000\";\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(2) = '1' then\n"
	        "\t\t\tresult := result(27 downto 0) & \"0000\";\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(1) = '1' then\n"
	        "\t\t\tresult := result(29 downto 0) & \"00\";\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(0) = '1' then\n"
	        "\t\t\tresult := result(30 downto 0) & \"0\";\n"
	        "\t\tend if;\n"
	        "\t\treturn unsigned(result);\n"
	        "\tend barrel_left;\n");
	fprintf(env.file,
	        "\n"
	        "\tfunction barrel_unsigned_right(x : unsigned; n : unsigned) return unsigned is\n"
	        "\t\tvariable xvec : std_logic_vector(x'length-1 downto 0);\n"
	        "\t\tvariable nvec : std_logic_vector(4 downto 0);\n"
	        "\t\tvariable result : std_logic_vector(31 downto 0);\n"
	        "\tbegin\n"
	        "\t\txvec := std_logic_vector(x);\n"
	        "\t\tnvec := std_logic_vector(n(4 downto 0));\n"
	        "\t\tresult := xvec;\n"
	        "\t\tif nvec(4) = '1' then\n"
	        "\t\t\tresult := \"0000000000000000\" & result(31 downto 16);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(3) = '1' then\n"
	        "\t\t\tresult := \"00000000\" & result(31 downto 8);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(2) = '1' then\n"
	        "\t\t\tresult := \"0000\" & result(31 downto 4);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(1) = '1' then\n"
	        "\t\t\tresult := \"00\" & result(31 downto 2);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(0) = '1' then\n"
	        "\t\t\tresult := \"0\" & result(31 downto 1);\n"
	        "\t\tend if;\n"
	        "\t\treturn unsigned(result);\n"
	        "\tend barrel_unsigned_right;\n");
	fprintf(env.file,
	        "\n"
	        "\tfunction barrel_signed_right(x : signed; n : unsigned) return signed is\n"
	        "\t\tvariable xvec : std_logic_vector(x'length-1 downto 0);\n"
	        "\t\tvariable nvec : std_logic_vector(4 downto 0);\n"
	        "\t\tvariable result : std_logic_vector(31 downto 0);\n"
	        "\t\tvariable signbit : std_logic_vector(31 downto 0);\n"
	        "\tbegin\n"
	        "\t\txvec := std_logic_vector(x);\n"
	        "\t\tnvec := std_logic_vector(n(4 downto 0));\n"
	        "\t\tresult := xvec;\n"
	        "\t\tsignbit := (others => xvec(x'length-1));\n"
	        "\t\tif nvec(4) = '1' then\n"
	        "\t\t\tresult := signbit(15 downto 0) & result(31 downto 16);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(3) = '1' then\n"
	        "\t\t\tresult := signbit(7 downto 0) & result(31 downto 8);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(2) = '1' then\n"
	        "\t\t\tresult := signbit(3 downto 0) & result(31 downto 4);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(1) = '1' then\n"
	        "\t\t\tresult := signbit(1 downto 0) & result(31 downto 2);\n"
	        "\t\tend if;\n"
	        "\t\tif nvec(0) = '1' then\n"
	        "\t\t\tresult := signbit(0 downto 0) & result(31 downto 1);\n"
	        "\t\tend if;\n"
	        "\t\treturn signed(result);\n"
	        "\tend barrel_signed_right;\n");

	fprintf(env.file, "begin\n");

	fprintf(env.file, "\nprocess (clk)\n");
	irg_walk_blkwise_graph(irg, 0, emit_variable, &env);
	fprintf(env.file, "begin\n");
	fprintf(env.file, "\tif rising_edge(clk) then\n");

	emit_start(output, irg);

	struct block_env blkenv;
	blkenv.file = output;
	blkenv.block = NULL;
	blkenv.valid = false;
	irg_walk_blkwise_graph(irg, 0, emit_process, &blkenv);

	fprintf(env.file, "\tend if;\n");
	fprintf(env.file, "end process;\n");

	fprintf(env.file, "end %s;\n", get_irg_name(irg));
}
