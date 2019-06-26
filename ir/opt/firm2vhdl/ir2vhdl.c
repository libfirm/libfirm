/**
 * @file
 * @brief    A tool to import IR and feed it to firm2vhdl.
 * @author   Andreas Seltenreich
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <firm.h>
#include "firm2vhdl.h"
#include "lower-for-vhdl.h"

static int true_p(ir_node const *sel, ir_node const *mux_false, ir_node const *mux_true)
{
	return 1;
}

static void irp2vhdl(char *filename)
{
	FILE *out = fopen(filename, "w");

	if (!out) {
		perror("Cannot open VHDL output file");
		exit(2);
	}

	if (EOF == fputs("library IEEE;\n"
	                 "use IEEE.STD_LOGIC_1164.ALL;\n"
	                 "use IEEE.NUMERIC_STD.ALL;\n\n",
	                 out)) {
		perror("Cannot write to VHDL output file");
		exit(2);
	}

	for (size_t i = 0; i < get_irp_n_irgs(); ++i) {
		ir_graph *irg = get_irp_irg(i);

		/* Rename the test_atom entity to allow concurrent build. */
		if (strchr(filename, '.'))
			*strchr(filename, '.') = '\0';
		if (! strcmp("test_atom", get_entity_ld_name(get_irg_entity(irg))))
			set_entity_ld_ident(get_irg_entity(irg), new_id_from_str(filename));

		dump_ir_graph(irg, "imported");

		// Optimizations

		firm_init_loop_opt();
		do_loop_unrolling(irg);
		dump_ir_graph(irg, "unrolled");

		opt_if_conv_cb(irg, &true_p);
		dump_ir_graph(irg, "if_conv");

		local_optimize_graph(irg);
		dump_ir_graph(irg, "local_opts");

		optimize_cf(irg);
		dump_ir_graph(irg, "cf");

		conv_opt(irg);
		dump_ir_graph(irg, "conv");

		lower_for_vhdl(irg);
		assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
		irg2vhdl(out, irg);
	};
}

void irg2vhdl(ir_graph *irg)
{
	char *entity_name = get_entity_ld_name(get_irg_entity(irg));
	char filename[1024];
	snprintf(filename, sizeof filename, "%s%s", entity_name, ".vhd");
	FILE *out = fopen(filename, "w");

	if (!out) {
		panic("Cannot open VHDL output file");
		//TODO
	}

	int result = fprintf(out, "library IEEE;\n"
	                          "use IEEE.STD_LOGIC_1164.ALL;\n"
	                          "use IEEE.NUMERIC_STD.ALL;\n\n"
	                          "entity %s_ent is\n"
	                          "\tport(\n"
	                          "\t\tcontrol : in  std_logic_vector(7 downto 0);\n"
	                          "\t\tclk     : in  std_logic;\n"
	                          "\t\tinput0  : in  std_logic_vector(31 downto 0);\n"
	                          "\t\tinput1  : in  std_logic_vector(31 downto 0);\n"
	                          "\t\toutput0 : out std_logic_vector(31 downto 0);\n"
	                          "\t\tstart   : in  std_logic;\n"
	                          "\t\tready   : out std_logic\n"
	                          "\t\t);\n"
	                          "\n"
	                          "\t--attribute mult_style         : string;\n"
	                          "\t--attribute mult_style of Atom : entity is \"lut\";\n"
	                          "\t---- alternative mult_styles are: {auto|block|lut|pipe_lut|CSD|KCM}\n"
	                          "end %s_ent;\n\n", entity_name, entity_name);
	if (result < 0) {
		panic("Cannot write to VHDL output file");
		//TODO
	}

	lower_for_vhdl(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	generate_architecture(out, irg);
	fclose(out);
}
