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

static int true_p(ir_node *sel, ir_node *mux_false, ir_node *mux_true)
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

    dump_ir_graph(irg, "-imported");

    firm_init_loop_opt();
    do_loop_unrolling(irg);
    dump_ir_graph(irg, "-unrolled");

    opt_if_conv_cb(irg, &true_p);
    dump_ir_graph(irg, "-if_conv");

    local_optimize_graph(irg);
    dump_ir_graph(irg, "-local_opts");

    optimize_cf(irg);
    dump_ir_graph(irg, "-cf");

    conv_opt(irg);

    irg2vhdl(out, irg);
  };
}

int main (int argc, char *argv[])
{
  if (argc != 3) {
    fprintf(stderr,"usage: ir2vhdl <irfile> <vhdlfile>\n");
    return 1;
  }

  ir_init();
  init_firm2vhdl();

  if (ir_import(argv[1])) {
    /* ir_import actually seems to call exit() instead of returning. */
    fprintf(stderr,"ir_import of file %s failed.\n", argv[1]);
    return 1;
  }

  irp2vhdl(argv[2]);

  return 0;
}
