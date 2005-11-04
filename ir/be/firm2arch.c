/**
 * Dumps Firm into CPU specific assembler format (the concrete
 * implementations can be found in the <arch> subdirectories)
 * @author Christian Wuerdig
 * @date 18.10.2005
 * @version $Id$
 */

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "debug.h"

#include "firm2arch.h"

/**
 * Transforms all irgs into assembler irgs.
 * Calls the interface function transform_node() which
 * needs to be implemented for each architecture.
 */
void transform_firm(ir_graph *irg) {
  firm_dbg_module_t *dbg = firm_dbg_register("be.transform");
  if (! is_pseudo_ir_graph(irg))
    irg_walk_blkwise_graph(irg, NULL, transform_node, dbg);
}

/**
 * Finishes the firm transformation. This function is called
 * after register allocation and scheduling to build everything
 * which can only be build after those phases e.g. function prolog
 * and epilog.
 * Calls the interface function finish_node_transformation which needs
 * to be implemented for each architecture.
 */
void finish_transform(ir_graph *irg) {
  if (! is_pseudo_ir_graph(irg)) {
 //   irg_walk_blkwise_graph(irg, NULL, finish_node_transformation, NULL);
  }
}

/**
 * Generates the architecture specific assembler code.
 * Calls the interface functions firmbe_gen_decls() (dumps all global
 * decls) and firmbe_gen_routine() (generates code for all routines) which
 * need to be implemented for each architecture.
 */
void firmbe_gen_code(FILE *out) {
  int i;

//  firmbe_gen_decls(out);
  for (i = 0; i < get_irp_n_irgs(); ++i) {
    ir_graph *irg = get_irp_irg(i);

//    if (! is_pseudo_ir_graph(irg))
 //     firmbe_gen_routine(out, irg);
  }
}
