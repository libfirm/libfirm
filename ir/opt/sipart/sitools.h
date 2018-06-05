#include <firm.h>
#include <irnodeset.h>

#define DB1(...) 	DB((dbg,LEVEL_1,__VA_ARGS__))
#define DB2(...) 	DB((dbg,LEVEL_2,__VA_ARGS__))
#define DB3(...) 	DB((dbg,LEVEL_3,__VA_ARGS__))

irg_walk_func unpin_loadstore;
irg_walk_func collect_deps_walker;
irg_walk_func uncontract_walker;

/* Move loop increment (more precisely: any small computation the CF
   depends on) from block to a newly created dominating block. */
bool expell_loop_increment(ir_node *block);

/**
 * Lower bit operations into slice and pack nodes.
 * @param irg The graph to perform the lowering on.
 */
void lower_slice_pack(ir_graph *irg);
