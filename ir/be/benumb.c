/**
 * Numbering implementation.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#include <stdlib.h>

#include "impl.h"

#include "irnode_t.h"
#include "irgwalk.h"
#include "xmalloc.h"

#include "be_t.h"
#include "benumb_t.h"

int numbering_irn_data_offset = 0;
int numbering_irg_data_offset = 0;

FIRM_IMPL1(get_irn_graph_nr, int, const ir_node *)
FIRM_IMPL1(get_irn_block_nr, int, const ir_node *)
FIRM_IMPL1(get_block_graph_nr, int, const ir_node *)
FIRM_IMPL1(get_block_node_count, int, const ir_node *)
FIRM_IMPL1(get_graph_block_count, int, const ir_graph *)
FIRM_IMPL2(get_irn_for_graph_nr, ir_node *, const ir_graph *, int)

static void numbering_walker(ir_node *irn, void *env)
{
	irg_numbering_t *graph_nr = get_irg_numbering(get_irn_irg(irn));
	numbering_t *irn_nr = get_irn_numbering(irn);

	if(is_Block(irn))
		irn_nr->global_nr = graph_nr->local_nr++;
	else {
		numbering_t *block_nr = get_irn_numbering(get_nodes_block(irn));
		irn_nr->local_nr = block_nr->local_nr++;
		irn_nr->global_nr = graph_nr->global_nr++;
	}
}

static void reverse_walker(ir_node *irn, void *env)
{
	if(!is_Block(irn)) {
		ir_node **map = env;
		map[get_irn_graph_nr(irn)] = irn;
	}
}

void be_numbering(ir_graph *irg)
{
	ir_node **reverse_map;

	irg_walk_graph(irg, numbering_walker, NULL, NULL);

	reverse_map = xcalloc(get_graph_node_count(irg), sizeof(reverse_map[0]));
	irg_walk_graph(irg, reverse_walker, NULL, reverse_map);

	get_irg_numbering(irg)->reverse_map = reverse_map;
}

void be_numbering_done(ir_graph *irg)
{
	free(get_irg_numbering(irg)->reverse_map);
}

void be_numbering_init(void)
{
	numbering_irn_data_offset = register_additional_node_data(sizeof(numbering_t));
	numbering_irg_data_offset = register_additional_graph_data(sizeof(irg_numbering_t));
}
