/**
 * Numbering for nodes.
 * @author Sebastian Hack
 * @date 8.11.2004
 */

#ifndef __BENUMB_T_H
#define __BENUMB_T_H

#include "firm_config.h"

#include "benumb.h"

typedef struct _numbering_t {
	int local_nr;
	int global_nr;
} numbering_t;

typedef struct _irg_numbering_t {
	int local_nr;
	int global_nr;
	ir_node **reverse_map;
} irg_numbering_t;

extern int numbering_irn_data_offset;
extern int numbering_irg_data_offset;

#define _get_irn_numbering(mod,irn) get_irn_data(irn, mod numbering_t, numbering_irn_data_offset)
#define _get_irg_numbering(mod,irg) get_irg_data(irg, mod irg_numbering_t, numbering_irg_data_offset)

#define get_irn_numbering_const(irn) _get_irn_numbering(const, irn)
#define get_irg_numbering_const(irg) _get_irg_numbering(const, irg)

#define get_irn_numbering(irn) _get_irn_numbering( , irn)
#define get_irg_numbering(irg) _get_irg_numbering( , irg)

static INLINE int __get_irn_graph_nr(const ir_node *irn)
{
	assert(!is_Block(irn) && "No block expected here");
	return get_irn_numbering_const(irn)->global_nr;
}

static INLINE int __get_irn_block_nr(const ir_node *irn)
{
	assert(!is_Block(irn) && "No block expected here");
	return get_irn_numbering_const(irn)->local_nr;
}

static INLINE int __get_block_graph_nr(const ir_node *irn)
{
	assert(is_Block(irn) && "Block expected here");
	return get_irn_numbering_const(irn)->global_nr;
}

static INLINE int __get_block_node_count(const ir_node *irn)
{
	assert(is_Block(irn) && "Block expected here");
	return get_irn_numbering_const(irn)->local_nr;
}

static INLINE int __get_graph_block_count(const ir_graph *irg)
{
	return get_irg_numbering_const(irg)->local_nr;
}

static INLINE int __get_graph_node_count(const ir_graph *irg)
{
	return get_irg_numbering_const(irg)->global_nr;
}

static INLINE ir_node *__get_irn_for_graph_nr(const ir_graph *irg, int nr)
{
	ir_node **map = get_irg_numbering_const(irg)->reverse_map;
	assert(nr >= 0 && nr <= __get_graph_node_count(irg) && map[nr] != NULL);
	return map[nr];
}

#define get_irn_graph_nr(irn) 				__get_irn_graph_nr(irn)
#define get_irn_block_nr(irn) 				__get_irn_block_nr(irn)
#define get_block_graph_nr(irn) 			__get_block_graph_nr(irn)
#define get_block_node_count(irn) 		__get_block_node_count(irn)
#define get_graph_block_count(irg) 		__get_graph_block_count(irg)
#define get_graph_node_count(irg) 		__get_graph_node_count(irg)
#define get_irn_for_graph_nr(irg,nr)	__get_irn_for_graph_nr(irg,nr)

#endif
