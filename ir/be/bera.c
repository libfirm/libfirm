/**
 * Base routines for register allocation.
 * @author Sebastian Hack
 * @date 22.11.2004
 */

#include "pset.h"
#include "impl.h"

#include "irnode.h"
#include "irmode.h"
#include "irdom.h"

#include "bera_t.h"
#include "besched_t.h"
#include "belive_t.h"

FIRM_IMPL1(is_allocatable_irn, int, const ir_node *)

size_t ra_irn_data_offset = 0;
size_t ra_irg_data_offset = 0;

void be_ra_init(void)
{
	ra_irn_data_offset = register_additional_node_data(sizeof(ra_info_t));
	ra_irg_data_offset = register_additional_graph_data(sizeof(void *));
}

static INLINE int values_interfere_dom(const ir_node *a, const ir_node *b)
{
	const ir_node *b1 = get_nodes_block(a);
	const ir_node *b2 = get_nodes_block(b);
	int lo_a, lo_b;

	assert(block_dominates(b1, b2));

	/*
	 * if the two blocks are not equal, a and b can only interfere if a is
	 * live in at b2.
	 */
	if(b1 != b2 && !is_live_in(b2, a))
		return 0;

	lo_a = is_live_end(b2, a);
	lo_b = is_live_end(b2, b);

	/*
	 * If the two blocks are the same and one value is live out and the
	 * definition of the other is after the definition ov the live out
	 * value, they interfere.
	 */
	if(b1 == b2) {
		int pos_a = sched_get_time_step(a);
		int pos_b = sched_get_time_step(b);

		if((pos_a < pos_b && lo_a) || (pos_b < pos_a && lo_b))
			return 1;
	}

	/*
	 * Now it is left to check, if the sequence from the last use of 'b'
	 * (or the end of the block b2, if b is live out)
	 * to the def of 'b' contains a use and NOT the def of 'a'. Then they
	 * also interfere
	 */
	{
		const ir_node *irn;

		/* Initialize the liveness. */
		int a_live = lo_a;
		int b_live = lo_b;

		/* Go from the end of block b2 and try to detect the liveness. */
		sched_foreach_reverse(b2, irn) {
			int i, n;

			/*
			 * If the definition of 'a' was found 'a' and 'b' interfere, if
			 * 'b' is live here.
			 */
			if(irn == a)
				return b_live;

			/* Same goes for 'b'. */
			if(irn == b)
				return a_live;

			/* If 'a' is not yet live, search for a use. */
			if(!a_live) {
				for(i = 0, n = get_irn_arity(irn); i < n; ++i)
					if(get_irn_n(irn, i) == a) {
						a_live = 1;
						break;
				}
			}

			/* Same for 'b' */
			if(!b_live) {
				for(i = 0, n = get_irn_arity(irn); i < n; ++i)
					if(get_irn_n(irn, i) == b) {
						b_live = 1;
						break;
					}
			}

		}
	}

	assert(0 && "You may never reach this place");

	/* This is never reached */
	return 0;
}

int values_interfere(const ir_node *a, const ir_node *b)
{
	const ir_node *b1 = get_nodes_block(a);
	const ir_node *b2 = get_nodes_block(b);

	if(block_dominates(b1, b2))
		return values_interfere_dom(a, b);
	else if(block_dominates(b2, b1))
		return values_interfere_dom(b, a);
	else
		return 0;
}
