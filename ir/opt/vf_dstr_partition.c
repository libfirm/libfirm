/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Weaken or strengthen VFirm graphs for Firm construction.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "vf_dstr_partition.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "xmalloc.h"
#include "obstack.h"
#include "irgmod.h"

#define WF_DEBUG_PARTITION 1

typedef struct obstack obstack;

struct vp_info {
	obstack  obst;
	vl_info *vli;
	plist_t *loops;
};

vp_info *vp_partition(ir_graph *irg)
{
	vl_theta_it     it;
	vl_eta_invar_it it_invar;
	vl_eta_theta_it it_theta;

	int       index;
	ir_node  *eta;
	vp_info  *vpi       = XMALLOC(vp_info);
	int       had_edges = edges_assure(irg);
	ir_node **loops;

	/* Setup restore information. */
	obstack_init(&vpi->obst);
	vpi->loops = plist_obstack_new(&vpi->obst);

	/* Obtain loop info. */
	vpi->vli = vl_init(irg);

	/* Construct loop nodes. */
	loops = OALLOCN(&vpi->obst, ir_node*, vl_get_eta_count(vpi->vli));
	index = 0;

	foreach_vl_eta(vpi->vli, eta, it) {
		vl_edge  edge;
		ir_node *loop;
		ir_node *block = get_nodes_block(eta);
		ir_mode *mode  = get_irn_mode(eta);

		/* Obtain memory for the invariant nodes. */
		int       count = 0, i;
		ir_node **invs  = NULL;

		/* Weaken edges that leave the loop. */
		foreach_vl_eta_invar(vpi->vli, eta, edge, it_invar) {
			/* Ignore const nodes, we don't need their conds. */
			if (is_Const(edge.dst)) continue;
			count++;
		}

		/* Now collect the nodes for the loop node. */
		invs = OALLOCN(&vpi->obst, ir_node*, count);
		i    = 0;

		foreach_vl_eta_invar(vpi->vli, eta, edge, it_invar) {
			/* Same as above. */
			if (is_Const(edge.dst)) continue;
			invs[i] = edge.dst; i++;
		}

#if WF_DEBUG_PARTITION
		printf("Building loop node for eta %ld.\n", get_irn_node_nr(eta));
#endif

		/* Create a loop node and reroute the eta. */
		/* TODO: validation code for loop. */
		loop = new_r_Loop(block, count, invs, mode, eta, NULL);
		set_Loop_next(loop, loop);
		loops[index] = loop;
		index++;

		obstack_free(&vpi->obst, invs);
	}

	/* First make all the loop nodes and then reroute. This is important, since
	 * a newly created loop node has edge to all invariants and if an eta that
	 * is already rerouted is also an invariant, we would end up with a loop to
	 * eta edge that doesn't decouple the etas loop. */
	index = 0;

	foreach_vl_eta(vpi->vli, eta, it) {
		ir_node *loop = loops[index];

		keep_alive(eta);
		edges_reroute(eta, loop, irg);

		/* Store the loop for restore. */
		assert(!plist_find_value(vpi->loops, loop));
		plist_insert_back(vpi->loops, loop);

		index++;
	}

	obstack_free(&vpi->obst, loops);

	/* Weaken the thetas next inputs AFTER creating the loop nodes. If we did
	 * this the other way round, rerouting the edges won't have an effect on
	 * the weak edge and when processing the next graph later, the loop won't
	 * be recognized, leaving the unprocessed eta node behind. */

	foreach_vl_eta(vpi->vli, eta, it) {
		ir_node *theta;

		/* Weaken next inputs on thetas. */
		foreach_vl_eta_theta(vpi->vli, eta, theta, it_theta) {
			ir_node *next = get_Theta_next(theta);

			/* We may iterate one theta multiple times. */
			if (!is_Weak(next)) {
				ir_mode *mode  = get_irn_mode(theta);
				ir_node *block = get_nodes_block(theta);
				ir_node *weak  = new_r_Weak(block, mode, next);
				set_Theta_next(theta, weak);
			}
		}
	}

	if (!had_edges) edges_deactivate(irg);

	return vpi;
}

void vp_combine(vp_info *vpi)
{
	plist_element_t *it;

	/* Exchange loop nodes by their etas value. */
	foreach_plist(vpi->loops, it) {
		vl_eta_theta_it it_theta;

		ir_node *loop  = plist_element_get_value(it);
		ir_node *eta   = get_Loop_eta(loop);
		ir_node *value = get_Eta_value(eta);
		ir_node *theta;

		exchange(loop, value);

		/* Replace the weaks on the theta nexts by their target. */
		foreach_vl_eta_theta(vpi->vli, eta, theta, it_theta) {
			ir_node *weak = get_Theta_next(theta);

			printf(
				"eta %ld, theta %ld.\n",
				get_irn_node_nr(eta), get_irn_node_nr(theta)
			);

			if (is_Weak(weak)) {
				assert(!is_Weak(get_Weak_target(weak)));
				set_Theta_next(theta, get_Weak_target(weak));
			}
		}
	}
}

void vp_free(vp_info *vpi)
{
	vl_free(vpi->vli);
	plist_free(vpi->loops);
	obstack_free(&vpi->obst, NULL);
	xfree(vpi);
}

vl_info *vp_get_vl_info(vp_info *vpi)
{
	return vpi->vli;
}
