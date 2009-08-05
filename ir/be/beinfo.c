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
 * @author      Matthias Braun
 * @version     $Id$
 */
#include "config.h"

#include "beinfo.h"
#include "bearch.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "error.h"

static copy_attr_func  old_phi_copy_attr;

void be_info_new_node(ir_node *node)
{
	struct obstack *obst;
	backend_info_t *info;
	sched_info_t   *sinfo;

	if (is_Anchor(node))
		return;

	obst  = get_irg_obstack(current_ir_graph);
	info  = obstack_alloc(obst, sizeof(*info));
	sinfo = &info->sched_info;

	memset(info, 0, sizeof(*info));
	sinfo->next = NULL;
	sinfo->prev = NULL;

	if (is_Phi(node)) {
		info->out_infos = NEW_ARR_D(reg_out_info_t, obst, 1);
		memset(info->out_infos, 0, 1 * sizeof(info->out_infos[0]));
	}
	assert(node->backend_info == NULL);
	node->backend_info = info;
}

static void new_Phi_copy_attr(const ir_node *old_node, ir_node *new_node)
{
	struct obstack *obst  = get_irg_obstack(get_irn_irg(new_node));
	backend_info_t *old_info = be_get_info(old_node);
	backend_info_t *new_info = be_get_info(new_node);

	old_phi_copy_attr(old_node, new_node);
	new_info->out_infos = DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
}

int be_info_equal(const ir_node *node1, const ir_node *node2)
{
	backend_info_t *info1 = be_get_info(node1);
	backend_info_t *info2 = be_get_info(node2);
	int             len   = ARR_LEN(info1->out_infos);
	int             i;

	if (ARR_LEN(info2->out_infos) != len)
		return 0;

	for (i = 0; i < len; ++i) {
		const reg_out_info_t *out1 = &info1->out_infos[i];
		const reg_out_info_t *out2 = &info2->out_infos[i];
		if (out1->reg != out2->reg)
			return 0;
		if (!reg_reqs_equal(out1->req, out2->req))
			return 0;
	}

	/* TODO: in reqs */

	return 1;
}

static void init_walker(ir_node *node, void *data)
{
	(void) data;
	be_info_new_node(node);
}

static int initialized = 0;

void be_info_init(void)
{
	if (initialized == 1)
		panic("double initialization of be_info");

	old_phi_copy_attr = op_Phi->ops.copy_attr;
	op_Phi->ops.copy_attr = new_Phi_copy_attr;
	initialized = 1;
}

void be_info_init_irg(ir_graph *irg)
{
	irg_walk_anchors(irg, init_walker, NULL, NULL);
}

void be_info_free(void)
{
	if (!initialized)
		panic("be_info_free called without prior init");

	assert(op_Phi->ops.copy_attr == new_Phi_copy_attr);
	op_Phi->ops.copy_attr = old_phi_copy_attr;
	initialized = 0;
}

int be_info_initialized(const ir_graph *irg)
{
	(void) irg;
	return initialized;
}
