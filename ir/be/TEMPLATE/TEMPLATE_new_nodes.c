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
 * @brief   This file implements the creation of the achitecture specific firm
 *          opcodes and the coresponding node constructors for the TEMPLATE
 *          assembler irg.
 * @version $Id$
 */
#include "config.h"

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "irvrfy_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "../bearch.h"

#include "TEMPLATE_nodes_attr.h"
#include "TEMPLATE_new_nodes.h"
#include "gen_TEMPLATE_regalloc_if.h"



/***********************************************************************************
 *      _                                   _       _             __
 *     | |                                 (_)     | |           / _|
 *   __| |_   _ _ __ ___  _ __   ___ _ __   _ _ __ | |_ ___ _ __| |_ __ _  ___ ___
 *  / _` | | | | '_ ` _ \| '_ \ / _ \ '__| | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \
 * | (_| | |_| | | | | | | |_) |  __/ |    | | | | | ||  __/ |  | || (_| | (_|  __/
 *  \__,_|\__,_|_| |_| |_| .__/ \___|_|    |_|_| |_|\__\___|_|  |_| \__,_|\___\___|
 *                       | |
 *                       |_|
 ***********************************************************************************/

/**
 * Dumps the register requirements for either in or out.
 */
static void dump_reg_req(FILE *F, ir_node *n, const arch_register_req_t **reqs,
                         int inout) {
	char *dir = inout ? "out" : "in";
	int   max = inout ? (int) arch_irn_get_n_outs(n) : get_irn_arity(n);
	char  buf[1024];
	int   i;

	memset(buf, 0, sizeof(buf));

	if (reqs) {
		for (i = 0; i < max; i++) {
			fprintf(F, "%sreq #%d =", dir, i);

			if (reqs[i]->type == arch_register_req_type_none) {
				fprintf(F, " n/a");
			}

			if (reqs[i]->type & arch_register_req_type_normal) {
				fprintf(F, " %s", reqs[i]->cls->name);
			}

			if (reqs[i]->type & arch_register_req_type_limited) {
				fprintf(F, " %s",
						arch_register_req_format(buf, sizeof(buf), reqs[i], n));
			}

			if (reqs[i]->type & arch_register_req_type_should_be_same) {
				const unsigned other = reqs[i]->other_same;
				int i;

				ir_fprintf(F, " same as");
				for (i = 0; 1U << i <= other; ++i) {
					if (other & (1U << i)) {
						ir_fprintf(F, " %+F", get_irn_n(n, i));
					}
				}
			}

			if (reqs[i]->type & arch_register_req_type_must_be_different) {
				const unsigned other = reqs[i]->other_different;
				int i;

				ir_fprintf(F, " different from");
				for (i = 0; 1U << i <= other; ++i) {
					if (other & (1U << i)) {
						ir_fprintf(F, " %+F", get_irn_n(n, i));
					}
				}
			}

			fprintf(F, "\n");
		}

		fprintf(F, "\n");
	} else {
		fprintf(F, "%sreq = N/A\n", dir);
	}
}


/**
 * Dumper interface for dumping TEMPLATE nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int TEMPLATE_dump_node(ir_node *n, FILE *F, dump_reason_t reason) {
  	ir_mode     *mode = NULL;
	int         bad  = 0;
	int         i, n_res, flags;
	const arch_register_req_t **reqs;

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			break;

		case dump_node_mode_txt:
			mode = get_irn_mode(n);

			if (mode) {
				fprintf(F, "[%s]", get_mode_name(mode));
			}
			else {
				fprintf(F, "[?NOMODE?]");
			}
			break;

		case dump_node_nodeattr_txt:

			/* TODO: dump some attributes which should show up */
			/* in node name in dump (e.g. consts or the like)  */

			break;

		case dump_node_info_txt:
			fprintf(F, "=== TEMPLATE attr begin ===\n");

			/* dump IN requirements */
			if (get_irn_arity(n) > 0) {
				reqs = get_TEMPLATE_in_req_all(n);
				dump_reg_req(F, n, reqs, 0);
			}

			n_res = arch_irn_get_n_outs(n);
			if (n_res > 0) {
				/* dump OUT requirements */
				reqs = get_TEMPLATE_out_req_all(n);
				dump_reg_req(F, n, reqs, 1);

				/* dump assigned registers */
				for (i = 0; i < n_res; i++) {
					const arch_register_t *reg = arch_irn_get_register(n, i);

					fprintf(F, "reg #%d = %s\n", i, reg ? arch_register_get_name(reg) : "n/a");
				}
				fprintf(F, "\n");
			}

			/* dump n_res */
			fprintf(F, "n_res = %d\n", n_res);

			/* dump flags */
			fprintf(F, "flags =");
			flags = arch_irn_get_flags(n);
			if (flags == arch_irn_flags_none) {
				fprintf(F, " none");
			}
			else {
				if (flags & arch_irn_flags_dont_spill) {
					fprintf(F, " unspillable");
				}
				if (flags & arch_irn_flags_rematerializable) {
					fprintf(F, " remat");
				}
				if (flags & arch_irn_flags_modify_flags) {
					fprintf(F, " modify_flags");
				}
			}
			fprintf(F, " (%d)\n", flags);

			/* TODO: dump all additional attributes */

			fprintf(F, "=== TEMPLATE attr end ===\n");
			/* end of: case dump_node_info_txt */
			break;
	}


	return bad;
}



/***************************************************************************************************
 *        _   _                   _       __        _                    _   _               _
 *       | | | |                 | |     / /       | |                  | | | |             | |
 *   __ _| |_| |_ _ __   ___  ___| |_   / /_ _  ___| |_   _ __ ___   ___| |_| |__   ___   __| |___
 *  / _` | __| __| '__| / __|/ _ \ __| / / _` |/ _ \ __| | '_ ` _ \ / _ \ __| '_ \ / _ \ / _` / __|
 * | (_| | |_| |_| |    \__ \  __/ |_ / / (_| |  __/ |_  | | | | | |  __/ |_| | | | (_) | (_| \__ \
 *  \__,_|\__|\__|_|    |___/\___|\__/_/ \__, |\___|\__| |_| |_| |_|\___|\__|_| |_|\___/ \__,_|___/
 *                                        __/ |
 *                                       |___/
 ***************************************************************************************************/

const TEMPLATE_attr_t *get_TEMPLATE_attr_const(const ir_node *node) {
	assert(is_TEMPLATE_irn(node) && "need TEMPLATE node to get attributes");
	return (const TEMPLATE_attr_t *)get_irn_generic_attr_const(node);
}

TEMPLATE_attr_t *get_TEMPLATE_attr(ir_node *node) {
	assert(is_TEMPLATE_irn(node) && "need TEMPLATE node to get attributes");
	return (TEMPLATE_attr_t *)get_irn_generic_attr(node);
}

/**
 * Returns the argument register requirements of a TEMPLATE node.
 */
const arch_register_req_t **get_TEMPLATE_in_req_all(const ir_node *node) {
	const TEMPLATE_attr_t *attr = get_TEMPLATE_attr_const(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an TEMPLATE node.
 */
const arch_register_req_t **get_TEMPLATE_out_req_all(const ir_node *node) {
	const TEMPLATE_attr_t *attr = get_TEMPLATE_attr_const(node);
	return attr->out_req;
}

/**
 * Returns the argument register requirement at position pos of an TEMPLATE node.
 */
const arch_register_req_t *get_TEMPLATE_in_req(const ir_node *node, int pos) {
	const TEMPLATE_attr_t *attr = get_TEMPLATE_attr_const(node);
	return attr->in_req[pos];
}

/**
 * Returns the result register requirement at position pos of an TEMPLATE node.
 */
const arch_register_req_t *get_TEMPLATE_out_req(const ir_node *node, int pos) {
	const TEMPLATE_attr_t *attr = get_TEMPLATE_attr_const(node);
	return attr->out_req[pos];
}

/**
 * Sets the OUT register requirements at position pos.
 */
void set_TEMPLATE_req_out(ir_node *node, const arch_register_req_t *req, int pos) {
	TEMPLATE_attr_t *attr   = get_TEMPLATE_attr(node);
	attr->out_req[pos] = req;
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_TEMPLATE_req_in(ir_node *node, const arch_register_req_t *req, int pos) {
	TEMPLATE_attr_t *attr  = get_TEMPLATE_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Initializes the nodes attributes.
 */
void init_TEMPLATE_attributes(ir_node *node, arch_irn_flags_t flags,
                              const arch_register_req_t **in_reqs,
                              const arch_register_req_t **out_reqs,
                              const be_execution_unit_t ***execution_units,
                              int n_res)
{
	ir_graph        *irg  = get_irn_irg(node);
	struct obstack  *obst = get_irg_obstack(irg);
	TEMPLATE_attr_t *attr = get_TEMPLATE_attr(node);
	backend_info_t  *info;
	(void) execution_units;

	arch_irn_set_flags(node, flags);
	attr->out_req = out_reqs;
	attr->in_req  = in_reqs;

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_D(reg_out_info_t, obst, n_res);
	memset(info->out_infos, 0, n_res * sizeof(info->out_infos[0]));
}

/***************************************************************************************
 *                  _                            _                   _
 *                 | |                          | |                 | |
 *  _ __   ___   __| | ___    ___ ___  _ __  ___| |_ _ __ _   _  ___| |_ ___  _ __ ___
 * | '_ \ / _ \ / _` |/ _ \  / __/ _ \| '_ \/ __| __| '__| | | |/ __| __/ _ \| '__/ __|
 * | | | | (_) | (_| |  __/ | (_| (_) | | | \__ \ |_| |  | |_| | (__| || (_) | |  \__ \
 * |_| |_|\___/ \__,_|\___|  \___\___/|_| |_|___/\__|_|   \__,_|\___|\__\___/|_|  |___/
 *
 ***************************************************************************************/

static
int TEMPLATE_compare_attr(ir_node *a, ir_node *b)
{
	const TEMPLATE_attr_t *attr_a = get_TEMPLATE_attr_const(a);
	const TEMPLATE_attr_t *attr_b = get_TEMPLATE_attr_const(b);
	(void) attr_a;
	(void) attr_b;

	return 0;
}


/* Include the generated constructor functions */
#include "gen_TEMPLATE_new_nodes.c.inl"
