/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief  This file implements the creation of the achitecture specific firm
 *         opcodes and the coresponding node constructors for the ppc assembler
 *         irg.
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "firm_common_t.h"
#include "irvrfy_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "../bearch_t.h"

#include "ppc32_nodes_attr.h"
#include "ppc32_new_nodes.h"
#include "gen_ppc32_regalloc_if.h"



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
static void dump_reg_req(FILE *F, ir_node *n, const arch_register_req_t **reqs, int inout) {
	char *dir = inout ? "out" : "in";
	int   max = inout ? get_ppc32_n_res(n) : get_irn_arity(n);
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
				ir_fprintf(F, " same as %+F", get_irn_n(n, reqs[i]->other_same));
			}

			if (reqs[i]->type & arch_register_req_type_should_be_different) {
				ir_fprintf(F, " different from %+F", get_irn_n(n, reqs[i]->other_different));
			}

			fprintf(F, "\n");
		}

		fprintf(F, "\n");
	} else {
		fprintf(F, "%sreq = N/A\n", dir);
	}
}


/**
 * Dumper interface for dumping ppc32 nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int ppc32_dump_node(ir_node *n, FILE *F, dump_reason_t reason) {
  	ir_mode     *mode = NULL;
	int          bad  = 0;
	int          i;
	ppc32_attr_t *attr;
	const arch_register_req_t **reqs;
	const arch_register_t     **slots;

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
			attr = get_ppc32_attr(n);
			fprintf(F, "=== ppc attr begin ===\n");

			/* dump IN requirements */
			if (get_irn_arity(n) > 0) {
				reqs = get_ppc32_in_req_all(n);
				dump_reg_req(F, n, reqs, 0);
			}

			/* dump OUT requirements */
			if (ARR_LEN(attr->slots) > 0) {
				reqs = get_ppc32_out_req_all(n);
				dump_reg_req(F, n, reqs, 1);
			}

			/* dump assigned registers */
			slots = get_ppc32_slots(n);
			if (slots && ARR_LEN(attr->slots) > 0) {
				for (i = 0; i < ARR_LEN(attr->slots); i++) {
					if (slots[i]) {
						fprintf(F, "reg #%d = %s\n", i, slots[i]->name);
					}
					else {
						fprintf(F, "reg #%d = n/a\n", i);
					}
				}
			}
			fprintf(F, "\n");

			/* dump n_res */
			fprintf(F, "n_res = %d\n", get_ppc32_n_res(n));

			/* dump flags */
			fprintf(F, "flags =");
			if (attr->flags == arch_irn_flags_none) {
				fprintf(F, " none");
			}
			else {
				if (attr->flags & arch_irn_flags_dont_spill) {
					fprintf(F, " unspillable");
				}
				if (attr->flags & arch_irn_flags_rematerializable) {
					fprintf(F, " remat");
				}
				if (attr->flags & arch_irn_flags_ignore) {
					fprintf(F, " ignore");
				}
			}
			fprintf(F, " (%d)\n", attr->flags);

			/* TODO: dump all additional attributes */

			fprintf(F, "=== ppc attr end ===\n");
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

/**
 * Wraps get_irn_generic_attr() as it takes no const ir_node, so we need to do a cast.
 * Firm was made by people hating const :-(
 */
ppc32_attr_t *get_ppc32_attr(const ir_node *node) {
	assert(is_ppc32_irn(node) && "need ppc node to get attributes");
	return (ppc32_attr_t *)get_irn_generic_attr((ir_node *)node);
}

/**
 * Returns the argument register requirements of a ppc node.
 */
const arch_register_req_t **get_ppc32_in_req_all(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an ppc node.
 */
const arch_register_req_t **get_ppc32_out_req_all(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->out_req;
}

/**
 * Returns the argument register requirement at position pos of an ppc node.
 */
const arch_register_req_t *get_ppc32_in_req(const ir_node *node, int pos) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->in_req[pos];
}

/**
 * Returns the result register requirement at position pos of an ppc node.
 */
const arch_register_req_t *get_ppc32_out_req(const ir_node *node, int pos) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->out_req[pos];
}

/**
 * Sets the OUT register requirements at position pos.
 */
void set_ppc32_req_out(ir_node *node, const arch_register_req_t *req, int pos) {
	ppc32_attr_t *attr   = get_ppc32_attr(node);
	attr->out_req[pos] = req;
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_ppc32_req_in(ir_node *node, const arch_register_req_t *req, int pos) {
	ppc32_attr_t *attr  = get_ppc32_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Returns the register flag of an ppc node.
 */
arch_irn_flags_t get_ppc32_flags(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->flags;
}

/**
 * Sets the register flag of an ppc node.
 */
void set_ppc32_flags(const ir_node *node, arch_irn_flags_t flags) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->flags      = flags;
}

/**
 * Returns the result register slots of an ppc node.
 */
const arch_register_t **get_ppc32_slots(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->slots;
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ppc32_out_reg_name(const ir_node *node, int pos) {
	ppc32_attr_t *attr = get_ppc32_attr(node);

	assert(is_ppc32_irn(node) && "Not an ppc node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_name(attr->slots[pos]);
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ppc32_out_regnr(const ir_node *node, int pos) {
	ppc32_attr_t *attr = get_ppc32_attr(node);

	assert(is_ppc32_irn(node) && "Not an ppc node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_index(attr->slots[pos]);
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ppc32_out_reg(const ir_node *node, int pos) {
	ppc32_attr_t *attr = get_ppc32_attr(node);

	assert(is_ppc32_irn(node) && "Not an ppc node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Returns the number of results.
 */
int get_ppc32_n_res(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return ARR_LEN(attr->slots);
}

/**
 * Sets the type of the constant (if any)
 * May be either iro_Const or iro_SymConst
 */
/* void set_ppc32_type(const ir_node *node, opcode type) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->type      = type;
}  */

/**
 * Returns the type of the content (if any)
 */
ppc32_attr_content_type get_ppc32_type(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->content_type;
}

/**
 * Sets a tarval type content (also updating the content_type)
 */
void set_ppc32_constant_tarval(const ir_node *node, tarval *const_tarval) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->content_type = ppc32_ac_Const;
	attr->data.constant_tarval = const_tarval;
}

/**
 * Returns a tarval type constant
 */
tarval *get_ppc32_constant_tarval(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->data.constant_tarval;
}

/**
 * Sets an ident type constant (also updating the content_type)
 */
void set_ppc32_symconst_ident(const ir_node *node, ident *symconst_ident) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->content_type = ppc32_ac_SymConst;
	attr->data.symconst_ident = symconst_ident;
}

/**
 * Returns an ident type constant
 */
ident *get_ppc32_symconst_ident(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->data.symconst_ident;
}


/**
 * Sets an entity (also updating the content_type)
 */
void set_ppc32_frame_entity(const ir_node *node, ir_entity *ent) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->content_type = ppc32_ac_FrameEntity;
	attr->data.frame_entity = ent;
}

/**
 * Returns an entity
 */
ir_entity *get_ppc32_frame_entity(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->data.frame_entity;
}

/**
 * Sets a Rlwimi const (also updating the content_type)
 */
void set_ppc32_rlwimi_const(const ir_node *node, unsigned shift, unsigned maskA, unsigned maskB) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->content_type = ppc32_ac_RlwimiConst;
	attr->data.rlwimi_const.shift = shift;
	attr->data.rlwimi_const.maskA = maskA;
	attr->data.rlwimi_const.maskB = maskB;
}

/**
 * Returns the rlwimi const structure
 */
rlwimi_const_t *get_ppc32_rlwimi_const(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return &attr->data.rlwimi_const;
}

/**
 * Sets a Proj number (also updating the content_type)
 */
void set_ppc32_proj_nr(const ir_node *node, int proj_nr) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->content_type = ppc32_ac_BranchProj;
	attr->data.proj_nr = proj_nr;
}

/**
 * Returns the proj number
 */
int get_ppc32_proj_nr(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->data.proj_nr;
}

/**
 * Sets an offset for a memory access (also updating the content_type)
 */
void set_ppc32_offset(const ir_node *node, int offset) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->content_type = ppc32_ac_Offset;
	attr->data.offset  = offset;
}

/**
 * Returns the offset
 */
int get_ppc32_offset(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->data.offset;
}

/**
 * Sets the offset mode (ppc32_ao_None, ppc32_ao_Lo16, ppc32_ao_Hi16 or ppc32_ao_Ha16)
 */
void set_ppc32_offset_mode(const ir_node *node, ppc32_attr_offset_mode mode) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	attr->offset_mode = mode;
}

/**
 * Returns the offset mode
 */
ppc32_attr_offset_mode get_ppc32_offset_mode(const ir_node *node) {
	ppc32_attr_t *attr = get_ppc32_attr(node);
	return attr->offset_mode;
}


/**
 * Initializes ppc specific node attributes
 */
void init_ppc32_attributes(ir_node *node, int flags,
						 const arch_register_req_t **in_reqs, const arch_register_req_t **out_reqs,
						 const be_execution_unit_t ***execution_units,
						 int n_res, unsigned latency) {
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	ppc32_attr_t   *attr = get_ppc32_attr(node);
	(void) execution_units;
	(void) latency;

	attr->flags   = flags;
	attr->in_req  = in_reqs;
	attr->out_req = out_reqs;

	attr->content_type = ppc32_ac_None;
	attr->offset_mode  = ppc32_ao_Illegal;
	attr->data.empty   = NULL;

	attr->slots = NEW_ARR_D(const arch_register_t*, obst, n_res);
	memset(attr->slots, 0, n_res * sizeof(attr->slots[0]));
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

/* Include the generated constructor functions */
#include "gen_ppc32_new_nodes.c.inl"
