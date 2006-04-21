/**
 * This file implements the creation of the achitecture specific firm opcodes
 * and the coresponding node constructors for the $arch assembler irg.
 * @author Christian Wuerdig
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
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

#include "../bearch.h"

#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"

/**
 * Returns the ident of a SymConst.
 * @param symc  The SymConst
 * @return The ident of the SymConst
 */
static ident *get_sc_ident(ir_node *symc) {
	assert(get_irn_opcode(symc) == iro_SymConst && "need symconst to get ident");

	switch (get_SymConst_kind(symc)) {
		case symconst_addr_name:
			return get_SymConst_name(symc);

		case symconst_addr_ent:
			return get_entity_ld_ident(get_SymConst_entity(symc));

		default:
			assert(0 && "Unsupported SymConst");
	}

	return NULL;
}



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
 * Returns a string containing the names of all registers within the limited bitset
 */
static char *get_limited_regs(const arch_register_req_t *req, char *buf, int max) {
	bitset_t *bs   = bitset_alloca(req->cls->n_regs);
	char     *p    = buf;
	int       size = 0;
	int       i, cnt;

	req->limited(NULL, bs);

	for (i = 0; i < req->cls->n_regs; i++) {
		if (bitset_is_set(bs, i)) {
			cnt = snprintf(p, max - size, " %s", req->cls->regs[i].name);
			if (cnt < 0) {
				fprintf(stderr, "dumper problem, exiting\n");
				exit(1);
			}

			p    += cnt;
			size += cnt;

			if (size >= max)
				break;
		}
	}

	return buf;
}

/**
 * Dumps the register requirements for either in or out.
 */
static void dump_reg_req(FILE *F, ir_node *n, const ia32_register_req_t **reqs, int inout) {
	char *dir = inout ? "out" : "in";
	int   max = inout ? get_ia32_n_res(n) : get_irn_arity(n);
	char *buf = alloca(1024);
	int   i;

	memset(buf, 0, 1024);

	if (reqs) {
		for (i = 0; i < max; i++) {
			fprintf(F, "%sreq #%d =", dir, i);

			if (reqs[i]->req.type == arch_register_req_type_none) {
				fprintf(F, " n/a");
			}

			if (reqs[i]->req.type & arch_register_req_type_normal) {
				fprintf(F, " %s", reqs[i]->req.cls->name);
			}

			if (reqs[i]->req.type & arch_register_req_type_limited) {
				fprintf(F, " %s", get_limited_regs(&reqs[i]->req, buf, 1024));
			}

			if (reqs[i]->req.type & arch_register_req_type_should_be_same) {
				ir_fprintf(F, " same as %+F", get_irn_n(n, reqs[i]->same_pos));
			}

			if (reqs[i]->req.type & arch_register_req_type_should_be_different) {
				ir_fprintf(F, " different from %+F", get_irn_n(n, reqs[i]->different_pos));
			}

			fprintf(F, "\n");
		}

		fprintf(F, "\n");
	}
	else {
		fprintf(F, "%sreq = N/A\n", dir);
	}
}

/**
 * Dumper interface for dumping ia32 nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int ia32_dump_node(ir_node *n, FILE *F, dump_reason_t reason) {
	ir_mode     *mode = NULL;
	int          bad  = 0;
	int          i, n_res, am_flav, flags;
	const ia32_register_req_t **reqs;
	const arch_register_t     **slots;

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			break;

		case dump_node_mode_txt:
			mode = get_irn_mode(n);

			if (is_ia32_Ld(n) || is_ia32_St(n)) {
				mode = get_ia32_ls_mode(n);
			}

			fprintf(F, "[%s]", mode ? get_mode_name(mode) : "?NOMODE?");
			break;

		case dump_node_nodeattr_txt:
			if (is_ia32_ImmConst(n) || is_ia32_ImmSymConst(n) || is_ia32_Cnst(n)) {
				char       *pref = is_ia32_ImmSymConst(n) || (get_ia32_op_type(n) == ia32_SymConst) ? "SymC " : "";
				const char *cnst = get_ia32_cnst(n);

				fprintf(F, "[%s%s]", pref, cnst ? cnst : "NONE");
			}

			if (! is_ia32_Lea(n)) {
				if (is_ia32_AddrModeS(n)) {
					fprintf(F, "[AM S] ");
				}
				else if (is_ia32_AddrModeD(n)) {
					fprintf(F, "[AM D] ");
				}
			}

			break;

		case dump_node_info_txt:
			n_res = get_ia32_n_res(n);
			fprintf(F, "=== IA32 attr begin ===\n");

			/* dump IN requirements */
			if (get_irn_arity(n) > 0) {
				reqs = get_ia32_in_req_all(n);
				dump_reg_req(F, n, reqs, 0);
			}

			/* dump OUT requirements */
			if (n_res > 0) {
				reqs = get_ia32_out_req_all(n);
				dump_reg_req(F, n, reqs, 1);
			}

			/* dump assigned registers */
			slots = get_ia32_slots(n);
			if (slots && n_res > 0) {
				for (i = 0; i < n_res; i++) {
					fprintf(F, "reg #%d = %s\n", i, slots[i] ? slots[i]->name : "n/a");
				}
				fprintf(F, "\n");
			}

			/* dump op type */
			fprintf(F, "op = ");
			switch (get_ia32_op_type(n)) {
				case ia32_Normal:
					fprintf(F, "Normal");
					break;
				case ia32_Const:
					fprintf(F, "Const");
					break;
				case ia32_SymConst:
					fprintf(F, "SymConst");
					break;
				case ia32_AddrModeD:
					fprintf(F, "AM Dest (Load+Store)");
					break;
				case ia32_AddrModeS:
					fprintf(F, "AM Source (Load)");
					break;
				default:
					fprintf(F, "unknown (%d)", get_ia32_op_type(n));
					break;
			}
			fprintf(F, "\n");


			/* dump supported am */
			fprintf(F, "AM support = ");
			switch (get_ia32_am_support(n)) {
				case ia32_am_None:
					fprintf(F, "none");
					break;
				case ia32_am_Source:
					fprintf(F, "source only (Load)");
					break;
				case ia32_am_Dest:
					fprintf(F, "dest only (Load+Store)");
					break;
				case ia32_am_Full:
					fprintf(F, "full");
					break;
				default:
					fprintf(F, "unknown (%d)", get_ia32_am_support(n));
					break;
			}
			fprintf(F, "\n");

			/* dump am flavour */
			fprintf(F, "AM flavour =");
			am_flav = get_ia32_am_flavour(n);
			if (am_flav == ia32_am_N) {
				fprintf(F, " none");
			}
			else {
				if (am_flav & ia32_O) {
					fprintf(F, " O");
				}
				if (am_flav & ia32_B) {
					fprintf(F, " B");
				}
				if (am_flav & ia32_I) {
					fprintf(F, " I");
				}
				if (am_flav & ia32_S) {
					fprintf(F, " S");
				}
			}
			fprintf(F, " (%d)\n", am_flav);

			/* dump AM offset */
			fprintf(F, "AM offset = ");
			if (get_ia32_am_offs(n)) {
				fprintf(F, "%s", get_ia32_am_offs(n));
			}
			else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");

			/* dump AM scale */
			fprintf(F, "AM scale = %d\n", get_ia32_am_scale(n));

			/* dump pn code */
			fprintf(F, "pn_code = %ld\n", get_ia32_pncode(n));

			/* dump n_res */
			fprintf(F, "n_res = %d\n", get_ia32_n_res(n));

			/* dump use_frame */
			fprintf(F, "use_frame = %d\n", is_ia32_use_frame(n));

			/* commutative */
			fprintf(F, "commutative = %d\n", is_ia32_commutative(n));

			/* dump flags */
			fprintf(F, "flags =");
			flags = get_ia32_flags(n);
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
				if (flags & arch_irn_flags_ignore) {
					fprintf(F, " ignore");
				}
			}
			fprintf(F, " (%d)\n", flags);

			/* dump frame entity */
			fprintf(F, "frame entity = ");
			if (get_ia32_frame_ent(n)) {
				ir_fprintf(F, "%+F", get_ia32_frame_ent(n));
			}
			else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");

#ifndef NDEBUG
			/* dump original ir node name */
			fprintf(F, "orig node = ");
			if (get_ia32_orig_node(n)) {
				fprintf(F, "%s", get_ia32_orig_node(n));
			}
			else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");
#endif /* NDEBUG */

			fprintf(F, "=== IA32 attr end ===\n");
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
 * Returns an ident for the given tarval tv.
 */
static ident *get_ident_for_tv(tarval *tv) {
	char buf[1024];

	assert(tarval_snprintf(buf, sizeof(buf), tv));
	return new_id_from_str(buf);
}

/**
 * Wraps get_irn_generic_attr() as it takes no const ir_node, so we need to do a cast.
 * Firm was made by people hating const :-(
 */
ia32_attr_t *get_ia32_attr(const ir_node *node) {
	assert(is_ia32_irn(node) && "need ia32 node to get ia32 attributes");
	return (ia32_attr_t *)get_irn_generic_attr((ir_node *)node);
}

/**
 * Gets the type of an ia32 node.
 */
ia32_op_type_t get_ia32_op_type(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.tp;
}

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.tp     = tp;
}

/**
 * Gets the immediate op type of an ia32 node.
 */
ia32_immop_type_t get_ia32_immop_type(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.imm_tp;
}

/**
 * Sets the immediate op type of an ia32 node.
 */
void set_ia32_immop_type(ir_node *node, ia32_immop_type_t tp) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.imm_tp = tp;
}

/**
 * Gets the supported addrmode of an ia32 node
 */
ia32_am_type_t get_ia32_am_support(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.am_support;
}

/**
 * Sets the supported addrmode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t am_tp) {
	ia32_attr_t *attr     = get_ia32_attr(node);
	attr->data.am_support = am_tp;
}

/**
 * Gets the addrmode flavour of an ia32 node
 */
ia32_am_flavour_t get_ia32_am_flavour(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.am_flavour;
}

/**
 * Sets the addrmode flavour of an ia32 node
 */
void set_ia32_am_flavour(ir_node *node, ia32_am_flavour_t am_flavour) {
	ia32_attr_t *attr     = get_ia32_attr(node);
	attr->data.am_flavour = am_flavour;
}

/**
 * Joins all offsets to one string with adds.
 */
char *get_ia32_am_offs(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	char        *res  = NULL;
	int          size;

	if (! attr->am_offs) {
		return NULL;
	}

	size = obstack_object_size(attr->am_offs);
	if (size > 0) {
		res    = xmalloc(size + 2);
		res[0] = attr->data.offs_sign ? '-' : '+';
		memcpy(&res[1], obstack_base(attr->am_offs), size);
		res[size + 1] = '\0';
	}

	return res;
}

/**
 * Add an offset for addrmode.
 */
static void extend_ia32_am_offs(ir_node *node, char *offset, char op) {
	ia32_attr_t *attr = get_ia32_attr(node);

	if (! offset || strlen(offset) < 1)
		return;

	/* offset could already have an explicit sign */
	/* -> supersede op if necessary               */
	if (offset[0] == '-' || offset[0] == '+') {
		if (offset[0] == '-') {
			op = (op == '-') ? '+' : '-';
		}

		/* skip explicit sign */
		offset++;
	}

	if (! attr->am_offs) {
		/* obstack is not initialized */
		attr->am_offs = xcalloc(1, sizeof(*(attr->am_offs)));
		obstack_init(attr->am_offs);

		attr->data.offs_sign = (op == '-') ? 1 : 0;
	}
	else {
		/* If obstack is initialized, connect the new offset with op */
		obstack_printf(attr->am_offs, "%c", op);
	}

	obstack_printf(attr->am_offs, "%s", offset);
}

/**
 * Add an offset for addrmode.
 */
void add_ia32_am_offs(ir_node *node, const char *offset) {
	extend_ia32_am_offs(node, (char *)offset, '+');
}

/**
 * Sub an offset for addrmode.
 */
void sub_ia32_am_offs(ir_node *node, const char *offset) {
	extend_ia32_am_offs(node, (char *)offset, '-');
}

/**
 * Returns the symconst ident associated to addrmode.
 */
ident *get_ia32_am_sc(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->am_sc;
}

/**
 * Sets the symconst ident associated to addrmode.
 */
void set_ia32_am_sc(ir_node *node, ident *sc) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_sc       = sc;
}

/**
 * Sets the sign bit for address mode symconst.
 */
void set_ia32_am_sc_sign(ir_node *node) {
	ia32_attr_t *attr     = get_ia32_attr(node);
	attr->data.am_sc_sign = 1;
}

/**
 * Clears the sign bit for address mode symconst.
 */
void clear_ia32_am_sc_sign(ir_node *node) {
	ia32_attr_t *attr     = get_ia32_attr(node);
	attr->data.am_sc_sign = 0;
}

/**
 * Returns the sign bit for address mode symconst.
 */
int is_ia32_am_sc_sign(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.am_sc_sign;
}

/**
 * Gets the addr mode const.
 */
int get_ia32_am_scale(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.am_scale;
}

/**
 * Sets the index register scale for addrmode.
 */
void set_ia32_am_scale(ir_node *node, int scale) {
	ia32_attr_t *attr   = get_ia32_attr(node);
	attr->data.am_scale = scale;
}

/**
 * Return the tarval of an immediate operation or NULL in case of SymConst
 */
tarval *get_ia32_Immop_tarval(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
    return attr->cnst_val.tv;
}

/**
 * Sets the attributes of an immediate operation to the specified tarval
 */
void set_ia32_Immop_tarval(ir_node *node, tarval *tv) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->cnst_val.tv = tv;
	attr->cnst        = get_ident_for_tv(tv);
}

/**
 * Return the sc attribute.
 */
ident *get_ia32_sc(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->cnst_val.sc;
}

/**
 * Sets the sc attribute.
 */
void set_ia32_sc(ir_node *node, ident *sc) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->cnst_val.sc = sc;
	attr->cnst        = attr->cnst_val.sc;
}

/**
 * Gets the string representation of the internal const (tv or symconst)
 */
const char *get_ia32_cnst(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	if (! attr->cnst)
		return NULL;
	return get_id_str(attr->cnst);
}

/**
 * Sets the string representation of the internal const.
 */
void set_ia32_cnst(ir_node *node, char *cnst) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->cnst        = new_id_from_str(cnst);
}

/**
 * Gets the ident representation of the internal const (tv or symconst)
 */
ident *get_ia32_id_cnst(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->cnst;
}

/**
 * Sets the ident representation of the internal const.
 */
void set_ia32_id_cnst(ir_node *node, ident *cnst) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->cnst        = cnst;
}

/**
 * Sets the uses_frame flag.
 */
void set_ia32_use_frame(ir_node *node) {
	ia32_attr_t *attr    = get_ia32_attr(node);
	attr->data.use_frame = 1;
}

/**
 * Clears the uses_frame flag.
 */
void clear_ia32_use_frame(ir_node *node) {
	ia32_attr_t *attr    = get_ia32_attr(node);
	attr->data.use_frame = 0;
}

/**
 * Gets the uses_frame flag.
 */
int is_ia32_use_frame(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.use_frame;
}

/**
 * Sets node to commutative.
 */
void set_ia32_commutative(ir_node *node) {
	ia32_attr_t *attr         = get_ia32_attr(node);
	attr->data.is_commutative = 1;
}

/**
 * Sets node to non-commutative.
 */
void clear_ia32_commutative(ir_node *node) {
	ia32_attr_t *attr         = get_ia32_attr(node);
	attr->data.is_commutative = 0;
}

/**
 * Checks if node is commutative.
 */
int is_ia32_commutative(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.is_commutative;
}

/**
 * Sets node emit_cl.
 */
void set_ia32_emit_cl(ir_node *node) {
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->data.emit_cl = 1;
}

/**
 * Clears node emit_cl.
 */
void clear_ia32_emit_cl(ir_node *node) {
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->data.emit_cl = 0;
}

/**
 * Checks if node needs %cl.
 */
int is_ia32_emit_cl(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.emit_cl;
}

/**
 * Sets node got_lea.
 */
void set_ia32_got_lea(ir_node *node) {
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->data.got_lea = 1;
}

/**
 * Clears node got_lea.
 */
void clear_ia32_got_lea(ir_node *node) {
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->data.got_lea = 0;
}

/**
 * Checks if node got lea.
 */
int is_ia32_got_lea(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.got_lea;
}

/**
 * Gets the mode of the stored/loaded value (only set for Store/Load)
 */
ir_mode *get_ia32_ls_mode(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->ls_mode;
}

/**
 * Sets the mode of the stored/loaded value (only set for Store/Load)
 */
void set_ia32_ls_mode(ir_node *node, ir_mode *mode) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->ls_mode     = mode;
}

/**
 * Gets the mode of the result.
 */
ir_mode *get_ia32_res_mode(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->res_mode;
}

/**
 * Sets the mode of the result.
 */
void set_ia32_res_mode(ir_node *node, ir_mode *mode) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->res_mode    = mode;
}

/**
 * Gets the source mode of conversion.
 */
ir_mode *get_ia32_src_mode(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->src_mode;
}

/**
 * Sets the source mode of conversion.
 */
void set_ia32_src_mode(ir_node *node, ir_mode *mode) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->src_mode    = mode;
}

/**
 * Gets the target mode of conversion.
 */
ir_mode *get_ia32_tgt_mode(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->tgt_mode;
}

/**
 * Sets the target mode of conversion.
 */
void set_ia32_tgt_mode(ir_node *node, ir_mode *mode) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->tgt_mode    = mode;
}

/**
 * Gets the frame entity assigned to this node;
 */
entity *get_ia32_frame_ent(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->frame_ent;
}

/**
 * Sets the frame entity for this node;
 */
void set_ia32_frame_ent(ir_node *node, entity *ent) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->frame_ent   = ent;
}

/**
 * Returns the argument register requirements of an ia32 node.
 */
const ia32_register_req_t **get_ia32_in_req_all(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->in_req;
}

/**
 * Sets the argument register requirements of an ia32 node.
 */
void set_ia32_in_req_all(ir_node *node, const ia32_register_req_t **reqs) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->in_req      = reqs;
}

/**
 * Returns the result register requirements of an ia32 node.
 */
const ia32_register_req_t **get_ia32_out_req_all(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->out_req;
}

/**
 * Sets the result register requirements of an ia32 node.
 */
void set_ia32_out_req_all(ir_node *node, const ia32_register_req_t **reqs) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->out_req     = reqs;
}

/**
 * Returns the argument register requirement at position pos of an ia32 node.
 */
const ia32_register_req_t *get_ia32_in_req(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->in_req[pos];
}

/**
 * Returns the result register requirement at position pos of an ia32 node.
 */
const ia32_register_req_t *get_ia32_out_req(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->out_req[pos];
}

/**
 * Sets the OUT register requirements at position pos.
 */
void set_ia32_req_out(ir_node *node, const ia32_register_req_t *req, int pos) {
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->out_req[pos] = req;
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_ia32_req_in(ir_node *node, const ia32_register_req_t *req, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Returns the register flag of an ia32 node.
 */
arch_irn_flags_t get_ia32_flags(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.flags;
}

/**
 * Sets the register flag of an ia32 node.
 */
void set_ia32_flags(ir_node *node, arch_irn_flags_t flags) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.flags  = flags;
}

/**
 * Returns the result register slots of an ia32 node.
 */
const arch_register_t **get_ia32_slots(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->slots;
}

/**
 * Sets the number of results.
 */
void set_ia32_n_res(ir_node *node, int n_res) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.n_res  = n_res;
}

/**
 * Returns the number of results.
 */
int get_ia32_n_res(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.n_res;
}

/**
 * Returns the flavour of an ia32 node,
 */
ia32_op_flavour_t get_ia32_flavour(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->data.op_flav;
}

/**
 * Sets the flavour of an ia32 node to flavour_Div/Mod/DivMod/Mul/Mulh.
 */
void set_ia32_flavour(ir_node *node, ia32_op_flavour_t op_flav) {
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->data.op_flav = op_flav;
}

/**
 * Returns the projnum code.
 */
long get_ia32_pncode(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->pn_code;
}

/**
 * Sets the projnum code
 */
void set_ia32_pncode(ir_node *node, long code) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->pn_code     = code;
}

#ifndef NDEBUG

/**
 * Returns the name of the original ir node.
 */
const char *get_ia32_orig_node(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->orig_node;
}

/**
 * Sets the name of the original ir node.
 */
void set_ia32_orig_node(ir_node *node, const char *name) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->orig_node   = name;
}

#endif /* NDEBUG */

/******************************************************************************************************
 *                      _       _         _   _           __                  _   _
 *                     (_)     | |       | | | |         / _|                | | (_)
 *  ___ _ __   ___  ___ _  __ _| |   __ _| |_| |_ _ __  | |_ _   _ _ __   ___| |_ _  ___  _ __    ___
 * / __| '_ \ / _ \/ __| |/ _` | |  / _` | __| __| '__| |  _| | | | '_ \ / __| __| |/ _ \| '_ \  / __|
 * \__ \ |_) |  __/ (__| | (_| | | | (_| | |_| |_| |    | | | |_| | | | | (__| |_| | (_) | | | | \__ \
 * |___/ .__/ \___|\___|_|\__,_|_|  \__,_|\__|\__|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_| |___/
 *     | |
 *     |_|
 ******************************************************************************************************/

/**
 * Gets the type of an ia32_Const.
 */
unsigned get_ia32_Const_type(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_Cnst(node) && "Need ia32_Const to get type");

	return attr->data.tp;
}

/**
 * Sets the type of an ia32_Const.
 */
void set_ia32_Const_type(ir_node *node, int type) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_Cnst(node) && "Need ia32_Const to set type");
	assert((type == ia32_Const || type == ia32_SymConst) && "Unsupported ia32_Const type");

	attr->data.tp = type;
}

/**
 * Copy the attributes from an ia32_Const to an Immop (Add_i, Sub_i, ...) node
 */
void set_ia32_Immop_attr(ir_node *node, ir_node *cnst) {
	ia32_attr_t *na = get_ia32_attr(node);
	ia32_attr_t *ca = get_ia32_attr(cnst);

	switch(get_ia32_Const_type(cnst)) {
		case ia32_Const:
			na->cnst_val.tv = ca->cnst_val.tv;
			na->cnst        = ca->cnst;
			set_ia32_immop_type(node, ia32_ImmConst);
			break;
		case ia32_SymConst:
			na->cnst_val.sc = ca->cnst_val.sc;
			na->cnst        = na->cnst_val.sc;
			set_ia32_immop_type(node, ia32_ImmSymConst);
			break;
		default:
			assert(0 && "Need ia32_Const to set Immop attr");
	}
}

/**
 * Copy the attributes from Immop to an Immop
 */
void copy_ia32_Immop_attr(ir_node *dst, ir_node *src) {
	ia32_attr_t *da = get_ia32_attr(dst);
	ia32_attr_t *sa = get_ia32_attr(src);

	switch(get_ia32_immop_type(src)) {
		case ia32_ImmConst:
			da->cnst_val.tv = sa->cnst_val.tv;
			da->cnst        = sa->cnst;
			set_ia32_immop_type(dst, ia32_ImmConst);
			break;
		case ia32_ImmSymConst:
			da->cnst_val.sc = sa->cnst_val.sc;
			da->cnst        = sa->cnst;
			set_ia32_immop_type(dst, ia32_ImmSymConst);
			break;
		default:
			assert(0 && "Need Immop to copy Immop attr");
	}
}

/**
 * Copy the attributes from a Firm Const to an ia32_Const
 */
void set_ia32_Const_attr(ir_node *ia32_cnst, ir_node *cnst) {
	ia32_attr_t *attr = get_ia32_attr(ia32_cnst);

	assert(is_ia32_Cnst(ia32_cnst) && "Need ia32_Const to set Const attr");

	switch (get_irn_opcode(cnst)) {
		case iro_Const:
			attr->data.tp     = ia32_Const;
			attr->cnst_val.tv = get_Const_tarval(cnst);
			attr->cnst        = get_ident_for_tv(attr->cnst_val.tv);
			break;
		case iro_SymConst:
			attr->data.tp     = ia32_SymConst;
			attr->cnst_val.sc = get_sc_ident(cnst);
			attr->cnst        = attr->cnst_val.sc;
			break;
		case iro_Unknown:
			assert(0 && "Unknown Const NYI");
			break;
		default:
			assert(0 && "Cannot create ia32_Const for this opcode");
	}
}

/**
 * Sets the AddrMode(S|D) attribute
 */
void set_ia32_AddrMode(ir_node *node, char direction) {
	ia32_attr_t *attr = get_ia32_attr(node);

	switch (direction) {
		case 'D':
			attr->data.tp = ia32_AddrModeD;
			break;
		case 'S':
			attr->data.tp = ia32_AddrModeS;
			break;
		default:
			assert(0 && "wrong AM type");
	}
}

/**
 * Returns whether or not the node is an immediate operation with Const.
 */
int is_ia32_ImmConst(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return (attr->data.imm_tp == ia32_ImmConst);
}

/**
 * Returns whether or not the node is an immediate operation with SymConst.
 */
int is_ia32_ImmSymConst(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return (attr->data.imm_tp == ia32_ImmSymConst);
}

/**
 * Returns whether or not the node is an AddrModeS node.
 */
int is_ia32_AddrModeS(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return (attr->data.tp == ia32_AddrModeS);
}

/**
 * Returns whether or not the node is an AddrModeD node.
 */
int is_ia32_AddrModeD(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return (attr->data.tp == ia32_AddrModeD);
}

/**
 * Checks if node is a Load or xLoad/vfLoad.
 */
int is_ia32_Ld(const ir_node *node) {
	return is_ia32_Load(node) || is_ia32_xLoad(node) || is_ia32_vfld(node) || is_ia32_fld(node);
}

/**
 * Checks if node is a Store or xStore/vfStore.
 */
int is_ia32_St(const ir_node *node) {
	return is_ia32_Store(node) || is_ia32_xStore(node) || is_ia32_vfst(node) || is_ia32_fst(node) || is_ia32_fstp(node);
}

/**
 * Checks if node is a Const or xConst/vfConst.
 */
int is_ia32_Cnst(const ir_node *node) {
	return is_ia32_Const(node) || is_ia32_xConst(node) || is_ia32_vfConst(node);
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ia32_out_reg_name(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->data.n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_name(attr->slots[pos]);
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ia32_out_regnr(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->data.n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_index(attr->slots[pos]);
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ia32_out_reg(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->data.n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Initializes the nodes attributes.
 */
void init_ia32_attributes(ir_node *node, arch_irn_flags_t flags, const ia32_register_req_t **in_reqs,
						  const ia32_register_req_t **out_reqs, int n_res)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	set_ia32_flags(node, flags);
	set_ia32_in_req_all(node, in_reqs);
	set_ia32_out_req_all(node, out_reqs);

	attr->data.n_res = n_res;
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

/* default compare operation to compare immediate ops */
int ia32_compare_immop_attr(ia32_attr_t *a, ia32_attr_t *b) {
	int equ = 0;

	if (a->data.tp == b->data.tp) {
		equ = (a->cnst == b->cnst);
		equ = equ ? (a->data.use_frame == b->data.use_frame) : 0;

		if (equ && a->data.use_frame && b->data.use_frame)
			equ = (a->frame_ent == b->frame_ent);
	}

	return !equ;
}

/* compare converts */
int ia32_compare_conv_attr(ia32_attr_t *a, ia32_attr_t *b) {
	int equ = ! ia32_compare_immop_attr(a, b);

	equ = equ ? (a->src_mode == b->src_mode) && (a->tgt_mode == b->tgt_mode) : equ;

	return !equ;
}

/* Include the generated constructor functions */
#include "gen_ia32_new_nodes.c.inl"
