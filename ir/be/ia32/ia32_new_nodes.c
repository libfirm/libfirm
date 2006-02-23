/**
 * This file implements the creation of the achitecture specific firm opcodes
 * and the coresponding node constructors for the $arch assembler irg.
 * @author Christian Wuerdig
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef _WIN32
#include <malloc.h>
#else
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

#ifdef obstack_chunk_alloc
# undef obstack_chunk_alloc
# define obstack_chunk_alloc xmalloc
#else
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free
#endif

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
 * Prints a tarval to file F.
 * @param F         output file
 * @param tv        tarval
 * @param brackets  1 == print square brackets around tarval
 */
static void fprintf_tv(FILE *F, tarval *tv, int brackets) {
	char buf[1024];
	tarval_snprintf(buf, sizeof(buf), tv);

	if (brackets)
		fprintf(F, "[%s]", buf);
	else
		fprintf(F, "%s", buf);
}

/**
 * Returns the name of a SymConst.
 * @param symc  the SymConst
 * @return name of the SymConst
 */
const char *get_sc_name(ir_node *symc) {
	if (get_irn_opcode(symc) != iro_SymConst)
		return "NONE";

	switch (get_SymConst_kind(symc)) {
		case symconst_addr_name:
			return get_id_str(get_SymConst_name(symc));

		case symconst_addr_ent:
			return get_entity_ld_name(get_SymConst_entity(symc));

		default:
			assert(0 && "Unsupported SymConst");
	}

	return NULL;
}

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
static int dump_node_ia32(ir_node *n, FILE *F, dump_reason_t reason) {
	ir_mode     *mode = NULL;
	int          bad  = 0;
	int          i;
	ia32_attr_t *attr;
	const ia32_register_req_t **reqs;
	const arch_register_t     **slots;

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			break;

		case dump_node_mode_txt:
			mode = get_irn_mode(n);

			if (mode == mode_BB || mode == mode_ANY || mode == mode_BAD || mode == mode_T) {
				mode = NULL;
			}
			else if (is_ia32_Load(n)) {
				mode = get_irn_mode(get_irn_n(n, 0));
			}
			else if (is_ia32_Store(n)) {
				mode = get_irn_mode(get_irn_n(n, 2));
			}

			if (mode) {
				fprintf(F, "[%s]", get_mode_name(mode));
			}
			break;

		case dump_node_nodeattr_txt:
			if (is_ia32_Call(n)) {
				fprintf(F, "&%s ", get_ia32_sc(n));
			}
			else if (get_ia32_cnst(n)) {
				char *pref = "";

				if (get_ia32_sc(n)) {
					pref = "SymC ";
				}

				fprintf(F, "[%s%s]", pref, get_ia32_cnst(n));
			}

			if (is_ia32_AddrModeS(n) || is_ia32_AddrModeD(n)) {
				fprintf(F, "[AM] ");
			}

			break;

		case dump_node_info_txt:
			attr = get_ia32_attr(n);
			fprintf(F, "=== IA32 attr begin ===\n");

			/* dump IN requirements */
			if (get_irn_arity(n) > 0) {
				reqs = get_ia32_in_req_all(n);
				dump_reg_req(F, n, reqs, 0);
			}

			/* dump OUT requirements */
			if (attr->n_res > 0) {
				reqs = get_ia32_out_req_all(n);
				dump_reg_req(F, n, reqs, 1);
			}

			/* dump assigned registers */
			slots = get_ia32_slots(n);
			if (slots && attr->n_res > 0) {
				for (i = 0; i < attr->n_res; i++) {
					if (slots[i]) {
						fprintf(F, "reg #%d = %s\n", i, slots[i]->name);
					}
					else {
						fprintf(F, "reg #%d = n/a\n", i);
					}
				}
			}
			fprintf(F, "\n");

			/* dump op type */
			fprintf(F, "op = ");
			switch (attr->tp) {
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
			}
			fprintf(F, "\n");


			/* dump supported am */
			fprintf(F, "AM support = ");
			switch (attr->am_support) {
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
			}
			fprintf(F, "\n");

			/* dump AM offset */
			fprintf(F, "AM offset = ");
			if (attr->am_offs) {
				fprintf(F, "%s", get_ia32_am_offs(n));
			}
			else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");

			/* dump AM scale */
			fprintf(F, "AM scale = %d\n", get_ia32_am_scale(n));

			/* dump pn code */
			fprintf(F, "pn_code = %d\n", get_ia32_pncode(n));

			/* dump n_res */
			fprintf(F, "n_res = %d\n", get_ia32_n_res(n));

			/* dump flags */
			fprintf(F, "flags =");
			if (attr->flags & arch_irn_flags_dont_spill) {
				fprintf(F, " unspillable");
			}
			if (attr->flags & arch_irn_flags_rematerializable) {
				fprintf(F, " remat");
			}
			if (attr->flags & arch_irn_flags_ignore) {
				fprintf(F, " ignore");
			}
			fprintf(F, "\n");

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

 static char *copy_str(char *dst, const char *src) {
	 dst = xcalloc(1, strlen(src) + 1);
	 strncpy(dst, src, strlen(src) + 1);
	 return dst;
 }

 static char *set_cnst_from_tv(char *cnst, tarval *tv) {
	 if (cnst) {
		 free(cnst);
	 }

	 cnst = xcalloc(1, 64);
	 assert(tarval_snprintf(cnst, 63, tv));
	 return cnst;
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
	return attr->tp;
}

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->tp          = tp;
}

/**
 * Gets the supported addrmode of an ia32 node
 */
ia32_am_type_t get_ia32_am_support(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->am_support;
}

/**
 * Sets the supported addrmode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t am_tp) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_support  = am_tp;
}

/**
 * Gets the addrmode flavour of an ia32 node
 */
ia32_am_flavour_t get_ia32_am_flavour(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->am_flavour;
}

/**
 * Sets the addrmode flavour of an ia32 node
 */
void set_ia32_am_flavour(ir_node *node, ia32_am_flavour_t am_flavour) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_support  = am_flavour;
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
		res = xcalloc(1, size + 1);
		memcpy(res, obstack_base(attr->am_offs), size);
    }

	res[size] = '\0';
	return res;
}

/**
 * Add an offset for addrmode.
 */
static void extend_ia32_am_offs(ir_node *node, char *offset, char op) {
	ia32_attr_t *attr = get_ia32_attr(node);

	if (!attr->am_offs) {
		/* obstack is not initialized */
		attr->am_offs = xcalloc(1, sizeof(*(attr->am_offs)));
		obstack_init(attr->am_offs);
	}
	else {
		/* obstack is initialized -> there is already one offset */
		/* present -> connect the offsets with an add            */
		obstack_printf(attr->am_offs, " %c ", op);
	}

	obstack_printf(attr->am_offs, "%s", offset);
}

/**
 * Add an offset for addrmode.
 */
void add_ia32_am_offs(ir_node *node, char *offset) {
	extend_ia32_am_offs(node, offset, '+');
}

/**
 * Sub an offset for addrmode.
 */
void sub_ia32_am_offs(ir_node *node, char *offset) {
	extend_ia32_am_offs(node, offset, '-');
}

/**
 * Gets the addr mode const.
 */
int get_ia32_am_scale(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->am_scale;
}

/**
 * Sets the index register scale for addrmode.
 */
void set_ia32_am_scale(ir_node *node, int scale) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_scale    = scale;
}

/**
 * Return the tarval of an immediate operation or NULL in case of SymConst
 */
tarval *get_ia32_Immop_tarval(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
    return attr->tv;
}

/**
 * Sets the attributes of an immediate operation to the specified tarval
 */
void set_ia32_Immop_tarval(ir_node *node, tarval *tv) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->tv          = tv;
	attr->cnst        = set_cnst_from_tv(attr->cnst, attr->tv);
}

/**
 * Return the sc attribute.
 */
char *get_ia32_sc(const ir_node *node) {
  ia32_attr_t *attr = get_ia32_attr(node);
  return attr->sc;
}

/**
 * Sets the sc attribute.
 */
void set_ia32_sc(ir_node *node, char *sc) {
  ia32_attr_t *attr = get_ia32_attr(node);
  attr->sc          = copy_str(attr->sc, sc);

  if (attr->cnst) {
	  free(attr->cnst);
  }
  attr->cnst = attr->sc;
}

/**
 * Gets the string representation of the internal const (tv or symconst)
 */
char *get_ia32_cnst(ir_node *node) {
  ia32_attr_t *attr = get_ia32_attr(node);
  return attr->cnst;
}

/**
 * Returns the argument register requirements of an ia32 node.
 */
const ia32_register_req_t **get_ia32_in_req_all(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an ia32 node.
 */
const ia32_register_req_t **get_ia32_out_req_all(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->out_req;
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
	return attr->flags;
}

/**
 * Sets the register flag of an ia32 node.
 */
void set_ia32_flags(const ir_node *node, arch_irn_flags_t flags) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->flags       = flags;
}

/**
 * Returns the result register slots of an ia32 node.
 */
const arch_register_t **get_ia32_slots(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->slots;
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ia32_out_reg_name(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_name(attr->slots[pos]);
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ia32_out_regnr(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_index(attr->slots[pos]);
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ia32_out_reg(const ir_node *node, int pos) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Sets the number of results.
 */
void set_ia32_n_res(ir_node *node, int n_res) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->n_res       = n_res;
}

/**
 * Returns the number of results.
 */
int get_ia32_n_res(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->n_res;
}

/**
 * Returns the flavour of an ia32 node,
 */
ia32_op_flavour_t get_ia32_flavour(const ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return attr->op_flav;
}

/**
 * Sets the flavour of an ia32 node to flavour_Div/Mod/DivMod/Mul/Mulh.
 */
void set_ia32_flavour(ir_node *node, ia32_op_flavour_t op_flav) {
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->op_flav     = op_flav;
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
unsigned get_ia32_Const_type(ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert((is_ia32_Const(node) || is_ia32_fConst(node)) && "Need ia32_Const to get type");

	return attr->tp;
}

/**
 * Sets the type of an ia32_Const.
 */
void set_ia32_Const_type(ir_node *node, int type) {
	ia32_attr_t *attr = get_ia32_attr(node);

	assert((is_ia32_Const(node) || is_ia32_fConst(node)) && "Need ia32_Const to set type");
	assert((type == ia32_Const || type == ia32_SymConst) && "Unsupported ia32_Const type");

	attr->tp = type;
}

/**
 * Copy the attributes from an ia32_Const to an Immop (Add_i, Sub_i, ...) node
 */
void set_ia32_Immop_attr(ir_node *node, ir_node *cnst) {
	ia32_attr_t *na = get_ia32_attr(node);
	ia32_attr_t *ca = get_ia32_attr(cnst);

	assert((is_ia32_Const(cnst) || is_ia32_fConst(cnst)) && "Need ia32_Const to set Immop attr");

	na->tv = ca->tv;

	if (ca->sc) {
		na->sc = copy_str(na->sc, ca->sc);
	}
	else {
		na->cnst = set_cnst_from_tv(na->cnst, na->tv);
		na->sc   = NULL;
	}
}

/**
 * Copy the attributes from a Const to an ia32_Const
 */
void set_ia32_Const_attr(ir_node *ia32_cnst, ir_node *cnst) {
	ia32_attr_t *attr = get_ia32_attr(ia32_cnst);

	assert((is_ia32_Const(ia32_cnst) || is_ia32_fConst(ia32_cnst)) && "Need ia32_Const to set Const attr");

	switch (get_irn_opcode(cnst)) {
		case iro_Const:
			attr->tp   = ia32_Const;
			attr->tv   = get_Const_tarval(cnst);
			attr->cnst = set_cnst_from_tv(attr->cnst, attr->tv);
			break;
		case iro_SymConst:
			attr->tp   = ia32_SymConst;
			attr->tv   = NULL;
			attr->sc   = copy_str(attr->sc, get_sc_name(cnst));
			attr->cnst = attr->sc;
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
			attr->tp = ia32_AddrModeD;
			break;
		case 'S':
			attr->tp = ia32_AddrModeS;
			break;
		default:
			assert(0 && "wrong AM type");
	}
}

/**
 * Returns whether or not the node is an AddrModeS node.
 */
int is_ia32_AddrModeS(ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return (attr->tp == ia32_AddrModeS);
}

/**
 * Returns whether or not the node is an AddrModeD node.
 */
int is_ia32_AddrModeD(ir_node *node) {
	ia32_attr_t *attr = get_ia32_attr(node);
	return (attr->tp == ia32_AddrModeD);
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
#include "gen_ia32_new_nodes.c.inl"
