/**
 * This file implements the creation of the achitecture specific firm opcodes
 * and the coresponding node constructors for the $arch assembler irg.
 * @author Christian Wuerdig
 * $Id$
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

#include "../bearch.h"

#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"



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
 * Dumper interface for dumping ia32 nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int dump_node_ia32(ir_node *n, FILE *F, dump_reason_t reason) {
  const char *name, *p;
  ir_mode    *mode = NULL;
  int        bad   = 0;
  asmop_attr *attr;
  int        i;
  const arch_register_req_t **reqs;
  const arch_register_t     **slots;

  switch (reason) {
    case dump_node_opcode_txt:
      name = get_irn_opname(n);
      fprintf(F, "%s", name);
      break;

    case dump_node_mode_txt:
      mode = get_irn_mode(n);

      if (mode == mode_BB || mode == mode_ANY || mode == mode_BAD || mode == mode_T) {
        mode = NULL;
      }
      else if (is_ia32_Load(n)) {
        mode = get_irn_mode(get_irn_n(n, 1));
      }
      else if (is_ia32_Store(n)) {
        mode = get_irn_mode(get_irn_n(n, 2));
      }

      if (mode)
        fprintf(F, "[%s]", get_mode_name(mode));
      break;

    case dump_node_nodeattr_txt:
      name = get_irn_opname(n);
      p = name + strlen(name) - 2;
      if ((p[0] == '_' && p[1] == 'i') || is_ia32_Const(n) || is_ia32_fConst(n)) {
        tarval *tv = get_ia32_Immop_tarval(n);
        if (tv)
          fprintf_tv(F, tv, 1);
        else {
          fprintf(F, "[SymC &%s]", get_sc_name(get_ia32_old_ir(n)));
        }
      }
      else if (is_ia32_Call(n)) {
        ir_node *sc = get_ia32_old_ir(n);

        fprintf(F, "&%s ", get_sc_name(sc));
      }
      break;

    case dump_node_info_txt:
      attr = get_ia32_attr(n);

      /* dump IN requirements */
      if (get_irn_arity(n) > 0) {
	reqs = get_ia32_in_req(n);

	if (reqs) {
	  for (i = 0; i < get_irn_arity(n); i++) {
	    if (reqs[i]->type != arch_register_req_type_none)
	      fprintf(F, "inreq[%d]=[%s]\n", i, reqs[i]->cls->name);
	    else
	      fprintf(F, "inreq[%d]=[none]\n", i);
	  }
	  fprintf(F, "\n");
	}
	else
	  fprintf(F, "NO IN REQS\n");
      }

      /* dump OUT requirements */
      if (attr->n_res > 0) {
	reqs = get_ia32_out_req(n);

	if (reqs) {
	  for (i = 0; i < attr->n_res; i++) {
	    if (reqs[i]->type != arch_register_req_type_none)
	      fprintf(F, "outreq[%d]=[%s]\n", i, reqs[i]->cls->name);
	    else
	      fprintf(F, "outreq[%d]=[none]\n", i);
	  }
	}
	else
	  fprintf(F, "NO OUT REQS\n");
      }

      /* dump assigned registers */
      slots = get_ia32_slots(n);
      if (slots && attr->n_res > 0) {
        for (i = 0; i < attr->n_res; i++) {
	  if (slots[i]) {
	    fprintf(F, "REG[%d]=[%s]\n", i, slots[i]->name);
	  }
	  else
	    fprintf(F, "REG[%d]=[none]\n", i);
        }
      }

      /* special for LEA */
      if (is_ia32_Lea(n)) {
        tarval *o  = get_ia32_offs(n);
        tarval *tv = get_ia32_Immop_tarval(n);

        fprintf(F, "LEA ");
        if (o)
          fprintf_tv(F, o, 0);
        fprintf(F, "(%s, %s", get_irn_opname(get_irn_n(n, 0)), get_irn_opname(get_irn_n(n, 1)));
        if (tv) {
          fprintf(F, ", ");
          fprintf_tv(F, tv, 0);
        }
        fprintf(F, ")\n");
      }
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
asmop_attr *get_ia32_attr(const ir_node *node) {
  return (asmop_attr *)get_irn_generic_attr((ir_node *)node);
}

/**
 * Return the tarval of an immediate operation or NULL in case of SymConst
 */
tarval *get_ia32_Immop_tarval(const ir_node *node) {
  asmop_attr *attr = get_ia32_attr(node);
  if (attr->tp == asmop_Const)
    return attr->tv;
  else
    return NULL;
}

/**
 * Return the old_ir attribute.
 */
ir_node *get_ia32_old_ir(const ir_node *node) {
  asmop_attr *attr = get_ia32_attr(node);
  return attr->old_ir;
}

/**
 * Copy the attributes from an ia32_Const to an Immop (Add_i, Sub_i, ...) node
 */
void set_ia32_Immop_attr(ir_node *node, ir_node *cnst) {
  asmop_attr *na = get_ia32_attr(node);
  asmop_attr *ca = get_ia32_attr(cnst);

  assert((is_ia32_Const(cnst) || is_ia32_fConst(cnst)) && "Need ia32_Const to set Immop attr");

  na->tp     = ca->tp;
  na->tv     = ca->tv;

  if (ca->old_ir) {
    na->old_ir = calloc(1, sizeof(*(ca->old_ir)));
    memcpy(na->old_ir, ca->old_ir, sizeof(*(ca->old_ir)));
  }
  else
    na->old_ir = NULL;
}

/**
 * Copy the attributes from a Const to an ia32_Const
 */
void set_ia32_Const_attr(ir_node *ia32_cnst, ir_node *cnst) {
  asmop_attr *attr = get_ia32_attr(ia32_cnst);

  assert((is_ia32_Const(ia32_cnst) || is_ia32_fConst(ia32_cnst)) && "Need ia32_Const to set Const attr");

  switch (get_irn_opcode(cnst)) {
    case iro_Const:
      attr->tp = asmop_Const;
      attr->tv = get_Const_tarval(cnst);
      break;
    case iro_SymConst:
      attr->tp     = asmop_SymConst;
      attr->old_ir = calloc(1, sizeof(*cnst));
      memcpy(attr->old_ir, cnst, sizeof(*cnst));
      break;
    case iro_Unknown:
      assert(0 && "Unknown Const NYI");
      break;
    default:
      assert(0 && "Cannot create ia32_Const for this opcode");
  }
}

/**
 * Sets the type of an ia32_Const.
 */
void set_ia32_Const_type(ir_node *node, int type) {
	asmop_attr *attr   = get_ia32_attr(node);

	assert((is_ia32_Const(node) || is_ia32_fConst(node)) && "Need ia32_Const to set type");
	assert((type == asmop_Const || type == asmop_SymConst) && "Unsupported ia32_Const type");

	attr->tp = type;
}

/**
 * Sets the attributes of an immediate operation to the specified tarval
 */
void set_ia32_Immop_tarval(ir_node *node, tarval *tv) {
	asmop_attr *attr = get_ia32_attr(node);

	attr->tp = asmop_Const;
	attr->tv = tv;
}

/**
 * Sets the offset for a Lea.
 */
void set_ia32_offs(ir_node *node, tarval *offs) {
	asmop_attr *attr = get_ia32_attr(node);
	attr->offset     = offs;
}

/**
 * Gets the offset for a Lea.
 */
tarval *get_ia32_offs(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->offset;
}

/**
 * Returns the argument register requirements of an ia32 node.
 */
const arch_register_req_t **get_ia32_in_req(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an ia32 node.
 */
const arch_register_req_t **get_ia32_out_req(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->out_req;
}

/**
 * Sets the OUT register requirements at position pos.
 */
void set_ia32_regreq_out(ir_node *node, const arch_register_req_t *req, int pos) {
	asmop_attr *attr   = get_ia32_attr(node);
	attr->out_req[pos] = req;
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_ia32_regreq_in(ir_node *node, const arch_register_req_t *req, int pos) {
	asmop_attr *attr  = get_ia32_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Returns the register flag of an ia32 node.
 */
arch_irn_flags_t get_ia32_flags(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->flags;
}

/**
 * Returns the result register slots of an ia32 node.
 */
const arch_register_t **get_ia32_slots(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->slots;
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ia32_out_reg_name(const ir_node *node, int pos) {
	asmop_attr *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos]->name;
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ia32_out_regnr(const ir_node *node, int pos) {
	asmop_attr *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos]->index;
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ia32_out_reg(const ir_node *node, int pos) {
	asmop_attr *attr = get_ia32_attr(node);

	assert(is_ia32_irn(node) && "Not an ia32 node.");
	assert(pos < attr->n_res && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Sets the number of results.
 */
void set_ia32_n_res(ir_node *node, int n_res) {
	asmop_attr *attr = get_ia32_attr(node);
	attr->n_res = n_res;
}

/**
 * Returns the number of results.
 */
int get_ia32_n_res(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->n_res;
}

/**
 * Sets the flavour of an ia32 DivMod node to flavour_Div/Mod/DivMod.
 */
void set_ia32_DivMod_flavour(ir_node *node, divmod_flavour_t dm_flav) {
	asmop_attr *attr = get_ia32_attr(node);
	attr->dm_flav    = dm_flav;
}

/**
 * Returns the projnum code.
 */
long get_ia32_pncode(const ir_node *node) {
	asmop_attr *attr = get_ia32_attr(node);
	return attr->pn_code;
}

/**
 * Sets the projnum code
 */
void set_ia32_pncode(ir_node *node, long code) {
	asmop_attr *attr = get_ia32_attr(node);
	attr->pn_code = code;
}

/* Include the generated functions */
#include "gen_ia32_new_nodes.c.inl"
