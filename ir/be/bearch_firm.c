
/**
 * ISA implementation for Firm IR nodes.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bitset.h"
#include "obst.h"

#include "irmode_t.h"
#include "irnode_t.h"
#include "irgmod.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "type.h"

#include "bearch.h"
#include "beutil.h"

#include "irreflect.h"

#define N_REGS 512

static arch_register_t datab_regs[N_REGS];

static arch_register_class_t reg_classes[] = {
  { "datab", N_REGS, datab_regs },
};

static ir_op *op_push_end;
static ir_op *op_push;
static ir_op *op_imm;
static type *push_type;

typedef struct {
	enum { imm_Const, imm_SymConst } tp;
	union {
		tarval *tv;
		entity *ent;
	} data;
} imm_attr_t;

#define N_CLASSES \
  (sizeof(reg_classes) / sizeof(reg_classes[0]))

#define CLS_DATAB 0

static void firm_init(void)
{
  static struct obstack obst;
  static int inited = 0;
  int k;

  if(inited)
    return;

  inited = 1;
  obstack_init(&obst);

  for(k = 0; k < N_CLASSES; ++k) {
    const arch_register_class_t *cls = &reg_classes[k];
    int i;

    for(i = 0; i < cls->n_regs; ++i) {
      int n;
      char buf[8];
      char *name;
      arch_register_t *reg = (arch_register_t *) &cls->regs[i];

      n = snprintf(buf, sizeof(buf), "r%d", i);
      name = obstack_copy0(&obst, buf, n);

      reg->name = name;
      reg->reg_class = cls;
      reg->index = i;
      reg->type = 0;
    }
  }

	/*
	 * Create some opcodes and types to let firm look a little
	 * bit more like real machines.
	 */
	if(!op_push_end) {
		op_push_end = new_ir_op(get_next_ir_opcode(), "PushEnd",
				op_pin_state_pinned, 0, oparity_unary, 0, 0);
	}

	if(!op_push) {
		op_push = new_ir_op(get_next_ir_opcode(), "Push",
				op_pin_state_pinned, 0, oparity_binary, 0, 0);
	}

	if(!op_imm) {
		op_imm = new_ir_op(get_next_ir_opcode(), "Imm",
				op_pin_state_pinned, 0, oparity_zero, 0, sizeof(imm_attr_t));
	}

	if(!push_type)
		push_type = new_type_pointer(new_id_from_str("push_ptr"), get_glob_type());

}

static int firm_get_n_reg_class(void)
{
  return N_CLASSES;
}

static const arch_register_class_t *firm_get_reg_class(int i)
{
  assert(i >= 0 && i < N_CLASSES);
  return &reg_classes[i];
}

static const arch_register_req_t firm_std_reg_req = {
  arch_register_req_type_normal,
  &reg_classes[CLS_DATAB],
  { NULL }
};

static const rflct_arg_t *get_arg(const ir_node *irn, int pos)
{
  int sig = rflct_get_signature(irn);
  const rflct_arg_t *args =
    rflct_get_args(get_irn_opcode(irn), sig, arch_pos_is_in(pos));
  return &args[arch_pos_get_index(pos)];
}

static const arch_register_req_t *
firm_get_irn_reg_req(const arch_irn_ops_t *self,
    arch_register_req_t *req, const ir_node *irn, int pos)
{
  if(is_firm_be_mode(get_irn_mode(irn)))
    memcpy(req, &firm_std_reg_req, sizeof(*req));
  else
    req = NULL;

  return req;
}

static int firm_get_n_operands(const arch_irn_ops_t *self, const ir_node *irn, int in_out)
{
  int sig = rflct_get_signature(irn);
  return rflct_get_args_count(get_irn_opcode(irn), sig, in_out >= 0);
}

struct irn_reg_assoc {
  const ir_node *irn;
  int pos;
  const arch_register_t *reg;
};

static int cmp_irn_reg_assoc(const void *a, const void *b, size_t len)
{
  const struct irn_reg_assoc *x = a;
  const struct irn_reg_assoc *y = b;

  return !(x->irn == y->irn && x->pos == y->pos);
}

static struct irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, int pos)
{
  static set *reg_set = NULL;
  struct irn_reg_assoc templ;
  unsigned int hash;

  if(!reg_set)
    reg_set = new_set(cmp_irn_reg_assoc, 1024);

  templ.irn = irn;
  templ.pos = pos;
  templ.reg = NULL;
  hash = HASH_PTR(irn) + 7 * pos;

  return set_insert(reg_set, &templ, sizeof(templ), hash);
}

static void firm_set_irn_reg(const arch_irn_ops_t *self, ir_node *irn,
    int pos, const arch_register_t *reg)
{
  struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn, pos);
  assoc->reg = reg;
}

static const arch_register_t *firm_get_irn_reg(const arch_irn_ops_t *self,
    const ir_node *irn, int pos)
{
  struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn, pos);
  return assoc->reg;
}

static arch_irn_class_t firm_classify(const arch_irn_ops_t *self, const ir_node *irn)
{
    arch_irn_class_t res;

    switch(get_irn_opcode(irn)) {
        case iro_Cond:
        case iro_Jmp:
            res = arch_irn_class_branch;
            break;
        default:
            res = arch_irn_class_normal;
    }

	return res;
}

static const arch_irn_ops_t irn_ops = {
  firm_get_irn_reg_req,
  firm_get_n_operands,
  firm_set_irn_reg,
  firm_get_irn_reg,
  firm_classify
};

static const arch_irn_ops_t *firm_get_irn_ops(const arch_irn_handler_t *self,
    const ir_node *irn)
{
  return &irn_ops;
}

const arch_irn_handler_t firm_irn_handler = {
  firm_get_irn_ops,
};

static ir_node *new_PushEnd(ir_graph *irg, ir_node *bl, ir_node *arg)
{
	ir_node *ins[1];
	ins[0] = arg;
	return new_ir_node(NULL, irg, bl, op_push_end, mode_P, 1, ins);
}

static ir_node *new_Push(ir_graph *irg, ir_node *bl, ir_node *push, ir_node *arg)
{
	ir_node *ins[2];
	ins[0] = push;
	ins[1] = arg;
	return new_ir_node(NULL, irg, bl, op_push, mode_P, 2, ins);
}

static ir_node *new_Imm(ir_graph *irg, ir_node *bl, ir_node *cnst)
{
	ir_node *ins[1];
	ir_node *res;
	imm_attr_t *attr;

	res = new_ir_node(NULL, irg, bl, op_imm, get_irn_mode(cnst), 0, ins);
	attr = (imm_attr_t *) &res->attr;

	if(get_irn_opcode(cnst) == iro_SymConst) {
		attr->tp = imm_SymConst;
		attr->data.ent = get_SymConst_entity(cnst);
	}

	else {
		attr->tp = imm_Const;
		attr->data.tv = get_Const_tarval(cnst);
	}

	return res;
}

static void prepare_walker(ir_node *irn, void *data)
{
	opcode opc = get_irn_opcode(irn);

	/* A replacement for this node has already been computed. */
	if(get_irn_link(irn))
		return;

	if(opc == iro_Call) {
		ir_node *bl   = get_nodes_block(irn);
		ir_graph *irg = get_irn_irg(bl);

		ir_node *ins[1];
		ir_node *store   = get_Call_mem(irn);
		ir_node *ptr     = get_Call_ptr(irn);
		type *ct         = get_Call_type(irn);
		int np           = get_Call_n_params(irn) > 0 ? 1 : 0;

		if(np > 0) {
			char buf[128];
			ir_node *nc;
			ir_node *push;
			int i, n;
			type *nt;

			push = new_PushEnd(irg, bl, get_Call_param(irn, 0));

			for(i = 1, n = get_Call_n_params(irn); i < n; ++i) {
				push = new_Push(irg, bl, push, get_Call_param(irn, i));
			}

			snprintf(buf, sizeof(buf), "push_%s", get_type_name(ct));

			n = get_method_n_ress(ct);
			nt = new_type_method(new_id_from_str(buf), 1, n);
			for(i = 0; i < n; ++i)
				set_method_res_type(nt, i, get_method_res_type(ct, i));

			set_method_param_type(nt, 0, push_type);

			ins[0] = push;
			nc = new_r_Call(irg, bl, store, ptr, 1, ins, nt);
			exchange(irn, nc);
			set_irn_link(nc, nc);
		}
	}

#if 0
	else if(opc == iro_Const || opc == iro_SymConst) {
		ir_node *bl   = get_nodes_block(irn);
		ir_graph *irg = get_irn_irg(bl);

		ir_node *imm = new_Imm(irg, bl, irn);
		exchange(irn, imm);
		set_irn_link(imm, imm);
	}
#endif

}

static void clear_link(ir_node *irn, void *data)
{
	set_irn_link(irn, NULL);
}

static void firm_prepare_graph(ir_graph *irg)
{
	irg_walk_graph(irg, clear_link, NULL, NULL);
	irg_walk_graph(irg, NULL, prepare_walker, NULL);
}

const arch_isa_if_t firm_isa = {
  firm_init,
  firm_get_n_reg_class,
  firm_get_reg_class,
	firm_prepare_graph
};
