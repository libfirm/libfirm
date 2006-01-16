
/**
 * ISA implementation for Firm IR nodes.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#endif

#include "bitset.h"
#include "obst.h"

#include "irmode_t.h"
#include "irnode_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "type.h"

#include "../bearch.h"
#include "../besched.h"
#include "../beutil.h"

#define N_REGS 3

typedef struct {
  enum  { imm_Const, imm_SymConst } tp;
  union {
    const_attr    cnst_attr;
    symconst_attr symc_attr;
  } data;
} imm_attr_t;

static arch_register_t datab_regs[N_REGS];

static arch_register_class_t reg_classes[] = {
  { "datab", N_REGS, datab_regs },
};

static ir_op *op_push;
static ir_op *op_imm;

const arch_isa_if_t firm_isa;

#define N_CLASSES \
  (sizeof(reg_classes) / sizeof(reg_classes[0]))

#define CLS_DATAB 0

tarval *get_Imm_tv(ir_node *n) {
  imm_attr_t *attr = (imm_attr_t *)get_irn_generic_attr(n);
  return attr->tp == imm_Const ? attr->data.cnst_attr.tv : NULL;
}

int is_Imm(const ir_node *irn) {
  return get_irn_op(irn) == op_imm;
}

static int dump_node_Imm(ir_node *n, FILE *F, dump_reason_t reason) {
  ir_mode    *mode;
  int        bad = 0;
  char       buf[1024];
  tarval     *tv;
  imm_attr_t *attr;

  switch (reason) {
    case dump_node_opcode_txt:
      tv = get_Imm_tv(n);

      if (tv) {
        tarval_snprintf(buf, sizeof(buf), tv);
        fprintf(F, "%s", buf);
      }
      else {
        fprintf(F, "immSymC");
      }
      break;

    case dump_node_mode_txt:
      mode = get_irn_mode(n);

      if (mode && mode != mode_BB && mode != mode_ANY && mode != mode_BAD && mode != mode_T) {
        fprintf(F, "[%s]", get_mode_name(mode));
      }
      break;

    case dump_node_nodeattr_txt:
      attr = (imm_attr_t *)get_irn_generic_attr(n);

      if (is_Imm(n) && attr->tp == imm_SymConst) {
        const char *name    = NULL;
        symconst_attr *sc_attr = &attr->data.symc_attr;

        switch (sc_attr->num) {
          case symconst_addr_name:
            name = get_id_str(sc_attr->sym.ident_p);
            break;

          case symconst_addr_ent:
            name = get_entity_ld_name(sc_attr->sym.entity_p);
            break;

          default:
            assert(!"Unsupported SymConst");
        }

        fprintf(F, "&%s ", name);
      }

      break;

    case dump_node_info_txt:
      break;
  }

  return bad;
}

static void *firm_init(void)
{
  static struct obstack obst;
  static int inited = 0;
	arch_isa_t *isa = malloc(sizeof(*isa));
  int k;

	isa->impl = &firm_isa;

  if(inited)
    return NULL;

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
	if(!op_push) {
		int push_opc = get_next_ir_opcode();

		op_push = new_ir_op(push_opc, "Push",
				op_pin_state_pinned, 0, oparity_binary, 0, 0, NULL);
	}

	if(!op_imm) {
		int imm_opc = get_next_ir_opcode();
		ir_op_ops ops;

		memset(&ops, 0, sizeof(ops));
		ops.dump_node = dump_node_Imm;

		op_imm = new_ir_op(imm_opc, "Imm",
				op_pin_state_pinned, 0, oparity_zero, 0, sizeof(imm_attr_t), &ops);
	}

	return isa;
}

static void firm_done(void *self)
{
	free(self);
}

static int firm_get_n_reg_class(const void *self)
{
  return N_CLASSES;
}

static const arch_register_class_t *firm_get_reg_class(const void *self, int i)
{
  assert(i >= 0 && i < N_CLASSES);
  return &reg_classes[i];
}

static const arch_register_req_t firm_std_reg_req = {
  arch_register_req_type_normal,
  &reg_classes[CLS_DATAB],
  NULL,
  0
};

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

struct irn_reg_assoc {
  const ir_node *irn;
  const arch_register_t *reg;
};

static int cmp_irn_reg_assoc(const void *a, const void *b, size_t len)
{
  const struct irn_reg_assoc *x = a;
  const struct irn_reg_assoc *y = b;

  return x->irn != y->irn;
}

static struct irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn)
{
  static set *reg_set = NULL;
  struct irn_reg_assoc templ;

  if(!reg_set)
    reg_set = new_set(cmp_irn_reg_assoc, 1024);

  templ.irn = irn;
  templ.reg = NULL;

  return set_insert(reg_set, &templ, sizeof(templ), HASH_PTR(irn));
}

static void firm_set_irn_reg(const arch_irn_ops_t *self, ir_node *irn, const arch_register_t *reg)
{
  struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn);
  assoc->reg = reg;
}

static const arch_register_t *firm_get_irn_reg(const arch_irn_ops_t *self, const ir_node *irn)
{
  struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn);
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

static arch_irn_flags_t firm_get_flags(const arch_irn_ops_t *self, const ir_node *irn)
{
	arch_irn_flags_t res = arch_irn_flags_spillable;

	if(get_irn_op(irn) == op_imm)
		res |= arch_irn_flags_rematerializable;

	switch(get_irn_opcode(irn)) {
		case iro_Add:
		case iro_Sub:
		case iro_Shl:
		case iro_Shr:
		case iro_Shrs:
		case iro_And:
		case iro_Or:
		case iro_Eor:
		case iro_Not:
			res |= arch_irn_flags_rematerializable;
		default:
			res = res;
	}

	return res;
}

static const arch_irn_ops_t irn_ops = {
  firm_get_irn_reg_req,
  firm_set_irn_reg,
  firm_get_irn_reg,
  firm_classify,
	firm_get_flags
};

static const arch_irn_ops_t *firm_get_irn_ops(const arch_irn_handler_t *self,
    const ir_node *irn)
{
  return &irn_ops;
}

const arch_irn_handler_t firm_irn_handler = {
  firm_get_irn_ops,
};

static ir_node *new_Push(ir_graph *irg, ir_node *bl, ir_node *push, ir_node *arg)
{
	ir_node *ins[2];
	ins[0] = push;
	ins[1] = arg;
	return new_ir_node(NULL, irg, bl, op_push, mode_M, 2, ins);
}

/**
 * Creates an op_Imm node from an op_Const.
 */
static ir_node *new_Imm(ir_graph *irg, ir_node *bl, ir_node *cnst) {
  ir_node    *ins[1];
  ir_node    *res;
  imm_attr_t *attr;

  res = new_ir_node(NULL, irg, bl, op_imm, get_irn_mode(cnst), 0, ins);
  attr = (imm_attr_t *) &res->attr;

  switch (get_irn_opcode(cnst)) {
    case iro_Const:
      attr->tp      = imm_Const;
      attr->data.cnst_attr = get_irn_const_attr(cnst);
      break;
    case iro_SymConst:
      attr->tp             = imm_SymConst;
      attr->data.symc_attr = get_irn_symconst_attr(cnst);
      break;
    case iro_Unknown:
      break;
    default:
      assert(0 && "Cannot create Imm for this opcode");
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

		ir_node *store   = get_Call_mem(irn);
		ir_node *ptr     = get_Call_ptr(irn);
		ir_type *ct      = get_Call_type(irn);
		int np           = get_Call_n_params(irn) > 0 ? 1 : 0;

		if(np > 0) {
			ir_node *ins[1];
			char buf[128];
			ir_node *nc;
			int i, n = get_Call_n_params(irn);
			ir_type *nt;
      unsigned cc = get_method_calling_convention(get_Call_type(irn));

      if (cc & cc_last_on_top) {
			  store = new_Push(irg, bl, store, get_Call_param(irn, 0));

			  for (i = 1; i < n; ++i)
				  store = new_Push(irg, bl, store, get_Call_param(irn, i));
      }
      else {
        store = new_Push(irg, bl, store, get_Call_param(irn, n - 1));

        for (i = n - 2; i >= 0; --i)
          store = new_Push(irg, bl, store, get_Call_param(irn, i));
      }

			snprintf(buf, sizeof(buf), "push_%s", get_type_name(ct));

			n = get_method_n_ress(ct);
			nt = new_type_method(new_id_from_str(buf), 0, n);
			for(i = 0; i < n; ++i)
				set_method_res_type(nt, i, get_method_res_type(ct, i));

			nc = new_r_Call(irg, bl, store, ptr, 0, ins, nt);
			exchange(irn, nc);
			set_irn_link(nc, nc);
		}
	}
}

static void localize_const_walker(ir_node *irn, void *data)
{
	if(!is_Block(irn)) {
		int i, n;
		ir_node *bl = get_nodes_block(irn);

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op    = get_irn_n(irn, i);
			opcode opc     = get_irn_opcode(op);

			if(opc == iro_Const
			|| opc == iro_Unknown
			|| (opc == iro_SymConst /*&& get_SymConst_kind(op) == symconst_addr_ent*/)) {
				ir_graph *irg   = get_irn_irg(bl);
				ir_node *imm_bl = is_Phi(irn) ? get_Block_cfgpred_block(bl, i) : bl;

				ir_node *imm = new_Imm(irg, imm_bl, op);
				set_irn_n(irn, i, imm);
			}
		}
	}
}

static const arch_irn_handler_t *firm_get_irn_handler(const void *self)
{
	return &firm_irn_handler;
}

typedef struct _firm_code_gen_t {
	const arch_code_generator_if_t *impl;
	ir_graph *irg;
} firm_code_gen_t;


static void clear_link(ir_node *irn, void *data)
{
	set_irn_link(irn, NULL);
}

static void firm_prepare_graph(void *self)
{
	firm_code_gen_t *cg = self;

	irg_walk_graph(cg->irg, clear_link, localize_const_walker, NULL);
	irg_walk_graph(cg->irg, NULL, prepare_walker, NULL);
}

static void firm_before_sched(void *self)
{
}

static void imm_scheduler(ir_node *irn, void *env) {
	if(is_Imm(irn)) {
		const ir_edge_t *e;
		ir_node *user, *user_block, *before, *tgt_block;

		if (1 != get_irn_n_edges(irn)) {
			printf("Out edges: %d\n", get_irn_n_edges(irn));
			assert(1 == get_irn_n_edges(irn));
		}

		e = get_irn_out_edge_first(irn);
		user = e->src;
		user_block = get_nodes_block(user);
		if (is_Phi(user)) {
			before = get_Block_cfgpred_block(user_block, e->pos);
			tgt_block = before;
		} else {
			before = user;
			tgt_block = user_block;
		}

		sched_remove(irn);
		set_nodes_block(irn, tgt_block);
		sched_add_before(before, irn);
	}
}

static void firm_before_ra(void *self)
{
	firm_code_gen_t *cg = self;
	irg_walk_graph(cg->irg, imm_scheduler, NULL, NULL);
}

static void firm_codegen_done(void *self)
{
	free(self);
}

static void *firm_cg_init(FILE *file_handle, ir_graph *irg, const arch_env_t *env);

static const arch_code_generator_if_t firm_code_gen = {
	firm_cg_init,
	firm_prepare_graph,
	firm_before_sched,
	firm_before_ra,
	firm_codegen_done
};

static void *firm_cg_init(FILE *file_handle, ir_graph *irg, const arch_env_t *env)
{
	firm_code_gen_t *cg = xmalloc(sizeof(*cg));
	cg->impl = &firm_code_gen;
	cg->irg  = irg;
	return cg;
}


static const arch_code_generator_if_t *firm_get_code_generator(void *self)
{
	return &firm_code_gen;
}

static const list_sched_selector_t *firm_get_list_sched_selector(const void *self) {
	return trivial_selector;
}

#ifdef WITH_LIBCORE
static void firm_register_options(lc_opt_entry_t *ent)
{
}
#endif

const arch_isa_if_t firm_isa = {
#ifdef WITH_LIBCORE
	firm_register_options,
#endif
	firm_init,
	firm_done,
	firm_get_n_reg_class,
	firm_get_reg_class,
	firm_get_irn_handler,
	firm_get_code_generator,
	firm_get_list_sched_selector
};
