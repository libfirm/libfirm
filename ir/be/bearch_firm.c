
/**
 * ISA implementation for Firm IR nodes.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bitset.h"
#include "bearch.h"

#include "irreflect.h"

#define N_REGS 64

static arch_register_t datab_regs[N_REGS];

static arch_register_class_t reg_classes[] = {
  { "datab", N_REGS, datab_regs },
};

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
firm_get_irn_reg_req(const ir_node *irn, int pos)
{
  return mode_is_datab(get_irn_mode(irn)) ? &firm_std_reg_req : NULL;
}

static int firm_get_n_operands(const ir_node *irn, int in_out)
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

static void firm_set_irn_reg(ir_node *irn, int pos, const arch_register_t *reg)
{
  struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn, pos);
  assoc->reg = reg;
}

static const arch_register_t *firm_get_irn_reg(const ir_node *irn, int pos)
{
  struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn, pos);
  return assoc->reg;
}

static arch_irn_class_t firm_classify(const ir_node *irn)
{
  return arch_irn_class_normal;
}

static const arch_irn_ops_t irn_ops = {
  firm_get_irn_reg_req,
  firm_get_n_operands,
  firm_set_irn_reg,
  firm_get_irn_reg,
  firm_classify
};

const arch_isa_if_t firm_isa = {
  firm_init,
  firm_get_n_reg_class,
  firm_get_reg_class
};

static const arch_irn_ops_t *firm_get_irn_ops(const ir_node *irn)
{
  return &irn_ops;
}

const arch_irn_handler_t firm_irn_handler = {
  firm_get_irn_ops,
};
