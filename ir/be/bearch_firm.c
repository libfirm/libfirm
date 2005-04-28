
/**
 * ISA implementation for Firm IR nodes.
 */

#include "bitset.h"
#include "bearch.h"

#define N_REGS 1024

static arch_register_t gp_regs[N_REGS];
static arch_register_t fp_regs[N_REGS];

static arch_register_class_t reg_classes[] = {
  { "gp", NULL, N_REGS, gp_regs },
  { "fp", NULL, N_REGS, fp_regs }
};

#define N_CLASSES \
  (sizeof(reg_classes) / sizeof(reg_classes[0]))

#define CLS_GP 0
#define CLS_FP 1

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

static const arch_register_class_t *firm_get_irn_reg_class(const ir_node *irn)
{
  ir_mode *mode = get_irn_mode(irn);

  if(mode_is_float(mode))
    return &reg_classes[CLS_FP];
  else if(mode_is_datab(mode))
    return &reg_classes[CLS_GP];

  return NULL;
}

static int firm_get_allocatable_regs(const ir_node *irn,
    const arch_register_class_t *cls, bitset_t *bs)
{
  int res = 0;

  if(firm_get_irn_reg_class(irn) != cls) {
    if(bs)
      bitset_clear_all(bs);
  }

  else {
    int i;

    res = cls->n_regs;
    if(bs) {
      for(i = 0; i < cls->n_regs; ++i)
        bitset_set(bs, i);
    }
  }

  return res;
}

const arch_isa_if_t arch_isa_if_firm = {
  firm_init,
  firm_get_n_reg_class,
  firm_get_reg_class,
  firm_get_allocatable_regs,
  firm_get_irn_reg_class,
  NULL
};
