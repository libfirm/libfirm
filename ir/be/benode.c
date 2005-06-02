/**
 * @file   benode.c
 * @date   17.05.2005
 * @author Sebastian Hack
 *
 * Backend node support.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include <stdlib.h>

#include "obst.h"
#include "set.h"
#include "pmap.h"
#include "offset.h"

#include "irop_t.h"
#include "irmode_t.h"
#include "irnode_t.h"

#include "benode_t.h"

struct _be_node_factory_t {
  const arch_isa_if_t *isa;

  struct obstack      obst;
  set                 *ops;
  pmap                *irn_op_map;
  pmap                *reg_req_map;

  arch_irn_handler_t  handler;
  arch_irn_ops_t      irn_ops;
};

typedef enum _node_kind_t {
  node_kind_spill,
  node_kind_reload,
  node_kind_perm,
  node_kind_copy,
  node_kind_last
} node_kind_t;

typedef struct {
  node_kind_t kind;
  const arch_register_class_t *cls;
  ir_op *op;
  int n_pos;
  int *pos;
} be_op_t;

static int templ_pos_Spill[] = {
  arch_pos_make_in(0)
};

static int templ_pos_Reload[] = {
  arch_pos_make_out(0)
};

static int templ_pos_Copy[] = {
  arch_pos_make_in(0),
  arch_pos_make_out(0)
};

#define ARRSIZE(x) (sizeof(x) / sizeof(x[0]))

static int cmp_op_map(const void *a, const void *b, size_t size)
{
  const be_op_t *x = a;
  const be_op_t *y = b;

  return !(x->kind == y->kind && x->cls == y->cls);
}

static be_op_t *get_op(const be_node_factory_t *fact,
    const arch_register_class_t *cls, node_kind_t kind)
{
  be_op_t templ;

  templ.kind = kind;
  templ.cls = cls;

  return set_insert(fact->ops, &templ, sizeof(templ),
      HASH_PTR(cls) + 7 * kind);
}

ir_node *new_Spill(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *bl, ir_node *node_to_spill)
{
  ir_node *in[1];
  ir_op *op = get_op(factory, cls, node_kind_spill)->op;

  assert(op && "Spill opcode must be present for this register class");
  in[0] = node_to_spill;

  return new_ir_node(NULL, irg, bl, op, mode_M, 1, in);
}

ir_node *new_Reload(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *bl, ir_node *spill_node)
{
  ir_mode *mode;
  ir_node *in[1];
  ir_op *op = get_op(factory, cls, node_kind_reload)->op;

  assert(op && "Reload opcode must be present for this register class");
  assert(is_Spill(factory, spill_node) && "Operand of Reload must be a Spill");
  in[0] = spill_node;
  mode = get_irn_mode(get_irn_n(spill_node, 0));

  return new_ir_node(NULL, irg, bl, op, mode, 1, in);
}

ir_node *new_Perm(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *bl, int arity, ir_node **in)
{
  ir_op *op = get_op(factory, cls, node_kind_perm)->op;

  return new_ir_node(NULL, irg, bl, op, mode_T, arity, in);
}

ir_node *new_Copy(const be_node_factory_t *factory,
    const arch_register_class_t *cls,
    ir_graph *irg, ir_node *bl, ir_node *in)
{
  ir_node *ins[1];
  ir_op *op = get_op(factory, cls, node_kind_perm)->op;

  ins[0] = in;

  return new_ir_node(NULL, irg, bl, op, get_irn_mode(in), 1, ins);
}

/**
 * If the node is a proj, reset the node to the proj's target and return
 * the proj number.
 * @param node The address of a node pointer.
 * @param def  A default value.
 * @return     If *node is a Proj, *node is set to the Proj's target and
 *             the Proj number is returned. Else *node remains the same and @p def
 *             is returned.
 */
static int redir_proj(const ir_node **node, int def)
{
  const ir_node *n = *node;

  if(is_Proj(n)) {
    *node = get_Proj_pred(n);
    def = get_Proj_proj(n);
  }

  return def;
}

static const arch_register_req_t *
be_node_get_irn_reg_req(const arch_irn_ops_t *_self,
    arch_register_req_t *req, const ir_node *irn, int pos)
{
  be_op_t *bo;
  const be_node_factory_t *factory =
    container_of(_self, const be_node_factory_t, irn_ops);

  pos = arch_pos_make_out(redir_proj(&irn, arch_pos_get_index(pos)));

  bo = pmap_get(factory->irn_op_map, get_irn_op(irn));

  if(bo) {
    int i;

    req->type = arch_register_req_type_normal;
    req->cls = bo->cls;

    for(i = 0; i < bo->n_pos; ++pos)
      if(pos == bo->pos[i])
        return req;
  }

  return NULL;
}

static int
be_node_get_n_operands(const arch_irn_ops_t *_self, const ir_node *irn, int in_out)
{
  be_op_t *bo;
  int i, res = 0;
  const be_node_factory_t *factory =
    container_of(_self, const be_node_factory_t, irn_ops);

  redir_proj(&irn, 0);
  bo = pmap_get(factory->irn_op_map, get_irn_op(irn));

  for(i = 0; i < bo->n_pos; ++i)
    res += (bo->pos[i] ^ in_out) > 0;

  return res;
}

void
be_node_set_irn_reg(const arch_irn_ops_t *_self, ir_node *irn,
    int idx, const arch_register_t *reg)
{
  const arch_register_t **regs;

  idx = redir_proj((const ir_node **) &irn, idx);
  regs = (const arch_register_t **) &irn->attr;
  regs[idx] = reg;
}

const arch_register_t *
be_node_get_irn_reg(const arch_irn_ops_t *_self, const ir_node *irn, int idx)
{
  const arch_register_t **regs;

  idx = redir_proj(&irn, idx);
  regs = (const arch_register_t **) &irn->attr;
  return regs[idx];
}

arch_irn_class_t be_node_classify(const arch_irn_ops_t *_self, const ir_node *irn)
{
  const be_node_factory_t *factory =
    container_of(_self, const be_node_factory_t, irn_ops);

  be_op_t *bo;
  int idx;

  idx = redir_proj(&irn, 0);
  bo = pmap_get(factory->irn_op_map, get_irn_op(irn));

  return 0;
}

static const arch_irn_ops_t *
be_node_get_irn_ops(const arch_irn_handler_t *_self, const ir_node *irn)
{
  be_op_t *bo;
  const be_node_factory_t *factory =
    container_of(_self, const be_node_factory_t, handler);

  redir_proj(&irn, 0);
  bo = pmap_get(factory->irn_op_map, get_irn_op(irn));

  return bo ? &factory->irn_ops : NULL;
}

const arch_irn_handler_t *be_node_get_irn_handler(const be_node_factory_t *f)
{
  return &f->handler;
}

int is_Spill(const be_node_factory_t *f, const ir_node *irn)
{
  be_op_t *bo;
  bo = pmap_get(f->irn_op_map, get_irn_op(irn));
  return bo->kind == node_kind_spill;
}

be_node_factory_t *be_new_node_factory(const arch_isa_if_t *isa)
{
  be_node_factory_t *factory;
  char buf[256];
  int i, j, n;

  factory = malloc(sizeof(*factory));
  factory->ops = new_set(cmp_op_map, 64);
  factory->irn_op_map = pmap_create();
  obstack_init(&factory->obst);

  factory->handler.get_irn_ops = be_node_get_irn_ops;

  factory->irn_ops.get_irn_reg_req = be_node_get_irn_reg_req;
  factory->irn_ops.get_n_operands  = be_node_get_n_operands;
  factory->irn_ops.set_irn_reg     = be_node_set_irn_reg;
  factory->irn_ops.get_irn_reg     = be_node_get_irn_reg;
  factory->irn_ops.classify        = be_node_classify;

  for(i = 0, n = isa->get_n_reg_class(); i < n; ++i) {
    const arch_register_class_t *cls = isa->get_reg_class(i);
    be_op_t *ent;

    ent = get_op(factory, cls, node_kind_spill);
    snprintf(buf, sizeof(buf), "Spill_%s", cls->name);
    ent->op = new_ir_op(get_next_ir_opcode(), buf, op_pin_state_pinned,
        0, 0, oparity_unary, 0);
    ent->n_pos = ARRSIZE(templ_pos_Spill);
    ent->pos = templ_pos_Spill;
    pmap_insert(factory->irn_op_map, ent->op, ent);

    ent = get_op(factory, cls, node_kind_reload);
    snprintf(buf, sizeof(buf), "Reload_%s", cls->name);
    ent->op = new_ir_op(get_next_ir_opcode(), buf, op_pin_state_pinned, 0, 0,
        oparity_unary, sizeof(const arch_register_t *));
    ent->n_pos = ARRSIZE(templ_pos_Reload);
    ent->pos = templ_pos_Reload;
    pmap_insert(factory->irn_op_map, ent->op, ent);

    ent = get_op(factory, cls, node_kind_copy);
    snprintf(buf, sizeof(buf), "Copy_%s", cls->name);
    ent->op = new_ir_op(get_next_ir_opcode(), buf, op_pin_state_pinned, 0, 0,
        oparity_unary, sizeof(const arch_register_t *));
    ent->n_pos = ARRSIZE(templ_pos_Copy);
    ent->pos = templ_pos_Copy;
    pmap_insert(factory->irn_op_map, ent->op, ent);

    ent = get_op(factory, cls, node_kind_perm);
    snprintf(buf, sizeof(buf), "Perm_%s", cls->name);
    ent->op = new_ir_op(get_next_ir_opcode(), buf, op_pin_state_pinned, 0, 0,
        oparity_variable, sizeof(const arch_register_t) * cls->n_regs);
    ent->n_pos = 2 * cls->n_regs;
    ent->pos = obstack_alloc(&factory->obst, sizeof(ent->pos[0]) * ent->n_pos);
    for(j = 0; j < ent->n_pos; j += 2) {
      ent->pos[j] = arch_pos_make_in(j);
      ent->pos[j + 1] = arch_pos_make_out(j);
    }
    pmap_insert(factory->irn_op_map, ent->op, ent);

  }

  return factory;
}

#if 0
ir_node *be_spill(const be_node_factory_t *factory,
    const arch_env_t *env, ir_node *node_to_spill)
{
  ir_node *res;
  if(is_Reload(node_to_spill))
    res = get_irn_n(node_to_spill, 0);
  else {
    ir_node *bl = get_nodes_block(node_to_spill);
    ir_graph *irg = get_irn_irg(bl);

    res = new_Spill(factory, cls, irg, bl, node_to_spill);
  }

  return res;
}

ir_node *be_reload(const be_node_factory_t *factory,
    const arch_env_t *env, ir_node *spill)
{
}
#endif
