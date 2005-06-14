/**
 * Processor architecture specification.
 * @author Sebastian Hack
 * @date 11.2.2005
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <string.h>

#include "bearch.h"
#include "ircons_t.h"

#include "bitset.h"
#include "pset.h"
#include "entity.h"

arch_env_t *arch_env_init(arch_env_t *env, const arch_isa_if_t *isa)
{
  memset(env, 0, sizeof(*env));
  env->isa = isa;
  return env;
}

arch_env_t *arch_env_add_irn_handler(arch_env_t *env,
    const arch_irn_handler_t *handler)
{
  assert(env->handlers_tos <= ARCH_MAX_HANDLERS);
  env->handlers[env->handlers_tos++] = handler;
  return env;
}

static const arch_irn_ops_t *fallback_irn_ops = NULL;

int arch_register_class_put(const arch_register_class_t *cls, struct _bitset_t *bs)
{
  if(bs) {
    int i, n;
    for(i = 0, n = cls->n_regs; i < n; ++i)
      bitset_set(bs, i);
  }

  return cls->n_regs;
}

/**
 * Get the isa responsible for a node.
 * @param env The arch environment with the isa stack.
 * @param irn The node to get the responsible isa for.
 * @return The irn operations given by the responsible isa.
 */
static INLINE const arch_irn_ops_t *
get_irn_ops(const arch_env_t *env, const ir_node *irn)
{
  int i;

  for(i = env->handlers_tos - 1; i >= 0; --i) {
    const arch_irn_handler_t *handler = env->handlers[i];
    const arch_irn_ops_t *ops = handler->get_irn_ops(handler, irn);

    if(ops)
      return ops;
  }

  return fallback_irn_ops;
}

const arch_register_req_t *arch_get_register_req(const arch_env_t *env,
    arch_register_req_t *req, const ir_node *irn, int pos)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->get_irn_reg_req(ops, req, irn, pos);
}

int arch_get_allocatable_regs(const arch_env_t *env, const ir_node *irn,
    int pos, const arch_register_class_t *cls, bitset_t *bs)
{
  arch_register_req_t local_req;
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  const arch_register_req_t *req = ops->get_irn_reg_req(ops, &local_req, irn, pos);

  switch(req->type) {
    case arch_register_req_type_normal:
      arch_register_class_put(req->cls, bs);
      return req->cls->n_regs;

    case arch_register_req_type_limited:
      return req->data.limited(irn, pos, bs);

    default:
      assert(0 && "This register requirement case is not covered");
  }

  return 0;
}

int arch_is_register_operand(const arch_env_t *env,
    const ir_node *irn, int pos)
{
  arch_register_req_t local_req;
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  const arch_register_req_t *req = ops->get_irn_reg_req(ops, &local_req, irn, pos);
  return req != NULL;
}

int arch_reg_is_allocatable(const arch_env_t *env, const ir_node *irn,
    int pos, const arch_register_t *reg)
{
  const arch_register_class_t *cls = arch_register_get_class(reg);
  int n_regs = arch_register_class_n_regs(cls);
  bitset_t *bs = bitset_alloca(n_regs);

  arch_get_allocatable_regs(env, irn, pos, cls, bs);
  return bitset_is_set(bs, arch_register_get_index(reg));
}

const arch_register_class_t *
arch_get_irn_reg_class(const arch_env_t *env, const ir_node *irn, int pos)
{
  arch_register_req_t local_req;
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  const arch_register_req_t *req = ops->get_irn_reg_req(ops, &local_req, irn, pos);
  return req ? req->cls : NULL;
}

extern const arch_register_t *
arch_get_irn_register(const arch_env_t *env, const ir_node *irn, int idx)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  assert(idx >= 0);
  return ops->get_irn_reg(ops, irn, idx);
}

extern void arch_set_irn_register(const arch_env_t *env,
    ir_node *irn, int idx, const arch_register_t *reg)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  assert(idx >= 0);
  ops->set_irn_reg(ops, irn, idx, reg);
}

extern arch_irn_class_t arch_irn_classify(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->classify(ops, irn);
}
