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

arch_env_t *arch_env_init(arch_env_t *env, const arch_isa_if_t *isa_if)
{
  memset(env, 0, sizeof(*env));
  env->isa = isa_if->init();
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

int arch_register_class_put(const arch_register_class_t *cls, bitset_t *bs)
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
  req->type = arch_register_req_type_none;
  return ops->get_irn_reg_req(ops, req, irn, pos);
}

int arch_get_allocatable_regs(const arch_env_t *env, const ir_node *irn,
    int pos, const arch_register_class_t *cls, bitset_t *bs)
{
  arch_register_req_t local_req;
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  const arch_register_req_t *req = ops->get_irn_reg_req(ops, &local_req, irn, pos);

  if(arch_register_req_is(req, none)) {
	  bitset_clear_all(bs);
	  return 0;
  }

  if(arch_register_req_is(req, limited))
	  return req->limited(irn, pos, bs);

  arch_register_class_put(req->cls, bs);
  return req->cls->n_regs;
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
	int res = 0;
	arch_register_req_t req;

	arch_get_register_req(env, &req, irn, pos);
	switch(req.type) {
		case arch_register_req_type_normal:
		case arch_register_req_type_should_be_different:
		case arch_register_req_type_should_be_same:
			res = req.cls == reg->reg_class;
			break;
		case arch_register_req_type_limited:
			{
				bitset_t *bs = bitset_alloca(req.cls->n_regs);
				req.limited(irn, pos, bs);
				res = bitset_is_set(bs, arch_register_get_index(reg));
			}
			break;
		default:
			res = 0;
	}

	return res;
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
arch_get_irn_register(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->get_irn_reg(ops, irn);
}

extern void arch_set_irn_register(const arch_env_t *env,
    ir_node *irn, const arch_register_t *reg)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  ops->set_irn_reg(ops, irn, reg);
}

extern arch_irn_class_t arch_irn_classify(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->classify(ops, irn);
}

extern arch_irn_flags_t arch_irn_get_flags(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->get_flags(ops, irn);
}
