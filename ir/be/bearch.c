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
  return ops->impl->get_irn_reg_req(ops, req, irn, pos);
}

void arch_set_stack_bias(const arch_env_t *env, ir_node *irn, int bias)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  ops->impl->set_stack_bias(ops, irn, bias);
}

entity *arch_get_frame_entity(const arch_env_t *env, ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->impl->get_frame_entity(ops, irn);
}


int arch_get_allocatable_regs(const arch_env_t *env, const ir_node *irn, int pos, bitset_t *bs)
{
  arch_register_req_t local_req;
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  const arch_register_req_t *req = ops->impl->get_irn_reg_req(ops, &local_req, irn, pos);

  if(req->type == arch_register_req_type_none) {
	  bitset_clear_all(bs);
	  return 0;
  }

  if(arch_register_req_is(req, limited)) {
	  req->limited(req->limited_env, bs);
	  return bitset_popcnt(bs);
  }

  arch_register_class_put(req->cls, bs);
  return req->cls->n_regs;
}

void arch_put_non_ignore_regs(const arch_env_t *env, const arch_register_class_t *cls, bitset_t *bs)
{
	int i;

	for(i = 0; i < cls->n_regs; ++i) {
		if(!arch_register_type_is(&cls->regs[i], ignore))
			bitset_set(bs, i);
	}
}

int arch_is_register_operand(const arch_env_t *env,
    const ir_node *irn, int pos)
{
	arch_register_req_t local_req;
	const arch_irn_ops_t *ops = get_irn_ops(env, irn);
	const arch_register_req_t *req = ops->impl->get_irn_reg_req(ops, &local_req, irn, pos);
	return req != NULL;
}

int arch_reg_is_allocatable(const arch_env_t *env, const ir_node *irn,
    int pos, const arch_register_t *reg)
{
	arch_register_req_t req;

	arch_get_register_req(env, &req, irn, pos);

	if(req.type == arch_register_req_type_none)
		return 0;

	if(arch_register_req_is(&req, limited)) {
		bitset_t *bs = bitset_alloca(req.cls->n_regs);
		req.limited(req.limited_env, bs);
		return bitset_is_set(bs, arch_register_get_index(reg));
	}

	return req.cls == reg->reg_class;
}

const arch_register_class_t *
arch_get_irn_reg_class(const arch_env_t *env, const ir_node *irn, int pos)
{
  arch_register_req_t local_req;
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  const arch_register_req_t *req = ops->impl->get_irn_reg_req(ops, &local_req, irn, pos);
  return req ? req->cls : NULL;
}

extern const arch_register_t *
arch_get_irn_register(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->impl->get_irn_reg(ops, irn);
}

extern void arch_set_irn_register(const arch_env_t *env,
    ir_node *irn, const arch_register_t *reg)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  ops->impl->set_irn_reg(ops, irn, reg);
}

extern arch_irn_class_t arch_irn_classify(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->impl->classify(ops, irn);
}

extern arch_irn_flags_t arch_irn_get_flags(const arch_env_t *env, const ir_node *irn)
{
  const arch_irn_ops_t *ops = get_irn_ops(env, irn);
  return ops->impl->get_flags(ops, irn);
}

extern const char *arch_irn_flag_str(arch_irn_flags_t fl)
{
	switch(fl) {
#define XXX(x) case arch_irn_flags_ ## x: return #x;
		XXX(dont_spill);
		XXX(ignore);
		XXX(rematerializable);
		XXX(none);
#undef XXX
	}
	return "n/a";
}

extern char *arch_register_req_format(char *buf, size_t len, const arch_register_req_t *req)
{
	char tmp[128];
	snprintf(buf, len, "class: %s", req->cls->name);

	if(arch_register_req_is(req, limited)) {
		bitset_pos_t elm;
		bitset_t *bs = bitset_alloca(req->cls->n_regs);
		req->limited(req->limited_env, bs);
		strncat(buf, " limited:", len);
		bitset_foreach(bs, elm) {
			strncat(buf, " ", len);
			strncat(buf, req->cls->regs[elm].name, len);
		}
	}

	if(arch_register_req_is(req, should_be_same)) {
		snprintf(tmp, sizeof(tmp), " same to: %+F", req->other_different);
		strncat(buf, tmp, len);
	}

	if(arch_register_req_is(req, should_be_different)) {
		snprintf(tmp, sizeof(tmp), " different to: %+F", req->other_different);
		strncat(buf, tmp, len);
	}

	return buf;
}
