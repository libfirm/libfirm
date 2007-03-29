#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"

#include "besched_t.h"
#include "beinsn_t.h"
#include "beabi.h"
#include "raw_bitset.h"

be_insn_t *be_scan_insn(const be_insn_env_t *env, ir_node *irn)
{
	const arch_env_t *arch_env = env->aenv;
	struct obstack *obst       = env->obst;
	be_operand_t o;
	be_insn_t *insn;
	int i, n;
	int pre_colored = 0;

	insn = obstack_alloc(obst, sizeof(insn[0]));
	memset(insn, 0, sizeof(insn[0]));

	insn->irn       = irn;
	insn->next_insn = sched_next(irn);
	if(get_irn_mode(irn) == mode_T) {
		ir_node *p;

		for(p = sched_next(irn); is_Proj(p); p = sched_next(p)) {
			if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, p)) {
				o.req             = arch_get_register_req(arch_env, p, -1);
				o.carrier         = p;
				o.irn             = irn;
				o.pos             = -(get_Proj_proj(p) + 1);
				o.partner         = NULL;
				o.has_constraints = arch_register_req_is(o.req, limited);
				obstack_grow(obst, &o, sizeof(o));
				insn->n_ops++;
				insn->out_constraints |= o.has_constraints;
				pre_colored += arch_get_irn_register(arch_env, p) != NULL;
			}
		}

		insn->next_insn = p;
	} else if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, irn)) {
		o.req     = arch_get_register_req(arch_env, irn, -1);
		o.carrier = irn;
		o.irn     = irn;
		o.pos     = -1;
		o.partner = NULL;
		o.has_constraints = arch_register_req_is(o.req, limited);
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
		insn->out_constraints |= o.has_constraints;
		pre_colored += arch_get_irn_register(arch_env, irn) != NULL;
	}

	if(pre_colored > 0) {
		assert(pre_colored == insn->n_ops && "partly pre-colored nodes not supported");
		insn->pre_colored = 1;
	}
	insn->use_start   = insn->n_ops;

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if(!arch_irn_consider_in_reg_alloc(arch_env, env->cls, op))
			continue;

		o.req     = arch_get_register_req(arch_env, irn, i);
		o.carrier = op;
		o.irn     = irn;
		o.pos     = i;
		o.partner = NULL;
		o.has_constraints = arch_register_req_is(o.req, limited);
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
		insn->in_constraints |= o.has_constraints;
	}

	insn->has_constraints = insn->in_constraints | insn->out_constraints;
	insn->ops = obstack_finish(obst);

	/* Compute the admissible registers bitsets. */
	for (i = 0; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];
		const arch_register_req_t   *req = op->req;
		const arch_register_class_t *cls = req->cls;
		arch_register_req_type_t    type = req->type;

		/* If there is no special requirement, we allow current class here */
		if (cls == NULL && req->type == arch_register_req_type_none) {
			cls  = env->cls;
			type = arch_register_req_type_normal;
		}

		assert(cls == env->cls);

		op->regs = bitset_obstack_alloc(obst, env->cls->n_regs);

		if (type & arch_register_req_type_limited) {
			rbitset_copy_to_bitset(req->limited, op->regs);
		} else {
			arch_put_non_ignore_regs(arch_env, env->cls, op->regs);
			if (env->ignore_colors)
				bitset_andnot(op->regs, env->ignore_colors);
		}
	}

	return insn;
}

be_insn_env_t *be_insn_env_init(be_insn_env_t *ie, const be_irg_t *birg, const arch_register_class_t *cls, struct obstack *obst)
{
	ie->aenv = birg->main_env->arch_env;
	ie->cls  = cls;
	ie->obst = obst;
	ie->ignore_colors = bitset_obstack_alloc(obst, cls->n_regs);
	be_abi_put_ignore_regs(birg->abi, cls, ie->ignore_colors);

	return ie;
}
