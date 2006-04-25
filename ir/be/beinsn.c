
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"

#include "besched_t.h"
#include "beinsn_t.h"
#include "beabi.h"

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
				arch_get_register_req(arch_env, &o.req, p, -1);
				o.carrier         = p;
				o.irn             = irn;
				o.pos             = -(get_Proj_proj(p) + 1);
				o.partner         = NULL;
				o.has_constraints = arch_register_req_is(&o.req, limited);
				obstack_grow(obst, &o, sizeof(o));
				insn->n_ops++;
				insn->out_constraints |= o.has_constraints;
				pre_colored += arch_get_irn_register(arch_env, p) != NULL;
			}
		}

		insn->next_insn = p;
	}

	else if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, irn)) {
		arch_get_register_req(arch_env, &o.req, irn, -1);
		o.carrier = irn;
		o.irn     = irn;
		o.pos     = -1;
		o.partner = NULL;
		o.has_constraints = arch_register_req_is(&o.req, limited);
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
		insn->out_constraints |= o.has_constraints;
		pre_colored += arch_get_irn_register(arch_env, irn) != NULL;
	}

	insn->pre_colored = pre_colored == insn->n_ops && insn->n_ops > 0;
	insn->use_start   = insn->n_ops;

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, op)) {
			arch_get_register_req(arch_env, &o.req, irn, i);
			o.carrier = op;
			o.irn     = irn;
			o.pos     = i;
			o.partner = NULL;
			o.has_constraints = arch_register_req_is(&o.req, limited);
			obstack_grow(obst, &o, sizeof(o));
			insn->n_ops++;
			insn->in_constraints |= o.has_constraints;
		}
	}

	insn->has_constraints = insn->in_constraints | insn->out_constraints;
	insn->ops = obstack_finish(obst);

	/* Compute the admissible registers bitsets. */
	for(i = 0; i < insn->n_ops; ++i) {
		be_operand_t *op = &insn->ops[i];

		assert(op->req.cls == env->cls);
		op->regs = bitset_obstack_alloc(obst, env->cls->n_regs);

		if(arch_register_req_is(&op->req, limited))
			op->req.limited(op->req.limited_env, op->regs);
		else {
			arch_put_non_ignore_regs(arch_env, env->cls, op->regs);
			if(env->ignore_colors)
				bitset_andnot(op->regs, env->ignore_colors);
		}

	}

	return insn;
}

be_insn_env_t *be_insn_env_init(be_insn_env_t *ie, be_irg_t *birg, const arch_register_class_t *cls, struct obstack *obst)
{
	ie->aenv = birg->main_env->arch_env;
	ie->cls  = cls;
	ie->obst = obst;
	be_abi_put_ignore_regs(birg->abi, cls, ie->ignore_colors);
	return ie;
}
