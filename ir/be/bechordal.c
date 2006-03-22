/**
 * Chordal register allocation.
 * @author Sebastian Hack
 * @date 8.12.2004
 *
 * Copyright (C) Universitaet Karlsruhe
 * Released under the GPL
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <ctype.h>

#include "obst.h"
#include "pset.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"
#include "bipartite.h"

#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom.h"
#include "debug.h"
#include "xmalloc.h"

#include "beutil.h"
#include "besched.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "benode_t.h"
#include "bearch.h"
#include "beifg.h"

#include "bechordal_t.h"
#include "bechordal_draw.h"

#define DBG_LEVEL SET_LEVEL_0
#define DBG_LEVEL_CHECK SET_LEVEL_0

#define NO_COLOR (-1)

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define DUMP_INTERVALS

typedef struct _be_chordal_alloc_env_t {
	be_chordal_env_t *chordal_env;

	firm_dbg_module_t *constr_dbg;  /**< Debug output for the constraint handler. */
	pset *pre_colored;              /**< Set of precolored nodes. */
	bitset_t *live;				    /**< A liveness bitset. */
	bitset_t *colors;			    /**< The color mask. */
	bitset_t *in_colors;            /**< Colors used by live in values. */
	bitset_t *ignore_regs;          /**< A bitset of all ignore registers in the current class. */
	int colors_n;                   /**< The number of colors. */
} be_chordal_alloc_env_t;

#include "fourcc.h"

/* Make a fourcc for border checking. */
#define BORDER_FOURCC				FOURCC('B', 'O', 'R', 'D')

static void check_border_list(struct list_head *head)
{
  border_t *x;
  list_for_each_entry(border_t, x, head, list) {
    assert(x->magic == BORDER_FOURCC);
  }
}

static void check_heads(be_chordal_env_t *env)
{
  pmap_entry *ent;
  for(ent = pmap_first(env->border_heads); ent; ent = pmap_next(env->border_heads)) {
    /* ir_printf("checking border list of block %+F\n", ent->key); */
    check_border_list(ent->value);
  }
}


/**
 * Add an interval border to the list of a block's list
 * of interval border.
 * @note You always have to create the use before the def.
 * @param env The environment.
 * @param head The list head to enqueue the borders.
 * @param irn The node (value) the border belongs to.
 * @param pressure The pressure at this point in time.
 * @param step A time step for the border.
 * @param is_def Is the border a use or a def.
 * @return The created border.
 */
static INLINE border_t *border_add(be_chordal_env_t *env, struct list_head *head,
			ir_node *irn, unsigned step, unsigned pressure,
			unsigned is_def, unsigned is_real)
{
	border_t *b;

	if(!is_def) {
		border_t *def;

		b = obstack_alloc(&env->obst, sizeof(*b));

		/* also allocate the def and tie it to the use. */
		def = obstack_alloc(&env->obst, sizeof(*def));
		memset(def, 0, sizeof(*def));
		b->other_end = def;
		def->other_end = b;

		/*
		 * Set the link field of the irn to the def.
		 * This strongly relies on the fact, that the use is always
		 * made before the def.
		 */
		set_irn_link(irn, def);

		b->magic = BORDER_FOURCC;
		def->magic = BORDER_FOURCC;
	}

	/*
	 * If the def is encountered, the use was made and so was the
	 * the def node (see the code above). It was placed into the
	 * link field of the irn, so we can get it there.
	 */
	else {
		b = get_irn_link(irn);

		assert(b && b->magic == BORDER_FOURCC && "Illegal border encountered");
	}

	b->pressure = pressure;
	b->is_def = is_def;
	b->is_real = is_real;
	b->irn = irn;
	b->step = step;
	list_add_tail(&b->list, head);
	DBG((env->dbg, LEVEL_5, "\t\t%s adding %+F, step: %d\n", is_def ? "def" : "use", irn, step));


	return b;
}

/**
 * Check, if an irn is of the register class currently under processing.
 * @param env The chordal environment.
 * @param irn The node.
 * @return 1, if the node is of that register class, 0 if not.
 */
static INLINE int has_reg_class(const be_chordal_env_t *env, const ir_node *irn)
{
	// return arch_irn_has_reg_class(env->main_env->arch_env, irn, -1, env->cls);
	return arch_irn_consider_in_reg_alloc(env->birg->main_env->arch_env, env->cls, irn);
}

#define has_limited_constr(req, irn) \
	(arch_get_register_req(arch_env, (req), irn, -1) && (req)->type == arch_register_req_type_limited)

static int get_next_free_reg(const be_chordal_alloc_env_t *alloc_env, bitset_t *colors)
{
	bitset_or(colors, alloc_env->ignore_regs);
	return bitset_next_clear(colors, 0);
}

typedef struct _operand_t operand_t;

struct _operand_t {
	ir_node *irn;
	ir_node *carrier;
	operand_t *partner;
	bitset_t *regs;
	int pos;
	arch_register_req_t req;
	unsigned has_constraints : 1;
};

typedef struct {
	operand_t *ops;
	int n_ops;
	int use_start;
	ir_node *next_insn;
	ir_node *irn;
	unsigned in_constraints  : 1;
	unsigned out_constraints : 1;
	unsigned has_constraints : 1;
	unsigned pre_colored     : 1;
} insn_t;

#define insn_n_defs(insn) ((insn)->use_start)
#define insn_n_uses(insn) ((insn)->n_ops - (insn)->use_start)

static insn_t *scan_insn(be_chordal_env_t *env, ir_node *irn, struct obstack *obst)
{
	const arch_env_t *arch_env = env->birg->main_env->arch_env;
	operand_t o;
	insn_t *insn;
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
		operand_t *op = &insn->ops[i];

		assert(op->req.cls == env->cls);
		op->regs   = bitset_obstack_alloc(obst, env->cls->n_regs);

		if(arch_register_req_is(&op->req, limited))
			op->req.limited(op->req.limited_env, op->regs);
		else
			arch_put_non_ignore_regs(env->birg->main_env->arch_env, env->cls, op->regs);
	}

	return insn;
}

static bitset_t *get_decisive_partner_regs(bitset_t *bs, const operand_t *o1, const operand_t *o2)
{
	bitset_t *res = bs;

	if(!o1) {
		bitset_copy(bs, o2->regs);
		return bs;
	}

	if(!o2) {
		bitset_copy(bs, o1->regs);
		return bs;
	}

	assert(o1->req.cls == o2->req.cls);

	if(bitset_contains(o1->regs, o2->regs))
		bitset_copy(bs, o1->regs);
	else if(bitset_contains(o2->regs, o1->regs))
		bitset_copy(bs, o2->regs);
	else
		res = NULL;

	return res;
}

static void pair_up_operands(const be_chordal_alloc_env_t *alloc_env, insn_t *insn)
{
	const be_chordal_env_t *env = alloc_env->chordal_env;
	const arch_env_t *aenv      = env->birg->main_env->arch_env;
	firm_dbg_module_t *dbg      = alloc_env->constr_dbg;

	int n_uses         = insn_n_uses(insn);
	int n_defs         = insn_n_defs(insn);
	int max_pairs      = MIN(n_uses, n_defs);
	bitset_t *bs       = bitset_alloca(env->cls->n_regs);
	bipartite_t *bp    = bipartite_new(n_defs, n_uses);
	int *pairing       = alloca(MAX(n_defs, n_uses) * sizeof(pairing[0]));

	int i, j;

	/*
		For each out operand, try to find an in operand which can be assigned the
		same register as the out operand.
	*/
	for(j = 0; j < insn->use_start; ++j) {
		operand_t *out_op = &insn->ops[j];

		/* Try to find an in operand which has ... */
		for(i = insn->use_start; i < insn->n_ops; ++i) {
			const operand_t *op = &insn->ops[i];

			/*
			The in operand can only be paired with a def, if the node defining the
			operand's value does not interfere with the instruction itself. That
			would mean, that it is live at the instruction, so no result of the instruction
			can have the same register as the operand.

			Furthermore, tow operands can be paired, if the admissible registers
			of one are a subset of the other's. We record the operand whose constraints
			count in the decisive array.
			*/
			if(!values_interfere(op->irn, op->carrier)) {
				if(get_decisive_partner_regs(bs, out_op, op))
					bipartite_add(bp, j, i - insn->use_start);
			}
		}
	}

	/* Compute the pairing. */
	bipartite_matching(bp, pairing);
	for(i = 0; i < insn->use_start; ++i) {
		int p = pairing[i] + insn->use_start;

		if(p >= insn->use_start) {
			insn->ops[i].partner = &insn->ops[p];
			insn->ops[p].partner = &insn->ops[i];
		}
	}

	bipartite_free(bp);
}


static ir_node *pre_process_constraints(be_chordal_alloc_env_t *alloc_env, insn_t **the_insn)
{
	be_chordal_env_t *env       = alloc_env->chordal_env;
	const arch_env_t *aenv      = env->birg->main_env->arch_env;
	firm_dbg_module_t *dbg      = alloc_env->constr_dbg;
	insn_t *insn                = *the_insn;
	ir_node *bl                 = get_nodes_block(insn->irn);
	ir_node *copy               = NULL;
	ir_node *perm               = NULL;
	bitset_t *out_constr        = bitset_alloca(env->cls->n_regs);
	bitset_t *bs                = bitset_alloca(env->cls->n_regs);

	int i;

	assert(insn->has_constraints && "only do this for constrained nodes");

	/*
		Collect all registers that occur in output constraints.
		This is necessary, since if the insn has one of these as an input constraint
		and the corresponding operand interferes with the insn, the operand must
		be copied.
	*/
	for(i = 0; i < insn->use_start; ++i) {
		operand_t *op = &insn->ops[i];
		if(op->has_constraints)
			bitset_or(out_constr, op->regs);
	}

	/*
		Now, figure out which input operand must be copied since it has input
		constraints which are also output constraints.
	*/
	for(i = insn->use_start; i < insn->n_ops; ++i) {
		operand_t *op = &insn->ops[i];
		if(op->has_constraints && values_interfere(op->carrier, insn->irn)) {
			bitset_copy(bs, op->regs);
			bitset_and(bs, out_constr);

			/*
				The operand (interfering with the node) has input constraints
				which also occur as output constraints, so insert a copy.
			*/
			if(bitset_popcnt(bs) > 0) {
				copy                 = be_new_Copy(op->req.cls, env->irg, bl, op->carrier);
				insn->ops[i].carrier = copy;
				sched_add_before(insn->irn, copy);

				DBG((dbg, LEVEL_2, "adding copy for interfering and constrained op %+F\n", op->carrier));
			}
		}
	}

	/*
		Make the Perm, recompute liveness and re-scan the insn since the
		in operands are now the Projs of the Perm.
	*/
	perm = insert_Perm_after(aenv, env->cls, env->dom_front, sched_prev(insn->irn));

	/* Registers are propagated by insert_Perm_after(). Clean them here! */
	if(perm) {
		const ir_edge_t *edge;

		foreach_out_edge(perm, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			arch_set_irn_register(aenv, proj, NULL);
		}

		/*
			We also have to re-build the insn since the input operands are now the Projs of
			the Perm. Recomputing liveness is also a good idea if a Perm is inserted, since
			the live sets may change.
		*/
		be_liveness(env->irg);
		obstack_free(&env->obst, insn);
		*the_insn = insn = scan_insn(env, insn->irn, &env->obst);

		/*
			Copy the input constraints of the insn to the Perm as output
			constraints. Succeeding phases (coalescing will need that).
		*/
		for(i = insn->use_start; i < insn->n_ops; ++i) {
			operand_t *op = &insn->ops[i];
			ir_node *proj = op->carrier;
			/*
				Note that the predecessor must not be a Proj of the Perm,
				since ignore-nodes are not Perm'ed.
			*/
			if(op->has_constraints &&  is_Proj(proj) && get_Proj_pred(proj) == perm) {
				be_set_constr_limited(perm, BE_OUT_POS(get_Proj_proj(proj)), &op->req);
			}
		}
	}

	return perm;
}

static ir_node *handle_constraints(be_chordal_alloc_env_t *alloc_env, ir_node *irn)
{
	be_chordal_env_t *env  = alloc_env->chordal_env;
	void *base             = obstack_base(&env->obst);
	insn_t *insn           = scan_insn(env, irn, &env->obst);
	ir_node *res           = insn->next_insn;

	if(insn->pre_colored) {
		int i;
		for(i = 0; i < insn->use_start; ++i)
			pset_insert_ptr(alloc_env->pre_colored, insn->ops[i].carrier);
	}

	if(be_is_Perm(irn) || be_is_RegParams(irn) || (be_is_Barrier(irn) && !insn->in_constraints))
		goto end;

	/*
		Perms inserted before the constraint handling phase are considered to be
		correctly precolored. These Perms arise during the ABI handling phase.
	*/
	if(insn->has_constraints) {
		firm_dbg_module_t *dbg = alloc_env->constr_dbg;
		const arch_env_t *aenv = env->birg->main_env->arch_env;
		int n_regs             = env->cls->n_regs;
		bitset_t *bs           = bitset_alloca(n_regs);
		bitset_t *non_ignore   = bitset_alloca(n_regs);
		ir_node **alloc_nodes  = alloca(n_regs * sizeof(alloc_nodes[0]));
		bipartite_t *bp        = bipartite_new(n_regs, n_regs);
		int *assignment        = alloca(n_regs * sizeof(assignment[0]));
		pmap *partners         = pmap_create();

		int i, n_alloc;
		long col;
		const ir_edge_t *edge;
		ir_node *perm = NULL;

		/*
			prepare the constraint handling of this node.
			Perms are constructed and Copies are created for constrained values
			interfering with the instruction.
		*/
		perm = pre_process_constraints(alloc_env, &insn);

		/* find suitable in operands to the out operands of the node. */
		pair_up_operands(alloc_env, insn);

		/*
			look at the in/out operands and add each operand (and its possible partner)
			to a bipartite graph (left: nodes with partners, right: admissible colors).
		*/
		for(i = 0, n_alloc = 0; i < insn->n_ops; ++i) {
			operand_t *op = &insn->ops[i];

			/*
				If the operand has no partner or the partner has not been marked
				for allocation, determine the admissible registers and mark it
				for allocation by associating the node and its partner with the
				set of admissible registers via a bipartite graph.
			*/
			if(!op->partner || !pmap_contains(partners, op->partner->carrier)) {

				pmap_insert(partners, op->carrier, op->partner ? op->partner->carrier : NULL);
				alloc_nodes[n_alloc] = op->carrier;

				DBG((dbg, LEVEL_2, "\tassociating %+F and %+F\n", op->carrier, op->partner ? op->partner->carrier : NULL));

 				bitset_clear_all(bs);
				get_decisive_partner_regs(bs, op, op->partner);

				DBG((dbg, LEVEL_2, "\tallowed registers for %+F: %B\n", op->carrier, bs));

				bitset_foreach(bs, col)
					bipartite_add(bp, n_alloc, col);

				n_alloc++;
			}
		}

		/*
			Put all nodes which live by the constrained instruction also to the
			allocation bipartite graph. They are considered unconstrained.
		*/
		if(perm) {
			foreach_out_edge(perm, edge) {
				ir_node *proj = get_edge_src_irn(edge);

				assert(is_Proj(proj));

				if(values_interfere(proj, irn)) {
					assert(n_alloc < n_regs);
					alloc_nodes[n_alloc] = proj;
					pmap_insert(partners, proj, NULL);

					bitset_clear_all(bs);
					arch_put_non_ignore_regs(aenv, env->cls, bs);
					bitset_foreach(bs, col)
						bipartite_add(bp, n_alloc, col);

					n_alloc++;
				}
			}
		}

		/* Compute a valid register allocation. */
		bipartite_matching(bp, assignment);

		/* Assign colors obtained from the matching. */
		for(i = 0; i < n_alloc; ++i) {
			const arch_register_t *reg;
			ir_node *nodes[2];
			int j;

			assert(assignment[i] >= 0 && "there must have been a register assigned");
			reg = arch_register_for_index(env->cls, assignment[i]);

			nodes[0] = alloc_nodes[i];
			nodes[1] = pmap_get(partners, alloc_nodes[i]);

			for(j = 0; j < 2; ++j) {
				if(!nodes[j])
					continue;

				arch_set_irn_register(aenv, nodes[j], reg);
				pset_hinsert_ptr(alloc_env->pre_colored, nodes[j]);
				DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", nodes[j], reg->name));
			}
		}


		/* Allocate the non-constrained Projs of the Perm. */
		if(perm) {

			bitset_clear_all(bs);

			/* Put the colors of all Projs in a bitset. */
			foreach_out_edge(perm, edge) {
				ir_node *proj              = get_edge_src_irn(edge);
				const arch_register_t *reg = arch_get_irn_register(aenv, proj);

				if(reg != NULL)
					bitset_set(bs, reg->index);
			}

			/* Assign the not yet assigned Projs of the Perm a suitable color. */
			foreach_out_edge(perm, edge) {
				ir_node *proj              = get_edge_src_irn(edge);
				const arch_register_t *reg = arch_get_irn_register(aenv, proj);

				DBG((dbg, LEVEL_2, "\tchecking reg of %+F: %s\n", proj, reg ? reg->name : "<none>"));

				if(reg == NULL) {
					col = get_next_free_reg(alloc_env, bs);
					reg = arch_register_for_index(env->cls, col);
					bitset_set(bs, reg->index);
					arch_set_irn_register(aenv, proj, reg);
					pset_insert_ptr(alloc_env->pre_colored, proj);
					DBG((dbg, LEVEL_2, "\tsetting %+F to register %s\n", proj, reg->name));
				}
			}
		}

		pmap_destroy(partners);
	}

end:
	obstack_free(&env->obst, base);
	return res;
}

/**
 * Handle constraint nodes in each basic block.
 * be_insert_constr_perms() inserts Perm nodes which perm
 * over all values live at the constrained node right in front
 * of the constrained node. These Perms signal a constrained node.
 * For further comments, refer to handle_constraints_at_perm().
 */
static void constraints(ir_node *bl, void *data)
{
	firm_dbg_module_t *dbg      = firm_dbg_register("firm.be.chordal.constr");
	be_chordal_alloc_env_t *env = data;
	arch_env_t *arch_env        = env->chordal_env->birg->main_env->arch_env;
	ir_node *irn;

	for(irn = sched_first(bl); !sched_is_end(irn);) {
		irn = handle_constraints(env, irn);
	}
}

/**
 * Annotate the register pressure to the nodes and compute
 * the liveness intervals.
 * @param block The block to do it for.
 * @param env_ptr The environment.
 */
static void pressure(ir_node *block, void *env_ptr)
{
/* Convenience macro for a def */
#define border_def(irn, step, real) \
	border_add(env, head, irn, step, pressure--, 1, real)

/* Convenience macro for a use */
#define border_use(irn, step, real) \
	border_add(env, head, irn, step, ++pressure, 0, real)

	be_chordal_alloc_env_t *alloc_env = env_ptr;
	be_chordal_env_t *env             = alloc_env->chordal_env;
	const arch_env_t *arch_env        = env->birg->main_env->arch_env;
	bitset_t *live                    = alloc_env->live;
	firm_dbg_module_t *dbg            = env->dbg;
	ir_node *irn;

	int i, n;
	unsigned step = 0;
	unsigned pressure = 0;
	struct list_head *head;
	pset *live_in = put_live_in(block, pset_new_ptr_default());
	pset *live_end = put_live_end(block, pset_new_ptr_default());

	DBG((dbg, LEVEL_1, "Computing pressure in block %+F\n", block));
	bitset_clear_all(live);

	/* Set up the border list in the block info */
	head = obstack_alloc(&env->obst, sizeof(*head));
	INIT_LIST_HEAD(head);
	assert(pmap_get(env->border_heads, block) == NULL);
	pmap_insert(env->border_heads, block, head);

	/*
	 * Make final uses of all values live out of the block.
	 * They are necessary to build up real intervals.
	 */
	for(irn = pset_first(live_end); irn; irn = pset_next(live_end)) {
		if(has_reg_class(env, irn)) {
			DBG((dbg, LEVEL_3, "\tMaking live: %+F/%d\n", irn, get_irn_graph_nr(irn)));
			bitset_set(live, get_irn_graph_nr(irn));
			border_use(irn, step, 0);
		}
	}
	++step;

	/*
	 * Determine the last uses of a value inside the block, since they are
	 * relevant for the interval borders.
	 */
	sched_foreach_reverse(block, irn) {
		DBG((dbg, LEVEL_1, "\tinsn: %+F, pressure: %d\n", irn, pressure));
		DBG((dbg, LEVEL_2, "\tlive: %b\n", live));

		/*
		 * If the node defines some value, which can put into a
		 * register of the current class, make a border for it.
		 */
		if(has_reg_class(env, irn)) {
			int nr = get_irn_graph_nr(irn);

			bitset_clear(live, nr);
			border_def(irn, step, 1);
		}

		/*
		 * If the node is no phi node we can examine the uses.
		 */
		if(!is_Phi(irn)) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);

				if(has_reg_class(env, op)) {
					int nr = get_irn_graph_nr(op);

					DBG((dbg, LEVEL_4, "\t\tpos: %d, use: %+F\n", i, op));

					if(!bitset_is_set(live, nr)) {
						border_use(op, step, 1);
						bitset_set(live, nr);
					}
				}
			}
		}
		++step;
	}

	/*
	 * Add initial defs for all values live in.
	 */
	for(irn = pset_first(live_in); irn; irn = pset_next(live_in)) {
		if(has_reg_class(env, irn)) {

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));

			/* Add the def */
			border_def(irn, step, 0);
		}
	}


  del_pset(live_in);
  del_pset(live_end);
}

static void assign(ir_node *block, void *env_ptr)
{
	be_chordal_alloc_env_t *alloc_env = env_ptr;
	be_chordal_env_t *env       = alloc_env->chordal_env;
	firm_dbg_module_t *dbg      = env->dbg;
	bitset_t *live              = alloc_env->live;
	bitset_t *colors            = alloc_env->colors;
	bitset_t *in_colors         = alloc_env->in_colors;
	const arch_env_t *arch_env  = env->birg->main_env->arch_env;

	const ir_node *irn;
	border_t *b;
	struct list_head *head = get_block_border_head(env, block);
	pset *live_in = put_live_in(block, pset_new_ptr_default());

	bitset_clear_all(colors);
	bitset_clear_all(live);
	bitset_clear_all(in_colors);

	DBG((dbg, LEVEL_4, "Assigning colors for block %+F\n", block));
	DBG((dbg, LEVEL_4, "\tusedef chain for block\n"));
	list_for_each_entry(border_t, b, head, list) {
		DBG((dbg, LEVEL_4, "\t%s %+F/%d\n", b->is_def ? "def" : "use",
					b->irn, get_irn_graph_nr(b->irn)));
	}

	/*
	 * Add initial defs for all values live in.
	 * Since their colors have already been assigned (The dominators were
	 * allocated before), we have to mark their colors as used also.
	 */
	for(irn = pset_first(live_in); irn; irn = pset_next(live_in)) {
		if(has_reg_class(env, irn)) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			int col;

			assert(reg && "Node must have been assigned a register");
			col = arch_register_get_index(reg);

			/* Mark the color of the live in value as used. */
			bitset_set(colors, col);
			bitset_set(in_colors, col);

			/* Mark the value live in. */
			bitset_set(live, get_irn_graph_nr(irn));
		}
	}

	/*
	 * Mind that the sequence
	 * of defs from back to front defines a perfect
	 * elimination order. So, coloring the definitions from first to last
	 * will work.
	 */
	list_for_each_entry_reverse(border_t, b, head, list) {
		ir_node *irn = b->irn;
		int nr = get_irn_graph_nr(irn);

		/*
		 * Assign a color, if it is a local def. Global defs already have a
		 * color.
		 */
		if(b->is_def && !is_live_in(block, irn)) {
			const arch_register_t *reg;
			int col = NO_COLOR;

			if(pset_find_ptr(alloc_env->pre_colored, irn)) {
				reg = arch_get_irn_register(arch_env, irn);
				col = reg->index;
				assert(!bitset_is_set(colors, col) && "pre-colored register must be free");
			}

			else {
				col = get_next_free_reg(alloc_env, colors);
				reg = arch_register_for_index(env->cls, col);
				assert(arch_get_irn_register(arch_env, irn) == NULL && "This node must not have been assigned a register yet");
			}

			bitset_set(colors, col);

			assert(!arch_register_type_is(reg, ignore) && "Must not assign ignore register");
			arch_set_irn_register(arch_env, irn, reg);

			DBG((dbg, LEVEL_1, "\tassigning register %s(%d) to %+F\n",
            arch_register_get_name(reg), col, irn));

			assert(!bitset_is_set(live, nr) && "Value's definition must not have been encountered");
			bitset_set(live, nr);
		}

		/* Clear the color upon a use. */
		else if(!b->is_def) {
			const arch_register_t *reg = arch_get_irn_register(arch_env, irn);
			int col;

			assert(reg && "Register must have been assigned");

			col = arch_register_get_index(reg);
			assert(bitset_is_set(live, nr) && "Cannot have a non live use");

			bitset_clear(colors, col);
			bitset_clear(live, nr);
		}
	}

	del_pset(live_in);
}

void be_ra_chordal_color(be_chordal_env_t *chordal_env)
{
	be_chordal_alloc_env_t env;
	char buf[256];
	int i;

	int colors_n          = arch_register_class_n_regs(chordal_env->cls);
	ir_graph *irg         = chordal_env->irg;


	if(get_irg_dom_state(irg) != dom_consistent)
		compute_doms(irg);

	env.chordal_env   = chordal_env;
	env.colors_n      = colors_n;
	env.colors        = bitset_malloc(colors_n);
	env.in_colors     = bitset_malloc(colors_n);
	env.ignore_regs   = bitset_malloc(colors_n);
	env.pre_colored   = pset_new_ptr_default();
	env.constr_dbg    = firm_dbg_register("firm.be.chordal.constr");

	for(i = 0; i < colors_n; ++i)
		if(arch_register_type_is(&chordal_env->cls->regs[i], ignore))
			bitset_set(env.ignore_regs, i);

	/* Handle register targeting constraints */
	dom_tree_walk_irg(irg, constraints, NULL, &env);

	if(chordal_env->opts->dump_flags & BE_CH_DUMP_CONSTR) {
		snprintf(buf, sizeof(buf), "-%s-constr", chordal_env->cls->name);
		be_dump(chordal_env->irg, buf, dump_ir_block_graph_sched);
	}

	be_numbering(irg);
	env.live = bitset_malloc(get_graph_node_count(chordal_env->irg));

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, pressure, NULL, &env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, &env);

	be_numbering_done(irg);

	if(chordal_env->opts->dump_flags & BE_CH_DUMP_TREE_INTV) {
    	plotter_t *plotter;
		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", chordal_env->cls->name, irg);
    	plotter = new_plotter_ps(buf);
    	draw_interval_tree(&draw_chordal_def_opts, chordal_env, plotter);
    	plotter_free(plotter);
	}

	free(env.live);
	free(env.colors);
	free(env.in_colors);
	free(env.ignore_regs);
	del_pset(env.pre_colored);
}
