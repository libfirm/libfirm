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

#define DUMP_INTERVALS

typedef struct _be_chordal_alloc_env_t {
	be_chordal_env_t *chordal_env;

	pset *pre_colored;    /**< Set of precolored nodes. */
	bitset_t *live;				/**< A liveness bitset. */
	bitset_t *colors;			/**< The color mask. */
	bitset_t *valid_colors;	    /**< A mask of colors which shall be considered during allocation.
								     Registers with the ignore bit on, must not be considered. */
	bitset_t *in_colors;        /**< Colors used by live in values. */
	int colors_n;               /**< The number of colors. */
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
	return arch_irn_consider_in_reg_alloc(env->main_env->arch_env, env->cls, irn);
}

#define has_limited_constr(req, irn) \
	(arch_get_register_req(arch_env, (req), irn, -1) && (req)->type == arch_register_req_type_limited)

typedef struct _operand_t operand_t;

struct _operand_t {
	ir_node *irn;
	ir_node *carrier;
	operand_t *partner;
	int pos;
	arch_register_req_t req;
};

typedef struct {
	operand_t *ops;
	int n_ops;
	int use_start;
	ir_node *next_insn;
	unsigned has_constraints : 1;
} insn_t;

static insn_t *scan_insn(be_chordal_env_t *env, ir_node *irn, struct obstack *obst)
{
	const arch_env_t *arch_env = env->main_env->arch_env;
	operand_t o;
	insn_t *insn;
	int i, n;

	insn = obstack_alloc(obst, sizeof(insn[0]));
	memset(insn, 0, sizeof(insn[0]));

	insn->next_insn = sched_next(irn);
	if(get_irn_mode(irn) == mode_T) {
		ir_node *p;

		for(p = sched_next(irn); is_Proj(p); p = sched_next(p)) {
			if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, p)) {
				o.carrier = p;
				o.irn     = irn;
				o.pos     = -(get_Proj_proj(p) + 1);
				o.partner = NULL;
				arch_get_register_req(arch_env, &o.req, p, -1);
				obstack_grow(obst, &o, sizeof(o));
				insn->n_ops++;
				insn->has_constraints |= arch_register_req_is(&o.req, limited);
			}
		}

		insn->next_insn = p;
	}

	else if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, irn)) {
		o.carrier = irn;
		o.irn     = irn;
		o.pos     = -1;
		o.partner = NULL;
		arch_get_register_req(arch_env, &o.req, irn, -1);
		obstack_grow(obst, &o, sizeof(o));
		insn->n_ops++;
		insn->has_constraints |= arch_register_req_is(&o.req, limited);
	}

	insn->use_start = insn->n_ops;

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if(arch_irn_consider_in_reg_alloc(arch_env, env->cls, op)) {
			o.carrier = op;
			o.irn     = irn;
			o.pos     = i;
			o.partner = NULL;
			arch_get_register_req(arch_env, &o.req, irn, i);
			obstack_grow(obst, &o, sizeof(o));
			insn->n_ops++;
			insn->has_constraints |= arch_register_req_is(&o.req, limited);
		}
	}

	insn->ops = obstack_finish(obst);
	return insn;
}

static operand_t *find_unpaired_use(insn_t *insn, const operand_t *op, int can_be_constrained)
{
	int i;
	operand_t *res = NULL;

	for(i = insn->use_start; i < insn->n_ops; ++i) {
		operand_t *op = &insn->ops[i];
		int has_constraint = arch_register_req_is(&op->req, limited);

		if(!values_interfere(op->carrier, op->irn) && !op->partner && (!has_constraint || can_be_constrained)) {
			if(arch_register_req_is(&op->req, should_be_same) && op->req.other_same == op->carrier)
				return op;
			else
				res = op;
		}
	}

	return res;
}

static void pair_up_operands(insn_t *insn)
{
	firm_dbg_module_t *dbg = firm_dbg_register("firm.be.chordal.constr");
	int i;

	for(i = 0; i < insn->use_start; ++i) {
		operand_t *op      = &insn->ops[i];
		int has_constraint = arch_register_req_is(&op->req, limited);
		operand_t *partner = find_unpaired_use(insn, op, !has_constraint);

		if(partner) {
			op->partner = partner;
			partner->partner = op;
		}
	}
}

static ir_node *handle_constraints(be_chordal_alloc_env_t *alloc_env, ir_node *irn)
{
	be_chordal_env_t *env  = alloc_env->chordal_env;
	void *base             = obstack_base(&env->obst);
	insn_t *insn           = scan_insn(env, irn, &env->obst);
	ir_node *res           = insn->next_insn;

	if(insn->has_constraints) {
		firm_dbg_module_t *dbg = firm_dbg_register("firm.be.chordal.constr");
		const arch_env_t *aenv = env->main_env->arch_env;
		int n_regs             = env->cls->n_regs;
		bitset_t *bs           = bitset_alloca(n_regs);
		ir_node **alloc_nodes  = alloca(n_regs * sizeof(alloc_nodes[0]));
		bipartite_t *bp        = bipartite_new(n_regs, n_regs);
		int *assignment        = alloca(n_regs * sizeof(assignment[0]));
		pmap *partners         = pmap_create();

		int i, n_alloc;
		long col;
		const ir_edge_t *edge;
		ir_node *perm = insert_Perm_after(aenv, env->cls, env->dom_front, sched_prev(irn));

		/* Registers are propagated by insert_Perm_after(). Clean them here! */
		if(perm) {
			foreach_out_edge(perm, edge) {
				ir_node *proj = get_edge_src_irn(edge);
				arch_set_irn_register(aenv, proj, NULL);
			}
		}


		be_liveness(env->irg);
		insn = scan_insn(env, irn, &env->obst);

		DBG((dbg, LEVEL_1, "handling constraints for %+F\n", irn));

		/*
		 * If there was no Perm made, nothing was alive in this register class.
		 * This means, that the node has no operands, thus no input constraints.
		 * so it had output constraints. The other results then can be assigned freeliy.
		 */

		pair_up_operands(insn);

		for(i = 0, n_alloc = 0; i < insn->n_ops; ++i) {
			operand_t *op = &insn->ops[i];
			if(arch_register_req_is(&op->req, limited)) {
				pmap_insert(partners, op->carrier, op->partner ? op->partner->carrier : NULL);
				alloc_nodes[n_alloc] = op->carrier;

				DBG((dbg, LEVEL_2, "\tassociating %+F and %+F\n", op->carrier, pmap_get(partners, op->carrier)));

				bitset_clear_all(bs);
				op->req.limited(op->req.limited_env, bs);
				bitset_and(bs, alloc_env->valid_colors);

				DBG((dbg, LEVEL_2, "\tallowed registers for %+F: %B\n", op->carrier, bs));

				bitset_foreach(bs, col)
					bipartite_add(bp, n_alloc, col);

				n_alloc++;
			}
		}

		if(perm) {
			foreach_out_edge(perm, edge) {
				ir_node *proj = get_edge_src_irn(edge);

				assert(is_Proj(proj));

				if(values_interfere(proj, irn)) {
					assert(n_alloc < n_regs);
					alloc_nodes[n_alloc] = proj;
					pmap_insert(partners, proj, NULL);

					bitset_clear_all(bs);
					arch_get_allocatable_regs(aenv, proj, -1, bs);
					bitset_and(bs, alloc_env->valid_colors);
					bitset_foreach(bs, col)
						bipartite_add(bp, n_alloc, col);

					n_alloc++;
				}
			}
		}

		bipartite_matching(bp, assignment);


		for(i = 0; i < n_alloc; ++i) {
			int j;
			ir_node *nodes[2];
			const arch_register_t *reg;

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


		if(perm) {
			bitset_clear_all(bs);
			foreach_out_edge(perm, edge) {
				ir_node *proj              = get_edge_src_irn(edge);
				const arch_register_t *reg = arch_get_irn_register(aenv, proj);

				if(reg != NULL)
					bitset_set(bs, reg->index);
			}

			// bitset_or(bs, alloc_env->ignore_colors);
			foreach_out_edge(perm, edge) {
				ir_node *proj              = get_edge_src_irn(edge);
				const arch_register_t *reg = arch_get_irn_register(aenv, proj);

				DBG((dbg, LEVEL_2, "\tchecking reg of %+F: %s\n", proj, reg ? reg->name : "<none>"));

				if(reg == NULL) {
					col = bitset_next_clear(bs, 0);
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
	arch_env_t *arch_env        = env->chordal_env->main_env->arch_env;
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
	const arch_env_t *arch_env        = env->main_env->arch_env;
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
	const arch_env_t *arch_env  = env->main_env->arch_env;

	const ir_node *irn;
	border_t *b;
	struct list_head *head = get_block_border_head(env, block);
	pset *live_in = put_live_in(block, pset_new_ptr_default());

	bitset_clear_all(live);
	bitset_clear_all(in_colors);

	bitset_copy(colors, alloc_env->valid_colors);
	bitset_flip_all(colors);

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
	 * Mind that the sequence of defs from back to front defines a perfect
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
				col = bitset_next_clear(colors, 0);
				reg = arch_register_for_index(env->cls, col);
				assert(arch_get_irn_register(arch_env, irn) == NULL && "This node must not have been assigned a register yet");
			}

			bitset_set(colors, col);
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

	int colors_n          = arch_register_class_n_regs(chordal_env->cls);
	ir_graph *irg         = chordal_env->irg;


	if(get_irg_dom_state(irg) != dom_consistent)
		compute_doms(irg);

	env.chordal_env   = chordal_env;
	env.colors_n      = colors_n;
	env.colors        = bitset_malloc(colors_n);
	env.valid_colors  = bitset_malloc(colors_n);
	env.in_colors     = bitset_malloc(colors_n);
	env.pre_colored   = pset_new_ptr_default();

	arch_put_non_ignore_regs(chordal_env->main_env->arch_env, chordal_env->cls, env.valid_colors);

	/* Handle register targeting constraints */
	dom_tree_walk_irg(irg, constraints, NULL, &env);

#if 0
	if(chordal_env->opts->dump_flags & BE_CH_DUMP_CONSTR) {
		snprintf(buf, sizeof(buf), "-%s-constr", chordal_env->cls->name);
		dump_ir_block_graph_sched(chordal_env->irg, buf);
	}
#endif

	be_numbering(irg);
	env.live = bitset_malloc(get_graph_node_count(chordal_env->irg));

	/* First, determine the pressure */
	dom_tree_walk_irg(irg, pressure, NULL, &env);

	/* Assign the colors */
	dom_tree_walk_irg(irg, assign, NULL, &env);

	be_numbering_done(irg);

#if 0
	if(chordal_env->opts->dump_flags & BE_CH_DUMP_TREE_INTV) {
    	plotter_t *plotter;

		ir_snprintf(buf, sizeof(buf), "ifg_%s_%F.eps", chordal_env->cls->name, irg);
    	plotter = new_plotter_ps(buf);
    	draw_interval_tree(&draw_chordal_def_opts, chordal_env, plotter);
    	plotter_free(plotter);
	}
#endif

	free(env.live);
	free(env.colors);
	free(env.in_colors);
	free(env.valid_colors);

	del_pset(env.pre_colored);
}
