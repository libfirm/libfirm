/**
 * Author:      Daniel Grund
 * Date:		25.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Performs SSA-Destruction.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"
#include "set.h"
#include "pmap.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irdump.h"
#include "irprintf.h"

#include "be_t.h"
#include "beutil.h"
#include "bechordal_t.h"
#include "bearch.h"
#include "belive_t.h"
#include "benode_t.h"
#include "besched_t.h"

static firm_dbg_module_t *dbg = NULL;
#define DUMP_GRAPHS

#define get_chordal_arch(ce) ((ce)->main_env->arch_env)
#define get_reg(irn) arch_get_irn_register(get_chordal_arch(chordal_env), irn)
#define set_reg(irn, reg) arch_set_irn_register(get_chordal_arch(chordal_env), irn, reg)

#define is_Perm(irn)            (arch_irn_classify(arch_env, irn) == arch_irn_class_perm)
#define get_reg_cls(irn)        (arch_get_irn_reg_class(arch_env, irn, -1))
#define is_curr_reg_class(irn)  (get_reg_cls(p) == chordal_env->cls)

static void clear_link(ir_node *irn, void *data)
{
  set_irn_link(irn, NULL);
}

/**
 * Build a list of phis of a block.
 */
static void collect_phis(ir_node *irn, void *data)
{
  be_chordal_env_t *env = data;
  if(is_Phi(irn) && chordal_has_class(env, irn)) {
    ir_node *bl = get_nodes_block(irn);
    set_irn_link(irn, get_irn_link(bl));
    set_irn_link(bl, irn);
  }
}

/**
 * Build a ring of phis for each block in the link field.
 * @param env The chordal env.
 */
static INLINE void build_phi_rings(be_chordal_env_t *env)
{
  irg_walk_graph(env->irg, clear_link, collect_phis, env);
}

/**
 * This struct represents a Proj for a Perm.
 * It records the argument in the Perm and the corresponding Proj of the
 * Perm.
 */
typedef struct {
	ir_node *arg;  /**< The phi argument to make the Proj for. */
	int pos;       /**< The proj number the Proj will get.
									 This also denotes the position of @p arg
									 in the in array of the Perm. */
	ir_node *proj; /**< The proj created for @p arg. */
} perm_proj_t;

static int cmp_perm_proj(const void *a, const void *b, size_t n)
{
	const perm_proj_t *p = a;
	const perm_proj_t *q = b;
	return !(p->arg == q->arg);
}

static void insert_all_perms_walker(ir_node *bl, void *data)
{
  be_chordal_env_t *chordal_env = data;
  pmap *perm_map = chordal_env->data;
  ir_graph *irg = chordal_env->irg;
  const be_node_factory_t *fact = chordal_env->main_env->node_factory;

  /* Dummy targets for the projs */
  ir_node *dummy = new_rd_Unknown(irg, mode_T);

  assert(is_Block(bl));

  /* If the link flag is NULL, this block has no phis. */
	if(get_irn_link(bl)) {
		int i, n;

		/* Look at all predecessors of the phi block */
		for(i = 0, n = get_irn_arity(bl); i < n; ++i) {
			ir_node *phi, *perm, *insert_after, **in;
			perm_proj_t *pp;

			set *arg_set     = new_set(cmp_perm_proj, chordal_env->cls->n_regs);
			ir_node *pred_bl = get_Block_cfgpred_block(bl, i);
			int n_projs      = 0;

			assert(!pmap_contains(perm_map, pred_bl) && "Already permed that block");

			/*
			 * Note that all phis in the list are in the same register class
			 * by construction.
			 */
			for(phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
				perm_proj_t templ;
				ir_node *arg     = get_irn_n(phi, i);
				unsigned hash    = HASH_PTR(arg);

				templ.arg  = arg;
				pp         = set_find(arg_set, &templ, sizeof(templ), hash);

				/*
				 * If a proj_perm_t entry has not been made in the argument set,
				 * create one. The only restriction is, that the phi argument
				 * mey not be live in at the current block, since this argument
				 * interferes with the phi and must thus not be member of a
				 * Perm. A copy will be inserted for this argument alter on.
				 */
				if(!pp && !is_live_in(bl, arg)) {
					templ.pos = n_projs++;
					set_insert(arg_set, &templ, sizeof(templ), hash);
				}
			}

			/*
			 * set the in array of the Perm to the arguments of the phis
			 * recorded above
			 */
			in = malloc(n_projs * sizeof(in[0]));
			for(pp = set_first(arg_set); pp; pp = set_next(arg_set))
				in[pp->pos] = pp->arg;

			perm = new_Perm(fact, chordal_env->cls, irg, pred_bl, n_projs, in);
			insert_after = sched_skip(sched_last(pred_bl), 0, sched_skip_cf_predicator,
					chordal_env->main_env->arch_env);
			sched_add_after(insert_after, perm);
			exchange(dummy, perm);

			/*
			 * Make the Projs for the Perm.
			 * register allocation is copied form former phi arguments
			 * to the projs (new phi arguments)
			 */
			for(pp = set_first(arg_set); pp; pp = set_next(arg_set)) {
				pp->proj = new_r_Proj(irg, pred_bl, perm, get_irn_mode(pp->arg), pp->pos);
				set_reg(pp->proj, get_reg(pp->arg));
				DBG((dbg, LEVEL_2, "Copy register assignment %s from %+F to %+F\n",
							get_reg(pp->arg)->name, pp->arg, pp->proj));
			}

			/*
			 * Set the phi nodes to their new arguments: The Projs of the Perm
			 */
			for(phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
				perm_proj_t templ;

				templ.arg = get_irn_n(phi, i);
				pp        = set_find(arg_set, &templ, sizeof(templ), HASH_PTR(templ.arg));

				assert(pp && "A Perm Proj must be created for this Phi argument");
				set_irn_n(phi, i, pp->proj);
			}

			free(in);
			del_set(arg_set);

			/* register in perm map */
			pmap_insert(perm_map, pred_bl, perm);
		}
	}
}

static void insert_all_perms(be_chordal_env_t *chordal_env) {
	DBG((dbg, LEVEL_1, "Placing perms...\n"));
	irg_block_walk_graph(chordal_env->irg, insert_all_perms_walker, NULL, chordal_env);
}

#define is_pinned(irn) (get_irn_link(irn))
#define get_pinning_block(irn) ((ir_node *)get_irn_link(irn))
#define pin_irn(irn, lock) (set_irn_link(irn, lock))


/**
 * Adjusts the register allocation for the phi-operands
 * by inserting perm nodes, if necessary.
 * @param phi The phi node to adjust operands for
 */
static void adjust_phi_arguments(be_chordal_env_t *chordal_env, ir_node *phi) {
	int i, max;
	ir_node *arg, *phi_block, *arg_block;
	const arch_register_t *phi_reg, *arg_reg;
	const arch_register_class_t *cls;

	assert(is_Phi(phi) && "Can only handle phi-destruction :)");

	phi_block = get_nodes_block(phi);
	phi_reg = get_reg(phi);
	cls = arch_get_irn_reg_class(get_chordal_arch(chordal_env), phi, -1);

	/* process all arguments of the phi */
	for(i=0, max=get_irn_arity(phi); i<max; ++i) {
		arg = get_irn_n(phi, i);
		arg_block = get_Block_cfgpred_block(phi_block, i);
		arg_reg = get_reg(arg);
		assert(arg_reg && "Register must be set while placing perms");

		DBG((dbg, LEVEL_1, "  for %+F(%s) -- %+F(%s)\n", phi, phi_reg->name, arg, arg_reg->name));

		if(nodes_interfere(chordal_env, phi, arg)) {
			/* Insert a duplicate in arguments block,
			 * make it the new phi arg,
			 * set its register,
			 * insert it into schedule,
			 * pin it
			 */
			ir_node *dupl = new_Copy(chordal_env->main_env->node_factory, cls, chordal_env->irg, arg_block, arg);
			assert(get_irn_mode(phi) == get_irn_mode(dupl));
			set_irn_n(phi, i, dupl);
			set_reg(dupl, phi_reg);
			sched_add_after(sched_skip(sched_last(arg_block), 0, sched_skip_cf_predicator, chordal_env->main_env->arch_env), dupl);
			pin_irn(dupl, phi_block);
			DBG((dbg, LEVEL_1, "    they do interfere: insert %+F(%s)\n", dupl, get_reg(dupl)->name));
		} else {
			/*
			 * First check if there is a phi
			 * - in the same block
			 * - having arg at the current pos in its arg-list
			 * - having the same color as arg
			 *
			 * If found, then pin the arg
			 */
			DBG((dbg, LEVEL_1, "    they do not interfere\n"));
			assert(is_Proj(arg));
			if (!is_pinned(arg)) {
				ir_node *other_phi;
				DBG((dbg, LEVEL_1, "      searching for phi with same arg having args register\n"));
				for(other_phi = get_irn_link(phi_block); other_phi; other_phi = get_irn_link(other_phi)) {
					assert(is_Phi(other_phi) && get_nodes_block(phi) == get_nodes_block(other_phi) && "link fields are screwed up");
					if (get_irn_n(other_phi, i) == arg && get_reg(other_phi) == arg_reg) {
						DBG((dbg, LEVEL_1, "        found %+F(%s)\n", other_phi, get_reg(other_phi)->name));
						pin_irn(arg, phi_block);
					}
				}
			}

			if (is_pinned(arg)) {
				/* Insert a duplicate of the original value in arguments block,
				 * make it the new phi arg,
				 * set its register,
				 * insert it into schedule,
				 * pin it
				 */
				ir_node *perm = get_Proj_pred(arg);
				ir_node *orig_val = get_irn_n(perm, get_Proj_proj(arg));
				ir_node *dupl = new_Copy(chordal_env->main_env->node_factory, cls, chordal_env->irg, arg_block, orig_val);
				assert(get_irn_mode(phi) == get_irn_mode(dupl));
				set_irn_n(phi, i, dupl);
				set_reg(dupl, phi_reg);
				sched_add_before(perm, dupl);
				pin_irn(dupl, phi_block);
				DBG((dbg, LEVEL_1, "      arg is pinned: insert %+F(%s)\n", dupl, get_reg(dupl)->name));
			} else {
				/* No other phi has the same color (else arg would be pinned),
				 * so just set the register and pin
				 */
				set_reg(arg, phi_reg);
				pin_irn(arg, phi_block);
				DBG((dbg, LEVEL_1, "      arg is not pinned: so pin %+F(%s)\n", arg, get_reg(arg)->name));
			}
		}
	}
}

static void	set_regs_or_place_dupls_walker(ir_node *bl, void *data) {
	be_chordal_env_t *chordal_env = data;
	ir_node *phi;

	for(phi = get_irn_link(bl); phi; phi = get_irn_link(phi))
		adjust_phi_arguments(chordal_env, phi);
}

static void	set_regs_or_place_dupls(be_chordal_env_t *chordal_env)
{
	DBG((dbg, LEVEL_1, "Setting regs and placing dupls...\n"));
	irg_block_walk_graph(chordal_env->irg, set_regs_or_place_dupls_walker, NULL, chordal_env);
}


void be_ssa_destruction(be_chordal_env_t *chordal_env) {
	pmap *perm_map = pmap_create();
	ir_graph *irg = chordal_env->irg;

	dbg = firm_dbg_register("ir.be.ssadestr");

	/* create a map for fast lookup of perms: block --> perm */
	chordal_env->data = perm_map;

	build_phi_rings(chordal_env);
	insert_all_perms(chordal_env);
#ifdef DUMP_GRAPHS
	dump_ir_block_graph_sched(irg, "-ssa_destr_perms_placed");
#endif
	set_regs_or_place_dupls(chordal_env);
#ifdef DUMP_GRAPHS
	dump_ir_block_graph_sched(irg, "-ssa_destr_regs_set");
#endif

	pmap_destroy(perm_map);
}

static void ssa_destruction_check_walker(ir_node *bl, void *data)
{
	be_chordal_env_t *chordal_env = data;
	ir_node *phi;
	int i, max;

	for(phi = get_irn_link(bl); phi; phi = get_irn_link(phi)) {
		const arch_register_t *phi_reg, *arg_reg;

		phi_reg = get_reg(phi);
		/* iterate over all args of phi */
		for(i=0, max=get_irn_arity(phi); i<max; ++i) {
			ir_node *arg = get_irn_n(phi, i);
			arg_reg = get_reg(arg);
			if(phi_reg != arg_reg) {
				DBG((dbg, 0, "Error: Registers of %+F and %+F differ: %s %s\n", phi, arg, phi_reg->name, arg_reg->name));
				assert(0);
			}
			if(!is_pinned(arg)) {
				DBG((dbg, 0, "Warning: Phi argument %+F is not pinned.\n", arg));
				assert(0);
			}
		}
	}
}

void be_ssa_destruction_check(be_chordal_env_t *chordal_env) {
	irg_block_walk_graph(chordal_env->irg, ssa_destruction_check_walker, NULL, chordal_env);
}
