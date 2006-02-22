#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "irprog_t.h"
#include "ircons.h"
#include "firm_types.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"

/**
 * creates a unique ident by adding a number to a tag
 *
 * @param tag   the tag string, must contain a %d if a number
 *              should be added
 */
static ident *unique_id(const char *tag)
{
	static unsigned id = 0;
	char str[256];

	snprintf(str, sizeof(str), tag, ++id);
	return new_id_from_str(str);
}



/**
 * Transforms a SymConst.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir SymConst node
 * @param mode    mode of the SymConst
 * @return the created ia32 Const node
 */
static ir_node *gen_SymConst(ia32_transform_env_t *env) {
	ir_node  *cnst;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;

	cnst = new_rd_ia32_Const(dbg, irg, block, mode);
	set_ia32_Const_attr(cnst, env->irn);
	return cnst;
}

/**
 * Get a primitive type for a mode.
 */
static ir_type *get_prim_type(pmap *types, ir_mode *mode)
{
	pmap_entry *e = pmap_find(types, mode);
	ir_type *res;

	if (! e) {
		char buf[64];
		snprintf(buf, sizeof(buf), "prim_type_%s", get_mode_name(mode));
		res = new_type_primitive(new_id_from_str(buf), mode);
		pmap_insert(types, mode, res);
	}
	else
		res = e->value;
	return res;
}

/**
 * Get an entity that is initialized with a tarval
 */
static entity *get_entity_for_tv(ia32_code_gen_t *cg, ir_node *cnst)
{
	tarval *tv    = get_Const_tarval(cnst);
	pmap_entry *e = pmap_find(cg->tv_ent, tv);
	entity *res;
	ir_graph *rem;

	if (! e) {
		ir_mode *mode = get_irn_mode(cnst);
		ir_type *tp = get_Const_type(cnst);
		if (tp == firm_unknown_type)
			tp = get_prim_type(cg->types, mode);

		res = new_entity(get_glob_type(), unique_id("ia32FloatCnst_%u"), tp);

		set_entity_ld_ident(res, get_entity_ident(res));
		set_entity_visibility(res, visibility_local);
		set_entity_variability(res, variability_constant);
		set_entity_allocation(res, allocation_static);

		 /* we create a new entity here: It's initialization must resist on the
		    const code irg */
		rem = current_ir_graph;
		current_ir_graph = get_const_code_irg();
		set_atomic_ent_value(res, new_Const_type(tv, tp));
		current_ir_graph = rem;
	}
	else
		res = e->value;
	return res;
}

/**
 * Transforms a Const.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Const node
 * @param mode    mode of the Const
 * @return the created ia32 Const node
 */
static ir_node *gen_Const(ia32_transform_env_t *env) {
	ir_node *cnst;
	symconst_symbol sym;
	ir_graph *irg   = env->irg;
	ir_node  *block = env->block;
	ir_node  *node  = env->irn;
	dbg_info *dbg   = env->dbg;
	ir_mode  *mode  = env->mode;

	if (mode_is_float(mode)) {
		sym.entity_p = get_entity_for_tv(env->cg, node);

		cnst = new_rd_SymConst(dbg, irg, block, sym, symconst_addr_ent);
		env->irn = cnst;
		cnst = gen_SymConst(env);
	}
	else {
		cnst = new_rd_ia32_Const(dbg, irg, block, get_irn_mode(node));
		set_ia32_Const_attr(cnst, node);
	}
	return cnst;
}



/**
 * Transforms (all) Const's into ia32_Const and places them in the
 * block where they are used (or in the cfg-pred Block in case of Phi's)
 */
void ia32_place_consts(ir_node *irn, void *env) {
	ia32_code_gen_t      *cg = env;
	ia32_transform_env_t  tenv;
	ir_mode              *mode;
	ir_node              *pred, *cnst;
	int                   i;
	opcode                opc;

	if (is_Block(irn))
		return;

	mode = get_irn_mode(irn);

	tenv.arch_env = cg->arch_env;
	tenv.block    = get_nodes_block(irn);
	tenv.cg       = cg;
	tenv.irg      = cg->irg;
	tenv.mod      = firm_dbg_register("firm.be.ia32.optimize");

	/* Loop over all predecessors and check for Sym/Const nodes */
	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		pred      = get_irn_n(irn, i);
		cnst      = NULL;
		opc       = get_irn_opcode(pred);
		tenv.irn  = pred;
		tenv.mode = get_irn_mode(pred);
		tenv.dbg  = get_irn_dbg_info(pred);

		/* If it's a Phi, then we need to create the */
		/* new Const in it's predecessor block       */
		if (is_Phi(irn)) {
			tenv.block = get_Block_cfgpred_block(get_nodes_block(irn), i);
		}

		switch (opc) {
			case iro_Const:
				cnst = gen_Const(&tenv);
				break;
			case iro_SymConst:
				cnst = gen_SymConst(&tenv);
				break;
			default:
				break;
		}

		/* if we found a const, then set it */
		if (cnst) {
			set_irn_n(irn, i, cnst);
		}
	}
}
