#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode.h"
#include "irprog_t.h"
#include "ircons.h"
#include "firm_types.h"
#include "iredges.h"
#include "tv.h"
#include "irgmod.h"

#include "../benode_t.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"

#undef is_NoMem
#define is_NoMem(irn) (get_irn_op(irn) == op_NoMem)

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
	tenv.mod      = cg->mod;

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


/******************************************************************
 *              _     _                   __  __           _
 *     /\      | |   | |                 |  \/  |         | |
 *    /  \   __| | __| |_ __ ___  ___ ___| \  / | ___   __| | ___
 *   / /\ \ / _` |/ _` | '__/ _ \/ __/ __| |\/| |/ _ \ / _` |/ _ \
 *  / ____ \ (_| | (_| | | |  __/\__ \__ \ |  | | (_) | (_| |  __/
 * /_/    \_\__,_|\__,_|_|  \___||___/___/_|  |_|\___/ \__,_|\___|
 *
 ******************************************************************/

static int node_is_comm(const ir_node *irn) {
	if (is_ia32_Add(irn)  ||
		is_ia32_fAdd(irn) ||
		is_ia32_Mul(irn)  ||
		is_ia32_Mulh(irn) ||
		is_ia32_fMul(irn) ||
		is_ia32_And(irn)  ||
		is_ia32_fAnd(irn) ||
		is_ia32_Or(irn)   ||
		is_ia32_fOr(irn)  ||
		is_ia32_Eor(irn)  ||
		is_ia32_fEor(irn) ||
		is_ia32_Min(irn)  ||
		is_ia32_fMin(irn) ||
		is_ia32_Max(irn)  ||
		is_ia32_fMax(irn))
	{
		return 1;
	}

	return 0;
}

static int ia32_get_irn_n_edges(const ir_node *irn) {
	const ir_edge_t *edge;
	int cnt = 0;

	foreach_out_edge(irn, edge) {
		cnt++;
	}

	return cnt;
}

/**
 * Returns the first mode_M Proj connected to irn.
 */
static ir_node *get_mem_proj(const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) == mode_M)
			return src;
	}

	return NULL;
}

/**
 * Returns the Proj with number 0 connected to irn.
 */
static ir_node *get_res_proj(const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_Proj_proj(src) == 0)
			return src;
	}

	return NULL;
}


/**
 * Determines if irn is a Proj and if is_op_func returns true for it's predecessor.
 */
static int pred_is_specific_node(const ir_node *irn, int (*is_op_func)(const ir_node *n)) {
	if (is_Proj(irn) && is_op_func(get_Proj_pred(irn))) {
		return 1;
	}

	return 0;
}

/**
 * Folds Add or Sub to LEA if possible
 */
static ir_node *fold_addr(ir_node *irn, firm_dbg_module_t *mod, ir_node *noreg) {
	ir_graph *irg      = get_irn_irg(irn);
	ir_mode  *mode     = get_irn_mode(irn);
	dbg_info *dbg      = get_irn_dbg_info(irn);
	ir_node  *block    = get_nodes_block(irn);
	ir_node  *res      = irn;
	char     *offs     = NULL;
	char     *new_offs = NULL;
	int       scale    = 0;
	int       isadd    = 0;
	int       dolea    = 0;
	ir_node  *left, *right, *temp;
	ir_node  *base, *index;
	ia32_am_flavour_t am_flav;

	if (is_ia32_Add(irn))
		isadd = 1;

	left  = get_irn_n(irn, 2);
	right = get_irn_n(irn, 3);

	/* "normalize" arguments in case of add */
	if  (isadd) {
		/* put LEA == ia32_am_O as right operand */
		if (is_ia32_Lea(left) && get_ia32_am_flavour(left) == ia32_am_O) {
			set_irn_n(irn, 2, right);
			set_irn_n(irn, 3, left);
			temp  = left;
			left  = right;
			right = temp;
		}

		/* put LEA != ia32_am_O as left operand */
		if (is_ia32_Lea(right) && get_ia32_am_flavour(right) != ia32_am_O) {
			set_irn_n(irn, 2, right);
			set_irn_n(irn, 3, left);
			temp  = left;
			left  = right;
			right = temp;
		}

		/* put SHL as right operand */
		if (pred_is_specific_node(left, is_ia32_Shl)) {
			set_irn_n(irn, 2, right);
			set_irn_n(irn, 3, left);
			temp  = left;
			left  = right;
			right = temp;
		}
	}

	/* Left operand could already be a LEA */
	if (is_ia32_Lea(left)) {
		DBG((mod, LEVEL_1, "\tgot LEA as left operand\n"));

		base  = get_irn_n(left, 0);
		index = get_irn_n(left, 1);
		offs  = get_ia32_am_offs(left);
		scale = get_ia32_am_scale(left);
	}
	else {
		base  = left;
		index = noreg;
		offs  = NULL;
		scale = 0;

	}

	/* check if operand is either const or right operand is AMConst (LEA with ia32_am_O) */
	if (get_ia32_cnst(irn)) {
		DBG((mod, LEVEL_1, "\tfound op with imm"));

		new_offs = get_ia32_cnst(irn);
		dolea    = 1;
	}
	else if (is_ia32_Lea(right) && get_ia32_am_flavour(right) == ia32_am_O) {
		DBG((mod, LEVEL_1, "\tgot op with LEA am_O"));

		new_offs = get_ia32_am_offs(right);
		dolea    = 1;
	}
	/* we can only get an additional index if there isn't already one */
	else if (isadd && be_is_NoReg(index)) {
		/* default for add -> make right operand to index */
		index = right;
		dolea = 1;

		DBG((mod, LEVEL_1, "\tgot LEA candidate with index %+F\n", index));
		/* check for SHL 1,2,3 */
		if (pred_is_specific_node(right, is_ia32_Shl)) {
			temp = get_Proj_pred(right);

			if (get_ia32_Immop_tarval(temp)) {
				scale = get_tarval_long(get_ia32_Immop_tarval(temp));

				if (scale <= 3) {
					scale = 1 << scale;
					index = get_irn_n(temp, 2);

					DBG((mod, LEVEL_1, "\tgot scaled index %+F\n", index));
				}
			}
		}
	}

	/* ok, we can create a new LEA */
	if (dolea) {
		res = new_rd_ia32_Lea(dbg, irg, block, base, index, mode_Is);

		/* add the old offset of a previous LEA */
		if (offs) {
			add_ia32_am_offs(res, offs);
		}

		/* add the new offset */
		if (isadd) {
			if (new_offs) {
				add_ia32_am_offs(res, new_offs);
			}
		}
		else {
			sub_ia32_am_offs(res, new_offs);
		}

		/* set scale */
		set_ia32_am_scale(res, scale);

		am_flav = ia32_am_N;
		/* determine new am flavour */
		if (offs || new_offs) {
			am_flav |= ia32_O;
		}
		if (! be_is_NoReg(base)) {
			am_flav |= ia32_B;
		}
		if (! be_is_NoReg(index)) {
			am_flav |= ia32_I;
		}
		if (scale > 0) {
			am_flav |= ia32_S;
		}
		set_ia32_am_flavour(res, am_flav);

		set_ia32_op_type(res, ia32_AddrModeS);

		DBG((mod, LEVEL_1, "\tLEA [%+F + %+F * %d + %s]\n", base, index, scale, get_ia32_am_offs(res)));

		/* get the result Proj of the Add/Sub */
		irn = get_res_proj(irn);

		assert(irn && "Couldn't find result proj");

		/* exchange the old op with the new LEA */
		exchange(irn, res);
	}

	return res;
}

/**
 * Optimizes a pattern around irn to address mode if possible.
 */
void ia32_optimize_am(ir_node *irn, void *env) {
	ia32_code_gen_t   *cg  = env;
	ir_graph          *irg = cg->irg;
	firm_dbg_module_t *mod = cg->mod;
	ir_node           *res = irn;
	dbg_info          *dbg;
	ir_mode           *mode;
	ir_node           *block, *noreg_gp, *noreg_fp;
	ir_node           *left, *right, *temp;
	ir_node           *store, *mem_proj;
	ir_node           *succ, *addr_b, *addr_i;
	int                check_am_src = 0;

	if (! is_ia32_irn(irn))
		return;

	dbg      = get_irn_dbg_info(irn);
	mode     = get_irn_mode(irn);
	block    = get_nodes_block(irn);
	noreg_gp = ia32_new_NoReg_gp(cg);
	noreg_fp = ia32_new_NoReg_fp(cg);

	DBG((mod, LEVEL_1, "checking for AM\n"));

	/* 1st part: check for address calculations and transform the into Lea */

	/* Following cases can occur:                                  */
	/* - Sub (l, imm) -> LEA [base - offset]                       */
	/* - Sub (l, r == LEA with ia32_am_O)   -> LEA [base - offset] */
	/* - Add (l, imm) -> LEA [base + offset]                       */
	/* - Add (l, r == LEA with ia32_am_O)  -> LEA [base + offset]  */
	/* - Add (l == LEA with ia32_am_O, r)  -> LEA [base + offset]  */
	/* - Add (l, r) -> LEA [base + index * scale]                  */
	/*              with scale > 1 iff l/r == shl (1,2,3)          */

	if (is_ia32_Sub(irn) || is_ia32_Add(irn)) {
		left  = get_irn_n(irn, 2);
		right = get_irn_n(irn, 3);

	    /* Do not try to create a LEA if one of the operands is a Load. */
		if (! pred_is_specific_node(left,  is_ia32_Load)  &&
			! pred_is_specific_node(right, is_ia32_Load))
		{
			res = fold_addr(irn, mod, noreg_gp);
		}
	}

	/* 2nd part: fold following patterns:                                               */
	/* - Load  -> LEA into Load  } TODO: If the LEA is used by more than one Load/Store */
	/* - Store -> LEA into Store }       it might be better to keep the LEA             */
	/* - op -> Load into AMop with am_Source                                            */
	/*   conditions:                                                                    */
	/*     - op is am_Source capable AND                                                */
	/*     - the Load is only used by this op AND                                       */
	/*     - the Load is in the same block                                              */
	/* - Store -> op -> Load  into AMop with am_Dest                                    */
	/*   conditions:                                                                    */
	/*     - op is am_Dest capable AND                                                  */
	/*     - the Store uses the same address as the Load AND                            */
	/*     - the Load is only used by this op AND                                       */
	/*     - the Load and Store are in the same block AND                               */
	/*     - nobody else uses the result of the op                                      */

	if ((res == irn) && (get_ia32_am_support(irn) != ia32_am_None) && !is_ia32_Lea(irn)) {
		/* 1st: check for Load/Store -> LEA   */
		if (is_ia32_Load(irn)  || is_ia32_fLoad(irn) ||
			is_ia32_Store(irn) || is_ia32_fStore(irn))
		{
			left = get_irn_n(irn, 0);

			if (is_ia32_Lea(left)) {
				/* get the AM attributes from the LEA */
				add_ia32_am_offs(irn, get_ia32_am_offs(left));
				set_ia32_am_scale(irn, get_ia32_am_scale(left));
				set_ia32_am_flavour(irn, get_ia32_am_flavour(left));
				set_ia32_op_type(irn, get_ia32_op_type(left));

				/* set base and index */
				set_irn_n(irn, 0, get_irn_n(left, 0));
				set_irn_n(irn, 1, get_irn_n(left, 1));
			}
		}
		/* check if at least one operand is a Load */
		else if (pred_is_specific_node(get_irn_n(irn, 2), is_ia32_Load)  ||
				 pred_is_specific_node(get_irn_n(irn, 2), is_ia32_fLoad) ||
				 pred_is_specific_node(get_irn_n(irn, 3), is_ia32_Load)  ||
				 pred_is_specific_node(get_irn_n(irn, 3), is_ia32_fLoad))
		{

			/* normalize commutative ops */
			if (node_is_comm(irn)) {
				left  = get_irn_n(irn, 2);
				right = get_irn_n(irn, 3);

				/* assure that Left operand is always a Load if there is one */
				if (pred_is_specific_node(right, is_ia32_Load) ||
					pred_is_specific_node(right, is_ia32_fLoad))
				{
					set_irn_n(irn, 2, right);
					set_irn_n(irn, 3, left);

					temp  = left;
					left  = right;
					right = temp;
				}
			}

			/* check for Store -> op -> Load */

			/* Store -> op -> Load optimization is only possible if supported by op */
			if (get_ia32_am_support(irn) & ia32_am_Dest) {

				/* An address mode capable op always has a result Proj.                  */
				/* If this Proj is used by more than one other node, we don't need to    */
				/* check further, otherwise we check for Store and remember the address, */
				/* the Store points to. */

				succ = get_res_proj(irn);
				assert(succ && "Couldn't find result proj");

				addr_b = NULL;
				addr_i = NULL;
				store  = NULL;

				/* now check for users and Store */
				if (ia32_get_irn_n_edges(succ) == 1) {
					succ = get_edge_src_irn(get_irn_out_edge_first(succ));

					if (is_ia32_fStore(succ) || is_ia32_Store(succ)) {
						store  = succ;
						addr_b = get_irn_n(store, 0);

						/* Could be that the Store is connected to the address    */
						/* calculating LEA while the Load is already transformed. */
						if (is_ia32_Lea(addr_b)) {
							succ   = addr_b;
							addr_b = get_irn_n(succ, 0);
							addr_i = get_irn_n(succ, 1);
						}
						else {
							addr_i = noreg_gp;
						}
					}
				}

				if (store) {
					/* we found a Store as single user: Now check for Load */
					left  = get_irn_n(irn, 2);
					right = get_irn_n(irn, 3);

					/* Could be that the right operand is also a Load, so we make */
					/* sure that the "interesting" Load is always the left one    */

					/* right != NoMem means, we have a "binary" operation */
					if (! is_NoMem(right) &&
						(pred_is_specific_node(right, is_ia32_Load) ||
						 pred_is_specific_node(right, is_ia32_fLoad)))
					{
						if ((addr_b == get_irn_n(get_Proj_pred(right), 0)) &&
							(addr_i == get_irn_n(get_Proj_pred(right), 1)))
						{
							/* We exchange left and right, so it's easier to kill     */
							/* the correct Load later and to handle unary operations. */
							set_irn_n(irn, 2, right);
							set_irn_n(irn, 3, left);

							temp  = left;
							left  = right;
							right = temp;
						}
					}

					/* skip the Proj for easier access */
					left  = get_Proj_pred(left);

					/* Compare Load and Store address */
					if ((addr_b == get_irn_n(left, 0)) && (addr_i == get_irn_n(left, 1)))
					{
						/* Left Load is from same address, so we can */
						/* disconnect the Load and Store here        */

						/* set new base, index and attributes */
						set_irn_n(irn, 0, addr_b);
						set_irn_n(irn, 1, addr_i);
						add_ia32_am_offs(irn, get_ia32_am_offs(left));
						set_ia32_am_scale(irn, get_ia32_am_scale(left));
						set_ia32_am_flavour(irn, get_ia32_am_flavour(left));
						set_ia32_op_type(irn, ia32_AddrModeD);

						/* connect to Load memory */
						if (get_irn_arity(irn) == 5) {
							/* binary AMop */
							set_irn_n(irn, 4, get_irn_n(left, 2));
						}
						else {
							/* unary AMop */
							set_irn_n(irn, 3, get_irn_n(left, 2));
						}

						/* disconnect from Load */
						set_irn_n(irn, 2, noreg_gp);

						/* connect the memory Proj of the Store to the op */
						mem_proj = get_mem_proj(store);
						set_Proj_pred(mem_proj, irn);
						set_Proj_proj(mem_proj, 1);
					}
				} /* if (store) */
				else if (get_ia32_am_support(irn) & ia32_am_Source) {
					/* There was no store, check if we still can optimize for source address mode */
					check_am_src = 1;
				}
			} /* if (support AM Dest) */
			else {
				/* op doesn't support am AM Dest -> check for AM Source */
				check_am_src = 1;
			}

			/* optimize op -> Load iff Load is only used by this op */
			if (check_am_src) {
				left = get_irn_n(irn, 2);

				if (ia32_get_irn_n_edges(left) == 1) {
					left = get_Proj_pred(left);

					addr_b = get_irn_n(left, 0);
					addr_i = get_irn_n(left, 1);

					/* set new base, index and attributes */
					set_irn_n(irn, 0, addr_b);
					set_irn_n(irn, 1, addr_i);
					add_ia32_am_offs(irn, get_ia32_am_offs(left));
					set_ia32_am_scale(irn, get_ia32_am_scale(left));
					set_ia32_am_flavour(irn, get_ia32_am_flavour(left));
					set_ia32_op_type(irn, ia32_AddrModeS);

					/* connect to Load memory */
					if (get_irn_arity(irn) == 5) {
						/* binary AMop */
						set_irn_n(irn, 4, get_irn_n(left, 2));
					}
					else {
						/* unary AMop */
						set_irn_n(irn, 3, get_irn_n(left, 2));
					}

					/* disconnect from Load */
					set_irn_n(irn, 2, noreg_gp);

					/* If Load has a memory Proj, connect it to the op */
					mem_proj = get_mem_proj(left);
					if (mem_proj) {
						set_Proj_pred(mem_proj, irn);
						set_Proj_proj(mem_proj, 1);
					}
				}
			}
		}
	}
}
