/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   The main ppc backend driver file.
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#include "config.h"

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "irdump.h"

#include "bitset.h"
#include "debug.h"
#include "error.h"

#include "../bearch_t.h"                /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "be.h"
#include "../beabi.h"
#include "../bemachine.h"
#include "../bemodule.h"
#include "../bespillslots.h"
#include "../beblocksched.h"
#include "../beirg_t.h"
#include "../begnuas.h"

#include "pset.h"

#include "bearch_ppc32_t.h"

#include "ppc32_new_nodes.h"           /* ppc nodes interface */
#include "gen_ppc32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "ppc32_transform.h"
#include "ppc32_transform_conv.h"
#include "ppc32_emitter.h"
#include "ppc32_map_regs.h"

#define DEBUG_MODULE "firm.be.ppc.isa"

int isleaf;

/* TODO: ugly, but we need it to get access to the registers assigned to Phi nodes */
static set *cur_reg_set = NULL;

/**************************************************
 *                         _ _              _  __
 *                        | | |            (_)/ _|
 *  _ __ ___  __ _    __ _| | | ___   ___   _| |_
 * | '__/ _ \/ _` |  / _` | | |/ _ \ / __| | |  _|
 * | | |  __/ (_| | | (_| | | | (_) | (__  | | |
 * |_|  \___|\__, |  \__,_|_|_|\___/ \___| |_|_|
 *            __/ |
 *           |___/
 **************************************************/

/**
 * Return register requirements for a ppc node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const arch_register_req_t *ppc32_get_irn_reg_req(const ir_node *irn,
                                                        int pos)
{
	long               node_pos = pos == -1 ? 0 : pos;
	ir_mode           *mode     = get_irn_mode(irn);
	FIRM_DBG_REGISTER(firm_dbg_module_t *mod, DEBUG_MODULE);

	if (is_Block(irn) || mode == mode_X || mode == mode_M) {
		DBG((mod, LEVEL_1, "ignoring block, mode_X or mode_M node %+F\n", irn));
		return arch_no_register_req;
	}

	if (mode == mode_T && pos < 0) {
		DBG((mod, LEVEL_1, "ignoring request for OUT requirements at %+F", irn));
		return arch_no_register_req;
	}

	DBG((mod, LEVEL_1, "get requirements at pos %d for %+F ... ", pos, irn));

	if (is_Proj(irn)) {
		/* in case of a proj, we need to get the correct OUT slot */
		/* of the node corresponding to the proj number */
		if (pos == -1) {
			node_pos = ppc32_translate_proj_pos(irn);
		} else {
			node_pos = pos;
		}

		irn = skip_Proj_const(irn);

		DB((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", irn, node_pos));
	}

	/* get requirements for our own nodes */
	if (is_ppc32_irn(irn)) {
		const arch_register_req_t *req;
		if (pos >= 0) {
			req = get_ppc32_in_req(irn, pos);
		} else {
			req = get_ppc32_out_req(irn, node_pos);
		}

		DB((mod, LEVEL_1, "returning reqs for %+F at pos %d\n", irn, pos));
		return req;
	}

	/* unknowns should be transformed by now */
	assert(!is_Unknown(irn));

	DB((mod, LEVEL_1, "returning NULL for %+F (node not supported)\n", irn));
	return arch_no_register_req;
}

static arch_irn_class_t ppc32_classify(const ir_node *irn)
{
	irn = skip_Proj_const(irn);

	if (is_cfop(irn)) {
		return arch_irn_class_branch;
	}

	return 0;
}

static ir_entity *ppc32_get_frame_entity(const ir_node *irn)
{
	if(!is_ppc32_irn(irn)) return NULL;
	if(get_ppc32_type(irn)!=ppc32_ac_FrameEntity) return NULL;
	return get_ppc32_frame_entity(irn);
}

static void ppc32_set_frame_entity(ir_node *irn, ir_entity *ent)
{
	if (! is_ppc32_irn(irn) || get_ppc32_type(irn) != ppc32_ac_FrameEntity)
		return;
	set_ppc32_frame_entity(irn, ent);
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void ppc32_set_stack_bias(ir_node *irn, int bias)
{
	set_ppc32_offset(irn, bias);
}

static int ppc32_get_sp_bias(const ir_node *irn)
{
	(void) irn;
	return 0;
}

typedef struct
{
	const be_abi_call_t *call;
	ir_graph *irg;
} ppc32_abi_env;

/**
 * Initialize the callback object.
 * @param call The call object.
 * @param aenv The architecture environment.
 * @param irg  The graph with the method.
 * @return     Some pointer. This pointer is passed to all other callback functions as self object.
 */
static void *ppc32_abi_init(const be_abi_call_t *call, const arch_env_t *aenv, ir_graph *irg)
{
	ppc32_abi_env *env = XMALLOC(ppc32_abi_env);
	(void) aenv;

	env->call = call;
	env->irg = irg;
	return env;
}

/**
 * Destroy the callback object.
 * @param self The callback object.
 */
static void ppc32_abi_done(void *self)
{
	free(self);
}

/**
 * Get the between type for that call.
 * @param self The callback object.
 * @return The between type of for that call.
 */
static ir_type *ppc32_abi_get_between_type(void *self)
{
	static ir_type *between_type = NULL;
	static ir_entity *old_bp_ent = NULL;
	(void) self;

	if(!between_type) {
		ir_entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);
		ir_type *old_bp_type   = new_type_primitive(new_id_from_str("bp"), mode_P);

		between_type           = new_type_class(new_id_from_str("ppc32_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("old_bp"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
	}

	return between_type;
}

/**
 * Generate the prologue.
 * @param self       The callback object.
 * @param mem        A pointer to the mem node. Update this if you define new memory.
 * @param reg_map    A mapping mapping all callee_save/ignore/parameter registers to their defining nodes.
 * @param stack_bias Points to the current stack bias, can be modified if needed.
 *
 * @return        The register which shall be used as a stack frame base.
 *
 * All nodes which define registers in @p reg_map must keep @p reg_map current.
 */
static const arch_register_t *ppc32_abi_prologue(void *self, ir_node **mem, pmap *reg_map, int *stack_bias)
{
	ppc32_abi_env *env = (ppc32_abi_env *) self;
	be_abi_call_flags_t flags = be_abi_call_get_flags(env->call);
	(void) mem;
	(void) reg_map;
	(void) stack_bias;
	isleaf = flags.bits.irg_is_leaf;

	if (flags.bits.try_omit_fp)
		return &ppc32_gp_regs[REG_R1];
	else
		return &ppc32_gp_regs[REG_R31];
}

/**
 * Generate the epilogue.
 * @param self    The callback object.
 * @param mem     Memory one can attach to.
 * @param reg_map A mapping mapping all callee_save/ignore/return registers to their defining nodes.
 *
 * All nodes which define registers in @p reg_map must keep @p reg_map current.
 * Also, the @p mem variable must be updated, if memory producing nodes are inserted.
 */
static void ppc32_abi_epilogue(void *self, ir_node *bl, ir_node **mem, pmap *reg_map)
{
	(void) self;
	(void) bl;
	(void) mem;
	(void) reg_map;
}

static const be_abi_callbacks_t ppc32_abi_callbacks = {
	ppc32_abi_init,
	ppc32_abi_done,
	ppc32_abi_get_between_type,
	ppc32_abi_prologue,
	ppc32_abi_epilogue,
};

/* fill register allocator interface */

static const arch_irn_ops_t ppc32_irn_ops = {
	ppc32_get_irn_reg_req,
	ppc32_classify,
	ppc32_get_frame_entity,
	ppc32_set_frame_entity,
	ppc32_set_stack_bias,
	ppc32_get_sp_bias,
	NULL,    /* get_inverse             */
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};

/**************************************************
 *                _                         _  __
 *               | |                       (_)/ _|
 *   ___ ___   __| | ___  __ _  ___ _ __    _| |_
 *  / __/ _ \ / _` |/ _ \/ _` |/ _ \ '_ \  | |  _|
 * | (_| (_) | (_| |  __/ (_| |  __/ | | | | | |
 *  \___\___/ \__,_|\___|\__, |\___|_| |_| |_|_|
 *                        __/ |
 *                       |___/
 **************************************************/

static void ppc32_before_abi(void *self) {
	ppc32_code_gen_t *cg = self;
	ir_type *frame_type = get_irg_frame_type(cg->irg);

	frame_alloc_area(frame_type, 24, 4, 1);

	ppc32_init_conv_walk();
	irg_walk_blkwise_graph(cg->irg, NULL, ppc32_conv_walk, cg);

	if (cg->area_size) {
		if(cg->area_size < 32) cg->area_size = 32;
		cg->area = frame_alloc_area(get_irg_frame_type(cg->irg), cg->area_size+24, 16, 1);
	}
}

static void ppc32_search_start_successor(ir_node *block, void *env) {
	ppc32_code_gen_t *cg = env;
	int n = get_Block_n_cfgpreds(block);
	ir_node *startblock = get_irg_start_block(cg->irg);
	if(block == startblock) return;

	for (n--; n >= 0; n--) {
		ir_node *predblock = get_irn_n(get_Block_cfgpred(block, n), -1);
		if(predblock == startblock)
		{
			cg->start_succ_block = block;
			return;
		}
	}
}

/**
 * Transforms the standard firm graph into
 * a ppc firm graph
 */
static void ppc32_prepare_graph(void *self) {
	ppc32_code_gen_t *cg = self;

	irg_block_walk_graph(cg->irg, NULL, ppc32_search_start_successor, cg);
	irg_walk_blkwise_graph(cg->irg, NULL, ppc32_pretransform_walk, cg);
	be_dump(cg->irg, "-pretransformed", dump_ir_block_graph);

	ppc32_register_transformers();
	irg_walk_blkwise_graph(cg->irg, NULL, ppc32_transform_node, cg);
	be_dump(cg->irg, "-transformed", dump_ir_block_graph);
	irg_walk_blkwise_graph(cg->irg, NULL, ppc32_transform_const, cg);
}



/**
 * Called immediatly before emit phase.
 */
static void ppc32_finish_irg(void *self) {
	(void) self;
	/* TODO: - fix offsets for nodes accessing stack
			 - ...
	*/
}


/**
 * Called before the register allocator.
 * Calculate a block schedule here. We need it for the x87
 * simulator and the emitter.
 */
static void ppc32_before_ra(void *self) {
	ppc32_code_gen_t *cg = self;
	cg->blk_sched = be_create_block_schedule(cg->irg, cg->birg->exec_freq);
}

static void ppc32_transform_spill(ir_node *node, void *env)
{
	(void)env;

	if(be_is_Spill(node))
	{
		ir_node  *store, *proj;
		dbg_info *dbg   = get_irn_dbg_info(node);
		ir_node  *block = get_nodes_block(node);

		const arch_register_class_t *regclass = arch_get_irn_reg_class(node, 1);

		if (regclass == &ppc32_reg_classes[CLASS_ppc32_gp])
		{
			store = new_bd_ppc32_Stw(dbg, block,
				get_irn_n(node, 0), get_irn_n(node, 1), new_NoMem());
		}
		else if (regclass == &ppc32_reg_classes[CLASS_ppc32_fp])
		{
			store = new_bd_ppc32_Stfd(dbg, block,
				get_irn_n(node, 0), get_irn_n(node, 1), new_NoMem());
		}
		else panic("Spill for register class not supported yet!");

		set_ppc32_frame_entity(store, be_get_frame_entity(node));

		proj = new_rd_Proj(dbg, block, store, mode_M, pn_Store_M);

		if (sched_is_scheduled(node)) {
			sched_add_after(sched_prev(node), store);
			sched_add_after(store, proj);

			sched_remove(node);
		}

		exchange(node, proj);
	}

	if(be_is_Reload(node))
	{
		ir_node *load, *proj;
		const arch_register_t *reg;
		dbg_info *dbg   = get_irn_dbg_info(node);
		ir_node  *block = get_nodes_block(node);
		ir_mode  *mode  = get_irn_mode(node);

		const arch_register_class_t *regclass = arch_get_irn_reg_class_out(node);

		if (regclass == &ppc32_reg_classes[CLASS_ppc32_gp])
		{
			load = new_bd_ppc32_Lwz(dbg, block,	get_irn_n(node, 0), get_irn_n(node, 1));
		}
		else if (regclass == &ppc32_reg_classes[CLASS_ppc32_fp])
		{
			load = new_bd_ppc32_Lfd(dbg, block,	get_irn_n(node, 0), get_irn_n(node, 1));
		}
		else panic("Reload for register class not supported yet!");

		set_ppc32_frame_entity(load, be_get_frame_entity(node));

		proj = new_rd_Proj(dbg, block, load, mode, pn_Load_res);

		if (sched_is_scheduled(node)) {
			sched_add_after(sched_prev(node), load);
			sched_add_after(load, proj);

			sched_remove(node);
		}

		/* copy the register from the old node to the new Load */
		reg = arch_get_irn_register(node);
		arch_set_irn_register(load, reg);

		exchange(node, proj);
	}
}

/**
 * Some stuff to do immediately after register allocation
 */
static void ppc32_after_ra(void *self) {
	ppc32_code_gen_t *cg = self;
	be_coalesce_spillslots(cg->birg);
	irg_walk_blkwise_graph(cg->irg, NULL, ppc32_transform_spill, NULL);
}

/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void ppc32_emit_and_done(void *self) {
	ppc32_code_gen_t *cg = self;
	ir_graph         *irg = cg->irg;

	dump_ir_block_graph_sched(irg, "-ppc-finished");
	ppc32_gen_routine(cg, irg);

	cur_reg_set = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);
}

int is_direct_entity(ir_entity *ent);

static void *ppc32_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t ppc32_code_gen_if = {
	ppc32_cg_init,
	NULL,                 /* get_pic_base */
	ppc32_before_abi,
	ppc32_prepare_graph,
	NULL,                 /* spill */
	ppc32_before_ra,      /* before register allocation hook */
	ppc32_after_ra,
	ppc32_finish_irg,
	ppc32_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *ppc32_cg_init(be_irg_t *birg) {
	ppc32_isa_t      *isa = (ppc32_isa_t *)birg->main_env->arch_env;
	ppc32_code_gen_t *cg  = XMALLOC(ppc32_code_gen_t);

	cg->impl      = &ppc32_code_gen_if;
	cg->irg       = birg->irg;
	cg->reg_set   = new_set(ppc32_cmp_irn_reg_assoc, 1024);
	cg->isa       = isa;
	cg->birg      = birg;
	cg->area_size = 0;
	cg->area      = NULL;
	cg->start_succ_block = NULL;
	cg->blk_sched = NULL;
	FIRM_DBG_REGISTER(cg->mod, "firm.be.ppc.cg");

	cur_reg_set = cg->reg_set;

	return (arch_code_generator_t *)cg;
}



/*****************************************************************
 *  ____             _                  _   _____  _____
 * |  _ \           | |                | | |_   _|/ ____|  /\
 * | |_) | __ _  ___| | _____ _ __   __| |   | | | (___   /  \
 * |  _ < / _` |/ __| |/ / _ \ '_ \ / _` |   | |  \___ \ / /\ \
 * | |_) | (_| | (__|   <  __/ | | | (_| |  _| |_ ____) / ____ \
 * |____/ \__,_|\___|_|\_\___|_| |_|\__,_| |_____|_____/_/    \_\
 *
 *****************************************************************/

static ppc32_isa_t ppc32_isa_template = {
	{
		&ppc32_isa_if,           /* isa interface */
		&ppc32_gp_regs[REG_R1],  /* stack pointer */
		&ppc32_gp_regs[REG_R31], /* base pointer */
		&ppc32_reg_classes[CLASS_ppc32_gp],  /* static link pointer class */
		-1,                      /* stack is decreasing */
		2,                       /* power of two stack alignment for calls, 2^2 == 4 */
		NULL,                    /* main environment */
		7,                       /* spill costs */
		5,                       /* reload costs */
	},
	NULL                    /* symbol set */
};

/**
 * Collects all SymConsts which need to be accessed "indirectly"
 *
 * @param node    the firm node
 * @param env     the symbol set
 */
static void ppc32_collect_symconsts_walk(ir_node *node, void *env) {
	pset *symbol_set = env;

	if (is_SymConst(node)) {
		ir_entity *ent = get_SymConst_entity(node);
		set_entity_backend_marked(ent, 1);
		if (! is_direct_entity(ent))
			pset_insert_ptr(symbol_set, ent);
	}
}

/**
 * Initializes the backend ISA and opens the output file.
 */
static arch_env_t *ppc32_init(FILE *file_handle) {
	static int inited = 0;
	ppc32_isa_t *isa;
	int i;

	if (inited)
		return NULL;

	isa = XMALLOC(ppc32_isa_t);
	memcpy(isa, &ppc32_isa_template, sizeof(*isa));

	be_emit_init(file_handle);

	ppc32_register_init();
	ppc32_create_opcodes(&ppc32_irn_ops);

	inited = 1;

	isa->symbol_set = pset_new_ptr(8);
	for (i = 0; i < get_irp_n_irgs(); ++i) {
		ir_graph *irg = get_irp_irg(i);
		irg_walk_blkwise_graph(irg, NULL, ppc32_collect_symconsts_walk, isa->symbol_set);
	}

	/* we mark referenced global entities, so we can only emit those which
	 * are actually referenced. (Note: you mustn't use the type visited flag
	 * elsewhere in the backend)
	 */
	inc_master_type_visited();

	return &isa->arch_env;
}

static void ppc32_dump_indirect_symbols(ppc32_isa_t *isa) {
	ir_entity *ent;

	foreach_pset(isa->symbol_set, ent) {
		const char *ld_name = get_entity_ld_name(ent);
		be_emit_irprintf(".non_lazy_symbol_pointer\n%s:\n\t.indirect_symbol _%s\n\t.long 0\n\n", ld_name, ld_name);
		be_emit_write_line();
	}
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void ppc32_done(void *self) {
	ppc32_isa_t *isa = self;

	be_gas_emit_decls(isa->arch_env.main_env, 1);
	be_gas_emit_switch_section(GAS_SECTION_DATA);
	ppc32_dump_indirect_symbols(isa);

	be_emit_exit();
	del_pset(isa->symbol_set);

	free(self);
}



static unsigned ppc32_get_n_reg_class(const void *self) {
	(void) self;
	return N_CLASSES;
}

static const arch_register_class_t *ppc32_get_reg_class(const void *self,
                                                        unsigned i) {
	(void) self;
	assert(i < N_CLASSES && "Invalid ppc register class requested.");
	return &ppc32_reg_classes[i];
}



/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *ppc32_get_reg_class_for_mode(const void *self, const ir_mode *mode) {
	(void) self;
	if (mode_is_float(mode))
		return &ppc32_reg_classes[CLASS_ppc32_fp];
	else
		return &ppc32_reg_classes[CLASS_ppc32_gp];
}


/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void ppc32_get_call_abi(const void *self, ir_type *method_type, be_abi_call_t *abi) {
	ir_type  *tp;
	ir_mode  *mode;
	int       i, n = get_method_n_params(method_type);
	int		  stackoffs = 0, lastoffs = 0, stackparamsize;

	int		  gpregi = REG_R3;
	int		  fpregi = REG_F1;

	const arch_register_t *reg;
	be_abi_call_flags_t call_flags = { { 0, 0, 1, 0, 0, 0, 1 } };

	(void) self;
	if(get_type_visibility(method_type)!=visibility_external_allocated)
		call_flags.bits.call_has_imm = 1;

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &ppc32_abi_callbacks);

	for (i = 0; i < n; i++) {
		tp   = get_method_param_type(method_type, i);
		mode = get_type_mode(tp);
		if(is_atomic_type(tp))
		{
			if(mode_is_float(mode))
			{
				if(fpregi <= REG_F13)
				{
					if(get_mode_size_bits(mode) == 32) gpregi++, stackparamsize=4;
					else gpregi += 2, stackparamsize=8;								// mode == irm_D
					reg = &ppc32_fp_regs[fpregi++];
				}
				else
				{
					if(get_mode_size_bits(mode) == 32) stackparamsize=4;
					else stackparamsize=8;								// mode == irm_D
					reg = NULL;
				}
			}
			else
			{
				if(gpregi <= REG_R10)
					reg = &ppc32_gp_regs[gpregi++];
				else
					reg = NULL;
				stackparamsize=4;
			}

			if(reg)
				be_abi_call_param_reg(abi, i, reg);
			else
			{
				be_abi_call_param_stack(abi, i, mode, 4, stackoffs - lastoffs, 0);
				lastoffs = stackoffs+stackparamsize;
			}
			stackoffs += stackparamsize;
		}
		else
		{
			be_abi_call_param_stack(abi, i, mode, 4, stackoffs - lastoffs, 0);
			stackoffs += (get_type_size_bytes(tp)+3) & -4;
			lastoffs = stackoffs;
		}
	}

	/* explain where result can be found if any */
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		be_abi_call_res_reg(abi, 0,
			mode_is_float(mode) ? &ppc32_fp_regs[REG_F1] : &ppc32_gp_regs[REG_R3]);
	}
}

int ppc32_to_appear_in_schedule(void *block_env, const ir_node *irn) {
	(void) block_env;
	if(!is_ppc32_irn(irn))
		return -1;

	return 1;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *ppc32_get_code_generator_if(void *self) {
	(void) self;
	return &ppc32_code_gen_if;
}

list_sched_selector_t ppc32_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() overloaded
 */
static const list_sched_selector_t *ppc32_get_list_sched_selector(const void *self, list_sched_selector_t *selector) {
	(void) self;
	(void) selector;
	ppc32_sched_selector = trivial_selector;
	ppc32_sched_selector.to_appear_in_schedule = ppc32_to_appear_in_schedule;
	return &ppc32_sched_selector;
}

static const ilp_sched_selector_t *ppc32_get_ilp_sched_selector(const void *self) {
	(void) self;
	return NULL;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int ppc32_get_reg_class_alignment(const void *self, const arch_register_class_t *cls) {
	ir_mode *mode = arch_register_class_mode(cls);
	(void) self;

	return get_mode_size_bytes(mode);
}

static const be_execution_unit_t ***ppc32_get_allowed_execution_units(const void *self, const ir_node *irn) {
	(void) self;
	(void) irn;
	/* TODO */
	panic("Unimplemented ppc32_get_allowed_execution_units()");
	return NULL;
}

static const be_machine_t *ppc32_get_machine(const void *self) {
	(void) self;
	/* TODO */
	panic("Unimplemented ppc32_get_machine()");
	return NULL;
}

/**
 * Return irp irgs in the desired order.
 */
static ir_graph **ppc32_get_irg_list(const void *self, ir_graph ***irg_list) {
	(void) self;
	(void) irg_list;
	return NULL;
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *ppc32_get_libfirm_params(void) {
	static backend_params p = {
		1,     /* need dword lowering */
		0,     /* don't support inline assembler yet */
		NULL,  /* will be set later */
		NULL,  /* but yet no creator function */
		NULL,  /* context for create_intrinsic_fkt */
		NULL,  /* no if conversion settings */
		NULL,  /* no float arithmetic mode (TODO) */
		0,     /* no trampoline support: size 0 */
		0,     /* no trampoline support: align 0 */
		NULL,  /* no trampoline support: no trampoline builder */
		4      /* alignment of stack parameter */
	};

	return &p;
}

static asm_constraint_flags_t ppc32_parse_asm_constraint(const void *self, const char **c)
{
	/* no asm support yet */
	(void) self;
	(void) c;
	return ASM_CONSTRAINT_FLAG_INVALID;
}

static int ppc32_is_valid_clobber(const void *self, const char *clobber)
{
	/* no asm support yet */
	(void) self;
	(void) clobber;
	return 0;
}

const arch_isa_if_t ppc32_isa_if = {
	ppc32_init,
	ppc32_done,
	NULL,             /* handle intrinsics */
	ppc32_get_n_reg_class,
	ppc32_get_reg_class,
	ppc32_get_reg_class_for_mode,
	ppc32_get_call_abi,
	ppc32_get_code_generator_if,
	ppc32_get_list_sched_selector,
	ppc32_get_ilp_sched_selector,
	ppc32_get_reg_class_alignment,
	ppc32_get_libfirm_params,
	ppc32_get_allowed_execution_units,
	ppc32_get_machine,
	ppc32_get_irg_list,
	NULL,                 /* mark remat */
	ppc32_parse_asm_constraint,
	ppc32_is_valid_clobber
};

void be_init_arch_ppc32(void)
{
	be_register_isa_if("ppc32", &ppc32_isa_if);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_ppc32);
