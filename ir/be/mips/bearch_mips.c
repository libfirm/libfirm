/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   The main mips backend driver file.
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "iredges.h"
#include "irdump.h"
#include "irextbb.h"
#include "error.h"

#include "bitset.h"
#include "debug.h"

#include "../bearch_t.h"
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "../be.h"
#include "../beabi.h"
#include "../bemachine.h"
#include "../bemodule.h"
#include "../bespillslots.h"
#include "../beemitter.h"
#include "../begnuas.h"

#include "bearch_mips_t.h"

#include "mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"
#include "mips_transform.h"
#include "mips_emitter.h"
#include "mips_map_regs.h"
#include "mips_util.h"
#include "mips_scheduler.h"

#define DEBUG_MODULE "firm.be.mips.isa"

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
 * Return register requirements for a mips node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const
arch_register_req_t *mips_get_irn_reg_req(const void *self,
                                          const ir_node *node, int pos) {
	long               node_pos = pos == -1 ? 0 : pos;
	ir_mode           *mode     = get_irn_mode(node);

	if (is_Block(node) || mode == mode_X || mode == mode_M) {
		return arch_no_register_req;
	}

	if (mode == mode_T && pos < 0) {
		return arch_no_register_req;
	}

	if (is_Proj(node)) {
		/* in case of a proj, we need to get the correct OUT slot */
		/* of the node corresponding to the proj number */
		if (pos == -1) {
			node_pos = mips_translate_proj_pos(node);
		}
		else {
			node_pos = pos;
		}

		node = skip_Proj_const(node);
	}

	/* get requirements for our own nodes */
	if (is_mips_irn(node)) {
		const arch_register_req_t *req;
		if (pos >= 0) {
			req = get_mips_in_req(node, pos);
		} else {
			req = get_mips_out_req(node, node_pos);
		}

		return req;
	}

	/* unknown should be translated by now */
	assert(!is_Unknown(node));

	return arch_no_register_req;
}

static void mips_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg) {
	int pos = 0;

	if (is_Proj(irn)) {

		if (get_irn_mode(irn) == mode_X) {
			return;
		}

		pos = mips_translate_proj_pos(irn);
		irn = skip_Proj(irn);
	}

	if (is_mips_irn(irn)) {
		const arch_register_t **slots;

		slots      = get_mips_slots(irn);
		slots[pos] = reg;
	}
	else {
		/* here we set the registers for the Phi nodes */
		mips_set_firm_reg(irn, reg, cur_reg_set);
	}
}

static const arch_register_t *mips_get_irn_reg(const void *self, const ir_node *irn) {
	int pos = 0;
	const arch_register_t *reg = NULL;

	if (is_Proj(irn)) {

		if (get_irn_mode(irn) == mode_X) {
			return NULL;
		}

		pos = mips_translate_proj_pos(irn);
		irn = skip_Proj_const(irn);
	}

	if (is_mips_irn(irn)) {
		const arch_register_t **slots;
		slots = get_mips_slots(irn);
		reg   = slots[pos];
	}
	else {
		reg = mips_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t mips_classify(const void *self, const ir_node *irn) {
	irn = skip_Proj_const(irn);

	if (is_cfop(irn)) {
		return arch_irn_class_branch;
	} else if (is_mips_irn(irn)) {
		return arch_irn_class_normal;
	}

	return 0;
}

static arch_irn_flags_t mips_get_flags(const void *self, const ir_node *irn) {
	irn = skip_Proj_const(irn);

	if (is_mips_irn(irn)) {
		return get_mips_flags(irn);
	}
	else if (is_Unknown(irn)) {
		return arch_irn_flags_ignore;
	}

	return 0;
}

static
ir_entity *mips_get_frame_entity(const void *self, const ir_node *node) {
	mips_attr_t *attr;

	if(!is_mips_irn(node))
		return NULL;

	attr = get_mips_attr(node);
	return attr->stack_entity;
}

static
void mips_set_frame_entity(const void *self, ir_node *irn, ir_entity *ent) {
	mips_attr_t *attr  = get_mips_attr(irn);
	attr->stack_entity = ent;
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void mips_set_frame_offset(const void *self, ir_node *irn, int offset)
{
	panic("TODO");
#if 0
	mips_attr_t *attr = get_mips_attr(irn);
	attr->stack_entity_offset = offset;
#endif
}

static int mips_get_sp_bias(const void *self, const ir_node *irn) {
	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_if_t mips_irn_ops_if = {
	mips_get_irn_reg_req,
	mips_set_irn_reg,
	mips_get_irn_reg,
	mips_classify,
	mips_get_flags,
	mips_get_frame_entity,
	mips_set_frame_entity,
	mips_set_frame_offset,
	mips_get_sp_bias,
	NULL,    /* get_inverse             */
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};

mips_irn_ops_t mips_irn_ops = {
	&mips_irn_ops_if,
	NULL
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


typedef struct {
	ir_node *start;
	ir_node *end;
	unsigned cnt;
} anchor;

/**
 * Ext-Block walker: create a block schedule
 */
static void create_block_list(ir_extblk *blk, void *env) {
	anchor *list = env;
	int i, n;

	for (i = 0, n = get_extbb_n_blocks(blk); i < n; ++i) {
		ir_node *block = get_extbb_block(blk, i);

		set_irn_link(block, NULL);
		if (list->start)
			set_irn_link(list->end, block);
		else
			list->start = block;

		list->end = block;
		list->cnt += 1;
	}
}

/* return the scheduled block at position pos */
ir_node *mips_get_sched_block(const mips_code_gen_t *cg, int pos) {
	if (0 <= pos && pos < ARR_LEN(cg->bl_list))
		return cg->bl_list[pos];
	return NULL;
}

/* return the number of scheduled blocks */
int mips_get_sched_n_blocks(const mips_code_gen_t *cg) {
	return ARR_LEN(cg->bl_list);
}

/* set a block schedule number */
void mips_set_block_sched_nr(ir_node *block, int nr) {
	set_irn_link(block, INT_TO_PTR(nr));
}

/* get a block schedule number */
int mips_get_block_sched_nr(ir_node *block) {
	return PTR_TO_INT(get_irn_link(block));
}

/**
 * Creates a block schedule for the given graph.
 */
static void mips_create_block_sched(mips_code_gen_t *cg) {
	anchor list;
	ir_node **bl_list, *block;
	unsigned i;

	if (cg->bl_list) {
		DEL_ARR_F(cg->bl_list);
		free_survive_dce(cg->bl_list_sdce);
	}

	/* calculate the block schedule here */
	compute_extbb(cg->irg);

	list.start = NULL;
	list.end   = NULL;
	list.cnt   = 0;
	irg_extblock_walk_graph(cg->irg, NULL, create_block_list, &list);


	bl_list = NEW_ARR_F(ir_node *, list.cnt);
	cg->bl_list_sdce = new_survive_dce();
	for (i = 0, block = list.start; block; block = get_irn_link(block)) {
		bl_list[i] = block;
		survive_dce_register_irn(cg->bl_list_sdce, &bl_list[i]);
		i++;
	}

	cg->bl_list = bl_list;
}

#if 0
typedef struct _wenv_t {
	ir_node *list;
} wenv_t;

/**
 * Walker: link all CopyB nodes
 */
static void collect_copyb_nodes(ir_node *node, void *env) {
	wenv_t *wenv = env;

	if (get_irn_op(node) == op_CopyB) {
		set_irn_link(node, wenv->list);
		wenv->list = node;
	}
}
#endif

static void replace_copyb_nodes(mips_code_gen_t *cg) {
#if 0
	wenv_t env;
	ir_node *copy, *next;
	ir_node *old_bl, *new_bl, *jmp, *new_jmp, *mem;
	const ir_edge_t *edge;

	/* build code for all copyB */
	env.list = NULL;
	irg_walk_graph(cg->irg, NULL, collect_copyb_nodes, &env);

	for (copy = env.list; copy; copy = next) {
		next = get_irn_link(copy);

		old_bl = get_nodes_block(copy);
		part_block(copy);
		jmp     = get_Block_cfgpred(old_bl, 0);
		new_jmp = new_r_Jmp(cg->irg, get_nodes_block(copy));

		new_bl = new_r_Block(cg->irg, 1, &new_jmp);
		set_nodes_block(jmp, new_bl);

		mem = gen_code_for_CopyB(new_bl, copy);

		/* fix copyB's out edges */
		foreach_out_edge(copy, edge) {
			ir_node *succ = get_edge_src_irn(edge);

			assert(is_Proj(succ));
			switch (get_Proj_proj(succ)) {
			case pn_CopyB_M_regular:
			case pn_CopyB_M_except:
				exchange(succ, mem);
				break;
			default:
				exchange(succ, get_irg_bad(cg->irg));
			}
		}
	}
#endif
	(void) cg;
}

/**
 * Transforms the standard firm graph into
 * a mips firm graph
 */
static void mips_prepare_graph(void *self) {
	mips_code_gen_t *cg = self;
	int bl_nr, n;

	// replace all copyb nodes in the block with a loop
	// and mips store/load nodes
	replace_copyb_nodes(cg);

	// Calculate block schedule
	mips_create_block_sched(cg);

	/* enter the block number into every blocks link field */
	for (bl_nr = 0, n = mips_get_sched_n_blocks(cg); bl_nr < n; ++bl_nr) {
		ir_node *bl = mips_get_sched_block(cg, bl_nr);
		mips_set_block_sched_nr(bl, bl_nr);
	}

	// walk the graph and transform firm nodes into mips nodes where possible
	irg_walk_blkwise_graph(cg->irg, mips_pre_transform_node, mips_transform_node, cg);

	dump_ir_block_graph_sched(cg->irg, "-transformed");
}

/**
 * Called immediately before emit phase.
 */
static void mips_finish_irg(void *self) {
	mips_code_gen_t *cg = self;
	ir_graph        *irg = cg->irg;

	dump_ir_block_graph_sched(irg, "-mips-finished");
}


/**
 * These are some hooks which must be filled but are probably not needed.
 */
static void mips_before_sched(void *self) {
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void mips_before_ra(void *self) {
	/* Some stuff you need to do immediately after register allocation */
}

static void mips_after_ra(void* self) {
	mips_code_gen_t *cg = self;
	be_coalesce_spillslots(cg->birg);
	irg_walk_blkwise_graph(cg->irg, NULL, mips_after_ra_walker, self);
}

/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void mips_emit_and_done(void *self) {
	mips_code_gen_t *cg  = self;
	ir_graph        *irg = cg->irg;

	mips_gen_routine(cg, irg);

	cur_reg_set = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	if (cg->bl_list) {
		DEL_ARR_F(cg->bl_list);
		free_survive_dce(cg->bl_list_sdce);
	}
	free(cg);
}

static void *mips_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t mips_code_gen_if = {
	mips_cg_init,
	NULL,                /* before abi introduce */
	mips_prepare_graph,
	NULL,                /* spill */
	mips_before_sched,   /* before scheduling hook */
	mips_before_ra,      /* before register allocation hook */
	mips_after_ra,
	mips_finish_irg,
	mips_emit_and_done
};

/**
 * Initializes the code generator.
 */
static void *mips_cg_init(be_irg_t *birg) {
	const arch_env_t *arch_env = be_get_birg_arch_env(birg);
	mips_isa_t       *isa      = (mips_isa_t *) arch_env->isa;
	mips_code_gen_t  *cg       = xmalloc(sizeof(*cg));

	cg->impl     = &mips_code_gen_if;
	cg->irg      = be_get_birg_irg(birg);
	cg->reg_set  = new_set(mips_cmp_irn_reg_assoc, 1024);
	cg->arch_env = arch_env;
	cg->isa      = isa;
	cg->birg     = birg;
	cg->bl_list  = NULL;

	cur_reg_set = cg->reg_set;

	mips_irn_ops.cg = cg;

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

static mips_isa_t mips_isa_template = {
	{
		&mips_isa_if,
		&mips_gp_regs[REG_SP],
		&mips_gp_regs[REG_FP],
		-1,		/* stack direction */
		NULL,	/* main environment */
		7,      /* spill costs */
		5,      /* reload costs */
	},
	{ NULL, },  /* emitter environment */
};

/**
 * Initializes the backend ISA and opens the output file.
 */
static void *mips_init(FILE *file_handle) {
	static int inited = 0;
	mips_isa_t *isa;

	if(inited)
		return NULL;
	inited = 1;

	isa = xcalloc(1, sizeof(isa[0]));
	memcpy(isa, &mips_isa_template, sizeof(isa[0]));

	be_emit_init_env(&isa->emit, file_handle);

	mips_register_init(isa);
	mips_create_opcodes();
	// mips_init_opcode_transforms();

	/* we mark referenced global entities, so we can only emit those which
	 * are actually referenced. (Note: you mustn't use the type visited flag
	 * elsewhere in the backend)
	 */
	inc_master_type_visited();

	return isa;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void mips_done(void *self) {
	mips_isa_t *isa = self;

	be_gas_emit_decls(&isa->emit, isa->arch_isa.main_env, 1);

	be_emit_destroy_env(&isa->emit);
	free(isa);
}

static int mips_get_n_reg_class(const void *self) {
	return N_CLASSES;
}

static const arch_register_class_t *mips_get_reg_class(const void *self, int i) {
	assert(i >= 0 && i < N_CLASSES && "Invalid mips register class requested.");
	return &mips_reg_classes[i];
}



/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *mips_get_reg_class_for_mode(const void *self, const ir_mode *mode) {
	ASSERT_NO_FLOAT(mode);
	return &mips_reg_classes[CLASS_mips_gp];
}

typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_isa_t *isa;
	const arch_env_t *arch_env;
	ir_graph *irg;
	// do special handling to support debuggers
	int debug;
} mips_abi_env_t;

static void *mips_abi_init(const be_abi_call_t *call, const arch_env_t *arch_env, ir_graph *irg)
{
	mips_abi_env_t *env    = xmalloc(sizeof(env[0]));
	be_abi_call_flags_t fl = be_abi_call_get_flags(call);
	env->flags             = fl.bits;
	env->irg               = irg;
	env->arch_env          = arch_env;
	env->isa               = arch_env->isa;
	env->debug             = 1;
	return env;
}

static void mips_abi_dont_save_regs(void *self, pset *s)
{
	mips_abi_env_t *env = self;

	if(env->flags.try_omit_fp)
		pset_insert_ptr(s, env->isa->bp);
}

static const arch_register_t *mips_abi_prologue(void *self, ir_node** mem, pmap *reg_map)
{
	mips_abi_env_t *env = self;
	ir_graph *irg = env->irg;
	dbg_info *dbg = NULL; // TODO where can I get this from?
	ir_node *block = get_irg_start_block(env->irg);
	mips_attr_t *attr;
	ir_node *sp = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_SP]);
	ir_node *fp = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_FP]);
	int initialstackframesize;

	if(env->debug) {
		/*
		 * The calling conventions wants a stack frame of at least 24bytes size with
		 *   a0-a3 saved in offset 0-12
		 *   fp saved in offset 16
		 *   ra saved in offset 20
		 */
		ir_node *mm[6];
		ir_node *sync, *reg, *store;
		initialstackframesize = 24;

		// - setup first part of stackframe
		sp = new_rd_mips_addiu(dbg, irg, block, sp);
		attr = get_mips_attr(sp);
		attr->tv = new_tarval_from_long(-initialstackframesize, mode_Is);
		mips_set_irn_reg(NULL, sp, &mips_gp_regs[REG_SP]);
		//arch_set_irn_register(mips_get_arg_env(), sp, &mips_gp_regs[REG_SP]);

		/* TODO: where to get an edge with a0-a3
		int i;
		for(i = 0; i < 4; ++i) {
			ir_node *reg = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_A0 + i]);
			ir_node *store = new_rd_mips_store_r(dbg, irg, block, *mem, sp, reg, mode_T);
			attr = get_mips_attr(store);
			attr->load_store_mode = mode_Iu;
			attr->tv = new_tarval_from_long(i * 4, mode_Is);

			mm[i] = new_r_Proj(irg, block, store, mode_M, pn_Store_M);
		}
		*/

		reg = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_FP]);
		store = new_rd_mips_sw(dbg, irg, block, *mem, sp, reg);
		attr = get_mips_attr(store);
		attr->tv = new_tarval_from_long(16, mode_Hs);

		mm[4] = store;

		reg = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_RA]);
		store = new_rd_mips_sw(dbg, irg, block, *mem, sp, reg);
		attr = get_mips_attr(store);
		attr->tv = new_tarval_from_long(20, mode_Hs);

		mm[5] = store;

		// TODO ideally we would route these mem edges directly towards the epilogue
		sync = new_r_Sync(irg, block, 2, mm+4);
		*mem = sync;
	} else {
		ir_node *reg, *store;
		initialstackframesize = 4;

		// save old framepointer
		sp = new_rd_mips_addiu(dbg, irg, block, sp);
		attr = get_mips_attr(sp);
		attr->tv = new_tarval_from_long(-initialstackframesize, mode_Is);
		mips_set_irn_reg(NULL, sp, &mips_gp_regs[REG_SP]);
		//arch_set_irn_register(mips_get_arg_env(), sp, &mips_gp_regs[REG_SP]);

		reg = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_FP]);
		store = new_rd_mips_sw(dbg, irg, block, *mem, sp, reg);
		attr = get_mips_attr(store);
		attr->tv = new_tarval_from_long(0, mode_Hs);

		*mem = store;
	}

	// setup framepointer
	fp = new_rd_mips_addiu(dbg, irg, block, sp);
	attr = get_mips_attr(fp);
	attr->tv = new_tarval_from_long(initialstackframesize, mode_Is);
	mips_set_irn_reg(NULL, fp, &mips_gp_regs[REG_FP]);
	//arch_set_irn_register(mips_get_arg_env(), fp, &mips_gp_regs[REG_FP]);

	be_abi_reg_map_set(reg_map, &mips_gp_regs[REG_FP], fp);
	be_abi_reg_map_set(reg_map, &mips_gp_regs[REG_SP], sp);

	return &mips_gp_regs[REG_SP];
}

static void mips_abi_epilogue(void *self, ir_node *block, ir_node **mem, pmap *reg_map)
{
	mips_abi_env_t *env = self;
	ir_graph *irg = env->irg;
	dbg_info *dbg = NULL; // TODO where can I get this from?
	mips_attr_t *attr;
	ir_node *sp = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_SP]);
	ir_node *fp = be_abi_reg_map_get(reg_map, &mips_gp_regs[REG_FP]);
	ir_node *load;
	int initial_frame_size = env->debug ? 24 : 4;
	int fp_save_offset = env->debug ? 16 : 0;

	// copy fp to sp
	sp = new_rd_mips_move(dbg, irg, block, fp);
	mips_set_irn_reg(NULL, sp, &mips_gp_regs[REG_SP]);
	//arch_set_irn_register(mips_get_arg_env(), fp, &mips_gp_regs[REG_SP]);

	// 1. restore fp
	load = new_rd_mips_lw(dbg, irg, block, *mem, sp);
	attr = get_mips_attr(load);
	// sp is at the fp address already, so we have to do fp_save_offset - initial_frame_size
	attr->tv = new_tarval_from_long(fp_save_offset - initial_frame_size, mode_Hs);

	fp = new_r_Proj(irg, block, load, mode_Iu, pn_mips_lw_res);
	*mem = new_r_Proj(irg, block, load, mode_Iu, pn_mips_lw_M);
	arch_set_irn_register(env->arch_env, fp, &mips_gp_regs[REG_FP]);

	be_abi_reg_map_set(reg_map, &mips_gp_regs[REG_FP], fp);
	be_abi_reg_map_set(reg_map, &mips_gp_regs[REG_SP], sp);
}

/**
 * Produces the type which sits between the stack args and the locals on the stack.
 * it will contain the return address and space to store the old frame pointer.
 * @return The Firm type modelling the ABI between type.
 */
static ir_type *mips_abi_get_between_type(void *self) {
	mips_abi_env_t *env = self;

	static ir_type *debug_between_type = NULL;
	static ir_type *opt_between_type = NULL;
	static ir_entity *old_fp_ent    = NULL;

	if(env->debug && debug_between_type == NULL) {
		ir_entity *a0_ent, *a1_ent, *a2_ent, *a3_ent;
		ir_entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);
		ir_type *old_fp_type   = new_type_primitive(new_id_from_str("fp"), mode_P);
		ir_type *old_param_type = new_type_primitive(new_id_from_str("param"), mode_Iu);

		debug_between_type     = new_type_class(new_id_from_str("mips_between_type"));
		a0_ent				   = new_entity(debug_between_type, new_id_from_str("a0_ent"), old_param_type);
		a1_ent				   = new_entity(debug_between_type, new_id_from_str("a1_ent"), old_param_type);
		a2_ent				   = new_entity(debug_between_type, new_id_from_str("a2_ent"), old_param_type);
		a3_ent				   = new_entity(debug_between_type, new_id_from_str("a3_ent"), old_param_type);
		old_fp_ent             = new_entity(debug_between_type, new_id_from_str("old_fp"), old_fp_type);
		ret_addr_ent           = new_entity(debug_between_type, new_id_from_str("ret_addr"), ret_addr_type);

		set_entity_offset(a0_ent, 0);
		set_entity_offset(a1_ent, 4);
		set_entity_offset(a2_ent, 8);
		set_entity_offset(a3_ent, 12);
		set_entity_offset(old_fp_ent, 16);
		set_entity_offset(ret_addr_ent, 20);

		set_type_size_bytes(debug_between_type, 24);
	} else if(!env->debug && opt_between_type == NULL) {
		ir_type *old_fp_type   = new_type_primitive(new_id_from_str("fp"), mode_P);
		ir_entity *old_fp_ent;

		opt_between_type       = new_type_class(new_id_from_str("mips_between_type"));
		old_fp_ent             = new_entity(opt_between_type, new_id_from_str("old_fp"), old_fp_type);
		set_entity_offset(old_fp_ent, 0);
		set_type_size_bytes(opt_between_type, 4);
	}

	return env->debug ? debug_between_type : opt_between_type;
}

static const be_abi_callbacks_t mips_abi_callbacks = {
	mips_abi_init,
	free,
	mips_abi_get_between_type,
	mips_abi_dont_save_regs,
	mips_abi_prologue,
	mips_abi_epilogue,
};

/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
static void mips_get_call_abi(const void *self, ir_type *method_type, be_abi_call_t *abi) {
	ir_type  *tp;
	ir_mode  *mode;
	int       n = get_method_n_params(method_type);
	int result_count;
	int       i;
	ir_mode **modes;
	const arch_register_t *reg;
	be_abi_call_flags_t call_flags;

	memset(&call_flags, 0, sizeof(call_flags));
	call_flags.bits.left_to_right         = 0;
	call_flags.bits.store_args_sequential = 0;
	call_flags.bits.try_omit_fp           = 1;
	call_flags.bits.fp_free               = 0;
	call_flags.bits.call_has_imm          = 1;

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, call_flags, &mips_abi_callbacks);

	/* collect the mode for each type */
	modes = alloca(n * sizeof(modes[0]));
	for (i = 0; i < n; i++) {
		tp       = get_method_param_type(method_type, i);
		modes[i] = get_type_mode(tp);
	}

	// assigns parameters to registers or stack
	for (i = 0; i < n; i++) {
		// first 4 params in $a0-$a3, the others on the stack
		if(i < 4) {
			reg = &mips_gp_regs[REG_A0 + i];
			be_abi_call_param_reg(abi, i, reg);
		} else {
			/* default: all parameters on stack */
			be_abi_call_param_stack(abi, i, 4, 0, 0);
		}
	}

	/* set return register */
	/* default: return value is in R0 (and maybe R1) */
	result_count = get_method_n_ress(method_type);
	assert(result_count <= 2 && "More than 2 result values not supported");
	for(i = 0; i < result_count; ++i) {
		const arch_register_t* reg;
		tp   = get_method_res_type(method_type, i);
		mode = get_type_mode(tp);
		ASSERT_NO_FLOAT(mode);

		reg = &mips_gp_regs[REG_V0 + i];
		be_abi_call_res_reg(abi, i, reg);
	}
}

static const void *mips_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn) {
	return &mips_irn_ops;
}

const arch_irn_handler_t mips_irn_handler = {
	mips_get_irn_ops
};

const arch_irn_handler_t *mips_get_irn_handler(const void *self) {
	return &mips_irn_handler;
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *mips_get_code_generator_if(void *self) {
	return &mips_code_gen_if;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int mips_get_reg_class_alignment(const void *self, const arch_register_class_t *cls) {
	ir_mode *mode = arch_register_class_mode(cls);
	return get_mode_size_bytes(mode);
}

static const be_execution_unit_t ***mips_get_allowed_execution_units(const void *self, const ir_node *irn) {
	/* TODO */
	assert(0);
	return NULL;
}

static const be_machine_t *mips_get_machine(const void *self) {
	/* TODO */
	assert(0);
	return NULL;
}

/**
 * Return irp irgs in the desired order.
 */
static ir_graph **mips_get_irg_list(const void *self, ir_graph ***irg_list) {
	return NULL;
}

/**
 * Called by the frontend to encode a register name into a backend specific way
 */
static unsigned mips_register_from_name(const char *regname) {
	/* NYI */
	return 0;
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *mips_get_libfirm_params(void) {
	static arch_dep_params_t ad = {
		1,  /* allow subs */
		0,	/* Muls are fast enough on Mips */
		31, /* shift would be ok */
		0,  /* no Mulhs */
		0,  /* no Mulhu */
		32, /* Mulhs & Mulhu available for 32 bit */
	};
	static backend_params p = {
		1,     /* need dword lowering */
		0,     /* don't support inlien assembler yet */
		NULL,  /* no additional opcodes */
		NULL,  /* will be set later */
		NULL,  /* but yet no creator function */
		NULL,  /* context for create_intrinsic_fkt */
		mips_register_from_name, /* register names */
	};

	p.dep_param = &ad;
	return &p;
}

const arch_isa_if_t mips_isa_if = {
	mips_init,
	mips_done,
	mips_get_n_reg_class,
	mips_get_reg_class,
	mips_get_reg_class_for_mode,
	mips_get_call_abi,
	mips_get_irn_handler,
	mips_get_code_generator_if,
	mips_get_list_sched_selector,
	mips_get_ilp_sched_selector,
	mips_get_reg_class_alignment,
	mips_get_libfirm_params,
	mips_get_allowed_execution_units,
	mips_get_machine,
	mips_get_irg_list,
};

void be_init_arch_mips(void)
{
	be_register_isa_if("mips", &mips_isa_if);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_mips);
