/* The main arm backend driver file. */
/* $Id$ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#include "pseudo_irg.h"
#include "irgwalk.h"
#include "irprog.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "lower_intrinsics.h"

#include "bitset.h"
#include "debug.h"

#include "../bearch_t.h"                /* the general register allocator interface */
#include "../benode_t.h"
#include "../belower.h"
#include "../besched_t.h"
#include "../be.h"
#include "../beabi.h"
#include "../bemachine.h"
#include "../beilpsched.h"
#include "../bemodule.h"
#include "../beirg_t.h"

#include "bearch_arm_t.h"

#include "arm_new_nodes.h"           /* arm nodes interface */
#include "gen_arm_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "arm_gen_decls.h"           /* interface declaration emitter */
#include "arm_transform.h"
#include "arm_emitter.h"
#include "arm_map_regs.h"

#define DEBUG_MODULE "firm.be.arm.isa"

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
 * Return register requirements for a arm node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const
arch_register_req_t *arm_get_irn_reg_req(const void *self, const ir_node *node,
                                         int pos) {
	long               node_pos = pos == -1 ? 0 : pos;
	ir_mode           *mode     = get_irn_mode(node);
	FIRM_DBG_REGISTER(firm_dbg_module_t *mod, DEBUG_MODULE);

	if (is_Block(node) || mode == mode_X || mode == mode_M) {
		DBG((mod, LEVEL_1, "ignoring mode_T, mode_M node %+F\n", node));
		return arch_no_register_req;
	}

	if (mode == mode_T && pos < 0) {
		DBG((mod, LEVEL_1, "ignoring request for OUT requirements at %+F\n", node));
		return arch_no_register_req;
	}

	DBG((mod, LEVEL_1, "get requirements at pos %d for %+F ... ", pos, node));

	if (is_Proj(node)) {
		/* in case of a proj, we need to get the correct OUT slot */
		/* of the node corresponding to the proj number */
		if (pos == -1) {
			node_pos = arm_translate_proj_pos(node);
		}
		else {
			node_pos = pos;
		}

		node = skip_Proj_const(node);

		DB((mod, LEVEL_1, "skipping Proj, going to %+F at pos %d ... ", node, node_pos));
	}

	/* get requirements for our own nodes */
	if (is_arm_irn(node)) {
		const arch_register_req_t *req;
		if (pos >= 0) {
			req = get_arm_in_req(node, pos);
		} else {
			req = get_arm_out_req(node, node_pos);
		}

		DB((mod, LEVEL_1, "returning reqs for %+F at pos %d\n", node, pos));
		return req;
	}

	/* unknown should be tranformed by now */
	assert(!is_Unknown(node));
	DB((mod, LEVEL_1, "returning NULL for %+F (node not supported)\n", node));

	return arch_no_register_req;
}

static void arm_set_irn_reg(const void *self, ir_node *irn, const arch_register_t *reg) {
	int pos = 0;

	if (is_Proj(irn)) {

		if (get_irn_mode(irn) == mode_X) {
			return;
		}

		pos = arm_translate_proj_pos(irn);
		irn = skip_Proj(irn);
	}

	if (is_arm_irn(irn)) {
		const arch_register_t **slots;

		slots      = get_arm_slots(irn);
		slots[pos] = reg;
	}
	else {
		/* here we set the registers for the Phi nodes */
		arm_set_firm_reg(irn, reg, cur_reg_set);
	}
}

static const arch_register_t *arm_get_irn_reg(const void *self, const ir_node *irn) {
	int pos = 0;
	const arch_register_t *reg = NULL;

	if (is_Proj(irn)) {

		if (get_irn_mode(irn) == mode_X) {
			return NULL;
		}

		pos = arm_translate_proj_pos(irn);
		irn = skip_Proj_const(irn);
	}

	if (is_arm_irn(irn)) {
		const arch_register_t **slots;
		slots = get_arm_slots(irn);
		reg   = slots[pos];
	}
	else {
		reg = arm_get_firm_reg(irn, cur_reg_set);
	}

	return reg;
}

static arch_irn_class_t arm_classify(const void *self, const ir_node *irn) {
	irn = skip_Proj_const(irn);

	if (is_cfop(irn)) {
		return arch_irn_class_branch;
	}
	else if (is_arm_irn(irn)) {
		return arch_irn_class_normal;
	}

	return 0;
}

static arch_irn_flags_t arm_get_flags(const void *self, const ir_node *irn) {
	irn = skip_Proj_const(irn);

	if (is_arm_irn(irn)) {
		return get_arm_flags(irn);
	}
	else if (is_Unknown(irn)) {
		return arch_irn_flags_ignore;
	}

	return 0;
}

static ir_entity *arm_get_frame_entity(const void *self, const ir_node *irn) {
	/* TODO: return the entity assigned to the frame */
	return NULL;
}

static void arm_set_frame_entity(const void *self, ir_node *irn, ir_entity *ent) {
	/* TODO: set the entity assigned to the frame */
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void arm_set_stack_bias(const void *self, ir_node *irn, int bias) {
	/* TODO: correct offset if irn accesses the stack */
}

static int arm_get_sp_bias(const void *self, const ir_node *irn) {
	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_if_t arm_irn_ops_if = {
	arm_get_irn_reg_req,
	arm_set_irn_reg,
	arm_get_irn_reg,
	arm_classify,
	arm_get_flags,
	arm_get_frame_entity,
	arm_set_frame_entity,
	arm_set_stack_bias,
	arm_get_sp_bias,
	NULL,    /* get_inverse             */
	NULL,    /* get_op_estimated_cost   */
	NULL,    /* possible_memory_operand */
	NULL,    /* perform_memory_operand  */
};

arm_irn_ops_t arm_irn_ops = {
	&arm_irn_ops_if,
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

/**
 * Transforms the standard Firm graph into
 * a ARM firm graph.
 */
static void arm_prepare_graph(void *self) {
	arm_code_gen_t *cg = self;

	arm_register_transformers();
	irg_walk_blkwise_graph(cg->irg, arm_move_consts, arm_transform_node, cg);
}



/**
 * Called immediately before emit phase.
 */
static void arm_finish_irg(void *self) {
	/* TODO: - fix offsets for nodes accessing stack
			 - ...
	*/
}


/**
 * These are some hooks which must be filled but are probably not needed.
 */
static void arm_before_sched(void *self) {
	/* Some stuff you need to do after scheduling but before register allocation */
}

static void arm_before_ra(void *self) {
	/* Some stuff you need to do immediately after register allocation */
}


/**
 * Emits the code, closes the output file and frees
 * the code generator interface.
 */
static void arm_emit_and_done(void *self) {
	arm_code_gen_t *cg = self;
	ir_graph           *irg = cg->irg;
	FILE               *out = cg->isa->out;

	if (cg->emit_decls) {
		arm_gen_decls(out);
		cg->emit_decls = 0;
	}

	dump_ir_block_graph_sched(irg, "-arm-finished");
	arm_gen_routine(out, irg, cg);

	cur_reg_set = NULL;

	/* de-allocate code generator */
	del_set(cg->reg_set);
	free(self);
}

/**
 * Move a double floating point value into an integer register.
 * Place the move operation into block bl.
 *
 * Handle some special cases here:
 * 1.) A constant: simply split into two
 * 2.) A load: siply split into two
 */
static ir_node *convert_dbl_to_int(ir_node *bl, ir_node *arg, ir_node *mem,
                                   ir_node **resH, ir_node **resL) {
	if (is_Const(arg)) {
		tarval *tv = get_Const_tarval(arg);
		unsigned v;

		/* get the upper 32 bits */
		v =            get_tarval_sub_bits(tv, 7);
		v = (v << 8) | get_tarval_sub_bits(tv, 6);
		v = (v << 8) | get_tarval_sub_bits(tv, 5);
		v = (v << 8) | get_tarval_sub_bits(tv, 4);
		*resH = new_Const_long(mode_Is, v);

		/* get the lower 32 bits */
		v =            get_tarval_sub_bits(tv, 3);
		v = (v << 8) | get_tarval_sub_bits(tv, 2);
		v = (v << 8) | get_tarval_sub_bits(tv, 1);
		v = (v << 8) | get_tarval_sub_bits(tv, 0);
		*resL = new_Const_long(mode_Is, v);
	}
	else if (get_irn_op(skip_Proj(arg)) == op_Load) {
		/* FIXME: handling of low/high depends on LE/BE here */
		assert(0);
	}
	else {
		ir_graph *irg = current_ir_graph;
		ir_node *conv;

		conv = new_rd_arm_fpaDbl2GP(NULL, irg, bl, arg, mem);
		/* move high/low */
		*resL = new_r_Proj(irg, bl, conv, mode_Is, pn_arm_fpaDbl2GP_low);
		*resH = new_r_Proj(irg, bl, conv, mode_Is, pn_arm_fpaDbl2GP_high);
		mem   = new_r_Proj(irg, bl, conv, mode_M,  pn_arm_fpaDbl2GP_M);
	}
	return mem;
}

/**
 * Move a single floating point value into an integer register.
 * Place the move operation into block bl.
 *
 * Handle some special cases here:
 * 1.) A constant: simply move
 * 2.) A load: siply load
 */
static ir_node *convert_sng_to_int(ir_node *bl, ir_node *arg) {
	if (is_Const(arg)) {
		tarval *tv = get_Const_tarval(arg);
		unsigned v;

		/* get the lower 32 bits */
		v =            get_tarval_sub_bits(tv, 3);
		v = (v << 8) | get_tarval_sub_bits(tv, 2);
		v = (v << 8) | get_tarval_sub_bits(tv, 1);
		v = (v << 8) | get_tarval_sub_bits(tv, 0);
		return new_Const_long(mode_Is, v);
	}
	else if (get_irn_op(skip_Proj(arg)) == op_Load) {
		ir_node *load;

		load = skip_Proj(arg);
	}
	assert(0);
	return NULL;
}

/**
 * Convert the arguments of a call to support the
 * ARM calling convention of general purpose AND floating
 * point arguments.
 */
static void handle_calls(ir_node *call, void *env)
{
	arm_code_gen_t *cg = env;
	int i, j, n, size, idx, flag, n_param, n_res;
	ir_type *mtp, *new_mtd, *new_tp[5];
	ir_node *new_in[5], **in;
	ir_node *bl;

	if (! is_Call(call))
		return;

	/* check, if we need conversions */
	n = get_Call_n_params(call);
	mtp = get_Call_type(call);
	assert(get_method_n_params(mtp) == n);

	/* it's always enough to handle the first 4 parameters */
	if (n > 4)
		n = 4;
	flag = size = idx = 0;
	bl = get_nodes_block(call);
	for (i = 0; i < n; ++i) {
		ir_type *param_tp = get_method_param_type(mtp, i);

		if (is_compound_type(param_tp)) {
			/* an aggregate parameter: bad case */
			assert(0);
		}
		else {
			/* a primitive parameter */
			ir_mode *mode = get_type_mode(param_tp);

			if (mode_is_float(mode)) {
				if (get_mode_size_bits(mode) > 32) {
					ir_node *mem = get_Call_mem(call);

					/* Beware: ARM wants the high part first */
					size += 2 * 4;
					new_tp[idx]   = cg->int_tp;
					new_tp[idx+1] = cg->int_tp;
					mem = convert_dbl_to_int(bl, get_Call_param(call, i), mem, &new_in[idx], &new_in[idx+1]);
					idx += 2;
					set_Call_mem(call, mem);
				}
				else {
					size += 4;
					new_tp[idx] = cg->int_tp;
					new_in[idx] = convert_sng_to_int(bl, get_Call_param(call, i));
					++idx;
				}
				flag = 1;
			}
			else {
				size += 4;
				new_tp[idx] = param_tp;
				new_in[idx] = get_Call_param(call, i);
				++idx;
			}
		}

		if (size >= 16)
			break;
	}

	/* if flag is NOT set, no need to translate the method type */
	if (! flag)
		return;

	/* construct a new method type */
	n       = i;
	n_param = get_method_n_params(mtp) - n + idx;
	n_res   = get_method_n_ress(mtp);
	new_mtd = new_d_type_method(get_type_ident(mtp), n_param, n_res, get_type_dbg_info(mtp));

	for (i = 0; i < idx; ++i)
		set_method_param_type(new_mtd, i, new_tp[i]);
	for (i = n, j = idx; i < get_method_n_params(mtp); ++i)
		set_method_param_type(new_mtd, j++, get_method_param_type(mtp, i));
	for (i = 0; i < n_res; ++i)
		set_method_res_type(new_mtd, i, get_method_res_type(mtp, i));

	set_method_calling_convention(new_mtd, get_method_calling_convention(mtp));
	set_method_first_variadic_param_index(new_mtd, get_method_first_variadic_param_index(mtp));

	if (is_lowered_type(mtp)) {
		mtp = get_associated_type(mtp);
	}
	set_lowered_type(mtp, new_mtd);

	set_Call_type(call, new_mtd);

	/* calculate new in array of the Call */
	NEW_ARR_A(ir_node *, in, n_param + 2);
	for (i = 0; i < idx; ++i)
		in[2 + i] = new_in[i];
	for (i = n, j = idx; i < get_method_n_params(mtp); ++i)
		in[2 + j++] = get_Call_param(call, i);

	in[0] = get_Call_mem(call);
	in[1] = get_Call_ptr(call);

	/* finally, change the call inputs */
	set_irn_in(call, n_param + 2, in);
}

/**
 * Handle graph transformations before the abi converter does its work.
 */
static void arm_before_abi(void *self) {
	arm_code_gen_t *cg = self;

	irg_walk_graph(cg->irg, NULL, handle_calls, cg);
}

static void *arm_cg_init(be_irg_t *birg);

static const arch_code_generator_if_t arm_code_gen_if = {
	arm_cg_init,
	arm_before_abi,     /* before abi introduce */
	arm_prepare_graph,
	NULL,               /* spill */
	arm_before_sched,   /* before scheduling hook */
	arm_before_ra,      /* before register allocation hook */
	NULL,               /* after register allocation */
	arm_finish_irg,
	arm_emit_and_done,
};

/**
 * Initializes the code generator.
 */
static void *arm_cg_init(be_irg_t *birg) {
	static ir_type *int_tp = NULL;
	arm_isa_t      *isa = (arm_isa_t *)birg->main_env->arch_env->isa;
	arm_code_gen_t *cg;

	if (! int_tp) {
		/* create an integer type with machine size */
		int_tp = new_type_primitive(new_id_from_chars("int", 3), mode_Is);
	}

	cg = xmalloc(sizeof(*cg));
	cg->impl     = &arm_code_gen_if;
	cg->irg      = birg->irg;
	cg->reg_set  = new_set(arm_cmp_irn_reg_assoc, 1024);
	cg->arch_env = birg->main_env->arch_env;
	cg->isa      = isa;
	cg->birg     = birg;
	cg->int_tp   = int_tp;
	cg->have_fp  = 0;

	FIRM_DBG_REGISTER(cg->mod, "firm.be.arm.cg");

	isa->num_codegens++;

	if (isa->num_codegens > 1)
		cg->emit_decls = 0;
	else
		cg->emit_decls = 1;

	cur_reg_set = cg->reg_set;

	arm_irn_ops.cg = cg;

	/* enter the current code generator */
	isa->cg = cg;

	return (arch_code_generator_t *)cg;
}


/**
 * Maps all intrinsic calls that the backend support
 * and map all instructions the backend did not support
 * to runtime calls.
 */
static void arm_handle_intrinsics(void) {
  ir_type *tp, *int_tp, *uint_tp;
  i_record records[8];
  int n_records = 0;

#define ID(x) new_id_from_chars(x, sizeof(x)-1)

  int_tp  = new_type_primitive(ID("int"), mode_Is);
  uint_tp = new_type_primitive(ID("uint"), mode_Iu);

	/* ARM has neither a signed div instruction ... */
  {
    runtime_rt rt_Div;
    i_instr_record *map_Div = &records[n_records++].i_instr;

    tp = new_type_method(ID("rt_iDiv"), 2, 1);
    set_method_param_type(tp, 0, int_tp);
    set_method_param_type(tp, 1, int_tp);
    set_method_res_type(tp, 0, int_tp);

    rt_Div.ent             = new_entity(get_glob_type(), ID("__divsi3"), tp);
    rt_Div.mode            = mode_T;
    rt_Div.mem_proj_nr     = pn_Div_M;
    rt_Div.exc_proj_nr     = pn_Div_X_except;
    rt_Div.exc_mem_proj_nr = pn_Div_M;
    rt_Div.res_proj_nr     = pn_Div_res;

    set_entity_visibility(rt_Div.ent, visibility_external_allocated);

    map_Div->kind     = INTRINSIC_INSTR;
    map_Div->op       = op_Div;
    map_Div->i_mapper = (i_mapper_func)i_mapper_RuntimeCall;
    map_Div->ctx      = &rt_Div;
  }
	/* ... nor a signed div instruction ... */
  {
    runtime_rt rt_Div;
    i_instr_record *map_Div = &records[n_records++].i_instr;

    tp = new_type_method(ID("rt_uDiv"), 2, 1);
    set_method_param_type(tp, 0, uint_tp);
    set_method_param_type(tp, 1, uint_tp);
    set_method_res_type(tp, 0, uint_tp);

    rt_Div.ent             = new_entity(get_glob_type(), ID("__udivsi3"), tp);
    rt_Div.mode            = mode_T;
    rt_Div.mem_proj_nr     = pn_Div_M;
    rt_Div.exc_proj_nr     = pn_Div_X_except;
    rt_Div.exc_mem_proj_nr = pn_Div_M;
    rt_Div.res_proj_nr     = pn_Div_res;

    set_entity_visibility(rt_Div.ent, visibility_external_allocated);

    map_Div->kind     = INTRINSIC_INSTR;
    map_Div->op       = op_Div;
    map_Div->i_mapper = (i_mapper_func)i_mapper_RuntimeCall;
    map_Div->ctx      = &rt_Div;
  }
	/* ... nor a signed mod instruction ... */
  {
    runtime_rt rt_Mod;
    i_instr_record *map_Mod = &records[n_records++].i_instr;

    tp = new_type_method(ID("rt_iMod"), 2, 1);
    set_method_param_type(tp, 0, int_tp);
    set_method_param_type(tp, 1, int_tp);
    set_method_res_type(tp, 0, int_tp);

    rt_Mod.ent             = new_entity(get_glob_type(), ID("__modsi3"), tp);
    rt_Mod.mode            = mode_T;
    rt_Mod.mem_proj_nr     = pn_Mod_M;
    rt_Mod.exc_proj_nr     = pn_Mod_X_except;
    rt_Mod.exc_mem_proj_nr = pn_Mod_M;
    rt_Mod.res_proj_nr     = pn_Mod_res;

    set_entity_visibility(rt_Mod.ent, visibility_external_allocated);

    map_Mod->kind     = INTRINSIC_INSTR;
    map_Mod->op       = op_Mod;
    map_Mod->i_mapper = (i_mapper_func)i_mapper_RuntimeCall;
    map_Mod->ctx      = &rt_Mod;
  }
	/* ... nor a unsigned mod. */
  {
    runtime_rt rt_Mod;
    i_instr_record *map_Mod = &records[n_records++].i_instr;

    tp = new_type_method(ID("rt_uMod"), 2, 1);
    set_method_param_type(tp, 0, uint_tp);
    set_method_param_type(tp, 1, uint_tp);
    set_method_res_type(tp, 0, uint_tp);

    rt_Mod.ent             = new_entity(get_glob_type(), ID("__umodsi3"), tp);
    rt_Mod.mode            = mode_T;
    rt_Mod.mem_proj_nr     = pn_Mod_M;
    rt_Mod.exc_proj_nr     = pn_Mod_X_except;
    rt_Mod.exc_mem_proj_nr = pn_Mod_M;
    rt_Mod.res_proj_nr     = pn_Mod_res;

    set_entity_visibility(rt_Mod.ent, visibility_external_allocated);

    map_Mod->kind     = INTRINSIC_INSTR;
    map_Mod->op       = op_Mod;
    map_Mod->i_mapper = (i_mapper_func)i_mapper_RuntimeCall;
    map_Mod->ctx      = &rt_Mod;
  }

  if (n_records > 0)
    lower_intrinsics(records, n_records);
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

static arm_isa_t arm_isa_template = {
	&arm_isa_if,           /* isa interface */
	&arm_gp_regs[REG_SP],  /* stack pointer */
	&arm_gp_regs[REG_R11], /* base pointer */
	-1,                    /* stack direction */
	0,                     /* number of codegenerator objects */
	0,                     /* use generic register names instead of SP, LR, PC */
	NULL,                  /* current code generator */
	NULL,                  /* output file */
	ARM_FPU_ARCH_FPE,      /* FPU architecture */
};

/**
 * Initializes the backend ISA and opens the output file.
 */
static void *arm_init(FILE *file_handle) {
	static int inited = 0;
	arm_isa_t *isa;

	if(inited)
		return NULL;

	isa = xmalloc(sizeof(*isa));
	memcpy(isa, &arm_isa_template, sizeof(*isa));

	arm_register_init(isa);

	isa->cg  = NULL;
	isa->out = file_handle;

	arm_create_opcodes();
	arm_handle_intrinsics();
	arm_switch_section(NULL, NO_SECTION);

	inited = 1;
	return isa;
}



/**
 * frees the ISA structure.
 */
static void arm_done(void *self) {
	free(self);
}


/**
 * Report the number of register classes.
 * If we don't have fp instructions, report only GP
 * here to speed up register allocation (and makes dumps
 * smaller and more readable).
 */
static int arm_get_n_reg_class(const void *self) {
	const arm_isa_t *isa = self;

	return isa->cg->have_fp ? 2 : 1;
}

/**
 * Return the register class with requested index.
 */
static const arch_register_class_t *arm_get_reg_class(const void *self, int i) {
	return i == 0 ? &arm_reg_classes[CLASS_arm_gp] : &arm_reg_classes[CLASS_arm_fpa];
}

/**
 * Get the register class which shall be used to store a value of a given mode.
 * @param self The this pointer.
 * @param mode The mode in question.
 * @return A register class which can hold values of the given mode.
 */
const arch_register_class_t *arm_get_reg_class_for_mode(const void *self, const ir_mode *mode) {
	if (mode_is_float(mode))
		return &arm_reg_classes[CLASS_arm_fpa];
	else
		return &arm_reg_classes[CLASS_arm_gp];
}

/**
 * Produces the type which sits between the stack args and the locals on the stack.
 * it will contain the return address and space to store the old base pointer.
 * @return The Firm type modelling the ABI between type.
 */
static ir_type *arm_get_between_type(void *self) {
	static ir_type *between_type = NULL;
	static ir_entity *old_bp_ent = NULL;

	if(!between_type) {
		ir_entity *ret_addr_ent;
		ir_type *ret_addr_type = new_type_primitive(new_id_from_str("return_addr"), mode_P);
		ir_type *old_bp_type   = new_type_primitive(new_id_from_str("bp"), mode_P);

		between_type           = new_type_class(new_id_from_str("arm_between_type"));
		old_bp_ent             = new_entity(between_type, new_id_from_str("old_bp"), old_bp_type);
		ret_addr_ent           = new_entity(between_type, new_id_from_str("old_bp"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size_bytes(old_bp_type));
		set_type_size_bytes(between_type, get_type_size_bytes(old_bp_type) + get_type_size_bytes(ret_addr_type));
	}

	return between_type;
}


typedef struct {
	be_abi_call_flags_bits_t flags;
	const arch_env_t *arch_env;
	const arch_isa_t *isa;
	ir_graph *irg;
} arm_abi_env_t;

static void *arm_abi_init(const be_abi_call_t *call, const arch_env_t *arch_env, ir_graph *irg)
{
	arm_abi_env_t *env     = xmalloc(sizeof(env[0]));
	be_abi_call_flags_t fl = be_abi_call_get_flags(call);
	env->flags    = fl.bits;
	env->irg      = irg;
	env->arch_env = arch_env;
	env->isa      = arch_env->isa;
	return env;
}

static void arm_abi_dont_save_regs(void *self, pset *s)
{
	arm_abi_env_t *env = self;
	if (env->flags.try_omit_fp)
		pset_insert_ptr(s, env->isa->bp);
}



/**
 * Build the ARM prolog
 */
static const arch_register_t *arm_abi_prologue(void *self, ir_node **mem, pmap *reg_map) {
	ir_node *keep, *store;
	arm_abi_env_t *env = self;
	ir_graph *irg = env->irg;
	ir_node *block = get_irg_start_block(irg);
//	ir_node *regs[16];
//	int n_regs = 0;
	arch_register_class_t *gp = &arm_reg_classes[CLASS_arm_gp];

	ir_node *fp = be_abi_reg_map_get(reg_map, env->isa->bp);
	ir_node *ip = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_R12]);
	ir_node *sp = be_abi_reg_map_get(reg_map, env->isa->sp);
	ir_node *lr = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_LR]);
	ir_node *pc = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_PC]);
// 	ir_node *r0 = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_R0]);
// 	ir_node *r1 = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_R1]);
// 	ir_node *r2 = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_R2]);
// 	ir_node *r3 = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_R3]);

	if(env->flags.try_omit_fp)
		return env->isa->sp;

	ip = be_new_Copy(gp, irg, block, sp );
	arch_set_irn_register(env->arch_env, ip, &arm_gp_regs[REG_R12]);
	be_set_constr_single_reg(ip, BE_OUT_POS(0), &arm_gp_regs[REG_R12] );

//	if (r0) regs[n_regs++] = r0;
//	if (r1) regs[n_regs++] = r1;
//	if (r2) regs[n_regs++] = r2;
//	if (r3) regs[n_regs++] = r3;
//	sp = new_r_arm_StoreStackMInc(irg, block, *mem, sp, n_regs, regs, get_irn_mode(sp));
//		set_arm_req_out(sp, &arm_default_req_arm_gp_sp, 0);
//		arch_set_irn_register(env->arch_env, sp, env->isa->sp);
	store = new_rd_arm_StoreStackM4Inc(NULL, irg, block, sp, fp, ip, lr, pc, *mem);
	// TODO
	// set_arm_req_out(store, &arm_default_req_arm_gp_sp, 0);
	// arch_set_irn_register(env->arch_env, store, env->isa->sp);

	sp = new_r_Proj(irg, block, store, env->isa->sp->reg_class->mode, pn_arm_StoreStackM4Inc_ptr);
	arch_set_irn_register(env->arch_env, sp, env->isa->sp);
	*mem = new_r_Proj(irg, block, store, mode_M, pn_arm_StoreStackM4Inc_M);

	keep = be_new_CopyKeep_single(gp, irg, block, ip, sp, get_irn_mode(ip));
	be_node_set_reg_class(keep, 1, gp);
	arch_set_irn_register(env->arch_env, keep, &arm_gp_regs[REG_R12]);
	be_set_constr_single_reg(keep, BE_OUT_POS(0), &arm_gp_regs[REG_R12] );

	fp = new_rd_arm_Sub_i(NULL, irg, block, keep, get_irn_mode(fp),
	                      new_tarval_from_long(4, get_irn_mode(fp)));
	// TODO...
	//set_arm_req_out_all(fp, fp_req);
	//set_arm_req_out(fp, &arm_default_req_arm_gp_r11, 0);
	arch_set_irn_register(env->arch_env, fp, env->isa->bp);

// 	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_R0], r0);
// 	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_R1], r1);
// 	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_R2], r2);
// 	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_R3], r3);
	be_abi_reg_map_set(reg_map, env->isa->bp, fp);
	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_R12], keep);
	be_abi_reg_map_set(reg_map, env->isa->sp, sp);
	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_LR], lr);
	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_PC], pc);

	return env->isa->bp;
}

static void arm_abi_epilogue(void *self, ir_node *bl, ir_node **mem, pmap *reg_map) {
	arm_abi_env_t *env = self;
	ir_node *curr_sp = be_abi_reg_map_get(reg_map, env->isa->sp);
	ir_node *curr_bp = be_abi_reg_map_get(reg_map, env->isa->bp);
	ir_node *curr_pc = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_PC]);
	ir_node	*curr_lr = be_abi_reg_map_get(reg_map, &arm_gp_regs[REG_LR]);

	// TODO: Activate Omit fp in epilogue
	if(env->flags.try_omit_fp) {
		curr_sp = be_new_IncSP(env->isa->sp, env->irg, bl, curr_sp, BE_STACK_FRAME_SIZE_SHRINK);
		add_irn_dep(curr_sp, *mem);

		curr_lr = be_new_CopyKeep_single(&arm_reg_classes[CLASS_arm_gp], env->irg, bl, curr_lr, curr_sp, get_irn_mode(curr_lr));
		be_node_set_reg_class(curr_lr, 1, &arm_reg_classes[CLASS_arm_gp]);
		arch_set_irn_register(env->arch_env, curr_lr, &arm_gp_regs[REG_LR]);
		be_set_constr_single_reg(curr_lr, BE_OUT_POS(0), &arm_gp_regs[REG_LR] );

		curr_pc = be_new_Copy(&arm_reg_classes[CLASS_arm_gp], env->irg, bl, curr_lr );
		arch_set_irn_register(env->arch_env, curr_pc, &arm_gp_regs[REG_PC]);
		be_set_constr_single_reg(curr_pc, BE_OUT_POS(0), &arm_gp_regs[REG_PC] );
	} else {
		ir_node *sub12_node;
		ir_node *load_node;
		tarval *tv = new_tarval_from_long(12,mode_Iu);
		sub12_node = new_rd_arm_Sub_i(NULL, env->irg, bl, curr_bp, mode_Iu, tv);
		// FIXME
		//set_arm_req_out_all(sub12_node, sub12_req);
		arch_set_irn_register(env->arch_env, sub12_node, env->isa->sp);
		load_node = new_rd_arm_LoadStackM3( NULL, env->irg, bl, sub12_node, *mem );
		// FIXME
		//set_arm_req_out(load_node, &arm_default_req_arm_gp_r11, 0);
		//set_arm_req_out(load_node, &arm_default_req_arm_gp_sp, 1);
		//set_arm_req_out(load_node, &arm_default_req_arm_gp_pc, 2);
		curr_bp = new_r_Proj(env->irg, bl, load_node, env->isa->bp->reg_class->mode, pn_arm_LoadStackM3_res0);
		curr_sp = new_r_Proj(env->irg, bl, load_node, env->isa->sp->reg_class->mode, pn_arm_LoadStackM3_res1);
		curr_pc = new_r_Proj(env->irg, bl, load_node, mode_Iu, pn_arm_LoadStackM3_res2);
		*mem    = new_r_Proj(env->irg, bl, load_node, mode_M, pn_arm_LoadStackM3_M);
		arch_set_irn_register(env->arch_env, curr_bp, env->isa->bp);
		arch_set_irn_register(env->arch_env, curr_sp, env->isa->sp);
		arch_set_irn_register(env->arch_env, curr_pc, &arm_gp_regs[REG_PC]);
	}
	be_abi_reg_map_set(reg_map, env->isa->sp, curr_sp);
	be_abi_reg_map_set(reg_map, env->isa->bp, curr_bp);
	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_LR], curr_lr);
	be_abi_reg_map_set(reg_map, &arm_gp_regs[REG_PC], curr_pc);
}

static const be_abi_callbacks_t arm_abi_callbacks = {
	arm_abi_init,
	free,
	arm_get_between_type,
	arm_abi_dont_save_regs,
	arm_abi_prologue,
	arm_abi_epilogue,
};


/**
 * Get the ABI restrictions for procedure calls.
 * @param self        The this pointer.
 * @param method_type The type of the method (procedure) in question.
 * @param abi         The abi object to be modified
 */
void arm_get_call_abi(const void *self, ir_type *method_type, be_abi_call_t *abi) {
	ir_type  *tp;
	ir_mode  *mode;
	int       i;
	int       n = get_method_n_params(method_type);
	be_abi_call_flags_t flags = {
		{
			0, /* store from left to right */
			0, /* store arguments sequential */
			1, /* try to omit the frame pointer */
			1, /* the function can use any register as frame pointer */
			1  /* a call can take the callee's address as an immediate */
		}
	};

	/* set stack parameter passing style */
	be_abi_call_set_flags(abi, flags, &arm_abi_callbacks);

	for (i = 0; i < n; i++) {
		/* reg = get reg for param i;          */
		/* be_abi_call_param_reg(abi, i, reg); */
		if (i < 4)

			be_abi_call_param_reg(abi, i, arm_get_RegParam_reg(i));
		else
			be_abi_call_param_stack(abi, i, 4, 0, 0);
	}

	/* default: return value is in R0 resp. F0 */
	assert(get_method_n_ress(method_type) < 2);
	if (get_method_n_ress(method_type) > 0) {
		tp   = get_method_res_type(method_type, 0);
		mode = get_type_mode(tp);

		be_abi_call_res_reg(abi, 0,
			mode_is_float(mode) ? &arm_fpa_regs[REG_F0] : &arm_gp_regs[REG_R0]);
	}
}

static const void *arm_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn) {
	return &arm_irn_ops;
}

const arch_irn_handler_t arm_irn_handler = {
	arm_get_irn_ops
};

const arch_irn_handler_t *arm_get_irn_handler(const void *self) {
	return &arm_irn_handler;
}

int arm_to_appear_in_schedule(void *block_env, const ir_node *irn) {
	return is_arm_irn(irn);
}

/**
 * Initializes the code generator interface.
 */
static const arch_code_generator_if_t *arm_get_code_generator_if(void *self) {
	return &arm_code_gen_if;
}

list_sched_selector_t arm_sched_selector;

/**
 * Returns the reg_pressure scheduler with to_appear_in_schedule() over\loaded
 */
static const list_sched_selector_t *arm_get_list_sched_selector(const void *self, list_sched_selector_t *selector) {
	memcpy(&arm_sched_selector, reg_pressure_selector, sizeof(list_sched_selector_t));
	arm_sched_selector.to_appear_in_schedule = arm_to_appear_in_schedule;
	return &arm_sched_selector;
}

static const ilp_sched_selector_t *arm_get_ilp_sched_selector(const void *self) {
	return NULL;
}

/**
 * Returns the necessary byte alignment for storing a register of given class.
 */
static int arm_get_reg_class_alignment(const void *self, const arch_register_class_t *cls) {
	ir_mode *mode = arch_register_class_mode(cls);
	return get_mode_size_bytes(mode);
}

static const be_execution_unit_t ***arm_get_allowed_execution_units(const void *self, const ir_node *irn) {
	/* TODO */
	assert(0);
	return NULL;
}

static const be_machine_t *arm_get_machine(const void *self) {
	/* TODO */
	assert(0);
	return NULL;
}

/**
 * Return irp irgs in the desired order.
 */
static ir_graph **arm_get_irg_list(const void *self, ir_graph ***irg_list) {
	return NULL;
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *arm_get_libfirm_params(void) {
	static arch_dep_params_t ad = {
		1,  /* allow subs */
		0,	/* Muls are fast enough on ARM */
		31, /* shift would be ok */
		0,  /* SMUL is needed, only in Arch M*/
		0,  /* UMUL is needed, only in Arch M */
		32, /* SMUL & UMUL available for 32 bit */
	};
	static backend_params p = {
		NULL,  /* no additional opcodes */
		NULL,  /* will be set later */
		1,     /* need dword lowering */
		NULL,  /* but yet no creator function */
		NULL,  /* context for create_intrinsic_fkt */
	};

	p.dep_param = &ad;
	return &p;
}

/* fpu set architectures. */
static const lc_opt_enum_int_items_t arm_fpu_items[] = {
	{ "softfloat", ARM_FPU_ARCH_SOFTFLOAT },
	{ "fpe",       ARM_FPU_ARCH_FPE },
	{ "fpa",       ARM_FPU_ARCH_FPA },
	{ "vfp1xd",    ARM_FPU_ARCH_VFP_V1xD },
	{ "vfp1",      ARM_FPU_ARCH_VFP_V1 },
	{ "vfp2",      ARM_FPU_ARCH_VFP_V2 },
	{ NULL,        0 }
};

static lc_opt_enum_int_var_t arch_fpu_var = {
	&arm_isa_template.fpu_arch, arm_fpu_items
};

static const lc_opt_table_entry_t arm_options[] = {
	LC_OPT_ENT_ENUM_INT("fpunit",    "select the floating point unit", &arch_fpu_var),
	LC_OPT_ENT_BOOL("gen_reg_names", "use generic register names", &arm_isa_template.gen_reg_names),
	{ NULL }
};

const arch_isa_if_t arm_isa_if = {
	arm_init,
	arm_done,
	arm_get_n_reg_class,
	arm_get_reg_class,
	arm_get_reg_class_for_mode,
	arm_get_call_abi,
	arm_get_irn_handler,
	arm_get_code_generator_if,
	arm_get_list_sched_selector,
	arm_get_ilp_sched_selector,
	arm_get_reg_class_alignment,
	arm_get_libfirm_params,
	arm_get_allowed_execution_units,
	arm_get_machine,
	arm_get_irg_list,
};

void be_init_arch_arm(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *arm_grp = lc_opt_get_grp(be_grp, "arm");

	lc_opt_add_table(arm_grp, arm_options);

	be_register_isa_if("arm", &arm_isa_if);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_arm);
