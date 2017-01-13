/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements vararg handling for AMD64
 * @author      Andreas Fried
 */
#include "amd64_varargs.h"

#include "amd64_abi.h"
#include "amd64_bearch_t.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "be.h"
#include "besched.h"
#include "betranshlp.h"
#include "bitfiddle.h"
#include "gen_amd64_regalloc_if.h"
#include "ident.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "panic.h"
#include "tv.h"
#include "typerep.h"
#include "util.h"

static struct va_list_members {
	ir_entity *gp_offset;
	ir_entity *xmm_offset;
	ir_entity *reg_save_ptr;
	ir_entity *stack_args_ptr;
} va_list_members;

static size_t            n_gp_params;
static size_t            n_xmm_params;
/* The register save area, and the slots for GP and XMM registers
 * inside of it. */
static ir_entity        *reg_save_area;
static ir_entity       **gp_save_slots;
static ir_entity       **xmm_save_slots;
/* Parameter entity pointing to the first variadic parameter on the
 * stack. */
static ir_entity        *stack_args_param;

static const size_t n_gp_args  =  6;
static const size_t n_xmm_args =  8;
static const size_t gp_size    =  8;
static const size_t xmm_size   = 16;

void amd64_set_va_stack_args_param(ir_entity *param)
{
	stack_args_param = param;
}

ir_type *amd64_build_va_list_type(void)
{
	ir_type *const int_type     = get_type_for_mode(mode_Is);
	ir_type *const ptr_type     = new_type_pointer(get_type_for_mode(mode_ANY));
	ir_type *const va_list_type = new_type_struct(new_id_from_str("builtin:va_list"));

	va_list_members.gp_offset      = new_entity(va_list_type, new_id_from_str("gp_offset"),      int_type);
	va_list_members.xmm_offset     = new_entity(va_list_type, new_id_from_str("xmm_offset"),     int_type);
	va_list_members.stack_args_ptr = new_entity(va_list_type, new_id_from_str("stack_args_ptr"), ptr_type);
	va_list_members.reg_save_ptr   = new_entity(va_list_type, new_id_from_str("reg_save_ptr"),   ptr_type);

	default_layout_compound_type(va_list_type);
	return va_list_type;
}

void amd64_collect_variadic_params(be_start_out *const outs, x86_cconv_t *const cconv)
{
	size_t gp_params  = 0;
	size_t xmm_params = 0;
	size_t p          = 0;
	for (size_t const n = cconv->n_parameters; p < n; p++) {
		const arch_register_t *reg = cconv->parameters[p].reg;
		if (reg) {
			if (reg->cls == &amd64_reg_classes[CLASS_amd64_gp]) {
				++gp_params;
			} else if (reg->cls == &amd64_reg_classes[CLASS_amd64_xmm]) {
				++xmm_params;
			} else {
				panic("unexpected register class");
			}
		}
	}

	n_gp_params  = gp_params;
	n_xmm_params = xmm_params;

	/* amd64_decide_calling_convention has appended the registers
	 * which might hold variadic arguments to the parameters
	 * array, first GP, then XMM. Get them out now. */
	for (size_t i = gp_params + xmm_params, n = cconv->n_param_regs; i < n; i++, p++) {
		const arch_register_t *reg = cconv->parameters[p].reg;
		outs[reg->global_index] = BE_START_REG;
	}
}

void amd64_insert_reg_save_area(ir_graph *irg, x86_cconv_t *cconv)
{
	ir_entity *const irg_ent = get_irg_entity(irg);
	ident     *const irg_id  = get_entity_ident(irg_ent);

	ident   *reg_save_type_id = new_id_fmt("__va_reg_save_%s_t", irg_id);
	ir_type *reg_save_type    = new_type_struct(reg_save_type_id);

	const size_t max_xmm_params = cconv->n_xmm_regs;
	const size_t max_gp_params  = cconv->n_param_regs - max_xmm_params;

	gp_save_slots = XMALLOCNZ(ir_entity*, max_gp_params);
	for (size_t i = 0; i < max_gp_params; i++) {
		ident *id = new_id_fmt("save_gp%d", i);
		gp_save_slots[i] = new_entity(reg_save_type, id, get_type_for_mode(mode_Lu));
	}

	xmm_save_slots = XMALLOCNZ(ir_entity*, max_xmm_params);
	for (size_t i = 0; i < max_xmm_params; i++) {
		ident *id = new_id_fmt("save_xmm%d", i);
		xmm_save_slots[i] = new_entity(reg_save_type, id, get_type_for_mode(amd64_mode_xmm));
	}

	default_layout_compound_type(reg_save_type);

	ir_type *frame_type  = get_irg_frame_type(irg);
	ident   *reg_save_id = new_id_fmt("__va_reg_save_%s", irg_id);
	reg_save_area = new_entity(frame_type, reg_save_id, reg_save_type);
}

/*
 * Lowering of va_arg
 *
 * For explanation see e.g.:
 * http://andrewl.dreamhosters.com/blog/variadic_functions_in_amd64_linux/index.html
 * and the AMD64 ABI (http://www.x86-64.org/documentation/abi.pdf)
 *
 * Given:
 * va_list ap;
 * va_start(ap);
 *
 * Generate one of the following for "va_arg(ap, T)":
 *
 * If T is an integral or pointer type:
 * if (ap.gp_offset < 6*8) {
 *         T result = ap.reg_save_ptr[gp_offset];
 *         gp_offset += 8;
 *         return result;
 * } else {
 *         T result = *ap.stack_args_ptr;
 *         ap.stack_args_ptr += (sizeof(T) rounded up to multiple of 8);
 *         return result;
 * }
 *
 * If T is an SSE floating point type:
 * if (ap.xmm_offset < 6*8+8*16) {
 *         T result = ap.reg_save_ptr[xmm_offset];
 *         xmm_offset += 16;
 *         return result;
 * } else {
 *         T result = *ap.stack_args_ptr;
 *         ap.stack_args_ptr += (sizeof(T) rounded up to multiple of 8);
 *         return result;
 * }
 *
 * If T is an x87 floating point type (i.e. long double):
 * T result = *ap.stack_args_ptr;
 * ap.stack_args_ptr += sizeof(T)
 * return result;
 */

static ir_node *load_result(dbg_info *dbgi, ir_node *block, ir_node *ptr, ir_type *type, ir_node **mem)
{
	ir_mode *mode    = get_type_mode(type);
	if (mode == NULL) {
		mode = mode_P;
	}
	ir_node *load    = new_rd_Load(dbgi, block, *mem, ptr, mode, type, cons_none);
	ir_node *result  = new_rd_Proj(dbgi, load, mode, pn_Load_res);
	ir_node *new_mem = new_rd_Proj(dbgi, load, mode_M, pn_Load_M);
	*mem = new_mem;
	return result;
}

static void make_store(dbg_info *dbgi, ir_node *block, ir_node *ptr, ir_node *value, ir_type *type, ir_node **mem)
{
	ir_node *store   = new_rd_Store(dbgi, block, *mem, ptr, value, type, cons_none);
	ir_node *new_mem = new_rd_Proj(dbgi, store, mode_M, pn_Store_M);
	*mem = new_mem;
}

static ir_node *get_va_stack_address(dbg_info *dbgi, ir_node *block, ir_type *restype, ir_node *ap, ir_node **mem)
{
	ir_graph *irg = get_irn_irg(block);

	// Load stack_args_ptr
	ir_node *stack_args_ptr  = new_rd_Member(dbgi, block, ap, va_list_members.stack_args_ptr);
	ir_type *stack_args_type = get_entity_type(va_list_members.stack_args_ptr);
	ir_node *stack_args      = load_result(dbgi, block, stack_args_ptr, stack_args_type, mem);

	// Increment stack_args and write back
	long     increment       = round_up2(get_type_size(restype), 8);
	ir_mode *offset_mode     = get_reference_offset_mode(mode_P);
	ir_node *sizeof_restype  = new_r_Const_long(irg, offset_mode, increment);
	ir_node *stack_args_inc  = new_rd_Add(dbgi, block, stack_args, sizeof_restype);
	make_store(dbgi, block, stack_args_ptr, stack_args_inc, stack_args_type, mem);

	return stack_args;
}

static ir_node *load_va_from_stack(dbg_info *dbgi, ir_node *block, ir_type *restype, ir_node *ap, ir_node **mem)
{
	ir_node *stack_args = get_va_stack_address(dbgi, block, restype, ap, mem);
	return load_result(dbgi, block, stack_args, restype, mem);
}

static ir_node *load_va_from_register_or_stack(dbg_info *dbgi, ir_node *block,
                                               ir_mode *resmode, ir_type *restype,
                                               ir_node *max, ir_entity *offset_entity, ir_node *stride,
                                               ir_node *ap, ir_node **mem)
{
	ir_graph *irg = get_irn_irg(block);

	// Load the current register offset
	ir_node *offset_ptr  = new_rd_Member(dbgi, block, ap, offset_entity);
	ir_type *offset_type = get_entity_type(offset_entity);
	ir_node *offset      = load_result(dbgi, block, offset_ptr, offset_type, mem);

	// Compare it to the maximum value
	ir_node *cmp = new_rd_Cmp(dbgi, block, offset, max, ir_relation_less);

	// Construct the if-diamond
	ir_node *lower_block = part_block_edges(cmp);
	ir_node *upper_block = get_nodes_block(cmp);
	ir_node *cond        = new_rd_Cond(dbgi, upper_block, cmp);
	ir_node *proj_true   = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node *proj_false  = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node *in_true[1]  = { proj_true };
	ir_node *in_false[1] = { proj_false };
	ir_node *true_block  = new_r_Block(irg, ARRAY_SIZE(in_true),  in_true);
	ir_node *false_block = new_r_Block(irg, ARRAY_SIZE(in_false), in_false);
	ir_node *true_jmp    = new_r_Jmp(true_block);
	ir_node *false_jmp   = new_r_Jmp(false_block);
	ir_node *lower_in[2] = { true_jmp, false_jmp };
	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);

	// True side: Load from the register save area
	// Load reg_save_ptr
	ir_node *true_mem        = *mem;
	ir_node *reg_save_ptr    = new_rd_Member(dbgi, true_block, ap, va_list_members.reg_save_ptr);
	ir_type *reg_save_type   = get_entity_type(va_list_members.reg_save_ptr);
	ir_node *reg_save        = load_result(dbgi, true_block, reg_save_ptr, reg_save_type, &true_mem);

	// Load from reg_save + offset
	ir_mode *mode_reg_save   = get_irn_mode(reg_save);
	ir_mode *offset_mode     = get_reference_offset_mode(mode_reg_save);
	ir_node *conv_offset     = new_r_Conv(true_block, offset, offset_mode);
	ir_node *true_result_ptr = new_rd_Add(dbgi, true_block, reg_save, conv_offset);
	ir_node *true_result     = load_result(dbgi, true_block, true_result_ptr, restype, &true_mem);

	// Increment offset and write back
	ir_node *offset_inc      = new_rd_Add(dbgi, true_block, offset, stride);
	make_store(dbgi, true_block, offset_ptr, offset_inc, offset_type, &true_mem);

	// False side: Load from the stack
	ir_node *false_mem    = *mem;
	ir_node *false_result = load_va_from_stack(dbgi, false_block, restype, ap, &false_mem);

	// Phi both sides together
	ir_node *phiM_in[]  = { true_mem, false_mem };
	ir_node *phiM       = new_rd_Phi(dbgi, lower_block, ARRAY_SIZE(phiM_in), phiM_in, mode_M);
	ir_node *phi_in[]   = { true_result, false_result };
	ir_node *phi        = new_rd_Phi(dbgi, lower_block, ARRAY_SIZE(phi_in), phi_in, resmode);

	*mem = phiM;
	return phi;
}

static ir_node *load_aggregate_va(dbg_info *dbgi, ir_node *block, ir_type *restype, ir_node *ap, ir_node **mem)
{
	// Check which registers the struct needs if we had enough of them.
	amd64_abi_state abi_state = {
		.integer_params = 0,
		.sse_params = 0,
	};
	aggregate_spec_t spec = amd64_lower_parameter(&abi_state, restype);

	ir_graph  *irg    = get_irn_irg(block);

	if (spec.length == 0) {
		// Empty type. Since we return the aggregate's address
		// in firm, and there are no fields to load/store, we
		// can return Unknown.
		return new_rd_Unknown(dbgi, irg, mode_P);
	} else if (spec.length == 1 && spec.modes[0] == mode_M) {
		// The type is always passed on stack. We return the
		// pointer to where the value is located on the stack.
		// The C standard is silent about whether the values
		// returned from va_arg are copies or the actual
		// arguments.
		return get_va_stack_address(dbgi, block, restype, ap, mem);
	}

	clear_irg_properties(irg, IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);

	// Check if enough registers of each required kind are available.
	unsigned n_gp  = 0;
	unsigned n_xmm = 0;
	for (unsigned i = 0; i < spec.length; i++) {
		ir_mode *mode = spec.modes[i];
		if (be_mode_needs_gp_reg(mode)) {
			n_gp++;
		} else if (mode_is_float(mode)) {
			n_xmm++;
		}
	}

	// Compare against GP offset
	ir_type *gp_offset_type = get_entity_type(va_list_members.gp_offset);
	ir_mode *gp_offset_mode = get_type_mode(gp_offset_type);
	ir_node *gp_offset_ptr  = new_rd_Member(dbgi, block, ap, va_list_members.gp_offset);
	ir_node *gp_offset      = load_result(dbgi, block, gp_offset_ptr, gp_offset_type, mem);
	ir_node *gp_max         = new_rd_Const_long(dbgi, irg, gp_offset_mode, n_gp_args * gp_size);
	ir_node *gp_check       = new_rd_Cmp(dbgi, block, gp_offset, gp_max, ir_relation_less);
	ir_node *gp_check_mem   = *mem;

	// If GP offset is too large, load from stack
	ir_node *lower_block     = part_block_edges(gp_check);
	ir_node *upper_block     = get_nodes_block(gp_check);
	ir_node *gp_cond         = new_rd_Cond(dbgi, upper_block, gp_check);
	ir_node *gp_proj_true    = new_r_Proj(gp_cond, mode_X, pn_Cond_true);
	ir_node *gp_proj_false   = new_r_Proj(gp_cond, mode_X, pn_Cond_false);
	ir_node *in_xmm_check[1] = { gp_proj_true };
	ir_node *xmm_check_block = new_r_Block(irg, ARRAY_SIZE(in_xmm_check),  in_xmm_check);

	// Compare against XMM offset
	ir_type *xmm_offset_type = get_entity_type(va_list_members.xmm_offset);
	ir_mode *xmm_offset_mode = get_type_mode(xmm_offset_type);
	ir_node *xmm_offset_ptr  = new_rd_Member(dbgi, xmm_check_block, ap, va_list_members.xmm_offset);
	ir_node *xmm_offset      = load_result(dbgi, xmm_check_block, xmm_offset_ptr, xmm_offset_type, mem);
	ir_node *xmm_max         = new_rd_Const_long(dbgi, irg, xmm_offset_mode, n_gp_args * gp_size + n_xmm_args * xmm_size);
	ir_node *xmm_check       = new_rd_Cmp(dbgi, xmm_check_block, xmm_offset, xmm_max, ir_relation_less);
	ir_node *xmm_check_mem   = *mem;

	// If XMM offset is too large, load from stack
	ir_node *xmm_cond       = new_rd_Cond(dbgi, xmm_check_block, xmm_check);
	ir_node *xmm_proj_true  = new_r_Proj(xmm_cond, mode_X, pn_Cond_true);
	ir_node *xmm_proj_false = new_r_Proj(xmm_cond, mode_X, pn_Cond_false);
	ir_node *in_true[1]     = { xmm_proj_true };
	ir_node *true_block     = new_r_Block(irg, ARRAY_SIZE(in_true), in_true);
	ir_node *in_false[2]    = { gp_proj_false, xmm_proj_false };
	ir_node *false_block    = new_r_Block(irg, ARRAY_SIZE(in_false), in_false);
	ir_node *true_jmp       = new_r_Jmp(true_block);
	ir_node *false_jmp      = new_r_Jmp(false_block);
	ir_node *lower_in[2]    = { true_jmp, false_jmp };
	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);

	// True side: Load from the register save area
	// Load reg_save_ptr
	ir_node *true_mem        = xmm_check_mem;

	ir_type   *frame    = get_irg_frame_type(irg);
	ident     *id       = id_unique("$vararg_param");
	ir_entity *entity   = new_entity(frame, id, restype);
	ir_node   *ent_addr = new_rd_Member(dbgi, true_block, get_irg_frame(irg), entity);

	ir_node *reg_save_ptr    = new_rd_Member(dbgi, true_block, ap, va_list_members.reg_save_ptr);
	ir_type *reg_save_type   = get_entity_type(va_list_members.reg_save_ptr);
	ir_node *reg_save        = load_result(dbgi, true_block, reg_save_ptr, reg_save_type, &true_mem);
	ir_mode *mode_reg_save   = get_irn_mode(reg_save);
	ir_mode *offset_mode     = get_reference_offset_mode(mode_reg_save);

	for (unsigned i = 0; i < spec.length; i++) {
		ir_mode *mode    = spec.modes[i];
		ir_type *regtype = get_type_for_mode(mode);
		ir_node *offset_ptr;
		ir_type *offset_type;
		unsigned reg_size;
		if (be_mode_needs_gp_reg(mode)) {
			offset_ptr  = gp_offset_ptr;
			offset_type = gp_offset_type;
			reg_size    = gp_size;
		} else {
			offset_ptr  = xmm_offset_ptr;
			offset_type = gp_offset_type;
			reg_size    = xmm_size;
		}
		ir_node *offset      = load_result(dbgi, true_block, offset_ptr, offset_type, &true_mem);

		// When we use an offset here, we should have compared against it before.
		assert(offset != NULL);

		// Load from reg_save + offset
		ir_node *conv_offset     = new_r_Conv(true_block, offset, offset_mode);
		ir_node *true_result_ptr = new_rd_Add(dbgi, true_block, reg_save, conv_offset);
		ir_node *reg             = load_result(dbgi, true_block, true_result_ptr, regtype, &true_mem);

		// Store to entity
		// Use gp_size here always, because for structs we only use 8 bytes of an XMM register.
		ir_node *ent_offset = new_rd_Const_long(dbgi, irg, offset_mode, gp_size * i);
		ir_node *dest       = new_rd_Add(dbgi, true_block, ent_addr, ent_offset);
		make_store(dbgi, true_block, dest, reg, regtype, &true_mem);

		// Increment offset and write back
		ir_node *stride     = new_r_Const_long(irg, gp_offset_mode, reg_size);
		ir_node *offset_inc = new_rd_Add(dbgi, true_block, offset, stride);
		make_store(dbgi, true_block, offset_ptr, offset_inc, offset_type, &true_mem);
	}

	// False side: Load from the stack
	ir_node *false_phiM_in[] = { gp_check_mem, xmm_check_mem };
	ir_node *false_mem    = new_rd_Phi(dbgi, false_block, ARRAY_SIZE(false_phiM_in), false_phiM_in, mode_M);
	ir_node *false_result = get_va_stack_address(dbgi, false_block, restype, ap, &false_mem);

	// Phi both sides together
	ir_node *phiM_in[]  = { true_mem, false_mem };
	ir_node *phiM       = new_rd_Phi(dbgi, lower_block, ARRAY_SIZE(phiM_in), phiM_in, mode_M);
	ir_node *phi_in[]   = { ent_addr, false_result };
	ir_node *phi        = new_rd_Phi(dbgi, lower_block, ARRAY_SIZE(phi_in), phi_in, mode_P);

	*mem = phiM;
	return phi;
}

void amd64_lower_va_arg(ir_node *const node)
{
	ir_type *const restype = get_method_res_type(get_Builtin_type(node), 0);
	ir_mode       *resmode = get_type_mode(restype);
	if (!resmode)
		resmode = mode_P;

	ir_node        *result;
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);
	ir_node  *const ap    = get_irn_n(node, pn_Builtin_max + 1);
	ir_node        *mem   = get_Builtin_mem(node);
	if (is_aggregate_type(restype)) {
		result = load_aggregate_va(dbgi, block, restype, ap, &mem);
	} else if (get_mode_arithmetic(resmode) == irma_x86_extended_float) {
		assert(get_mode_size_bytes(resmode) == 10);
		result = load_va_from_stack(dbgi, block, restype, ap, &mem);
	} else {
		ir_entity *offset_entity;
		long       size;
		long       maxv = n_gp_args * gp_size;
		if (be_mode_needs_gp_reg(resmode)) {
			offset_entity = va_list_members.gp_offset;
			size          = gp_size;
		} else if (mode_is_float(resmode)) {
			offset_entity = va_list_members.xmm_offset;
			size          = xmm_size;
			maxv         += n_xmm_args * xmm_size;
		} else {
			panic("amd64_lower_va_arg does not support mode %+F", resmode);
		}
		ir_mode  *const omode  = get_type_mode(get_entity_type(offset_entity));
		ir_graph *const irg    = get_irn_irg(node);
		ir_node  *const max    = new_r_Const_long(irg, omode, maxv);
		ir_node  *const stride = new_r_Const_long(irg, omode, size);
		result = load_va_from_register_or_stack(dbgi, block, resmode, restype, max, offset_entity, stride, ap, &mem);
	}
	ir_node *const tuple_in[] = { mem, result };
	turn_into_tuple(node, ARRAY_SIZE(tuple_in), tuple_in);
}

/*
 * Make a mov to store the given immediate value into (base + base_offset),
 * i.e. mov $value, base_offset(%base_reg)
 */
static ir_node *make_mov_imm32_to_offset_mem(dbg_info *dbgi, ir_node *block, ir_node *mem, ir_node *base, ir_entity *offset_ent, long value)
{
	ir_node *const mov_in[] = { base, mem };
	int32_t  const offset   = get_entity_offset(offset_ent);
	amd64_binop_addr_attr_t mov_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_ADDR_IMM,
				.size    = X86_SIZE_32,
			},
			.addr = {
				.immediate = {
					.offset = offset,
					.kind   = X86_IMM_VALUE,
				},
				.variant    = X86_ADDR_BASE,
				.base_input = 0,
				.mem_input  = 1,
			},
		},
		.u = {
			.immediate = {
				.offset = value,
				.kind   = X86_IMM_VALUE,
			},
		},
	};
	return new_bd_amd64_mov_store(dbgi, block, ARRAY_SIZE(mov_in), mov_in, gp_am_reqs[1], &mov_attr);
}

/*
 * Make a mov to store the given value into (base + base_offset),
 * i.e. mov %value_reg, base_offset(%base_reg)
 */
static ir_node *make_mov_val64_to_offset_mem(dbg_info *dbgi, ir_node *block, ir_node *mem, ir_node *base, ir_entity *entity, ir_entity *offset_ent, ir_node *value)
{
	ir_node *const mov_in[] = { value, base, mem };
	int32_t  const offset   = get_entity_offset(offset_ent);
	amd64_binop_addr_attr_t mov_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_ADDR_REG,
				.size    = X86_SIZE_64,
			},
			.addr = {
				.immediate = {
					.entity = entity,
					.offset = offset,
					.kind   = entity ? X86_IMM_FRAMEENT : X86_IMM_VALUE,
				},
				.variant    = X86_ADDR_BASE,
				.base_input = 1,
				.mem_input  = 2,
			},
		},
		.u = {
			.reg_input = 0,
		},
	};
	return new_bd_amd64_mov_store(dbgi, block, ARRAY_SIZE(mov_in), mov_in, gp_am_reqs[2], &mov_attr);
}

/*
 * Make a mov to store the given XMM value into (base + base_offset),
 * i.e. movsd %xmm_value_reg, base_offset(%base_reg)
 */
static ir_node *make_mov_xmmval64_to_offset_mem(dbg_info *dbgi, ir_node *block, ir_node *mem, ir_node *base, ir_entity *entity, ir_entity *offset_ent, ir_node *value)
{
	ir_node *const mov_in[] = { value, base, mem };
	int32_t  const offset   = get_entity_offset(offset_ent);
	amd64_binop_addr_attr_t mov_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_ADDR_REG,
				.size    = X86_SIZE_64,
			},
			.addr = {
				.immediate = {
					.entity = entity,
					.offset = offset,
					.kind   = X86_IMM_FRAMEENT,
				},
				.variant    = X86_ADDR_BASE,
				.base_input = 1,
				.mem_input  = 2,
			},
		},
		.u = {
			.reg_input = 0,
		},
	};
	return new_bd_amd64_movs_store_xmm(dbgi, block, ARRAY_SIZE(mov_in), mov_in, xmm_reg_mem_reqs, &mov_attr);
}

/*
 * Make a lea to compute the address of the given entity
 * i.e. lea entity_offset(%base_reg), %result_reg
 */
static ir_node *make_lea_with_offset_entity(dbg_info *dbgi, ir_node *block,
                                            ir_node *base, ir_entity *offset)
{
	ir_node *lea_in[] = { base };
	x86_addr_t lea_addr = {
		.immediate = {
			.entity = offset,
			.kind   = X86_IMM_FRAMEENT,
		},
		.variant    = X86_ADDR_BASE,
		.base_input = 0,
	};
	return new_bd_amd64_lea(dbgi, block, ARRAY_SIZE(lea_in), lea_in, reg_reqs, X86_SIZE_64, lea_addr);
}

ir_node *amd64_initialize_va_list(dbg_info *dbgi, ir_node *block, x86_cconv_t *cconv,
                                  ir_node *mem, ir_node *ap, ir_node *frame)
{
	const size_t max_xmm_params = cconv->n_xmm_regs;
	const size_t max_gp_params  = cconv->n_param_regs - max_xmm_params;

	size_t const initial_gp_offset = n_gp_params * 8;
	mem = make_mov_imm32_to_offset_mem(dbgi, block, mem, ap, va_list_members.gp_offset, initial_gp_offset);

	// XMM parameters are behind the gp parameters in reg_save_area.
	size_t const initial_xmm_offset = max_gp_params * 8 + n_xmm_params * 16;
	mem = make_mov_imm32_to_offset_mem(dbgi, block, mem, ap, va_list_members.xmm_offset, initial_xmm_offset);

	ir_node *const reg_save_ptr = make_lea_with_offset_entity(dbgi, block, frame, reg_save_area);
	mem = make_mov_val64_to_offset_mem(dbgi, block, mem, ap, NULL, va_list_members.reg_save_ptr, reg_save_ptr);

	ir_node *const stack_args = make_lea_with_offset_entity(dbgi, block, frame, stack_args_param);
	mem = make_mov_val64_to_offset_mem(dbgi, block, mem, ap, NULL, va_list_members.stack_args_ptr, stack_args);

	return mem;
}

void amd64_save_vararg_registers(ir_graph *const irg, x86_cconv_t const *const cconv, ir_node *const frame)
{
	size_t         gp_params      = n_gp_params;
	size_t         xmm_params     = n_xmm_params;
	size_t         reg_params     = gp_params + xmm_params;
	size_t   const max_reg_params = cconv->n_param_regs;
	ir_node *const block          = get_irg_start_block(irg);
	ir_node *const initial_mem    = get_irg_initial_mem(irg);
	ir_node       *mem            = initial_mem;
	ir_node       *first_mov      = NULL;
	for (size_t p = cconv->n_parameters; reg_params != max_reg_params; ++p, ++reg_params) {
		arch_register_t const *const reg       = cconv->parameters[p].reg;
		ir_node               *const reg_value = be_get_Start_proj(irg, reg);
		if (reg->cls == &amd64_reg_classes[CLASS_amd64_gp]) {
			mem = make_mov_val64_to_offset_mem(NULL, block, mem, frame, reg_save_area, gp_save_slots[gp_params++], reg_value);
		} else if (reg->cls == &amd64_reg_classes[CLASS_amd64_xmm]) {
			mem = make_mov_xmmval64_to_offset_mem(NULL, block, mem, frame, reg_save_area, xmm_save_slots[xmm_params++], reg_value);
		} else {
			panic("unexpected register class");
		}
		if (!first_mov)
			first_mov = mem;
	}

	if (mem != initial_mem) {
		edges_reroute_except(initial_mem, mem, first_mov);
		set_irg_initial_mem(irg, initial_mem);
	}

	// We are now done with vararg handling for this irg, free the memory.
	free(gp_save_slots);
	free(xmm_save_slots);
}
