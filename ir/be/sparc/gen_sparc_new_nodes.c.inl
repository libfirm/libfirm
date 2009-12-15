#include "gen_sparc_regalloc_if.h"


ir_op *op_sparc_SubSP = NULL;
ir_op *op_sparc_Add = NULL;
ir_op *op_sparc_FrameAddr = NULL;
ir_op *op_sparc_Store = NULL;
ir_op *op_sparc_Branch = NULL;
ir_op *op_sparc_Mov = NULL;
ir_op *op_sparc_Tst = NULL;
ir_op *op_sparc_SwitchJmp = NULL;
ir_op *op_sparc_Cmp = NULL;
ir_op *op_sparc_SymConst = NULL;
ir_op *op_sparc_Sub = NULL;
ir_op *op_sparc_Load = NULL;
ir_op *op_sparc_AddSP = NULL;

ir_op *get_op_sparc_SubSP(void)         { return op_sparc_SubSP; }
int    is_sparc_SubSP(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_SubSP; }

ir_op *get_op_sparc_Add(void)         { return op_sparc_Add; }
int    is_sparc_Add(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Add; }

ir_op *get_op_sparc_FrameAddr(void)         { return op_sparc_FrameAddr; }
int    is_sparc_FrameAddr(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_FrameAddr; }

ir_op *get_op_sparc_Store(void)         { return op_sparc_Store; }
int    is_sparc_Store(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Store; }

ir_op *get_op_sparc_Branch(void)         { return op_sparc_Branch; }
int    is_sparc_Branch(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Branch; }

ir_op *get_op_sparc_Mov(void)         { return op_sparc_Mov; }
int    is_sparc_Mov(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Mov; }

ir_op *get_op_sparc_Tst(void)         { return op_sparc_Tst; }
int    is_sparc_Tst(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Tst; }

ir_op *get_op_sparc_SwitchJmp(void)         { return op_sparc_SwitchJmp; }
int    is_sparc_SwitchJmp(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_SwitchJmp; }

ir_op *get_op_sparc_Cmp(void)         { return op_sparc_Cmp; }
int    is_sparc_Cmp(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Cmp; }

ir_op *get_op_sparc_SymConst(void)         { return op_sparc_SymConst; }
int    is_sparc_SymConst(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_SymConst; }

ir_op *get_op_sparc_Sub(void)         { return op_sparc_Sub; }
int    is_sparc_Sub(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Sub; }

ir_op *get_op_sparc_Load(void)         { return op_sparc_Load; }
int    is_sparc_Load(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_Load; }

ir_op *get_op_sparc_AddSP(void)         { return op_sparc_AddSP; }
int    is_sparc_AddSP(const ir_node *n) { return get_sparc_irn_opcode(n) == iro_sparc_AddSP; }



static int sparc_opcode_start = -1;
static int sparc_opcode_end   = -1;


/** A tag for the sparc opcodes. Note that the address is used as a tag value, NOT the FOURCC code. */
#define sparc_op_tag FOURCC('S', 'P', 'A', 'R')

/** Return the opcode number of the first sparc opcode. */
int get_sparc_opcode_first(void) {
	return sparc_opcode_start;
}

/** Return the opcode number of the last sparc opcode + 1. */
int get_sparc_opcode_last(void) {
	return sparc_opcode_end;
}

/** Return 1 if the given opcode is a sparc machine op, 0 otherwise */
int is_sparc_op(const ir_op *op) {
	return get_op_tag(op) == sparc_op_tag;
}

/** Return 1 if the given node is a sparc machine node, 0 otherwise */
int is_sparc_irn(const ir_node *node) {
	return is_sparc_op(get_irn_op(node));
}

int get_sparc_irn_opcode(const ir_node *node) {
	if (is_sparc_irn(node))
		return get_irn_opcode(node) - sparc_opcode_start;
	return -1;
}

#ifdef BIT
#undef BIT
#endif
#define BIT(x)  (1 << (x % 32))

static const unsigned sparc_limit_gp_sp[] = { BIT(REG_SP), 0 };

static const arch_register_req_t sparc_requirements_gp_sp = {
	arch_register_req_type_limited,
	& sparc_reg_classes[CLASS_sparc_gp],
	sparc_limit_gp_sp,
	0,        /* same pos */
	0        /* different pos */
};


static const arch_register_req_t sparc_requirements_gp_gp = {
	arch_register_req_type_normal,
	& sparc_reg_classes[CLASS_sparc_gp],
	NULL,        /* limit bitset */
	0,           /* same pos */
	0            /* different pos */
};


static const arch_register_req_t sparc_requirements_gp_sp_I_S = {
	arch_register_req_type_ignore | arch_register_req_type_produces_sp | arch_register_req_type_limited,
	& sparc_reg_classes[CLASS_sparc_gp],
	sparc_limit_gp_sp,
	0,        /* same pos */
	0        /* different pos */
};


static const arch_register_req_t sparc_requirements__none = {
	arch_register_req_type_none,
	NULL,                         /* regclass */
	NULL,                         /* limit bitset */
	0,                            /* same pos */
	0                             /* different pos */
};


static const arch_register_req_t sparc_requirements_flags_flags = {
	arch_register_req_type_normal,
	& sparc_reg_classes[CLASS_sparc_flags],
	NULL,        /* limit bitset */
	0,           /* same pos */
	0            /* different pos */
};



/**
 * free stack space
 */
ir_node *new_bd_sparc_SubSP(dbg_info *dbgi, ir_node *block, ir_node *stack, ir_node *size, ir_node *mem)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_SubSP;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 3;
	ir_node        *in[3];
	int             n_res   = 2;
	ir_mode        *mode    = mode_T;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_sp,
		& sparc_requirements_gp_gp,
		& sparc_requirements__none,
	};

	/* construct in array */
	in[0] = stack;
	in[1] = size;
	in[2] = mem;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_sp_I_S;
info->out_infos[1].req = &sparc_requirements__none;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Add node
 */
ir_node *new_bd_sparc_Add_imm(dbg_info *dbgi, ir_node *block, ir_node *left, int immediate_value)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Add;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	sparc_set_attr_imm(res, immediate_value);
	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Add node
 */
ir_node *new_bd_sparc_Add_reg(dbg_info *dbgi, ir_node *block, ir_node *left, ir_node *right)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Add;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 2;
	ir_node        *in[2];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;
	in[1] = right;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct FrameAddr node
 */
ir_node *new_bd_sparc_FrameAddr(dbg_info *dbgi, ir_node *block, ir_node *base, ir_entity *entity)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_FrameAddr;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = base;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	init_sparc_symconst_attributes(res, entity);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Store: Store(ptr, val, mem) = ST ptr,val
 */
ir_node *new_bd_sparc_Store(dbg_info *dbgi, ir_node *block, ir_node *ptr, ir_node *val, ir_node *mem, ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Store;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 3;
	ir_node        *in[3];
	int             n_res   = 1;
	ir_mode        *mode    = mode_M;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
		& sparc_requirements_gp_gp,
		& sparc_requirements__none,
	};

	/* construct in array */
	in[0] = ptr;
	in[1] = val;
	in[2] = mem;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	init_sparc_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements__none;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Branch node
 */
ir_node *new_bd_sparc_Branch(dbg_info *dbgi, ir_node *block, ir_node *op0, int proj_num)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Branch;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 2;
	ir_mode        *mode    = mode_T;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_flags_flags,
	};
	sparc_jmp_cond_attr_t *attr;

	/* construct in array */
	in[0] = op0;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements__none;
info->out_infos[1].req = &sparc_requirements__none;


	attr = get_irn_generic_attr(res);
		set_sparc_jmp_cond_proj_num(res, proj_num);
	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Mov node
 */
ir_node *new_bd_sparc_Mov_imm(dbg_info *dbgi, ir_node *block, int immediate_value)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Mov;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 0;
	ir_node       **in      = NULL;
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t **in_reqs = NULL;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	sparc_set_attr_imm(res, immediate_value);
	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Mov node
 */
ir_node *new_bd_sparc_Mov_reg(dbg_info *dbgi, ir_node *block, ir_node *op0)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Mov;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = op0;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Tst node
 */
ir_node *new_bd_sparc_Tst(dbg_info *dbgi, ir_node *block, ir_node *left, bool ins_permuted, bool is_unsigned)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Tst;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Bu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;

	/* flags */
	flags |= arch_irn_flags_rematerializable;
	flags |= arch_irn_flags_modify_flags;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	init_sparc_cmp_attr(res, ins_permuted, is_unsigned);
	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_flags_flags;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct SwitchJmp node
 */
ir_node *new_bd_sparc_SwitchJmp(dbg_info *dbgi, ir_node *block, ir_node *op0, int n_projs, long def_proj_num)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_SwitchJmp;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_T;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};
	sparc_jmp_switch_attr_t *attr;

	/* construct in array */
	in[0] = op0;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements__none;


	attr = get_irn_generic_attr(res);
		set_sparc_jmp_switch_n_projs(res, n_projs);
	set_sparc_jmp_switch_default_proj_num(res, def_proj_num);
	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Cmp node
 */
ir_node *new_bd_sparc_Cmp_imm(dbg_info *dbgi, ir_node *block, ir_node *left, int immediate_value, bool ins_permuted, bool is_unsigned)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Cmp;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Bu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;

	/* flags */
	flags |= arch_irn_flags_rematerializable;
	flags |= arch_irn_flags_modify_flags;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	sparc_set_attr_imm(res, immediate_value);	init_sparc_cmp_attr(res, ins_permuted, is_unsigned);
	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_flags_flags;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Cmp node
 */
ir_node *new_bd_sparc_Cmp_reg(dbg_info *dbgi, ir_node *block, ir_node *left, ir_node *right, bool ins_permuted, bool is_unsigned)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Cmp;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 2;
	ir_node        *in[2];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Bu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;
	in[1] = right;

	/* flags */
	flags |= arch_irn_flags_rematerializable;
	flags |= arch_irn_flags_modify_flags;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	init_sparc_cmp_attr(res, ins_permuted, is_unsigned);
	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_flags_flags;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct SymConst node
 */
ir_node *new_bd_sparc_SymConst(dbg_info *dbgi, ir_node *block, ir_entity *entity)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_SymConst;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 0;
	ir_node       **in      = NULL;
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t **in_reqs = NULL;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	init_sparc_symconst_attributes(res, entity);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Sub node
 */
ir_node *new_bd_sparc_Sub_imm(dbg_info *dbgi, ir_node *block, ir_node *left, int immediate_value)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Sub;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 1;
	ir_node        *in[1];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	sparc_set_attr_imm(res, immediate_value);
	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Sub node
 */
ir_node *new_bd_sparc_Sub_reg(dbg_info *dbgi, ir_node *block, ir_node *left, ir_node *right)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Sub;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 2;
	ir_node        *in[2];
	int             n_res   = 1;
	ir_mode        *mode    = mode_Iu;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
		& sparc_requirements_gp_gp,
	};

	/* construct in array */
	in[0] = left;
	in[1] = right;

	/* flags */
	flags |= arch_irn_flags_rematerializable;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * construct Load: Load(ptr, mem) = LD ptr -> reg
 */
ir_node *new_bd_sparc_Load(dbg_info *dbgi, ir_node *block, ir_node *ptr, ir_node *mem, ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_Load;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 2;
	ir_node        *in[2];
	int             n_res   = 2;
	ir_mode        *mode    = mode_T;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_gp,
		& sparc_requirements__none,
	};

	/* construct in array */
	in[0] = ptr;
	in[1] = mem;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);
	init_sparc_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_gp;
info->out_infos[1].req = &sparc_requirements__none;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}

/**
 * alloc stack space
 */
ir_node *new_bd_sparc_AddSP(dbg_info *dbgi, ir_node *block, ir_node *stack, ir_node *size, ir_node *mem)
{
	ir_node        *res;
	ir_op          *op      = op_sparc_AddSP;
	int             flags   = 0;
	backend_info_t *info;
	int             arity   = 3;
	ir_node        *in[3];
	int             n_res   = 3;
	ir_mode        *mode    = mode_T;
	static const be_execution_unit_t ***exec_units = NULL;
	static const arch_register_req_t *in_reqs[] =
	{
		& sparc_requirements_gp_sp,
		& sparc_requirements_gp_gp,
		& sparc_requirements__none,
	};

	/* construct in array */
	in[0] = stack;
	in[1] = size;
	in[2] = mem;

	/* create node */
	assert(op != NULL);
	res = new_ir_node(dbgi, current_ir_graph, block, op, mode, arity, in);

	/* init node attributes */
		init_sparc_attributes(res, flags, in_reqs, exec_units, n_res);

	info = be_get_info(res);
	info->out_infos[0].req = &sparc_requirements_gp_sp_I_S;
info->out_infos[1].req = &sparc_requirements_gp_gp;
info->out_infos[2].req = &sparc_requirements__none;


	/* optimize node */
	res = optimize_node(res);
	irn_vrfy_irg(res, current_ir_graph);

	return res;
}



/**
 * Creates the sparc specific Firm machine operations
 * needed for the assembler irgs.
 */
void sparc_create_opcodes(const arch_irn_ops_t *be_ops) {
#define N   irop_flag_none
#define L   irop_flag_labeled
#define C   irop_flag_commutative
#define X   irop_flag_cfopcode
#define I   irop_flag_ip_cfopcode
#define F   irop_flag_fragile
#define Y   irop_flag_forking
#define H   irop_flag_highlevel
#define c   irop_flag_constlike
#define K   irop_flag_keep
#define M   irop_flag_machine
#define O   irop_flag_machine_op
#define NB  irop_flag_dump_noblock
#define NI  irop_flag_dump_noinput
#define R   (irop_flag_user << 0)

	ir_op_ops  ops;
	int        cur_opcode;
	static int run_once = 0;

	if (run_once)
		return;
	run_once = 1;

	cur_opcode = get_next_ir_opcodes(iro_sparc_last);

	sparc_opcode_start = cur_opcode;

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_SubSP = new_ir_op(cur_opcode + iro_sparc_SubSP, "sparc_SubSP", op_pin_state_floats, N|M, oparity_trinary, 0, sizeof(sparc_attr_t), &ops);
	set_op_tag(op_sparc_SubSP, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Add = new_ir_op(cur_opcode + iro_sparc_Add, "sparc_Add", op_pin_state_floats, C|M, oparity_zero, 0, sizeof(sparc_attr_t), &ops);
	set_op_tag(op_sparc_Add, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_symconst;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_FrameAddr = new_ir_op(cur_opcode + iro_sparc_FrameAddr, "sparc_FrameAddr", op_pin_state_floats, c|M, oparity_unary, 0, sizeof(sparc_symconst_attr_t), &ops);
	set_op_tag(op_sparc_FrameAddr, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_load_store;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Store = new_ir_op(cur_opcode + iro_sparc_Store, "sparc_Store", op_pin_state_exc_pinned, L|F|M, oparity_trinary, 0, sizeof(sparc_load_store_attr_t), &ops);
	set_op_tag(op_sparc_Store, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_jmp_cond;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Branch = new_ir_op(cur_opcode + iro_sparc_Branch, "sparc_Branch", op_pin_state_pinned, L|X|Y|M, oparity_unary, 0, sizeof(sparc_jmp_cond_attr_t), &ops);
	set_op_tag(op_sparc_Branch, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Mov = new_ir_op(cur_opcode + iro_sparc_Mov, "sparc_Mov", op_pin_state_floats, N|M, oparity_variable, 0, sizeof(sparc_attr_t), &ops);
	set_op_tag(op_sparc_Mov, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_cmp;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Tst = new_ir_op(cur_opcode + iro_sparc_Tst, "sparc_Tst", op_pin_state_floats, N|M, oparity_unary, 0, sizeof(sparc_cmp_attr_t), &ops);
	set_op_tag(op_sparc_Tst, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_jmp_switch;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_SwitchJmp = new_ir_op(cur_opcode + iro_sparc_SwitchJmp, "sparc_SwitchJmp", op_pin_state_pinned, L|X|Y|M, oparity_unary, 0, sizeof(sparc_jmp_switch_attr_t), &ops);
	set_op_tag(op_sparc_SwitchJmp, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_cmp;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Cmp = new_ir_op(cur_opcode + iro_sparc_Cmp, "sparc_Cmp", op_pin_state_floats, N|M, oparity_zero, 0, sizeof(sparc_cmp_attr_t), &ops);
	set_op_tag(op_sparc_Cmp, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_symconst;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_SymConst = new_ir_op(cur_opcode + iro_sparc_SymConst, "sparc_SymConst", op_pin_state_floats, c|M, oparity_zero, 0, sizeof(sparc_symconst_attr_t), &ops);
	set_op_tag(op_sparc_SymConst, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Sub = new_ir_op(cur_opcode + iro_sparc_Sub, "sparc_Sub", op_pin_state_floats, N|M, oparity_binary, 0, sizeof(sparc_attr_t), &ops);
	set_op_tag(op_sparc_Sub, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc_load_store;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_Load = new_ir_op(cur_opcode + iro_sparc_Load, "sparc_Load", op_pin_state_exc_pinned, L|F|M, oparity_binary, 0, sizeof(sparc_load_store_attr_t), &ops);
	set_op_tag(op_sparc_Load, sparc_op_tag);

	memset(&ops, 0, sizeof(ops));
	ops.be_ops        = be_ops;
	ops.dump_node     = sparc_dump_node;
	ops.node_cmp_attr = cmp_attr_sparc;
	ops.copy_attr = sparc_copy_attr;
	op_sparc_AddSP = new_ir_op(cur_opcode + iro_sparc_AddSP, "sparc_AddSP", op_pin_state_floats, N|M, oparity_trinary, 0, sizeof(sparc_attr_t), &ops);
	set_op_tag(op_sparc_AddSP, sparc_op_tag);

	sparc_opcode_end = cur_opcode + iro_sparc_last;
}
