// PROJ_1(load32(VAR_1,VAR_0)) ==> ia32_Load
static ir_node *transform_f49(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	if (!(is_Proj(node) && get_Proj_num(node) == 1)) {
	return NULL;
	}
	if (!(is_Load(get_Proj_pred(node)) && (get_Load_mode(get_Proj_pred(node)) == mode_Is || get_Load_mode(get_Proj_pred(node)) == mode_Iu || get_Load_mode(get_Proj_pred(node)) == mode_P))) {
		return NULL;
	}
	if (var1 == NULL || var1 == get_irn_n(get_Proj_pred(node), 0)) {
		var1 = get_irn_n(get_Proj_pred(node), 0);
	} else {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(get_Proj_pred(node), 1)) {
		var0 = get_irn_n(get_Proj_pred(node), 1);
	} else {
		return NULL;
	}
	ir_node *pred = be_transform_node(get_Proj_pred(node));
	return be_new_Proj(pred, 0);
}
// PROJ_0(load32(VAR_1,VAR_0)) ==> ia32_Load
static ir_node *transform_f50(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	// ProjM hack taken from old instruction selector
	if (!is_Proj(node)) {
		return NULL;
	}
	ir_node *pred = get_Proj_pred(node);
	unsigned pn   = get_Proj_num(node);
	if (is_Load(pred) && pn == pn_Load_M) {
		if (get_irn_n_edges(pred) > 1) {
			/* this is needed, because sometimes we have loops that are only
			   reachable through the ProjM */
			be_enqueue_preds(node);
			/* do it in 2 steps, to silence firm verifier */
			ir_node *const res = new_r_Proj(pred, mode_M, pn_Load_M);
			set_Proj_num(res, pn_ia32_M);
			return res;
		} else {
			ir_node *new_pred = be_transform_node(pred);
			return be_new_Proj(new_pred, pn_ia32_Load_M);
		}
	} else {
		return NULL;
	}
}
// load32(VAR_1,VAR_0) ==> ia32_Load
static ir_node *transform_f51(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	if (!(is_Load(node) && (get_Load_mode(node) == mode_Is || get_Load_mode(node) == mode_Iu || get_Load_mode(node) == mode_P))) {
		return NULL;
	}
	if (var1 == NULL || var1 == get_irn_n(node, 0)) {
		var1 = get_irn_n(node, 0);
	} else {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(node, 1)) {
		var0 = get_irn_n(node, 1);
	} else {
		return NULL;
	}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Load(dbgi, block, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), noreg_GP, is_ia32_Immediate(var1) ? var1 : be_transform_node(var1)); set_ia32_ls_mode(new_node, get_Load_mode(node));
	return new_node;
}
// Eor32(IMM_1,VAR_0) ==> ia32_Xor
static ir_node *transform_f52(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	if (!(is_Eor(node) && (get_irn_mode(node) == mode_Is || get_irn_mode(node) == mode_Iu || get_irn_mode(node) == mode_P))) {
		return NULL;
	}
	if (x86_match_immediate(&tmp_imm, get_irn_n(node, 0), 'g')) {
		if (var1 == NULL) {
			var1 = ia32_create_Immediate_full(get_irn_irg(block), &tmp_imm);
		} else if (!x86_imm32_equal(&get_ia32_immediate_attr(var1)->imm, &tmp_imm)) {
			return NULL;
		}
	} else {
	return NULL;
}
	if (var0 == NULL || var0 == get_irn_n(node, 1)) {
		var0 = get_irn_n(node, 1);
	} else {
		return NULL;
	}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Xor(dbgi, block, noreg_GP, noreg_GP, nomem, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), is_ia32_Immediate(var1) ? var1 : be_transform_node(var1)); set_ia32_commutative(new_node); set_ia32_ls_mode(new_node, get_irn_mode(node)); set_ia32_am_support(new_node, ia32_am_none);
	return new_node;
}
// Eor32(VAR_0,IMM_1) ==> ia32_Xor
static ir_node *transform_f53(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	if (!(is_Eor(node) && (get_irn_mode(node) == mode_Is || get_irn_mode(node) == mode_Iu || get_irn_mode(node) == mode_P))) {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(node, 0)) {
		var0 = get_irn_n(node, 0);
	} else {
		return NULL;
	}
	if (x86_match_immediate(&tmp_imm, get_irn_n(node, 1), 'g')) {
		if (var1 == NULL) {
			var1 = ia32_create_Immediate_full(get_irn_irg(block), &tmp_imm);
		} else if (!x86_imm32_equal(&get_ia32_immediate_attr(var1)->imm, &tmp_imm)) {
			return NULL;
		}
	} else {
	return NULL;
}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Xor(dbgi, block, noreg_GP, noreg_GP, nomem, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), is_ia32_Immediate(var1) ? var1 : be_transform_node(var1)); set_ia32_commutative(new_node); set_ia32_ls_mode(new_node, get_irn_mode(node)); set_ia32_am_support(new_node, ia32_am_none);
	return new_node;
}
// Eor32(VAR_0,VAR_1) ==> ia32_Xor
static ir_node *transform_f54(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	if (!(is_Eor(node) && (get_irn_mode(node) == mode_Is || get_irn_mode(node) == mode_Iu || get_irn_mode(node) == mode_P))) {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(node, 0)) {
		var0 = get_irn_n(node, 0);
	} else {
		return NULL;
	}
	if (var1 == NULL || var1 == get_irn_n(node, 1)) {
		var1 = get_irn_n(node, 1);
	} else {
		return NULL;
	}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Xor(dbgi, block, noreg_GP, noreg_GP, nomem, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), is_ia32_Immediate(var1) ? var1 : be_transform_node(var1)); set_ia32_commutative(new_node); set_ia32_ls_mode(new_node, get_irn_mode(node)); set_ia32_am_support(new_node, ia32_am_none);
	return new_node;
}
// Eor32(VAR_1,VAR_0) ==> ia32_Xor
static ir_node *transform_f55(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;

	if (!(is_Eor(node) && (get_irn_mode(node) == mode_Is || get_irn_mode(node) == mode_Iu || get_irn_mode(node) == mode_P))) {
		return NULL;
	}
	if (var1 == NULL || var1 == get_irn_n(node, 0)) {
		var1 = get_irn_n(node, 0);
	} else {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(node, 1)) {
		var0 = get_irn_n(node, 1);
	} else {
		return NULL;
	}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Xor(dbgi, block, noreg_GP, noreg_GP, nomem, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), is_ia32_Immediate(var1) ? var1 : be_transform_node(var1)); set_ia32_commutative(new_node); set_ia32_ls_mode(new_node, get_irn_mode(node)); set_ia32_am_support(new_node, ia32_am_none);
	return new_node;
}
// Add32(PROJ_1(load32(VAR_1,VAR_0)),VAR_2) ==> ia32_Add_src
static ir_node *transform_f56(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;
	ir_node *var2 = NULL;

	if (!(is_Add(node) && (get_irn_mode(node) == mode_Is || get_irn_mode(node) == mode_Iu || get_irn_mode(node) == mode_P))) {
		return NULL;
	}
	if (!(is_Proj(get_irn_n(node, 0)) && get_Proj_num(get_irn_n(node, 0)) == 1 && get_irn_n_edges(get_irn_n(node, 0)) == 1)) {
	return NULL;
	}
	if (!(is_Load(get_Proj_pred(get_irn_n(node, 0))) && (get_Load_mode(get_Proj_pred(get_irn_n(node, 0))) == mode_Is || get_Load_mode(get_Proj_pred(get_irn_n(node, 0))) == mode_Iu || get_Load_mode(get_Proj_pred(get_irn_n(node, 0))) == mode_P) && get_irn_n_edges(get_Proj_pred(get_irn_n(node, 0))) == 1)) {
		return NULL;
	}
	if (var1 == NULL || var1 == get_irn_n(get_Proj_pred(get_irn_n(node, 0)), 0)) {
		var1 = get_irn_n(get_Proj_pred(get_irn_n(node, 0)), 0);
	} else {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(get_Proj_pred(get_irn_n(node, 0)), 1)) {
		var0 = get_irn_n(get_Proj_pred(get_irn_n(node, 0)), 1);
	} else {
		return NULL;
	}
	if (var2 == NULL || var2 == get_irn_n(node, 1)) {
		var2 = get_irn_n(node, 1);
	} else {
		return NULL;
	}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Add(dbgi, block, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), noreg_GP, is_ia32_Immediate(var1) ? var1 : be_transform_node(var1), is_ia32_Immediate(var2) ? var2 : be_transform_node(var2), noreg_GP); set_ia32_ls_mode(new_node, get_irn_mode(node)); set_ia32_op_type(new_node, ia32_AddrModeS); set_irn_mode(new_node, mode_T);
		new_node = be_new_Proj(new_node, 0);
	return new_node;
}
// PROJ_0(load32(VAR_1,VAR_0)) ==> ia32_Add_src
static ir_node *transform_f57(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;
	ir_node *var2 = NULL;

	// ProjM hack taken from old instruction selector
	if (!is_Proj(node)) {
		return NULL;
	}
	ir_node *pred = get_Proj_pred(node);
	unsigned pn   = get_Proj_num(node);
	if (is_Load(pred) && pn == pn_Load_M) {
		if (get_irn_n_edges(pred) > 1) {
			/* this is needed, because sometimes we have loops that are only
			   reachable through the ProjM */
			be_enqueue_preds(node);
			/* do it in 2 steps, to silence firm verifier */
			ir_node *const res = new_r_Proj(pred, mode_M, pn_Load_M);
			set_Proj_num(res, pn_ia32_M);
			return res;
		} else {
			ir_node *new_pred = be_transform_node(pred);
			return be_new_Proj(new_pred, pn_ia32_Load_M);
		}
	} else {
		return NULL;
	}
}
// Add32(VAR_2,PROJ_1(load32(VAR_1,VAR_0))) ==> ia32_Add_src
static ir_node *transform_f58(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;
	ir_node *var2 = NULL;

	if (!(is_Add(node) && (get_irn_mode(node) == mode_Is || get_irn_mode(node) == mode_Iu || get_irn_mode(node) == mode_P))) {
		return NULL;
	}
	if (var2 == NULL || var2 == get_irn_n(node, 0)) {
		var2 = get_irn_n(node, 0);
	} else {
		return NULL;
	}
	if (!(is_Proj(get_irn_n(node, 1)) && get_Proj_num(get_irn_n(node, 1)) == 1 && get_irn_n_edges(get_irn_n(node, 1)) == 1)) {
	return NULL;
	}
	if (!(is_Load(get_Proj_pred(get_irn_n(node, 1))) && (get_Load_mode(get_Proj_pred(get_irn_n(node, 1))) == mode_Is || get_Load_mode(get_Proj_pred(get_irn_n(node, 1))) == mode_Iu || get_Load_mode(get_Proj_pred(get_irn_n(node, 1))) == mode_P) && get_irn_n_edges(get_Proj_pred(get_irn_n(node, 1))) == 1)) {
		return NULL;
	}
	if (var1 == NULL || var1 == get_irn_n(get_Proj_pred(get_irn_n(node, 1)), 0)) {
		var1 = get_irn_n(get_Proj_pred(get_irn_n(node, 1)), 0);
	} else {
		return NULL;
	}
	if (var0 == NULL || var0 == get_irn_n(get_Proj_pred(get_irn_n(node, 1)), 1)) {
		var0 = get_irn_n(get_Proj_pred(get_irn_n(node, 1)), 1);
	} else {
		return NULL;
	}
	ir_node *new_node = NULL;
	new_node = new_bd_ia32_Add(dbgi, block, is_ia32_Immediate(var0) ? var0 : be_transform_node(var0), noreg_GP, is_ia32_Immediate(var1) ? var1 : be_transform_node(var1), is_ia32_Immediate(var2) ? var2 : be_transform_node(var2), noreg_GP); set_ia32_ls_mode(new_node, get_irn_mode(node)); set_ia32_op_type(new_node, ia32_AddrModeS); set_irn_mode(new_node, mode_T);
		new_node = be_new_Proj(new_node, 0);
	return new_node;
}
// PROJ_0(load32(VAR_1,VAR_0)) ==> ia32_Add_src
static ir_node *transform_f59(ir_node *node, ir_node *block, dbg_info *dbgi)
{
	(void)block;
	(void)dbgi;
	x86_imm32_t tmp_imm;
	(void)tmp_imm;
	ir_node *var0 = NULL;
	ir_node *var1 = NULL;
	ir_node *var2 = NULL;

	// ProjM hack taken from old instruction selector
	if (!is_Proj(node)) {
		return NULL;
	}
	ir_node *pred = get_Proj_pred(node);
	unsigned pn   = get_Proj_num(node);
	if (is_Load(pred) && pn == pn_Load_M) {
		if (get_irn_n_edges(pred) > 1) {
			/* this is needed, because sometimes we have loops that are only
			   reachable through the ProjM */
			be_enqueue_preds(node);
			/* do it in 2 steps, to silence firm verifier */
			ir_node *const res = new_r_Proj(pred, mode_M, pn_Load_M);
			set_Proj_num(res, pn_ia32_M);
			return res;
		} else {
			ir_node *new_pred = be_transform_node(pred);
			return be_new_Proj(new_pred, pn_ia32_Load_M);
		}
	} else {
		return NULL;
	}
}
ir_node *ia32_autotransform(ir_node *node, int *rule)
{
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *new_node = NULL;
	*rule = -1;

	new_node = transform_f49(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f50(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f51(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f52(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f53(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f54(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f55(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f56(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f57(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f58(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	new_node = transform_f59(node, block, dbgi);
	if (new_node != NULL) {
		return new_node;
	}
	return NULL;
}
