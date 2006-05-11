#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irop.h"
#include "irnode_t.h"
#include "ircons.h"
#include "irprog_t.h"
#include "lower_intrinsics.h"
#include "lower_dw.h"
#include "mangle.h"
#include "array.h"

/** The array of all intrinsics that must be mapped. */
static i_record *intrinsics;

/** An array to cache all entities */
static entity *i_ents[iro_MaxOpcode];

/**
 * Maps all intrinsic calls that the backend support
 * and map all instructions the backend did not support
 * to runtime calls.
 */
void ia32_handle_intrinsics(void) {
	if (intrinsics && ARR_LEN(intrinsics) > 0)
		lower_intrinsics(intrinsics, ARR_LEN(intrinsics));
}

#define BINOP_Left_Low   0
#define BINOP_Left_High  1
#define BINOP_Right_Low  2
#define BINOP_Right_High 3

/**
 * Map an Add (a_l, a_h, b_l, b_h)
 */
static int map_Add(ir_node *call, void *ctx) {
	ir_graph *irg = current_ir_graph;
	ir_node *block = get_nodes_block(call);
	ir_node **params = get_Call_param_arr(call);
	ir_node *l_res, *h_res, *res, *in[2];
	ir_node *a_l = params[0];
	ir_node *a_h = params[1];
	ir_node *b_l = params[2];
	ir_node *b_h = params[3];

	/* l_res = a_l + b_l */
	/* h_res = a_h + b_h + carry */

	in[0] = l_res;
	in[1] = h_res;
	res = new_r_Tuple(irg, block, 2, in);

	turn_into_tuple(call, pn_Call_max);
	set_Tuple_pred(call, pn_Call_M_regular,        get_irg_no_mem(irg));
	set_Tuple_pred(call, pn_Call_X_except,         get_irg_bad(irg));
	set_Tuple_pred(call, pn_Call_T_result,         res);
	set_Tuple_pred(call, pn_Call_M_except,         get_irg_bad(irg));
	set_Tuple_pred(call, pn_Call_P_value_res_base, get_irg_bad(irg));

	return 1;
}

/**
 * Map a Sub (a_l, a_h, b_l, b_h)
 */
static int map_Sub(ir_node *call, void *ctx) {
	ir_graph *irg = current_ir_graph;
	ir_node *block = get_nodes_block(call);
	ir_node **params = get_Call_param_arr(call);
	ir_node *l_res, *h_res, *res, *in[2];
	ir_node *a_l = params[0];
	ir_node *a_h = params[1];
	ir_node *b_l = params[2];
	ir_node *b_h = params[3];

	/* l_res = a_l - b_l */
	/* h_res = a_h - b_h - carry */

	in[0] = l_res;
	in[1] = h_res;
	res = new_r_Tuple(irg, block, 2, in);

	turn_into_tuple(call, pn_Call_max);
	set_Tuple_pred(call, pn_Call_M_regular,        get_irg_no_mem(irg));
	set_Tuple_pred(call, pn_Call_X_except,         get_irg_bad(irg));
	set_Tuple_pred(call, pn_Call_T_result,         res);
	set_Tuple_pred(call, pn_Call_M_except,         get_irg_bad(irg));
	set_Tuple_pred(call, pn_Call_P_value_res_base, get_irg_bad(irg));

	return 1;
}

/**
 * Ia32 implementation.
 *
 * @param method   the method type of the emulation function entity
 * @param op       the emulated ir_op
 * @param imode    the input mode of the emulated opcode
 * @param omode    the output mode of the emulated opcode
 * @param context  the context parameter
 */
entity *ia32_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                  const ir_mode *imode, const ir_mode *omode,
                                  void *context)
{
	i_record      elt;
	entity        **ent = NULL;
	i_mapper_func mapper;

	if (! intrinsics)
		intrinsics = NEW_ARR_F(i_record, 0);

	switch (get_op_code(op)) {
	case iro_Add:
		ent    = &i_ents[iro_Add];
		mapper = map_Add;
		break;
	case iro_Sub:
		ent    = &i_ents[iro_Sub];
		mapper = map_Sub;
		break;
	default:
		return def_create_intrinsic_fkt(method, op, imode, omode, context);
	}

	if (ent && ! *ent) {
#define IDENT(s)  new_id_from_chars(s, sizeof(s)-1)

		ident *id = mangle(IDENT("L"), get_op_name(op));
		*ent = new_entity(get_glob_type(), id, method);
	}

	elt.i_call.kind     = INTRINSIC_CALL;
	elt.i_call.i_ent    = *ent;
	elt.i_call.i_mapper = mapper;
	elt.i_call.ctx      = NULL;
	elt.i_call.link     = NULL;

	ARR_APP1(i_record, intrinsics, elt);
}
