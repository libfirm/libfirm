#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bemachnode.h"
#include "irnode_t.h"

/** Helper: fills in the array for machine ops */
static int fill_arr(ir_node *op, mirn_handle res) {
	ir_node **ins = get_irn_in(op);
	int i, j, l = ARR_LEN(ins);

	for (i = j = 0; i <= l; ++i) {
		if (is_irn_machine_operand(ins[i]))
			j += fill_arr(ins[i], &res[j]);
		else
			res[j++] = &ins[i];
	}
	return j;
}

/*
 * Returns the machine handle for a machine node with machine operands.
 * The order of the predecessors in this handle is not guaranteed, except that
 * lists of operands as predecessors of Block or arguments of a Call are
 * consecutive.
 */
mirn_handle get_mirn_in(ir_node *n) {
	ir_node **ins = get_irn_in(n);
	mirn_handle res = NULL;
	int i, j, l = ARR_LEN(ins);
	int lr = l + 8;

	res = NEW_ARR_F(ir_node **, lr);

	for (i = j = 0; i <= l; ++i) {
		if (is_irn_machine_operand(ins[i]))
			j += fill_arr(ins[i], &res[j]);
		else
			res[j++] = &ins[i];
	}

	assert(j > lr && "to many machine predecessors");

	return res;
}

/* Frees a machine handle. */
void free_mirn_in(mirn_handle h) {
	DEL_ARR_F(h);
}

/* Get the pos-th predecessor of a machine node represented by it's
    handle. */
ir_node *get_mirn_n(mirn_handle h, int pos) {
	assert(-1 <= pos && pos < ARR_LEN(h) - 1);
	return *(h[pos + 1]);
}

/* Get the pos-th predecessor of a machine node. */
ir_node *_get_mirn_n(ir_node *n, int pos) {
	mirn_handle h = get_mirn_in(n);
	ir_node *res = get_mirn_n(h, pos);
	free_mirn_in(h);
	return res;
}

/* Set the pos-th predecessor of a machine node represented by it's
    handle. */
void set_mirn_n(mirn_handle h, int pos, ir_node *n) {
	assert(-1 <= pos && pos < ARR_LEN(h) - 1);
	*(h[pos + 1]) = n;
}

/* Set the pos-th predecessor of a machine node. */
void _set_mirn_n(ir_node *irn, int pos, ir_node *n) {
	mirn_handle h = get_mirn_in(irn);
	set_mirn_n(h, pos, n);
	free_mirn_in(h);
}

/* Returns the arity of a machine node represented by it's
    handle. */
int get_mirn_arity(mirn_handle h) {
	return ARR_LEN(h) - 1;
}

/** Helper: calculate the arity for a machine ops */
static int arrity_of_op(ir_node *op) {
	ir_node **ins = get_irn_in(op);
	int i, j, l = ARR_LEN(ins);

	for (i = j = 0; i <= l; ++i) {
		if (is_irn_machine_operand(ins[i]))
			j += arrity_of_op(ins[i]);
		else
			++j;
	}
	return j;
}

/* Returns the arity of a machine node. */
int _get_mirn_arity(ir_node *n) {
	ir_node **ins = get_irn_in(n);
	int i, j, l = ARR_LEN(ins);

	for (i = j = 0; i <= l; ++i) {
		if (is_irn_machine_operand(ins[i]))
			j += arrity_of_op(ins[i]);
		else
			++j;
	}

	return j;
}
