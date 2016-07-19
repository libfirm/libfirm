#include <stdbool.h>
#include "cloning_vector_t.h"

static bool constlike_equal(const ir_node *a, const ir_node *b)
{
	if (a == b) return true;
	if (a == NULL || b == NULL) return false;

	assert(is_irn_constlike(a));
	assert(is_irn_constlike(b));
	assert(get_irn_arity(a) == 0);
	assert(get_irn_arity(b) == 0);

	if ((get_irn_op(a) != get_irn_op(b)) ||
	    (get_irn_mode(a) != get_irn_mode(b)))
		return false;

	// the nodes have the same type, now check their attributes
	return a->op->ops.attrs_equal(a, b);
}

cloning_vector_t cv_new(const ir_node *call, const bitset_t *callee_vips,
                        struct obstack *obst)
{
	assert(call != NULL);
	assert(callee_vips != NULL);

	size_t const n_params = get_Call_n_params(call);
	cloning_vector_t cv   = NEW_ARR_DZ(ir_node *, obst, n_params);

	bitset_foreach (callee_vips, i) {
		ir_node *const arg = get_Call_param(call, i);
		cv[i]              = is_irn_constlike(arg) ? arg : NULL;
	}
	return cv;
}

size_t cv_get_size(const cloning_vector_t cv)
{
	assert(cv != NULL);

	size_t size = 0;
	cv_foreach (cv, i, val)
		++size;

	return size;
}

ir_node *cv_get(const cloning_vector_t cv, size_t pos)
{
	assert(cv != NULL);
	assert(pos < ARR_LEN(cv));
	return cv[pos];
}

bool cv_equal(const cloning_vector_t a, const cloning_vector_t b)
{
	if (a == b) return true;

	// TODO Review for var_args
	if (ARR_LEN_SAFE(a) != ARR_LEN_SAFE(b)) return false;

	ARR_FOREACH (a, i, ir_node *, node_a) {
		if (!constlike_equal(node_a, cv_get(b, i))) return false;
	}

	return true;
}

unsigned cv_hash(const cloning_vector_t cv)
{
	assert(cv != NULL);
	size_t n = ARR_LEN(cv);
	unsigned node_hashes[n];
	ARR_FOREACH (cv, i, ir_node *, node) {
		node_hashes[i] = node == NULL ? 0 : node->op->ops.hash(node);
	}
	return hash_data((unsigned char *)node_hashes, n * (sizeof *node_hashes));
}

size_t cv_get_new_idx(const cloning_vector_t cv, size_t idx)
{
	assert(cv != NULL);
	assert(!cv_get(cv, idx));

	size_t new_idx = idx;
	cv_foreach (cv, i, val) {
		if (i > idx) break;
		--new_idx;
	}

	assert(new_idx <= idx);
	return new_idx;
}

bitset_t *cv_get_undef(cloning_vector_t cv, struct obstack *obst)
{
	assert(cv != NULL);

	bitset_t *undef = bitset_obstack_alloc(obst, ARR_LEN(cv));
	bitset_set_all(undef);
	cv_foreach (cv, i, val)
		bitset_clear(undef, i);

	return undef;
}
