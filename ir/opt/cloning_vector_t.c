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

	// We never clone on variadic arguments. So to simplifiy things further down
	// the road, the CV's size matches the number of non-variadic arguments.
	ir_type const *const callee_type = get_entity_type(get_Call_callee(call));
	size_t const n_params            = get_method_n_params(callee_type);
	cloning_vector_t cv              = NEW_ARR_DZ(ir_node *, obst, n_params);

	bitset_foreach (callee_vips, i) {
		ir_node *const arg = get_Call_param(call, i);

		// TODO This excludes sums of address and offset from CVs
		// To support them, we need following additional functionality:
		//   - constant checking for subgraphs
		//   - equality checking for constant subgraphs
		//   - hashing for constant subgraphs
		//   - copying of subgraph arguments to the clone graph
		cv[i] = is_irn_constlike(arg) ? arg : NULL;
	}
	return cv;
}

// TODO Could be calculated on creation
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
	return pos < ARR_LEN(cv) ? cv[pos] : NULL;
}

bool cv_equal(const cloning_vector_t a, const cloning_vector_t b)
{
	if (a == b) return true;

	// Matching CVs must have the same length because they never include
	// variadic arguments
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
