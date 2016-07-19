#include "testlib.h"
#include "irprog_t.h"
#include "iroptimize.h"
#include "bitset.h"
#include "cloning_vector_t.h"
#include "call_sites.h"

struct obstack obst;

int main(int argc, const char **argv)
{
	(void)argc;
	setup(argv[1]);

	obstack_init(&obst);

	// Get call site info
	call_sites_t call_sites;
	call_sites_init(&call_sites);

	ir_graph *irg = irg_get_by_name("f");
	assert(call_sites_get_n_calls_to(&call_sites, irg) == 2);
	ir_node **calls = call_sites_get_calls_to(&call_sites, irg);

	bitset_t *vips = bitset_obstack_alloc(&obst, 1);
	bitset_set_all(vips);

	cloning_vector_t cv1 = cv_new(calls[0], vips, &obst);
	cloning_vector_t cv2 = cv_new(calls[1], vips, &obst);

	assert(cv_equal(cv1, cv2));
	assert(cv_hash(cv1) == cv_hash(cv2));

	obstack_free(&obst, 0);
	teardown();
}
