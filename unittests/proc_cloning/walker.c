#include "testlib.h"
#include "irprog_t.h"
#include "iroptimize.h"
#include "call_sites.h"
#include "firm_types.h"
#include "irnode.h"

int main(int argc, const char **argv)
{
	(void)argc;
	setup(argv[1]);

	assert_long_equal(2, get_irp_n_irgs());
	proc_cloning(23);

	assert_long_equal(3, get_irp_n_irgs());
	assert(irg_get_by_name("main"));
	assert(irg_get_by_name("walkTree"));

	assure_callgraph_consistent();

	ir_graph *main_f = irg_get_by_name("main");
	assert_long_equal(1, get_irg_n_callees(main_f));

	ir_graph *walk_tree_clone = get_irg_callee(main_f, 0);
	assert(irg_get_by_name("walkTree") != walk_tree_clone);
	ir_type *walker_type = get_entity_type(get_irg_entity(walk_tree_clone));
	assert_long_equal(1, get_method_n_params(walker_type));
	assert_long_equal(1, get_irg_n_callees(walk_tree_clone));
	assert(get_irg_callee(walk_tree_clone, 0) == walk_tree_clone);

	teardown();
}
