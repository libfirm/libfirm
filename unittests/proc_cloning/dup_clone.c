#include "testlib.h"
#include "irprog_t.h"
#include "iroptimize.h"

int main(int argc, const char **argv)
{
	(void)argc;
	setup(argv[1]);

	assert_long_equal(2, get_irp_n_irgs());
	proc_cloning(23);

	assert_long_equal(3, get_irp_n_irgs());
	assert(irg_get_by_name("main"));
	assert(irg_get_by_name("f"));

	teardown();
	return 0;
}
