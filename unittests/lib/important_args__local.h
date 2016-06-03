#include <stdio.h>
#include <stdlib.h>

#include "firm_common.h"
#include "irprog.h"
#include "irio.h"
#include "bitset.h"
#include "important_args.h"

#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_YELLOW "\x1b[33m"
#define ANSI_COLOR_RESET "\x1b[0m"

#define FAILURE_MSG_TEMPLATE                                                   \
	ANSI_COLOR_RED "FAILURE: " ANSI_COLOR_RESET "Expected " ANSI_COLOR_GREEN   \
	               "0x%x" ANSI_COLOR_RESET " but was " ANSI_COLOR_RED          \
	               "0x%x" ANSI_COLOR_RESET " for %s\n"

void test_irg_has_expected_important_args(const char *irp_path,
                                          unsigned expected,
                                          const char *test_name);

void test_irg_has_expected_important_args(const char *irp_path,
                                          unsigned expected,
                                          const char *test_name)
{
	ir_init();

	ir_import(irp_path);

	assert(get_irp_n_irgs() == 1);
	ir_graph *proc = get_irp_irg(0);

	bitset_t *result = local_important_args(proc);
	unsigned actual  = *result->data;

	if (actual != expected) {
		fprintf(stderr, FAILURE_MSG_TEMPLATE, expected, actual, test_name);
		abort();
	}
	ir_finish();
}
