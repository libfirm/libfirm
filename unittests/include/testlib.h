#include <stdio.h>
#include <stdlib.h>

#include "firm_common.h"
#include "irprog.h"
#include "irprog_t.h"
#include "irio.h"
#include "bitset.h"
#include "important_args.h"

#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_YELLOW "\x1b[33m"
#define ANSI_COLOR_RESET "\x1b[0m"

#define S1(x) #x
#define S2(x) S1(x)
#define LOCATION __FILE__ ":" S2(__LINE__)

#define FAILURE_MSG_TEMPLATE                                                   \
	"" ANSI_COLOR_RED "FAILURE: " ANSI_COLOR_RESET                             \
	"Expected " ANSI_COLOR_GREEN "0x%lx" ANSI_COLOR_RESET                      \
	" but was " ANSI_COLOR_RED "0x%lx" ANSI_COLOR_RESET " in " LOCATION "\n"

#define assert_long_equal(expected, actual)                                    \
	do {                                                                       \
		if ((actual) != (expected)) {                                          \
			fprintf(stderr, FAILURE_MSG_TEMPLATE, (long)(expected),            \
			        (long)(actual));                                           \
			abort();                                                           \
		}                                                                      \
	} while (0)

#define assert_local_important_args(proc, expected)                            \
	do {                                                                       \
		ir_graph *irg = irg_get_by_name((proc));                               \
		assert(irg);                                                           \
		bitset_t *actual = local_important_args(irg);                          \
		assert_long_equal((expected), *actual->data);                          \
	} while (0)

#define assert_important_args(proc, expected)                                  \
	do {                                                                       \
		ir_graph *irg = irg_get_by_name((proc));                               \
		assert(irg);                                                           \
		pmap *result     = important_args_get();                               \
		bitset_t *actual = pmap_get(bitset_t, result, irg);                    \
		assert_long_equal((expected), *actual->data);                          \
	} while (0)

ir_graph *irg_get_by_name(const char *name);
ir_graph *irg_get_by_name(const char *name)
{
	foreach_irp_irg (i, irg) {
		ir_entity *const entity    = get_irg_entity(irg);
		const char *const ent_name = get_entity_name(entity);
		if (!strcmp(name, ent_name)) {
			return irg;
		}
	}
	return NULL;
}

void setup(const char *irp_path);
void setup(const char *irp_path)
{
	ir_init();
	ir_import(irp_path);
}

void teardown(void);
void teardown(void)
{
	ir_finish();
}
