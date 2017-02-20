#include "firm.h"
#include "irprog.h"
#include "typerep.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

int main(void)
{
	ir_init();

	ir_type   *type   = new_type_primitive(get_modeIs());
	ident     *id1    = new_id_from_str("foo");
	ir_type   *glob   = get_glob_type();
	ir_entity *x      = new_global_entity(glob, id1, type, ir_visibility_external, IR_LINKAGE_DEFAULT);
	assert(get_entity_owner(x) == glob);

	ident     *id2    = new_id_from_str("bar");
	ir_type   *cls    = new_type_class(id2);
	set_entity_owner(x, cls);
	assert(get_entity_owner(x) == cls);

	ir_entity *gx = ir_get_global(id1);
	assert(gx == NULL);

	set_entity_owner(x, glob);
	assert(get_entity_owner(x) == glob);

	return 0;
}
