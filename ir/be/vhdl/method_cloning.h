
#ifndef LIBFIRM_METHOD_CLONING_H
#define LIBFIRM_METHOD_CLONING_H

#include "firm_types.h"

void create_clone_proc_irg(ir_entity *);
ir_entity *clone_method(ir_graph *);

#endif //LIBFIRM_METHOD_CLONING_H
