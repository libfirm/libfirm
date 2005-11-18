#ifndef _EMITTER_H_
#define _EMITTER_H_

#include "irnode.h"

char *get_dest_reg(ir_node *n, int num);

char *get_source_reg(ir_node *n, int num);

char *node_const_to_str(ir_node *n);

char *node_offset_to_str(ir_node *n);

#endif /* _EMITTER_H_ */
