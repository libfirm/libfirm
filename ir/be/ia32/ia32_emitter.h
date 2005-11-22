#ifndef _IA32_EMITTER_H_
#define _IA32_EMITTER_H_

#include "irnode.h"

const char *get_dest_reg_name(ir_node *n, int num);

const char *get_source_reg_name(ir_node *n, int num);

char *node_const_to_str(ir_node *n);

char *node_offset_to_str(ir_node *n);

void equalize_dest_src(FILE *F, ir_node *n);

#endif /* _IA32_EMITTER_H_ */
