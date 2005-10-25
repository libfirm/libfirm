#ifndef FIRM2ARCH_H
#define FIRM2ARCH_H

#include <libfirm/firm.h>

void create_bearch_asm_opcodes(void);

void transform_firm(void);

extern void transform_node(ir_node *node, void *env);

void finish_transform(void);

extern void finish_node_transformation(ir_node *node, void *env);

void firmbe_gen_code(FILE *out);

extern void firmbe_gen_decls(FILE *out);
extern void firmbe_gen_routine(FILE *out, ir_graph *irg);

#endif
