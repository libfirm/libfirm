#ifndef _BEARCH_MIPS_H_
#define _BEARCH_MIPS_H_

#include "../bearch_t.h"

typedef struct _mips_code_gen_t mips_code_gen_t;

extern const arch_isa_if_t mips_isa_if;

/** return the scheduled block at position pos */
ir_node *mips_get_sched_block(const mips_code_gen_t *cg, int pos);

/** return the number of scheduled blocks */
int mips_get_sched_n_blocks(const mips_code_gen_t *cg);

/** set a block schedule number */
void mips_set_block_sched_nr(ir_node *block, int nr);

/** get a block schedule number */
int mips_get_block_sched_nr(ir_node *block);

#endif /* _BEARCH_MIPS_H_ */
