/**
 * Instructions
 *
 * A data structure to treat nodes and node-proj collections uniformly.
 */

#ifndef _BEINSN_T_H
#define _BEINSN_T_H

#include "bitset.h"

#include "bearch.h"

typedef struct _be_operand_t  be_operand_t;
typedef struct _be_insn_t     be_insn_t;
typedef struct _be_insn_env_t be_insn_env_t;

struct _be_operand_t {
	ir_node *irn;
	ir_node *carrier;
	be_operand_t *partner;
	bitset_t *regs;
	int pos;
	arch_register_req_t req;
	unsigned has_constraints : 1;
};

struct _be_insn_t {
	be_operand_t *ops;
	int n_ops;
	int use_start;
	ir_node *next_insn;
	ir_node *irn;
	unsigned in_constraints  : 1;
	unsigned out_constraints : 1;
	unsigned has_constraints : 1;
	unsigned pre_colored     : 1;
};

struct _be_insn_env_t {
	struct obstack              *obst;
	const arch_env_t            *aenv;
	const arch_register_class_t *cls;
	bitset_t                    *ignore_colors;
};

#define be_insn_n_defs(insn) ((insn)->use_start)
#define be_insn_n_uses(insn) ((insn)->n_ops - (insn)->use_start)

be_insn_t *be_scan_insn(const be_insn_env_t *env, ir_node *irn);

#endif /* _BEINSN_T_H */
