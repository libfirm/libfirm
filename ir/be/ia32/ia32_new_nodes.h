#ifndef _IA32_NEW_NODES_H_
#define _IA32_NEW_NODES_H_

/**
 * Function prototypes for the assembler ir node constructors.
 * @author Christian Wuerdig
 * $Id$
 */

#include "ia32_nodes_attr.h"

asmop_attr *get_ia32_attr(const ir_node *node);

ir_node *get_ia32_old_ir(const ir_node *node);

const char *get_sc_name(ir_node *symc);

void    set_ia32_Immop_attr(ir_node *node, ir_node *imm);
tarval *get_ia32_Immop_tarval(const ir_node *node);
void    set_ia32_Immop_tarval(ir_node *node, tarval *tv);

void set_ia32_Const_attr(ir_node *ia32_cnst, ir_node *cnst);
void set_ia32_Const_type(ir_node *node, int type);

void set_ia32_pncode(ir_node *node, long code);
long get_ia32_pncode(const ir_node *node);

void    set_ia32_offs(ir_node *node, tarval *offs);
tarval *get_ia32_offs(const ir_node *node);
void    set_ia32_n_res(ir_node *node, int n_res);
int     get_ia32_n_res(const ir_node *node);

void set_ia32_DivMod_flavour(ir_node *node, divmod_flavour_t dm_flav);

arch_irn_flags_t            get_ia32_flags(const ir_node *node);
const arch_register_req_t **get_ia32_in_req(const ir_node *node);
const arch_register_req_t **get_ia32_out_req(const ir_node *node);
const arch_register_t     **get_ia32_slots(const ir_node *node);

void set_ia32_regreq_out(ir_node *node, const arch_register_req_t *req, int pos);
void set_ia32_regreq_in(ir_node *node, const arch_register_req_t *req, int pos);

const char *get_ia32_out_reg_name(const ir_node *node, int pos);
int         get_ia32_out_regnr(const ir_node *node, int pos);

int is_ia32_irn(const ir_node *node);

/* Inlcude the generated headers */
#include "gen_ia32_new_nodes.h"

#endif /* _IA32_NEW_NODES_H_ */
