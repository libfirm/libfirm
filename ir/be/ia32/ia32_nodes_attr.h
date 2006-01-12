#ifndef _IA32_NODES_ATTR_H_
#define _IA32_NODES_ATTR_H_

#include "firm_types.h"
#include "../bearch.h"

typedef enum { flavour_Div, flavour_Mod, flavour_DivMod } divmod_flavour_t;
typedef enum { pn_EAX, pn_EDX } pn_ia32_Register;

typedef struct {
  enum { asmop_Const, asmop_SymConst, asmop_AddrMode } tp;

  tarval  *tv;       /**<< tarval for immediate operations */
  tarval  *offset;   /**<< offset for AddrMode */
  ir_node *old_ir;   /**<< old ir node to avoid duplicating information (symconst in case of asmop_SymConst) */

  divmod_flavour_t dm_flav;   /**<< flavour of a DivMod (flavour_Div/Mod/DivMod) */

  long pn_code;			/**<< projnum "types" (e.g. indicates the compare operator of a conditional jump
                          or an argument number) */

  unsigned n_res;      /**<< number of results */

  arch_irn_flags_t    flags;     /**<< indicating if spillable and/or rematerializeable */

  const arch_register_req_t **in_req;  /**<< register requirements for arguments */
  const arch_register_req_t **out_req; /**<< register requirements for results */

  const arch_register_t **slots;          /**<< register slots for assigned registers */
} asmop_attr;

#endif /* _IA32_NODES_ATTR_H_ */
