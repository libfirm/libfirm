#ifndef _IA32_NODES_ATTR_H_
#define _IA32_NODES_ATTR_H_

#include "firm_types.h"
#include "../bearch.h"

typedef enum { flavour_Div, flavour_Mod, flavour_DivMod } divmod_flavour_t;
typedef enum { pn_EAX, pn_EDX } pn_ia32_Register;

typedef struct {
  enum { asmop_Const, asmop_SymConst } tp;

  tarval  *tv;       /**<< tarval for immediate operations */
  tarval  *offset;   /**<< offset for LEA */
  ir_node *old_ir;   /**<< old ir node to avoid duplicating information (symconst in case of asmop_SymConst) */

  divmod_flavour_t dm_flav;   /**<< flavour of a DivMod (flavour_Div/Mod/DivMod) */

  unsigned n_res;      /**<< number of results */

  arch_irn_flags_t    flags;     /**<< indicating if spillable and/or rematerializeable */

  const arch_register_req_t **in_req;  /**<< register requirements for arguments */
  const arch_register_req_t **out_req; /**<< register requirements for results */

  const arch_register_t **in;          /**<< register slots for arguments */
  const arch_register_t **out;         /**<< register slots for results */
} asmop_attr;

#endif /* _IA32_NODES_ATTR_H_ */
