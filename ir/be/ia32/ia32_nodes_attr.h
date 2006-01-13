#ifndef _IA32_NODES_ATTR_H_
#define _IA32_NODES_ATTR_H_

#include "firm_types.h"
#include "../bearch.h"

typedef enum { flavour_Div = 1, flavour_Mod, flavour_DivMod } divmod_flavour_t;
typedef enum { pn_EAX, pn_EDX } pn_ia32_Register;
typedef enum { asmop_Normal, asmop_Const, asmop_SymConst, asmop_AddrMode } asmop_type_t;
typedef	enum {
	am_Reg = 1,          /**<<  (%reg)              */
	am_OffsReg,          /**<< o(%reg)              */
	am_RegReg,           /**<<  (%reg, %reg)        */
	am_RegConst,         /**<<  (    , %reg, const) */
	am_OffsRegConst,     /**<< o(    , %reg, const) */
	am_OffsRegReg,       /**<< o(%reg, %reg)        */
	am_RegRegConst,      /**<<  (%reg, %reg, const) */
	am_OffsRegRegConst   /**<< o(%reg, %reg, const) */
} addrmode_type_t;


typedef struct {
	asmop_type_t    tp;      /**<< ia32 node type */
	addrmode_type_t am_tp;   /**<< addr mode type */

	tarval  *am_offs;  /**<< offset for AddrMode */
	tarval  *am_const; /**<< shift const for AddrMode */

	tarval  *tv;       /**<< tarval for immediate operations */
	ir_node *old_ir;   /**<< old ir node to avoid duplicating information (symconst in case of asmop_SymConst) */

	divmod_flavour_t dm_flav;   /**<< flavour of a DivMod (flavour_Div/Mod/DivMod) */
	long             pn_code;   /**<< projnum "types" (e.g. indicate compare operators and argument numbers) */
	long             n_res;     /**<< number of results */
	arch_irn_flags_t flags;     /**<< indicating if spillable and/or rematerializeable */

	const arch_register_req_t **in_req;  /**<< register requirements for arguments */
	const arch_register_req_t **out_req; /**<< register requirements for results */

	const arch_register_t **slots;          /**<< register slots for assigned registers */
} asmop_attr;

#endif /* _IA32_NODES_ATTR_H_ */
