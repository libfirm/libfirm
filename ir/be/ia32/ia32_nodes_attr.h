#ifndef _IA32_NODES_ATTR_H_
#define _IA32_NODES_ATTR_H_

#include <obstack.h>

#include "firm_types.h"
#include "../bearch.h"

typedef enum { flavour_Div = 1, flavour_Mod, flavour_DivMod, flavour_Mul, flavour_Mulh } ia32_op_flavour_t;
typedef enum { pn_EAX, pn_EDX } pn_ia32_Register;
typedef enum { ia32_Normal, ia32_Const, ia32_SymConst, ia32_AddrModeD, ia32_AddrModeS } ia32_op_type_t;
typedef	enum {
	ia32_am_None   = 0,   /**<< no addrmode support */
	ia32_am_Dest   = 1,   /**<< addrmode for destination only */
	ia32_am_Source = 2,   /**<< addrmode for source only */
	ia32_am_Full   = 3    /**<< full addmode support */
} ia32_am_type_t;

/**
 * Different AM types:
 * O - Offset is set
 * B - Base is set
 * I - Index is set
 * S - Scale is set
 */
typedef enum {
	ia32_am_N = 0, ia32_am_O, ia32_am_B, ia32_am_I, ia32_am_IS, ia32_am_OB, ia32_am_OI, ia32_am_OIS, ia32_am_OBIS
} ia32_am_flavour_t;

typedef struct _ia32_register_req_t {
	const arch_register_req_t req;
	int same_pos;        /**<< in case of "should be same" we need to remember the pos to get the irn */
	int different_pos;   /**<< in case of "should be different" we need to remember the pos to get the irn */
} ia32_register_req_t;

typedef struct _ia32_attr_t {
	ia32_op_type_t    tp;           /**<< ia32 node type */
	ia32_am_type_t    am_support;   /**<< indicates addrmode type supported by this node */
	ia32_am_flavour_t am_flavour;   /**<< the concrete addrmode characteristics */

	struct obstack *am_offs;  /**<< offsets for AddrMode */
	int             am_scale; /**<< addrmode scale for index register */

	tarval *tv;   /**<< tarval for immediate operations */
	char   *sc;   /**<< symconst name */
	char   *cnst; /**<< points to the string representation of the constant value (either tv or sc) */

	ia32_op_flavour_t op_flav;   /**<< flavour of an op (flavour_Div/Mod/DivMod/Mul/Mulh) */
	long              pn_code;   /**<< projnum "types" (e.g. indicate compare operators and argument numbers) */
	long              n_res;     /**<< number of results */
	arch_irn_flags_t  flags;     /**<< indicating if spillable and/or rematerializeable */

	const ia32_register_req_t **in_req;  /**<< register requirements for arguments */
	const ia32_register_req_t **out_req; /**<< register requirements for results */

	const arch_register_t **slots;          /**<< register slots for assigned registers */
} ia32_attr_t;

#endif /* _IA32_NODES_ATTR_H_ */
