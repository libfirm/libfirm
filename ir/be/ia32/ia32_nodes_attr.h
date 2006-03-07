#ifndef _IA32_NODES_ATTR_H_
#define _IA32_NODES_ATTR_H_

#include <obstack.h>

#include "firm_types.h"
#include "../bearch.h"

typedef enum { flavour_Div = 1, flavour_Mod, flavour_DivMod } ia32_op_flavour_t;
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

enum {
	ia32_O = (1 << 0),
	ia32_B = (1 << 1),
	ia32_I = (1 << 2),
	ia32_S = (1 << 3)
};

typedef enum {
	ia32_am_N    = 0,
	ia32_am_O    = ia32_O,
	ia32_am_B    = ia32_B,
	ia32_am_I    = ia32_I,
	ia32_am_IS   = ia32_I | ia32_S,
	ia32_am_OB   = ia32_O | ia32_B,
	ia32_am_OI   = ia32_O | ia32_I,
	ia32_am_OIS  = ia32_O | ia32_I | ia32_S,
	ia32_am_OBIS = ia32_O | ia32_B | ia32_I | ia32_S
} ia32_am_flavour_t;

typedef struct _ia32_register_req_t {
	const arch_register_req_t req;
	int same_pos;        /**< in case of "should be same" we need to remember the pos to get the irn */
	int different_pos;   /**< in case of "should be different" we need to remember the pos to get the irn */
} ia32_register_req_t;

typedef struct _ia32_attr_t {
	struct {
		unsigned tp:3;              /**< ia32 node type */

		unsigned am_support:2;      /**< indicates addrmode type supported by this node */
		unsigned am_flavour:4;      /**< the concrete addrmode characteristics */
		unsigned am_scale:2;        /**< addrmode scale for index register */

		unsigned offs_sign:1;       /**< sign bit of the first offset */

		unsigned use_frame:1;       /**< indicates whether the operation uses the frame pointer or not */

		unsigned op_flav:2;         /**< flavour of an op (flavour_Div/Mod/DivMod) */

		unsigned flags:4;           /**< indicating if spillable and/or rematerializeable */

		unsigned is_commutative:1;  /**< indicates whether op is commutative or not */

		unsigned n_res:10;          /**< number of results produced by this node */
	} data;

	struct obstack *am_offs;    /**< offsets for AddrMode */

	tarval *tv;   /**< tarval for immediate operations */
	char   *sc;   /**< symconst name */
	char   *cnst; /**< points to the string representation of the constant value (either tv or sc) */

	ir_mode *ls_mode;  /**< the mode of the stored/loaded value */

	entity *frame_ent; /**< the frame entity attached to this node */

	long pn_code;   /**< projnum "types" (e.g. indicate compare operators and argument numbers) */

	const ia32_register_req_t **in_req;  /**< register requirements for arguments */
	const ia32_register_req_t **out_req; /**< register requirements for results */

	const arch_register_t **slots;          /**< register slots for assigned registers */
} ia32_attr_t;

#endif /* _IA32_NODES_ATTR_H_ */
