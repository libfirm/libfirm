#ifndef _PPC32_NODES_ATTR_H_
#define _PPC32_NODES_ATTR_H_

#include "../bearch.h"

typedef struct _ppc32_register_req_t {
	const arch_register_req_t req;
	int same_pos;        /**<< in case of "should be same" we need to remember the pos to get the irn */
	int different_pos;   /**<< in case of "should be different" we need to remember the pos to get the irn */
} ppc32_register_req_t;

typedef struct
{
	unsigned shift:5;
	unsigned maskA:5;
	unsigned maskB:5;
} rlwimi_const_t;


typedef enum {
	ppc32_ac_None, ppc32_ac_Const, ppc32_ac_SymConst, ppc32_ac_FrameEntity, ppc32_ac_RlwimiConst, ppc32_ac_BranchProj,
	ppc32_ac_Offset
} ppc32_attr_content_type;

typedef enum {
	ppc32_ao_None, ppc32_ao_Lo16, ppc32_ao_Hi16, ppc32_ao_Ha16, ppc32_ao_Illegal
} ppc32_attr_offset_mode;

typedef struct _ppc32_attr_t {
	arch_irn_flags_t flags;     /**<< indicating if spillable, rematerializeable ... etc. */
	int              n_res;     /**<< number of results for this node */

	const ppc32_register_req_t **in_req;  /**<< register requirements for arguments */
	const ppc32_register_req_t **out_req; /**<< register requirements for results */

	const arch_register_t **slots;          /**<< register slots for assigned registers */

	ppc32_attr_content_type content_type;
	ppc32_attr_offset_mode offset_mode;
	union {
		tarval *constant_tarval;
		ident *symconst_ident;
		entity *frame_entity;
		rlwimi_const_t rlwimi_const;
		int proj_nr;
		int offset;
		void* empty;
	};

} ppc32_attr_t;

#endif /* _PPC32_NODES_ATTR_H_ */
