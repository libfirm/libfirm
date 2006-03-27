#ifndef _ARM_NODES_ATTR_H_
#define _ARM_NODES_ATTR_H_

#include "../bearch.h"
#include "../../common/firm_types.h"


typedef struct _arm_register_req_t {
	const arch_register_req_t req;
	int same_pos;        /**<< in case of "should be same" we need to remember the pos to get the irn */
	int different_pos;   /**<< in case of "should be different" we need to remember the pos to get the irn */
} arm_register_req_t;


typedef struct _arm_attr_t {
	arch_irn_flags_t flags;     /**<< indicating if spillable, rematerializeable ... etc. */
	int              n_res;     /**<< number of results for this node */

	const arm_register_req_t **in_req;  /**<< register requirements for arguments */
	const arm_register_req_t **out_req; /**<< register requirements for results */

	const arch_register_t **slots;          /**<< register slots for assigned registers */

	tarval *value;
	const char *symconst_label;
	int proj_num;
	int n_projs;
	long default_proj_num;
} arm_attr_t;

#endif /* _ARM_NODES_ATTR_H_ */
