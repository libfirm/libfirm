#ifndef _TEMPLATE_NODES_ATTR_H_
#define _TEMPLATE_NODES_ATTR_H_

#include "../bearch.h"

typedef struct _TEMPLATE_register_req_t {
	const arch_register_req_t req;
	int pos;   /**<< in case of "should be same/different" we need to remember the pos to get the irn */
} TEMPLATE_register_req_t;


typedef struct _TEMPLATE_attr_t {
	arch_irn_flags_t flags;     /**<< indicating if spillable, rematerializeable ... etc. */
	int              n_res;     /**<< number of results for this node */

	const TEMPLATE_register_req_t **in_req;  /**<< register requirements for arguments */
	const TEMPLATE_register_req_t **out_req; /**<< register requirements for results */

	const arch_register_t **slots;          /**<< register slots for assigned registers */
} TEMPLATE_attr_t;

#endif /* _TEMPLATE_NODES_ATTR_H_ */
