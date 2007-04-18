#ifndef _TEMPLATE_NODES_ATTR_H_
#define _TEMPLATE_NODES_ATTR_H_

#include "../bearch_t.h"

typedef struct _TEMPLATE_attr_t {
	arch_irn_flags_t flags;     /**< indicating if spillable, rematerializeable ... etc. */
	int              n_res;     /**< number of results for this node */

	const arch_register_req_t **in_req;  /**< register requirements for arguments */
	const arch_register_req_t **out_req; /**< register requirements for results */

	/* must be last, dynamically allocated */
	const arch_register_t *slots[1];         /**< register slots for assigned registers */
} TEMPLATE_attr_t;

#endif /* _TEMPLATE_NODES_ATTR_H_ */
