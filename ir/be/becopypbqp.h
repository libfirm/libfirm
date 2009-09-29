/*
 * becopypbqp.h
 *
 *  Created on: Aug 28, 2009
 *      Author: bersch
 */

#ifndef BECOPYPBQP_H_
#define BECOPYPBQP_H_

#include "plist.h"
#include "pmap.h"
#include "be_types.h"
#include "pbqp_t.h"
#include "bitset.h"
#include "bechordal.h"
#include "pqueue.h"
#include "beifg.h"

typedef struct _pbqp_co_t pbqp_co_t;

/* struct for pbqp based copy min. optimization */
struct _pbqp_co_t {
	const arch_register_class_t *cls;	// current register class
	plist_t *rpeo;						// reverse perfect elimination order
	pmap *map;							// contains relation between irn and pbqp node
	pbqp *pbqp;
	bitset_t *ignore_reg;
	bitset_t *constatNodes;
	be_ifg_t *ifg;
};

#endif /* BECOPYPBQP_H_ */
