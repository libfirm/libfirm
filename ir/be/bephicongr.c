/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#include <stdio.h>
#include <string.h>

#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irop.h"
#include "irprog.h"
#include "bephicongr_t.h"


size_t phi_irn_data_offset = 0;

void be_phi_congr_class_init(void) {
	phi_irn_data_offset = register_additional_node_data(sizeof(phi_info_t));
}


#define set_phi_class(n,new_tgt) get_irn_phi_info(n)->phi_congr_class=new_tgt
#define set_phi_class_size(n,v)  get_irn_phi_info(n)->phi_class_size=v
#define inc_phi_class_size(n)    get_irn_phi_info(n)->phi_class_size++

static void correct_phi_class(ir_node *n, ir_node *new_tgt){
	int i, max, check;
	check = get_phi_class_size(n) + get_phi_class_size(new_tgt);

	for (i = 0, max = get_irn_arity(n); i < max; i++) {
		ir_node *arg = get_irn_n(n, i);
		set_phi_class(arg, new_tgt);
		inc_phi_class_size(new_tgt);
	}

	get_irn_phi_info(n)->phi_class_size=0;
	//set_phi_class_size(n, 0);

	assert((check == get_phi_class_size(n) + get_phi_class_size(new_tgt)) && "Re-setting pointers went wrong.");
}


void det_phi_congr_class(ir_node *curr_phi) {
    int i, n;
    assert(is_Phi(curr_phi) && "This must be a phi node.");

	set_phi_class(curr_phi, curr_phi);

	for (i = 0, n = get_irn_arity(curr_phi); i < n; i++) {
		ir_node *arg, *pc;

		arg = get_irn_n(curr_phi, i);
		pc = get_phi_class(arg);
		if (!pc)
			set_phi_class(arg, curr_phi);
		else
			correct_phi_class(pc, curr_phi);
	}
}


void be_det_phi_congr_classes(void) {
	//forall phis: det_phi_congr_classes(n);
	assert(0 && "NYI");
}
