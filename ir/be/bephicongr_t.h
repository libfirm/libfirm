/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#ifndef _BEPHICONGR_T_H
#define _BEPHICONGR_T_H

#include "irnode.h"

typedef struct _phi_info_t {
	ir_node *phi_congr_class;
	unsigned int phi_class_size;
} phi_info_t;

extern size_t phi_irn_data_offset;

#define get_irn_phi_info(irn) get_irn_data(irn, phi_info_t, phi_irn_data_offset)
#define get_phi_class(n)      get_irn_phi_info(n)->phi_congr_class
#define get_phi_class_size(n) get_irn_phi_info(n)->phi_class_size

void be_phi_congr_class_init(void);
void be_det_phi_congr_classes(void);

void det_phi_congr_class(ir_node *curr_phi);

#endif
