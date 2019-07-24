#ifndef VHDLMODES_H
#define VHDLMODES_H

#include "irmode_t.h"

void vhdl_init_modes(void);

ir_mode *get_mode_signed_vector(unsigned);
ir_mode *get_mode_unsigned_vector(unsigned);
ir_mode *get_mode_std_logic_vector(unsigned);

#endif
