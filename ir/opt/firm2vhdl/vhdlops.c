/**
 * @file
 * @brief    New opcodes to support VHDL-specific transformations.
 * @author   Andreas Seltenreich
 *
 *
 */

#include <firm.h>

static ir_op *op_Slice;
static ir_op *op_Pack;

typedef struct slice_attr {
  char first;
  char last;
} slice_attr;

void create_vhdl_opcodes(void)
{
  if (! op_Slice)
    op_Slice = new_ir_op(get_next_ir_opcode(),
			 "Slice",
			 op_pin_state_floats,
			 irop_flag_none,
			 oparity_any,
			 0,
			 sizeof(slice_attr));

  if (! op_Pack)
    op_Pack = new_ir_op(get_next_ir_opcode(),
			 "Pack",
			 op_pin_state_floats,
			 irop_flag_none,
			 oparity_dynamic,
			 0,
			 0);
}
