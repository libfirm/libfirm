/**
 * @file
 * @brief    Replaces Const nodes with PinnedConst nodes in the block(s) in which they are needed.
 *           In case of Phis, places PinnedConsts in the appropriate predecessor.
 * @author   Andreas Fried
 *
 */

#include "irgraph.h"

void lower_for_vhdl(ir_graph *irg);
