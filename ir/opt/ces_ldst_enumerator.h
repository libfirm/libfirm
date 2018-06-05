

#include "firm.h"
#include "irnodemap.h"

ir_graph* prepare_si_irg_from(ir_graph* irg);
struct stream_description* ces_identify_streams(ir_graph* si_irg);
void ces_enum_ldst_init(ir_graph* irg); //for unit testingx
int ces_color_dumper(FILE *out, ir_node *node, ir_node *local);
