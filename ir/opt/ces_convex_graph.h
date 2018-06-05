#ifndef __CES_CONVEX_GRAPH_H__
#define __CES_CONVEX_GRAPH_H__

#include "adt/plist.h"
#include "irnodemap.h"

#include "ces_agu_emulator.h"
#include "ces_si_tools.h"


void ces_convex_simple(ir_graph* new_graph, ir_graph* old_graph, ir_node* start, ir_node* stop);
int ces_check_convexity(ir_graph* irg, struct stream_description memory_streams[]);
#endif//__CES_CONVEX_GRAPH_H__
