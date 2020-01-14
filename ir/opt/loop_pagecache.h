#ifndef FIRM_LOOP_PAGECACHE_H
#define FIRM_LOOP_PAGECACHE_H

#include <firm_types.h>
#include <adt/list.h>
#include "irgmod.h"
#include "irgraph.h"
#include "irgraph_t.h"
#include "tv_t.h"
#include "dijkstra.h"

typedef struct mem_op {
    list_head list;
    ir_node *irn;
    ir_node *address;
    ir_mode *mode;
    unsigned int count;
    bool duplicate;
} mem_op_t;

typedef struct loop_var {
    list_head list;
    ir_node *phi;
    ir_node *phi_loop;
    ir_node *back;
    ir_node *init;
    ir_node *limit;
    ir_node *confirm;
    ir_node *step;
    ir_loop *loop;
    ir_node *iterations;
} loop_var_t;

void do_loop_pagecache(ir_graph *const irg);

#endif
