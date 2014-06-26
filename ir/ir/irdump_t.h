/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Private header for irdump
 */
#ifndef FIRM_IR_IRDUMPT_T_H
#define FIRM_IR_IRDUMPT_T_H

#include "irdump.h"
#include "irgraph_t.h"

void print_nodeid(FILE *F, const ir_node *node);
void print_irgid(FILE *F, const ir_graph *irg);
void print_typeid(FILE *F, const ir_type *type);
void print_entityid(FILE *F, const ir_entity *entity);
void print_loopid(FILE *F, const ir_loop *loop);

const char *get_irg_dump_name(const ir_graph *irg);

const char *get_ent_dump_name(const ir_entity *ent);

/** dump the name of a node n to the File F. */
void dump_node_opcode(FILE *out, const ir_node *n);

void dump_node_label(FILE *out, const ir_node *n);

/** Writes vcg representation with title "PRINT_TYPEID(tp)" to file F. */
void dump_type_node(FILE *out, ir_type *tp);

void dump_vcg_header(FILE *out, const char *name, const char *layout, const char *orientation);
void dump_vcg_footer(FILE *out);
void dump_vcg_header_colors(FILE *out);
void dump_vcg_infonames(FILE *out);
void dump_node(FILE *out, const ir_node *node);
void dump_ir_data_edges(FILE *F, const ir_node *n);
void dump_begin_block_subgraph(FILE *F, const ir_node *block);
void dump_end_block_subgraph(FILE *F, const ir_node *block);
void dump_block_edges(FILE *F, const ir_node *block);

/** Write the irnode and all its attributes to the file passed.
 * (plain text format) */
void dump_irnode_to_file(FILE *out, const ir_node *node);

#endif
