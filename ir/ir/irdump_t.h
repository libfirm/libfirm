/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Private header for irdump
 * @version $Id$
 */
#ifndef FIRM_IR_IRDUMPT_T_H
#define FIRM_IR_IRDUMPT_T_H

#include "irdump.h"
#include "irgraph_t.h"

/**
 * Symbolic names for the different dumping colors.
 */
typedef enum ird_color_t {
	ird_color_prog_background,
	ird_color_block_background,
	ird_color_dead_block_background,
	ird_color_block_inout,
	ird_color_default_node,
	ird_color_phi,
	ird_color_memory,
	ird_color_controlflow,
	ird_color_const,
	ird_color_anchor,
	ird_color_proj,
	ird_color_uses_memory,
	ird_color_error,
	ird_color_entity,
	ird_color_count
} ird_color_t;

/**
 * Edge kinds.
 */
typedef enum {
	data_edge           = 0x01,   /**< A data edge between two basic blocks. */
	block_edge          = 0x02,   /**< An edge from a node to its basic block. */
	cf_edge             = 0x03,   /**< A regularly control flow edge. */
	exc_cf_edge         = 0x04,   /**< An exceptional control flow edge. */
	mem_edge            = 0x05,   /**< A memory edge. */
	dominator_edge      = 0x06,   /**< A dominator edge from a block to its immediate dominator. */
	node2type_edge      = 0x07,   /**< An edge from an IR node to a type. */

	ent_type_edge       = 0x11,   /**< An edge from an entity to its type. */
	ent_own_edge        = 0x12,   /**< An edge from an entity to its owner type. */
	ent_overwrites_edge = 0x13,   /**< An edge from an entity to the entity it overwrites. */
	ent_value_edge      = 0x14,   /**< An edge from an entity to its value entity. */
	ent_corr_edge       = 0x15,   /**< An edge from an entity to the member entity its initializes. */

	meth_par_edge       = 0x21,   /**< An edge from a method type to one of its parameter types. */
	meth_res_edge       = 0x22,   /**< An edge from a method type to one of its result types. */
	type_super_edge     = 0x23,   /**< An edge from a class type to its super/basis type. */
	union_edge          = 0x24,   /**< An edge from a union type to its member types. */
	ptr_pts_to_edge     = 0x25,   /**< An edge from a pointer type to its points-to type. */
	arr_elt_type_edge   = 0x26,   /**< An edge from an array type to its element type. */
	arr_ent_edge        = 0x27,   /**< An edge from a array type to its element entity. */
	type_member_edge    = 0x28,   /**< An edge from a compound type to its member entities. */

	/* additional flags */
	intra_edge          = 0,      /**< Intra edge flag: edge do not cross basic block boundaries */
	inter_edge          = 0x40,   /**< Inter edge flag: edge cross basic block boundaries */
	back_edge           = 0x80    /**< Backwards edge flag. */
} edge_kind;

/* Attributes of nodes */
#define PRINT_DEFAULT_NODE_ATTR
#define DEFAULT_NODE_ATTR " "
#define DEFAULT_TYPE_ATTRIBUTE " "
#define DEFAULT_ENUM_ITEM_ATTRIBUTE " "

/* Attributes of edges between Firm nodes */
#define INTRA_DATA_EDGE_ATTR     "class:1  priority:50"
#define INTER_DATA_EDGE_ATTR     "class:16 priority:10"
#define BLOCK_EDGE_ATTR          "class:2  priority:50 linestyle:dotted"
#define CF_EDGE_ATTR             "class:13 priority:60 color:red"
#define EXC_CF_EDGE_ATTR         "class:18 priority:60 color:blue"
#define INTRA_MEM_EDGE_ATTR      "class:14 priority:50 color:blue"
#define INTER_MEM_EDGE_ATTR      "class:17 priority:10 color:blue"
#define DOMINATOR_EDGE_ATTR      "class:15 color:red"
#define POSTDOMINATOR_EDGE_ATTR  "class:19 color:red linestyle:dotted"
#define KEEP_ALIVE_EDGE_ATTR     "class:20 priority:10 color:purple"
#define KEEP_ALIVE_CF_EDGE_ATTR  "class:20 priority:60 color:purple"
#define KEEP_ALIVE_DF_EDGE_ATTR  "class:20 priority:10 color:purple"
#define ANCHOR_EDGE_ATTR         "class:20 priority:60 color:purple linestyle:dotted"
#define OUT_EDGE_ATTR            "class:21 priority:10 color:gold linestyle:dashed"
#define MACROBLOCK_EDGE_ATTR     "class:22 priority:10 color:green linestyle:dashed"

#define BACK_EDGE_ATTR "linestyle:dashed "

/* Attributes of edges between Firm nodes and type/entity nodes */
#define NODE2TYPE_EDGE_ATTR "class:2 priority:2 linestyle:dotted"

/* Attributes of edges in type/entity graphs. */
#define TYPE_METH_NODE_ATTR      "color: lightyellow"
#define TYPE_CLASS_NODE_ATTR     "color: green"
#define TYPE_DESCRIPTION_NODE_ATTR "color: lightgreen"
#define ENTITY_NODE_ATTR         "color: yellow"
#define ENUM_ITEM_NODE_ATTR      "color: green"
#define ENT_TYPE_EDGE_ATTR       "class: 3 label: \"type\" color: red"
#define ENT_OWN_EDGE_ATTR        "class: 4 label: \"owner\" color: black"
#define METH_PAR_EDGE_ATTR       "class: 5 label: \"param %d\" color: green"
#define METH_RES_EDGE_ATTR       "class: 6 label: \"res %d\" color: green"
#define TYPE_SUPER_EDGE_ATTR     "class: 7 label: \"supertype\" color: red"
#define UNION_EDGE_ATTR          "class: 8 label: \"component\" color: blue"
#define PTR_PTS_TO_EDGE_ATTR     "class: 9 label: \"points to\" color:green"
#define ARR_ELT_TYPE_EDGE_ATTR   "class: 10 label: \"arr elt tp\" color:green"
#define ARR_ENT_EDGE_ATTR        "class: 10 label: \"arr ent\" color: green"
#define ENT_OVERWRITES_EDGE_ATTR "class: 11 label: \"overwrites\" color:red"
#define ENT_VALUE_EDGE_ATTR      "label: \"value %d\""
#define ENT_CORR_EDGE_ATTR       "label: \"value %d corresponds to \" "
#define TYPE_MEMBER_EDGE_ATTR    "class: 12 label: \"member\" color:blue"
/* #define CALLGRAPH_EDGE_ATTR      "calls" */

#define PRINT_NODEID(X)       fprintf(F, "n%ld", get_irn_node_nr(X))
#define PRINT_TYPEID(X)       fprintf(F, "\"t%ld\"", get_type_nr(X))
#define PRINT_ENTID(X)        fprintf(F, "e%ld", get_entity_nr(X))
#define PRINT_IRGID(X)        fprintf(F, "g%ld", get_irg_graph_nr(X))
#define PRINT_CONSTID(X,Y)    fprintf(F, "\"n%ldn%ld\"", get_irn_node_nr(X),get_irn_node_nr(Y))
#define PRINT_CONSTBLKID(X,Y) fprintf(F, "n%ldb%ld", get_irn_node_nr(X),get_irn_node_nr(Y))
#define PRINT_LOOPID(X)       fprintf(F, "l%d", get_loop_loop_nr(X))
#define PRINT_ITEMID(X,Y)     fprintf(F, "i%ldT%d", get_type_nr(X), (Y))
#define PRINT_EXTBBID(X)      fprintf(F, "x%ld", get_irn_node_nr(X))

void dump_vcg_header(FILE *out, const char *name, const char *layout, const char *orientation);
void dump_vcg_footer(FILE *out);
const char *get_irg_dump_name(const ir_graph *irg);

const char *get_ent_dump_name(const ir_entity *ent);

/**
 * returns the name of a mode or "<ERROR>" if mode is NOT a mode object.
 * in the later case, sets bad.
 */
const char *get_mode_name_ex(const ir_mode *mode, int *bad);
/** dump the name of a node n to the File F. */
void dump_node_opcode(FILE *out, ir_node *n);

void dump_node_label(FILE *out, ir_node *n);

void dump_vrp_info(FILE *out, ir_node *n);

/** Writes vcg representation with title "PRINT_TYPEID(tp)" to file F. */
void dump_type_node(FILE *out, ir_type *tp);

#endif
