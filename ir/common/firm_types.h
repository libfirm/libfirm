/*
 * Project:     libFIRM
 * File name:   ir/common/firm_types.c
 * Purpose:     Definition of opaque firm types
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_TYPES_H_
#define _FIRM_TYPES_H_

#ifndef _IDENT_TYPEDEF_
#define _IDENT_TYPEDEF_
typedef const struct _ident ident;
#endif

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

#ifndef _IR_MODE_TYPEDEF_
#define _IR_MODE_TYPEDEF_
typedef struct ir_mode ir_mode;
#endif

#ifndef _IR_EDGE_TYPEDEF_
#define _IR_EDGE_TYPEDEF_
typedef struct _ir_edge_t ir_edge_t;
#endif

#ifndef _IR_BLOCK_EDGE_TYPEDEF_
#define _IR_BLOCK_EDGE_TYPEDEF_
typedef struct _ir_block_edge_t ir_block_edge_t;
#endif

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
typedef struct tarval tarval;
#endif

#ifndef _TYPE_TYPEDEF_
#define _TYPE_TYPEDEF_
typedef struct type type;
#endif

#ifndef _IR_GRAPH_TYPEDEF_
#define _IR_GRAPH_TYPEDEF_
typedef struct ir_graph ir_graph;
#endif

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
typedef struct entity entity;
#endif

#ifndef _COMPOUND_GRAPH_PATH_TYPEDEF_
#define _COMPOUND_GRAPH_PATH_TYPEDEF_
typedef struct compound_graph_path compound_graph_path;
#endif

#ifndef _IR_EXTBB_TYPEDEF_
#define _IR_EXTBB_TYPEDEF_
typedef struct _ir_extblk ir_extblk;
#endif

#ifndef _IRG_WALK_FUNC_TYPEDEF_
#define _IRG_WALK_FUNC_TYPEDEF_
typedef void irg_walk_func(ir_node *, void *);
#endif

#ifndef _SEQNO_T_TYPEDEF_
#define _SEQNO_T_TYPEDEF_
typedef struct sn_entry *seqno_t;
#endif

#endif /* _FIRM_TYPES_H_ */
