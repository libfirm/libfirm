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

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
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

#endif /* _FIRM_TYPES_H_ */
