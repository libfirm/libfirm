#ifndef _PATTERN_DMP_H_
#define _PATTERN_DMP_H_

typedef struct _pattern_dumper_t pattern_dumper_t;
typedef void (*DUMP_NODE_FUNC)(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code);
typedef void (*DUMP_REF_FUNC)(pattern_dumper_t *self, unsigned id);
typedef void (*DUMP_EDGE_FUNC)(pattern_dumper_t *self, unsigned id, unsigned parent, unsigned position);
typedef void (*DUMP_START_CHILDREN_FUNC)(pattern_dumper_t *self, unsigned id);
typedef void (*DUMP_FINISH_CHILDREN_FUNC)(pattern_dumper_t *self, unsigned id);

struct _pattern_dumper_t {
  DUMP_NODE_FUNC             dump_node;
  DUMP_REF_FUNC              dump_ref;
  DUMP_EDGE_FUNC             dump_edge;
  DUMP_START_CHILDREN_FUNC   dump_start_children;
  DUMP_FINISH_CHILDREN_FUNC  dump_finish_children;
};

extern pattern_dumper_t vcg_dump, stdout_dump;

/**
 * Dumps a node
 */
void pattern_dump_node(pattern_dumper_t *self, unsigned id, unsigned op_code, unsigned mode_code);

/**
 * Dump a ref
 */
void pattern_dump_ref(pattern_dumper_t *self, unsigned id);

/**
 * Dump an edge
 */
void pattern_dump_edge(pattern_dumper_t *self, unsigned id, unsigned parent, unsigned position);

/**
 * Start children dumper
 */
void pattern_start_children(pattern_dumper_t *self, unsigned id);

/**
 * finishes childred  dumper
 */
void pattern_finish_children(pattern_dumper_t *self, unsigned id);

#endif /* _PATTERN_DMP_H_ */
