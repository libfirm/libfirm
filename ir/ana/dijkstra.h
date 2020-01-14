#ifndef FIRM_DIJKSTRA
#define FIRM_DIJKSTRA

#include <pqueue.h>
#include <tv_t.h>

typedef struct dijkstra_node {
    void *ptr;
    ir_tarval *dist;
} dijkstra_node_t;

typedef struct dijkstra_edge {
    dijkstra_node_t *from;
    dijkstra_node_t *to;
    ir_tarval *weight;
} dijkstra_edge_t;

typedef struct dijkstra_graph {
    dijkstra_node_t **nodes;
    dijkstra_edge_t *edges;
    ir_mode *mode;
} dijkstra_graph_t;

dijkstra_graph_t *new_dijkstra(ir_mode *mode);
dijkstra_node_t *dijkstra_add_node(dijkstra_graph_t *graph, void *ptr);
dijkstra_edge_t *dijkstra_add_edge(dijkstra_graph_t *graph, dijkstra_node_t *from, dijkstra_node_t *to, ir_tarval *weight);
ir_tarval *dijkstra_get_max(dijkstra_graph_t *graph);
void free_dijkstra(dijkstra_graph_t *graph);

#endif
