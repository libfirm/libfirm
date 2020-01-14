#include "dijkstra.h"
#include "array.h"
#include "xmalloc.h"

dijkstra_graph_t *new_dijkstra(ir_mode *mode) {
    dijkstra_graph_t *graph = XMALLOC(dijkstra_graph_t);
    graph->nodes = NEW_ARR_F(dijkstra_node_t*, 0);
    graph->edges = NEW_ARR_F(dijkstra_edge_t, 0);
    graph->mode = mode;
    return graph;
}

dijkstra_node_t *dijkstra_add_node(dijkstra_graph_t *graph, void *ptr) {
    dijkstra_node_t *node = XMALLOC(dijkstra_node_t);
    node->ptr = ptr;
    node->dist = get_mode_max(graph->mode);
    ARR_APP1(dijkstra_node_t*, graph->nodes, node);
    return node;
}

dijkstra_edge_t *dijkstra_add_edge(dijkstra_graph_t *graph, dijkstra_node_t *from, dijkstra_node_t *to, ir_tarval *weight) {
    ir_tarval *w = weight;
    if (get_tarval_mode(w) != graph->mode) {
        w = tarval_convert_to(w, graph->mode);
    }
    ARR_APP1(dijkstra_edge_t, graph->edges, ((dijkstra_edge_t){from, to, w}));
    return &graph->edges[ARR_LEN(graph->edges) - 1];
}

ir_tarval *dijkstra_get_max(dijkstra_graph_t *graph) {
    pqueue_t *pq = new_pqueue();
    graph->nodes[0]->dist = get_mode_null(graph->mode);
    pqueue_put(pq, graph->nodes[0], 0);
    while (!pqueue_empty(pq)) {
        dijkstra_node_t *u = (dijkstra_node_t*)pqueue_pop_front(pq);
        for (size_t i = 0; i < ARR_LEN(graph->edges); i++) {
            dijkstra_edge_t edge = graph->edges[i];
            if (edge.from != u) continue;
            dijkstra_node_t *v = edge.to;
            if (tarval_cmp(v->dist, tarval_add(u->dist, edge.weight)) == ir_relation_greater) {
                    v->dist = tarval_add(u->dist, edge.weight);
                    pqueue_put(pq, v, (int)get_tarval_long(v->dist));
            }
        }
    }
    return graph->nodes[ARR_LEN(graph->nodes) - 1]->dist;
}

void free_dijkstra(dijkstra_graph_t *graph) {
    DEL_ARR_F(graph->nodes);
    DEL_ARR_F(graph->edges);
    free(graph);
}
