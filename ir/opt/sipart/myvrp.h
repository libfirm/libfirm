
typedef struct myvrpdata {
  ir_tarval *bottom;
  ir_tarval *top;
} myvrpdata_t;

void myvrp_analyze(ir_graph *irg, struct obstack *client_obst);
