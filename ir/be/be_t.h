/**
 * Internal backend global data structures.
 * @author Sebastian Hack
 * @date 8.12.2004
 */

#ifndef _BE_T_H
#define _BE_T_H

typedef struct _phase_t {
	const char *name;
	int id;
} phase_t;

int phase_register(phase_t *phase);
void phase_applied(const ir_graph *irg, const phase_t *phase);
int phase_depends_on(const ir_graph *irg, const phase_t *phase, int n, ...);
int phase_invalidates(const ir_graph *irg, const phase_t *phase, int n, ...);

#endif
