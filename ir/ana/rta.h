/* -*- c -*- */

#ifndef _RTA_H_
#define _RTA_H_

# include "entity.h"

/* Initialise the RTA data structures and perform RTA */
void rta_init(int verbose);
/* Delete all graphs that have been found to be dead */
void rta_delete_dead_graphs (void);
/* Clean up our data structures */
void rta_cleanup(void);

/* Inquire whether the given class is live */
int rta_is_alive_class(type *clazz);
/* Inquire whether the given graph is alive */
int rta_is_alive_graph(ir_graph *graph);
/* Inquire whether the given field is alive */
int rta_is_alive_field(entity *field);

#endif /* def _RTA_H_ */

/*
 * $Log$
 * Revision 1.5  2004/06/17 08:33:01  liekweg
 * Added comments; added remove_irg
 *
 * Revision 1.4  2004/06/15 11:44:54  beck
 * New inlining schema implemented:
 *
 * small functions that should be inlined in libFirm are implemented in _t.h files
 * with a __ prefix.
 * Preprocessor magic is used to automatically inline these functions whenever a _t.h
 * file is included instead of a .h file.
 * Note that this magic did not work outside libFirm without accessing _t.h files.
 *
 * Revision 1.3  2004/06/13 15:03:45  liekweg
 * RTA auf Iterative RTA aufgebohrt --flo
 *
 * Revision 1.2  2004/06/12 17:09:46  liekweg
 * RTA works, outedges breaks.  "Yay." --flo
 *
 * Revision 1.1  2004/06/11 18:24:18  liekweg
 * Added RTA --flo
 *
 */
