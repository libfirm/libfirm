/* -*- c -*- */

#ifndef _RTA_H_
#define _RTA_H_

# include "entity.h"

/**
 * Initialise the RTA data structures and perform RTA.
 *
 * @param do_verbose If == 1, print statistics, if > 1, chatter about every detail
 */
void rta_init(int do_verbose);

/**
 * Delete all graphs that have been found to be dead.
 */
void rta_delete_dead_graphs (void);

/** Clean up our data structures.
 * Finishes the RTA.
 */
void rta_cleanup(void);

/** Returns non-zero if the given class is alive. */
int rta_is_alive_class(type *clazz);

/** Returns non-zero if the given graph is alive. */
int rta_is_alive_graph(ir_graph *graph);

/** report for all graphs and types wheater they are alive */
void rta_report (void);

#endif /* not defined _RTA_H_ */

/*
 * $Log$
 * Revision 1.11  2004/10/18 12:47:46  liekweg
 * minor fix
 *
 * Revision 1.10  2004/09/24 13:59:04  beck
 * fixed doxygen comments, removed initialization for description entities
 *
 * Revision 1.9  2004/08/19 16:51:02  goetz
 * fixed some errors, pushed closer to inteded firm semantics
 *
 * Revision 1.8  2004/06/18 17:34:31  liekweg
 * Removed field checks --flo
 *
 * Revision 1.7  2004/06/18 13:12:43  liekweg
 * final bug fix (calls via consts)
 *
 * Revision 1.6  2004/06/17 14:21:14  liekweg
 * major bugfix
 *
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
