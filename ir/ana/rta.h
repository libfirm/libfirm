/* -*- c -*- */

#ifndef _RTA_H_
#define _RTA_H_

# include "entity.h"

void rta_init    (int);
void rta_cleanup (void);

int  rta_is_alive_class  (type*);
int  rta_is_alive_graph  (ir_graph*);
int  rta_is_alive_field  (entity*);

#endif /* def _RTA_H_ */

/*
 * $Log$
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
