/* -*- c -*- */

#ifndef _RTA_H_
#define _RTA_H_

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include "cgana.h"              /* get_implementation */

#include "eset.h"
/* #include "pmap.h" */
/* #include "array.h" */
#include "irprog.h"
#include "irgwalk.h"
/* #include "ircons.h" */
/* #include "irgmod.h" */
#include "irnode_t.h"
/* #include "irflag_t.h" */

/* #include "dbginfo_t.h" */

void rta_init    (void);
void rta_cleanup (void);

int  rta_is_alive_class  (type*);
int  rta_is_alive_graph  (ir_graph*);
int  rta_is_alive_field  (entity*);

#endif /* def _RTA_H_ */

/*
 * $Log$
 * Revision 1.1  2004/06/11 18:24:18  liekweg
 * Added RTA --flo
 *
 */
