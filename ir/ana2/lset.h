/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/lset.h
 * Purpose:     Lists, err, Sets
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _LSET_H_
# define _LSET_H_

/*
  Data Types and Structures
*/
/* Lists, err, Sets */
typedef struct lset_entry
{
  void *data;
  struct lset_entry *next;
} lset_entry_t;

typedef struct lset
{
  lset_entry_t *first;
  lset_entry_t *last;           /* useful for lset_append */
  lset_entry_t *curs;           /* for lset_first/lset_next */
  int n_entries;
} lset_t;

/* create a new lset */
lset_t *lset_create (void);
/* check whether the lset contains an entry for the given data */
int lset_contains (lset_t*, void*);
/* check whether the given lset is empty */
int lset_empty (lset_t*);
/* insert the data into the lset (unless there's an entry for it
   already) */
void lset_insert (lset_t*, void*);
/* insert all entries from src into tgt */
void lset_insert_all (lset_t*, lset_t*);
/* append src to tgt. src is deallocated. */
void lset_append (lset_t*, lset_t*);

/* remove the entry for the given data element from the lset. return
   TRUE iff it was on the list in the first place, FALSE else */
int lset_remove (lset_t*, void*);
/* prepare the given lset for an iteration. return the first element. */
void *lset_first (lset_t*);
/* after calling lset_first, get the next element, if applicable, or
   NULL */
void *lset_next (lset_t*);
/* say how many entries there are in the given lset */
int lset_n_entries (lset_t*);
/* deallocate the lset and all of its entries */
void lset_destroy (lset_t*);



# endif /* not defined _LSET_H_ */


/*
  $Log$
  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
