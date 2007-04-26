/* -*- c -*- */

/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Lists, err, Sets
 * @author   Florian
 * @date     Mon 18 Oct 2004
 * @version  $Id$
 */
# ifndef FIRM_ANA2_LSET_H
# define FIRM_ANA2_LSET_H

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



# endif


/*
  $Log$
  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
