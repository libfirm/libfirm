/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/lset.c
 * Purpose:     Lists, err, Sets
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif

# include "lset.h"
# include "xmalloc.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

# include <assert.h>

#ifdef HAVE_STRING_H
# include <string.h>             /* need memset */
#endif

/*
  Lists, err, Sets
 */

/* create a new lset */
lset_t *lset_create (void)
{
  lset_t *lset = xmalloc(sizeof(*lset));

  return (lset);
}

/* check whether the lset contains an entry for the given data */
int lset_contains (lset_t *lset, void *data)
{
  lset_entry_t *entry = lset->first;

  while (NULL != entry) {
    if (data == entry->data) {
      return (TRUE);
    }

    entry = entry->next;
  }

  return (FALSE);
}

/* check whether the given lset is empty */
int lset_empty (lset_t *lset)
{
  return (NULL == lset->first);
}


/* insert the data into the lset (unless there's an entry for it
   already) */
void lset_insert (lset_t *lset, void *data)
{
  if (! lset_contains (lset, data)) {
    lset_entry_t *entry = xmalloc(sizeof(*entry));
    entry->data = data;
    entry->next = lset->first;
    lset->first = entry;

    if (NULL == lset->last) {
      lset->last = entry;
    }

    lset->n_entries ++;
  }
}

/* insert all entries from src into tgt */
void lset_insert_all (lset_t *tgt, lset_t *src)
{
  lset_entry_t *curs = src->first;

  while (NULL != curs) {
    lset_insert (tgt, curs->data);

    curs = curs->next;
  }
}

/* append src to tgt. src is deallocated. */
void lset_append (lset_t *tgt, lset_t *src)
{
  assert (! tgt->last->next);

  tgt->last->next = src->first;
  tgt->last = src->last;
  tgt->n_entries += src->n_entries;

  memset (src, 0x00, sizeof (lset_t));
  free (src);
}

/* remove the entry for the given data element from the lset. return
   TRUE iff it was on the list in the first place, FALSE else */
int lset_remove (lset_t *lset, void *data)
{
  lset_entry_t *entry = lset->first;
  lset_entry_t *prev = NULL;

  while (NULL != entry) {
    if (data == entry->data) {
      /* ok, dike it out */

      if (NULL == prev) { /* ok, it's lset->first that needs diking */
        lset->first = entry->next;
      } else {
        prev->next = entry->next;
      }

      memset (entry, 0x00, sizeof (lset_entry_t));
      free (entry);

      lset->n_entries --;

      return (TRUE);
    }

    prev = entry;
    entry = entry->next;
  }

  return (FALSE);
}

/* prepare the given lset for an iteration. return the first element. */
void *lset_first (lset_t *lset)
{
  lset->curs = lset->first;

  if (lset->first) {
    return (lset->first->data);
  } else {
    return (NULL);
  }
}

/* after calling lset_first, get the next element, if applicable, or
   NULL */
void *lset_next (lset_t *lset)
{
  lset->curs = lset->curs->next;

  if (lset->curs) {
    return (lset->curs->data);
  } else {
    return (NULL);
  }
}

/* say how many entries there are in the given lset */
int lset_n_entries (lset_t *lset)
{
  return (lset->n_entries);
}

/* deallocate the lset and all of its entries */
void lset_destroy (lset_t *lset)
{
  lset_entry_t *curs = lset->first;

  while (NULL != curs) {
    lset_entry_t *tmp = curs->next;

    memset (curs, 0x00, sizeof (lset_entry_t));
    free (curs);

    curs = tmp;
  }

  memset (lset, 0x00, sizeof (lset_t));
  free (lset);
}



/*
  $Log$
  Revision 1.3  2004/12/22 14:43:14  beck
  made allocations C-like

  Revision 1.2  2004/12/02 16:17:51  beck
  fixed config.h include

  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
