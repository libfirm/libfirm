/* -*- c -*- */

/*
 * Time-stamp: <17.12.2004 20:26:51h liekweg>
 * Project:     libFIRM
 * File name:   ir/ana2/qset.c
 * Purpose:     yet another set implementation
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

# include <stdio.h>
# include <stdlib.h>
# include <assert.h>
# include <string.h>

# include "obst.h"
# include "timing.h"
# include "qset.h"

/* local globals */
int qset_id = 0;

/* local protos */

# ifdef UNSINN
/* Check whether the given list of sortables is sorted. */
static void q_check (sortable_t*, const int);
# endif /* defined UNSINN */

/* Test whether the given val is among values.  Return the index of
   val in values, or -1. */
static int q_test (sortable_t*, const sortable_t, const int);

/* Sort n_elems entries  in 'values' */
static void q_sort (sortable_t*, const int);

/* Compare funktion, meant to be qsort(3)-compatible */
static INLINE int sortable_compare (const void *pa, const void *pb)
{
  const int a = * (unsigned int*) pa;
  const int b = * (unsigned int*) pb;

  if (a==b) {
    return (0);
  }

  return ((a < b) ? -1 : +1);
}

/*
   Wrapper for mixed allocation
*/
static void *mix_malloc (struct obstack *obst, size_t size)
{
  if (NULL != obst) {
    return (obstack_alloc (obst, size));
  } else {
    return (malloc (size));
  }
}

/*
  Allocate a list of sortables of the given length.
*/
static sortable_t *alloc_list (const int len, struct obstack *obst)
{
  sortable_t *values = (sortable_t*) mix_malloc (obst, len * sizeof (sortable_t));

  memset (values, 0x00, len * sizeof (sortable_t));

  return (values);
}

# ifdef UNSINN
/*
  Create a list of sortables of the given length.
*/
static sortable_t *gen_list (const int len, struct obstack *obst)
{
  int i;
  sortable_t *values = alloc_list (len, obst);

  for (i = 0; i < len; i ++) {
    values [i] = rand () >> 16;
  }

  return (values);
}
# endif /* defined UNSINN */

/*
  Helper to q_test --- return the index of val in values, or -1
*/
static int _q_test (sortable_t *values,
                    const sortable_t val,
                    const int lo, const int hi)
{
  int mid;

  /* fprintf (stdout, "%s for %d [%i:%i]\n", __FUNCTION__, val, lo, hi); */

  if (lo == hi) {
    if (EQUAL (val, values [lo])) {
      return (lo);
    } else {
      return (-1);
    }
  }

  if (COMPARE (val, values [lo])) {
    /* too low */
    return (-1);
  }

  if (COMPARE (values [hi], val)) {
    /* too high */
    return (-1);
  }

  mid = (hi + lo) / 2;

  if (EQUAL (val, values [mid])) {
    return (mid);
  }

  if (COMPARE (val, values [mid])) {
    return (_q_test (values, val, lo, mid));
  } else {
    return (_q_test (values, val, mid+1, hi));
  }
}

/*
  Helper to q_sort
*/
static void _q_sort (sortable_t *values, const int lo, const int hi)
{
  sortable_t pivot;
  int p_lo;
  int p_hi;

  /* handle base case: */
  if (lo >= hi) {
    return;
  }

  if (1 == hi - lo) {
    if (COMPARE (values [hi], values [lo])) {
      sortable_t tmp = values [lo];
      values [lo] = values [hi];
      values [hi] = tmp;
    }

    return;
  }

  pivot = values [lo];

  p_lo = lo+1;
  p_hi = hi;

  while (p_lo <= p_hi) {
    if (COMPARE (values [p_lo], pivot)) {
      values [p_lo-1] = values [p_lo];

      p_lo ++;
    } else {
      sortable_t tmp = values [p_lo];

      values [p_lo] = values [p_hi];
      values [p_hi] = tmp;

      p_hi --;
    }
  }

  values [p_lo-1] = pivot;

  _q_sort (values, lo, p_lo-1);
  _q_sort (values, p_lo, hi);
}

# ifdef UNSINN
/*
  Little test harness for q_sort and friends.
*/
static void test_qsort (const int n_runs, const int max_elems, const int incr)
{
  int n_elems = 0;
  int i;

  fprintf (stdout, "# n_elems: q_sort time : libc qsort time\n");

  while (n_elems <= max_elems) {
    int total = 0;
    int qtotal = 0;

    for (i = 0; i < n_runs; i ++) {
      sortable_t *list = gen_list (n_elems, NULL);

      timing_t *timing = start_timing ();
      q_sort (list, n_elems);
      total += end_timing (timing);

      q_check (list, n_elems);

      free (list);
    }

    for (i = 0; i < n_runs; i ++) {
      sortable_t *list = gen_list (n_elems, NULL);

      timing_t *timing = start_timing ();
      qsort (list, n_elems, sizeof (sortable_t), sortable_compare);
      qtotal += end_timing (timing);

      q_check (list, n_elems);

      free (list);
    }

    fprintf (stdout, "%d %d %d\n", n_elems, total/n_runs, qtotal/n_runs);
    fflush (stdout);
    n_elems += incr;
  }


  fprintf (stdout, "\n");
}

/*
  Little test harness for the qset implementation
*/
static void test_qset (const int n_entries)
{
  int i;
  int n1, n2, n;

  qset_t *q1 = qset_new (n_entries, NULL);
  qset_t *q2 = qset_new (n_entries, NULL);
  qset_t *q = NULL;

  sortable_t *values = gen_list (n_entries, NULL);

  for (i = 0; i < n_entries; i ++) {
    qset_insert (q1, values [i]);
    qset_insert (q2, values [i]);
  }

  fprintf (stdout, "q1: \t\t");
  qset_print (q1, stdout);
  qset_sort (q1);
  qset_sort (q2);

  /* TEST */
  q2->is_sorted = FALSE;
  qset_sort (q2);

  fprintf (stdout, "q1 (sorted):\t");
  qset_print (q1, stdout);

  assert (qset_compare (q1, q2));

  q = qset_union (q1, q2);
  fprintf (stdout, "qq (union):\t");
  qset_print (q, stdout);

  n1 = qset_size (q1);
  n2 = qset_size (q2);
  n = qset_size (q);

  fprintf (stdout, "q1.size = %i\n", n1);
  fprintf (stdout, "q1.slots = %i\n", q1->n_slots);
  fprintf (stdout, "q2.size = %i\n", n2);
  fprintf (stdout, "q2.slots = %i\n", q2->n_slots);
  fprintf (stdout, "q.size  = %i\n", n);
  fprintf (stdout, "q.slots  = %i\n", q->n_slots);

  assert (n1 == n2);
  assert (n2 == n);

  assert (qset_compare (q1, q2));
  assert (qset_compare (q2, q));
  assert (qset_compare (q, q1));

  for (i = 0; i < n_entries; i ++) {
    assert (qset_contains (q1, values [i]));
    assert (qset_contains (q2, values [i]));
    assert (qset_contains (q, values [i]));
  }

  qset_compact (q1);
  qset_compact (q);

  fprintf (stdout, "q1.size = %i\n", n1);
  fprintf (stdout, "q1.slots = %i\n", q1->n_slots);
  fprintf (stdout, "q2.size = %i\n", n2);
  fprintf (stdout, "q2.slots = %i\n", q2->n_slots);
  fprintf (stdout, "q.size  = %i\n", n);
  fprintf (stdout, "q.slots  = %i\n", q->n_slots);

  assert (qset_compare (q1, q2));
  assert (qset_compare (q2, q));
  assert (qset_compare (q, q1));

  for (i = 0; i < n_entries; i ++) {
    assert (qset_contains (q1, values [i]));
    assert (qset_contains (q2, values [i]));
    assert (qset_contains (q, values [i]));
  }

  qset_delete (q);
  qset_delete (q1);
  qset_delete (q2);
}
# endif /* defned UNSINN */

/* PRIVATE QSET IMPLEMENTATION */
/*
   Resize a qset to (at least) the given size.
*/
static void qset_resize (qset_t *qset, const int n_slots)
{
  int new_size;
  sortable_t *values = NULL;

  if (qset->n_slots >= n_slots) {
    return;
  }

  new_size = qset->n_slots;

  while (new_size < n_slots) {
    new_size *= 2;
  }

  values = alloc_list (new_size, qset->obst);

  memcpy (values, qset->values, qset->n_elems * sizeof (sortable_t));
  memset (qset->values, 0x00, qset->n_elems * sizeof (sortable_t)); /* debug only */

  if (NULL == qset->obst) {
    free (qset->values);
  }

  qset->values = values;
  qset->n_slots = new_size;
}

/*
   Print a list of sortables.
*/
static void q_print (sortable_t *values, const int n_values, FILE *stream)
{
  int i;

  fprintf (stream, "{");

  for (i = 0; i < n_values; i ++) {
    if (0 == values [i]) {
      fprintf (stream, "_");
    } else {
      fprintf (stream, "0x08%x", (int) values [i]);
    }

    if (i + 1 != n_values) {
      fprintf (stream, ", ");
    }
  }

  fprintf (stream, "}\n");
}

# ifdef UNSINN
/*
  Check whether the given list of sortables is sorted.
*/
static void q_check (sortable_t *values, const int n_values)
{
  int i;

  for (i = 1; i < n_values; i ++) {
    assert (COMPARE (values [i-1], values [i]) ||
            EQUAL   (values [i-1], values [i]));
  }

  for (i = 1; i < n_values; i ++) {
    assert (-1 != q_test (values, values [i], n_values));
  }
}
# endif /* defined UNSINN */

/*
  Test whether the given val is among values.  Return the lowest index of val
  in values, or -1.
*/
static int q_test (sortable_t *values, const sortable_t val, const int n_elems)
{
  int idx = _q_test (values, val, 0, n_elems-1);

  while ((0 <= idx-1) && EQUAL (values [idx], val)) {
    idx --;
  }

  return (idx);
}

/*
  Sort entries in 'values' from index 'lo' to 'hi' (inclusive)
*/
static void q_sort (sortable_t *values, const int n_elems)
{
  _q_sort (values, 0, n_elems-1);
}

/* PUBLIC INTERFACE */

/*
  Allocate a new qset with initial space for up to n_elems.
  If a non-NULL obstack is given, it is used for all allocations of this qset
  and must be initialised and deleted by the user of the qset.
*/
qset_t *qset_new (const int n_elems, struct obstack *obst)
{
  qset_t *qset = (qset_t*) mix_malloc (obst, sizeof (qset_t));

  qset->obst = obst;
  qset->values = alloc_list (n_elems, obst);
  memset (qset->values, 0x00, n_elems * sizeof (sortable_t));

  qset->n_slots = n_elems;
  qset->n_elems = 0;
  qset->is_sorted = FALSE;
  qset->id = qset_id ++;

  return (qset);
}

/*
  Sort the entries of the given qset.
*/
void qset_sort (qset_t *qset)
{
  int occ = 0;
  int i;

  if (qset->is_sorted) {
    return;
  }

  q_sort (qset->values, qset->n_elems);

  for (i = 1; i < qset->n_elems; i ++) {
    if (! EQUAL (qset->values [i], qset->values [occ])) {
      qset->values [++ occ] = qset->values [i];
    }
  }

  occ ++;

  /*
  if (qset->n_elems != occ) {
    fprintf (stdout, "removed %i duplicates\n", qset->n_elems - occ);
    }*/

  qset->n_elems = occ;

  qset->is_sorted = TRUE;
}

/*
  Compact a qset to take up no more space than required.
*/
void qset_compact (qset_t *qset)
{
  if (NULL == qset->obst) {
    sortable_t *values = (sortable_t*) mix_malloc (qset->obst,
                                                   qset->n_elems * sizeof (sortable_t));
    memcpy (values, qset->values, qset->n_elems * sizeof (sortable_t));

    memset (qset->values, 0x00, qset->n_elems * sizeof (sortable_t));

    free (qset->values);

    qset->values = values;
    qset->n_slots = qset->n_elems;
  }
}

/*
  Free the memory associated with the given qset
*/
void qset_delete (qset_t *qset)
{
  memset (qset->values, 0x00, qset->n_elems * sizeof (sortable_t));

  if (NULL == qset->obst) {
    free (qset->values);
  }

  memset (qset, 0x00, sizeof (qset_t));

  if (NULL == qset->obst) {
    free (qset);
  }
}

/*
  Test wether the given qset contains the given value.
*/
int qset_contains (qset_t *qset, sortable_t val)
{
  qset_sort (qset);

  return (-1 != q_test (qset->values, val, qset->n_elems));
}

/*
  Delete the given value from the given qset (if it exists)
*/
void qset_remove (qset_t *qset, sortable_t val)
{
  int idx;
  int occ = 0;
  int i;

  qset_sort (qset);

  idx = q_test (qset->values, val, qset->n_elems);

  if (-1 == idx) {
    return;
  }

  while (EQUAL (qset->values [idx + occ], val)) {
    occ ++;
  }

  for (i = idx; i < qset->n_elems - occ; i ++) {
    qset->values [i] = qset->values [i+occ];
  }

  qset->n_elems -= occ;
}

/*
  Insert the given elem into the given qset; return nonzero iff any
  involved values change.
*/
int qset_insert (qset_t *qset, sortable_t val)
{
  int i;
  const int n_elems = qset->n_elems;

  /* todo: sort, find */
  assert (0 != val);

  for (i = 0; i < n_elems; i ++) {
    if (EQUAL (val, qset->values [i])) {
      return (FALSE);
    }
  }

  qset_resize (qset, qset->n_elems+1);

  /* qset_print (qset, stdout); */
  /* fprintf (stdout, "%s: must insert 0x%08x\n", __FUNCTION__, (void*) val); */

  qset->values [qset->n_elems++] = val;
  qset->is_sorted = FALSE;

  return (TRUE);
}

/*
  Insert all elems of qset2 into qset1; return nonzero iff any
  involved values change. qset2 is *not* deleted.
*/
int qset_insert_all (qset_t *qset1, qset_t *qset2)
{
  int change = FALSE;
  sortable_t val = qset_start (qset2);

  while (0 != val) {
    change |= qset_insert (qset1, val);

    val = qset_next (qset2);
  }

  qset_sort (qset1);

  return (change);
}

/*
  Compare two qsets.
*/
int qset_compare (qset_t *qset1, qset_t *qset2)
{
  int i;
  const int n_elems = qset1->n_elems;

  qset_sort (qset1);
  qset_sort (qset2);

  if (qset1->n_elems != qset2->n_elems) {
    return (FALSE);
  }

  for (i = 0; i < n_elems; i ++) {
    if (qset1->values [i] != qset2->values [i]) {
      return (FALSE);
    }
  }

  return (TRUE);
}

/*
  Returns the union of two qsets.
*/
qset_t *qset_union (qset_t *qset1, qset_t *qset2)
{
  qset_t *qset = (qset_t*) mix_malloc (qset1->obst, sizeof (qset_t));
  const int n_elems = qset1->n_elems + qset2->n_elems;

  qset->values = alloc_list (n_elems, qset1->obst);

  memcpy (qset->values, qset1->values,
          qset1->n_elems * sizeof (sortable_t));

  memcpy (qset->values+qset1->n_elems,
          qset2->values, qset2->n_elems * sizeof (sortable_t));

  qset->obst = qset1->obst;
  qset->n_elems = n_elems;
  qset->n_slots = qset->n_elems;

  qset_sort (qset);

  return (qset);
}

/*
  Report the size of the given qset.
*/
int qset_size (qset_t *qset)
{
  return (qset->n_elems);
}

/*
  Print the given qset to the given stream.
*/
void qset_print (qset_t *qset, FILE *stream)
{
  q_print (qset->values, qset->n_elems, stream);
}

/*
   Check wether the given qset is empty
*/
int qset_is_empty (qset_t *qset)
{
  return (0 == qset->n_elems);
}

/*
  Initialise a new iteration over a qset
*/
sortable_t *qset_start (qset_t *qset)
{
  sortable_t *start;

  qset->cursor = 0;

  start = qset_next (qset);

  return (start);    /* works for empty sets, too */
}

/*
  Step to the next element
*/
sortable_t *qset_next (qset_t *qset)
{
  if (qset->n_elems == qset->cursor) {
    return (NULL);
  }

  /* quick fix to skip NULL entries */
  while ((qset->cursor < qset->n_elems) &&
         (NULL == qset->values [qset->cursor])) {
    qset->cursor ++;
  }

  return (qset->values [qset->cursor++]);
}


# ifdef UNSINN
int qset_test_main (int argc, char **argv)
{
  struct timeval tp;
  unsigned int seed;

  if (-1 == gettimeofday (&tp, NULL)) {
    perror ("gettimeofday");
    exit (EXIT_FAILURE);
  }

  seed = (unsigned int) tp.tv_usec;
  srand (seed);

  /* test_qsort (20, 1000000, 100000); */

  test_qset (atoi (argv [1]));

  exit (EXIT_SUCCESS);
}
# endif /* defined UNSINN */

/*
  $Log$
  Revision 1.10  2004/12/21 15:37:31  beck
  added config.h include
  removed unused sys/times.h
  removed C99 constructs

  Revision 1.9  2004/12/20 17:34:35  liekweg
  fix recursion handling

  Revision 1.8  2004/12/06 12:49:26  liekweg
  virtually no change

  Revision 1.7  2004/11/30 14:47:11  liekweg
  insert report changes

  Revision 1.6  2004/11/26 15:58:30  liekweg
  don't free inside obstacks (thx, michael)

  Revision 1.5  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.4  2004/11/18 16:35:45  liekweg
  Added unique ids for debugging

  Revision 1.3  2004/11/09 16:45:36  liekweg
  print pointers

  Revision 1.2  2004/11/08 12:32:00  liekweg
  Moved q_* methods into private section

  Revision 1.1  2004/11/04 14:55:13  liekweg
  added qset


 */
