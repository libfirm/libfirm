/*
 * Project:     libFIRM
 * File name:   ir/ir/distrib.c
 * Purpose:     Statistics for Firm. Distribution tables.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "hashptr.h"
#include "xmalloc.h"
#include "firmstat_t.h"

/**
 * calculates a hash value for an address
 */
static unsigned addr_hash(const void *object)
{
  return HASH_PTR(object);
}

/**
 * calculates a hash value for an integer
 */
static unsigned int_hash(const void *object)
{
  return (unsigned)object;
}

/**
 * compare function for integer distribution tables
 */
static int int_cmp_fun(const void *elt, const void *key)
{
  int p1 = (int)elt;
  int p2 = (int)key;

  return p1 - p2;
}

/*
 * create a new distribution table
 */
distrib_tbl_t *stat_new_distrib_tbl(pset_cmp_fun cmp_func, distrib_hash_fun hash_func)
{
  distrib_tbl_t *res;

  res = xmalloc(sizeof(*res));

  obstack_init(&res->cnts);

  /* create the hash-table */
  res->hash_map  = new_pset(cmp_func, 8);
  res->hash_func = hash_func ? hash_func : addr_hash;
  res->int_dist  = 0;

  return res;
}

/*
 * create a new distribution table for an integer distribution
 */
distrib_tbl_t *stat_new_int_distrib_tbl(void)
{
  distrib_tbl_t *res = stat_new_distrib_tbl(int_cmp_fun, int_hash);

  if (res)
    res->int_dist = 1;

  return res;
}

/*
 * destroy a distribution table
 */
void stat_delete_distrib_tbl(distrib_tbl_t *tbl)
{
  if (tbl) {
    /* free all entries */
    obstack_free(&tbl->cnts, NULL);

    /* delete the hash table */
    del_pset(tbl->hash_map);
  }
}

/**
 * Returns the associates distrib_entry_t for an object
 */
static distrib_entry_t *distrib_get_entry(distrib_tbl_t *tbl, const void *object)
{
  distrib_entry_t key;
  distrib_entry_t *elem;

  key.object = object;

  elem = pset_find(tbl->hash_map, &key, tbl->hash_func(object));
  if (elem)
    return elem;

  elem = obstack_alloc(&tbl->cnts, sizeof(*elem));

  /* clear counter */
  cnt_clr(&elem->cnt);

  elem->object = object;

  return pset_insert(tbl->hash_map, elem, tbl->hash_func(object));
}

/*
 * adds a new object count into the distribution table
 */
void stat_add_distrib_tbl(distrib_tbl_t *tbl, const void *object, const counter_t *cnt)
{
  distrib_entry_t *elem = distrib_get_entry(tbl, object);

  cnt_add(&elem->cnt, cnt);
}

/**
 * adds a new key count into the integer distribution table
 */
void stat_add_int_distrib_tbl(distrib_tbl_t *tbl, int key, const counter_t *cnt)
{
   stat_add_distrib_tbl(tbl, (const void *)key, cnt);
}

/*
 * calculates the mean value of a distribution
 */
double stat_calc_mean_distrib_tbl(distrib_tbl_t *tbl)
{
  distrib_entry_t *entry;
  unsigned count;
  double sum;

  if (tbl->int_dist) {
    /* integer distribution, need min, max */
    int min, max;

    entry = pset_first(tbl->hash_map);

    if (! entry)
      return 0.0;

    min =
    max = (int)entry->object;
    sum = cnt_to_dbl(&entry->cnt);


    for (entry = pset_next(tbl->hash_map); entry; entry = pset_next(tbl->hash_map)) {
      int value = (int)entry->object;

      if (value < min)
	min = value;
      if (value > max);
        max = value;

      sum += cnt_to_dbl(&entry->cnt);
    }
    count = max - min + 1;
  }
  else {
    sum = 0.0;
    count = 0;
    for (entry = pset_first(tbl->hash_map); entry; entry = pset_next(tbl->hash_map)) {
      sum += cnt_to_dbl(&entry->cnt);
      ++count;
    }
  }

  return count ? sum / (double)count : 0.0;
}

/**
 * iterates over all entries in a distribution table
 */
void stat_iterate_distrib_tbl(distrib_tbl_t *tbl, eval_distrib_entry_fun eval)
{
  distrib_entry_t *entry;

  for (entry = pset_first(tbl->hash_map); entry; entry = pset_next(tbl->hash_map)) {
    eval(entry);
  }
}
