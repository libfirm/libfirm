/*
 * Project:     libFIRM
 * File name:   ir/adt/eset.h
 * Purpose:     Datentyp: Vereinfachte Menge (hash-set) zum Speichern von
 *              Zeigern/Adressen.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _ESET_H_
#define _ESET_H_

/**
 * "eset" is a set of addresses. The addresses are used for element
 * compare and hash calculation.
 * The value "NULL" could not be stored, as it is used as internal sentinel.
 */
typedef struct eset eset;

/** Creates a new empty set. */
eset *eset_create(void);

/**
 * Creates a copy of the given set. Did NOT work if NULL is contained in source. */
eset *eset_copy(eset *source);

/** Deletes a set. */
void eset_destroy(eset *s);

/** Inserts an address into the set. */
void eset_insert(eset *s, void *p);

/** Checks, wheater an address is element of a set. */
int eset_contains(eset *s, void *p);

/**
 * Starts the iteration over a set and returns the first element or NULL
 * if the set is empty.
 *
 * @note: It is NOT possible to add new elements while iterating through a set.
 */
void *eset_first(eset *s);

/**
 * Continues iteration through a set and returns the next element or NULL if the
 * iteration is finished.
 *
 * @note: It is NOT possible to add new elements while iterating through a set.
 */
void *eset_next(eset *s);

/** Inserts all elements of source into target (union). Did NOT work if NULL is contained in source. */
void eset_insert_all(eset *target, eset *source);

#endif /* _ESET_H_ */
