/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date   17.02.2013
 * @author Tobias Rapp
 * @brief  Implementation of an addressable priority queue.
 */
#ifndef FIRM_ADT_APQUEUE_H
#define FIRM_ADT_APQUEUE_H

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup apqueue Addressable Priority Queue
 * Implementation of a pairing heap datastructure
 * @{
 */

/** Addressable priority queue. */
typedef struct apqueue_t apqueue_t;

/** Address to an element in the priority queue. */
typedef struct apqueue_el_t apqueue_el_t;

/**
 * Create a new addressable priority queue.
 * @return An addressable priority queue of initial length 0.
 */
apqueue_t *new_apqueue(void);

/**
 * Free all allocated memory and thereby destroy the priority queue.
 * @param q The priority queue to destroy.
 */
void del_apqueue(apqueue_t *q);

/**
 * Insert a new element into the addressable priority queue.
 * @param q        The priority queue in which the element will be inserted.
 * @param data     The actual data.
 * @param priority The priority of the element.
 */
apqueue_el_t* apqueue_put(apqueue_t *q, void *data, int priority);

/**
 * Return and remove the element with the highest priority.
 * @note Asserts if the queue is empty.
 *
 * @param q The priority queue which will be used.
 * @return  The element with the highest priority.
 */
void *apqueue_pop_front(apqueue_t *q);

/**
 * Get the length of the addressable priority queue.
 * @param q  The priority queue.
 * @return   The length of the queue.
 */
size_t apqueue_length(const apqueue_t *q);

/**
 * Returns true if the priority queue is empty.
 * @param q The priority queue.
 * @return  1 if the queue is empty, 0 otherwise.
 */
int apqueue_empty(const apqueue_t *q);

/**
 * Returns true if the element with the specified address is in the priority queue.
 * @param address  A pointer to the element which will be tested.
 * @return         1 if the priority queue contains the element, 0 otherwise.
 */
int apqueue_contains(apqueue_el_t* address);

/**
 * Get the priority of the element with the specified address.
 * @param address  A pointer to the element.
 */
int apqueue_get_priority(apqueue_el_t* address);

/**
 * Remove and return an element from the priority queue.
 * @param q       The priority queue.
 * @param address A pointer to the element which will be deleted and returned.
 *
 * @return The removed element.
 */
void *apqueue_remove(apqueue_t *q, apqueue_el_t* address);

/**
 * Change the priority of an element in the priority queue.
 * @note Asserts if the queue does not contain the addressed element.
 *
 * @param q        The priority queue.
 * @param address  A pointer to the element whose priority will be changed.
 * @param priority The new priority.
 */
void apqueue_change_priority(apqueue_t *q, apqueue_el_t* address, int priority);

/** @} */

#include "../end.h"

#endif
