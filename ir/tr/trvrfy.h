/*
 * Project:     libFIRM
 * File name:   ir/tr/trvrfy.h
 * Purpose:     Check types and entities for correctness.
 * Author:      Michael Beck, Goetz Lindenmaier
 * Modified by:
 * Created:     29.1.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef TRVRFY_H
#define TRVRFY_H

#include "firm_types.h"

/**
 * @file trvrfy.h
 *
 * Methods to verify the type representations.
 *
 * @author Michael Beck, Goetz Lindenmaier
 *
 * Methods to verify the type representations.
 * Copyright 2003 University of Karlsruhe.
 * Created 29.1.2003.
 *
 */

/**
 * possible trvrfy() error codes
 */
enum trvrfy_error_codes {
  no_error = 0,                      /**< no error */
  error_ent_not_cont,                /**< overwritten entity not in superclass */
  error_null_mem,                    /**< compound contains NULL member */
  error_const_on_wrong_irg,          /**< constant placed on wrong IRG */
  error_existent_entity_without_irg, /**< Method entities with pecularity_exist must have an irg */
  error_wrong_ent_overwrites,        /**< number of entity overwrites exceeds number of class overwrites */
  error_inherited_ent_without_const, /**< inherited method entity not pointing to existent entity */
  error_glob_ent_allocation,         /**< wrong allocation of a global entity */
  error_ent_const_mode,              /**< Mode of constant in entity did not match entities type. */
  error_ent_wrong_owner              /**< Mode of constant in entity did not match entities type. */
};

/**
 * Checks a type.
 *
 * @return
 *  0   if no error encountered
 */
int check_type(ir_type *tp);

/**
 * Check an entity. Currently, we check only if initialized constants
 * are build on the const irg graph.
 *
 * @return
 *  0   if no error encountered
 *  != 0    a trvrfy_error_codes code
 */
int check_entity(ir_entity *ent);

/**
 * Walks the type information and performs a set of sanity checks.
 *
 * Currently, the following checks are executed:
 * - values of initialized entities must be allocated on the constant IRG
 * - class types: doesn't have NULL members
 * - class types: all overwrites are existent in the super type
 *
 * @return
 *    0 if graph is correct
 *    else error code.
 */
int tr_vrfy(void);

/**
 * If NDEBUG is defined performs nothing, else calls the tr_vrfy() function.
 */
#ifdef NDEBUG
#define TR_VRFY()	0
#else
#define TR_VRFY()	tr_vrfy()
#endif

#endif /* TRVRFY_H */
