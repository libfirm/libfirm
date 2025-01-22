/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Memory disambiguator
 * @author   Michael Beck
 * @date     27.12.2006
 */
#ifndef FIRM_ANA_IRMEMORY_T_H
#define FIRM_ANA_IRMEMORY_T_H

#include "irmemory.h"
#include <stdbool.h>

/**
 * One-time inititialization of the memory< disambiguator.
 */
void firm_init_memory_disambiguator(void);

bool is_partly_volatile(ir_node *ptr);

/**
 * Classify storage locations.
 * Except ir_sc_pointer they are all disjoint.
 * ir_sc_pointer potentially aliases all classes which don't have a
 * NOTTAKEN modifier.
 */
typedef enum ir_storage_class_class_t {
	ir_sc_pointer           = 0x00, /**< generic pointer, may be anything */
	ir_sc_globalvar         = 0x01, /**< an address of a global variable */
	ir_sc_localvar          = 0x02, /**< an address of a local variable */
	ir_sc_argument          = 0x03, /**< an address of a function argument */
	ir_sc_tls               = 0x04, /**< an address of a thread local storage
	                                     variable */
	ir_sc_malloced          = 0x05, /**< an allocated heap address */
	ir_sc_globaladdr        = 0x06, /**< a constant address of something */
	ir_sc_null              = 0x07, /**< null pointer */

	ir_sc_modifier_nottaken = 0x10 << 0, /**< if set, the address of the
	                                          variable was not taken */
	ir_sc_modifier_obj_prim = 0x10 << 1, /**< if set the address is for an
	                                          object with primitive type */
	ir_sc_modifier_obj_comp = 0x10 << 2, /**< if set the address is for an
	                                          object with compound type */
	ir_sc_modifiers         = 0xf0,
} ir_storage_class_class_t;
ENUM_BITSET(ir_storage_class_class_t)

/** Returns the base storage class (ignore modifier) */
ir_storage_class_class_t get_base_sc(ir_storage_class_class_t x);

/**
 * Classify a pointer.
 * @param addr the node representing the address
 * @param base the node representing the base address
 */
ir_storage_class_class_t classify_pointer(const ir_node *addr,
                                          const ir_node *base);

#endif
