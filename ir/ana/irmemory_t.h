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
	ir_sc_pointer           = 0x0,  /**< generic pointer, may be anything */
	ir_sc_globalvar         = 0x1,  /**< an address of a global variable */
	ir_sc_localvar          = 0x2,  /**< an address of a local variable */
	ir_sc_argument          = 0x3,  /**< an address of a function argument */
	ir_sc_tls               = 0x4,  /**< an address of a thread local storage
	                                     variable */
	ir_sc_malloced          = 0x5,  /**< an allocated heap address */
	ir_sc_globaladdr        = 0x6,  /**< a constant address of something */
	ir_sc_null              = 0x7,  /**< null pointer */

	ir_sc_modifier_nottaken = 0x80, /**< if set, the address of the variable
	                                     was not taken */
	ir_sc_modifiers         = ir_sc_modifier_nottaken
} ir_storage_class_class_t;
ENUM_BITSET(ir_storage_class_class_t)

/** Returns the base storage class (ignore modifier) */
ir_storage_class_class_t get_base_sc(ir_storage_class_class_t x);

/**
 * Classify a base pointer.
 *
 * @param irn  the node representing the base address
 * @param ent  the base entity of the base address iff any
 */
ir_storage_class_class_t classify_pointer(const ir_node *irn,
                                          const ir_entity *ent);

#endif
