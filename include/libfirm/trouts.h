/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 */
#ifndef FIRM_ANA_TROUTS_H
#define FIRM_ANA_TROUTS_H

#include <stddef.h>

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup ir_type
 * @defgroup trouts Reverse Type Edges
 * Trouts list all uses of types and entities.
 * Each type gets a list of all Alloc nodes allocating it.
 * Each entity gets two lists:
 *   - one containing all accesses (Load, (Call), Store),
 *   - and one containing all uses to get a reference (Sel, EntConst).
 * @{
 */

/** Returns number of Load/Store nodes that possibly access entity @p entity. */
FIRM_API size_t get_entity_n_accesses(const ir_entity *entity);
/** Returns Load/Store node number @p pos that possibly accesses entity @p entity. */
FIRM_API ir_node *get_entity_access(const ir_entity *entity, size_t pos);

/** Returns number of references to entity @p entity, in form of EntConst/Sel,
 * including references from constant entities and the like. */
FIRM_API size_t get_entity_n_references(const ir_entity *entity);
/** Returns reference number @p pos of references to an entity, in form of
 * EntConst/Sel, including references from constants. */
FIRM_API ir_node *get_entity_reference(const ir_entity *entity, size_t pos);

/** Returns number of Alloc nodes that create an instance of type @p type. */
FIRM_API size_t get_type_n_allocs(const ir_type *type);
/** Returns Alloc node number @p pos that create an instance of type @p type. */
FIRM_API ir_node *get_type_alloc(const ir_type *type, size_t pos);

/** Returns number of pointertypes that point to type @p type. */
FIRM_API size_t get_type_n_pointertypes_to(const ir_type *type);
/** Returns pointer type number @p pos that points to type @p type. */
FIRM_API ir_type *get_type_pointertype_to(const ir_type *type, size_t pos);

/** Returns number of array types with element type @p type. */
FIRM_API size_t get_type_n_arraytypes_of(const ir_type *type);
/** Returns array type number @p pos with element type @p type. */
FIRM_API ir_type *get_type_arraytype_of(const ir_type *type, size_t pos);

/** Computes the outs of types and entities.
 *
 *  Collects all reference from irnodes to types or entities in the
 *  corresponding types/entities.  Further reverses references between
 *  types and entities.
 *
 *  Annotates the following nodes:
 *    Alloc    --> get_Alloc_type()
 *    Cast     --> get_Cast_type()
 *    Sel      --> get_Sel_entity()
 *    EntConst --> get_EntConst_entity()
 *    Load(addr)  --> get_addr_entity() \  ent von EntConst, oder falls Sel: ent von
 *    Store(addr) --> get_addr_entity() /  outermost im compound.  Ansonsten: nirgends.
 *                                         d.h. wir bekommen die array Elementzugriffe
 *                                         an die jack array Klasse annotiert.
 *    Call(Sel)   --> get_Sel_entity()  // ev. Tabellenzugriff --> Load.
 *
 *   type --> pointer type refering to this type.
 *   type --> entity of this type. @@@ to be implemented.
 *
 *  Sets trout state to outs_consistent.
 */
FIRM_API void compute_trouts(void);

/** Frees trout data. */
FIRM_API void free_trouts(void);

/** @} */

#include "end.h"

#endif
