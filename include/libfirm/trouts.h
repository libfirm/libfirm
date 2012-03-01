/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 */
#ifndef FIRM_ANA_TROUTS_H
#define FIRM_ANA_TROUTS_H

#include "firm_types.h"
#include "irgraph.h"

#include "begin.h"

/**
 * @ingroup ir_type
 * @defgroup trouts Reverse Type Edges
 * Trouts list all uses of types and entities.
 * Each type gets a list of all Alloc nodes allocating it.
 * Each entity gets two lists:
 *   - one containing all accesses (Load, (Call), Store),
 *   - and one containing all uses to get a reference (Sel, SymConst).
 * @{
 */

/** Returns number of Load/Store nodes that possibly access entity @p entity. */
FIRM_API size_t get_entity_n_accesses(const ir_entity *entity);
/** Returns Load/Store node number @p pos that possibly accesses entity @p entity. */
FIRM_API ir_node *get_entity_access(const ir_entity *entity, size_t pos);

/** Returns number of references to entity @p entity, in form of SymConst/Sel,
 * including references from constant entities and the like. */
FIRM_API size_t get_entity_n_references(const ir_entity *entity);
/** Returns reference number @p pos of references to an entity, in form of
 * SymConst/Sel, including references from constants. */
FIRM_API ir_node *get_entity_reference(const ir_entity *entity, size_t pos);

/** Returns number of Alloc nodes that create an instance of type @p type. */
FIRM_API size_t get_type_n_allocs(const ir_type *type);
/** Returns Alloc node number @p pos that create an instance of type @p type. */
FIRM_API ir_node *get_type_alloc(const ir_type *type, size_t pos);

/** Returns number of Cast nodes that cast a pointer to type @p type. */
FIRM_API size_t get_type_n_casts(const ir_type *type);
/** Cast node that cast a pointer to this type. */
FIRM_API ir_node *get_type_cast(const ir_type *type, size_t pos);
/** Returns number of upcasts. O(\#casts). */
FIRM_API size_t get_class_n_upcasts(const ir_type *clss);
/** Returns number of downcasts. O(\#casts). */
FIRM_API size_t get_class_n_downcasts(const ir_type *clss);

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
 *    SymConst --> get_SymConst_entity()
 *    Load(addr)  --> get_addr_entity() \  ent von SymConst, oder falls Sel: ent von
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
