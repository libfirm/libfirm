/*
 * Project:     libFIRM
 * File name:   ir/ana/field_temperature.h
 * Purpose:     Compute an estimate of field temperature, i.e., field access heuristic.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     21.7.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _FIELD_TEMPERATURE_H_
#define _FIELD_TEMPERATURE_H_

/**
 * @file field_temperature.h
 *
 *  Watch it! This is highly java dependent.
 *
 * - All Sel nodes get an array with possibly accessed entities.
 *   (resolve polymorphy on base of inherited entities.)
 *   (the mentioned entity in first approximation.)
 *
 * - Each entity gets all SymConst/Sel nodes, that reference the
 *   entity (Sel: references as accessed entity.)
 *
 * - We compute a value for the entity based on the Sel nodes.
 */

#include "irnode.h"
#include "entity.h"


/** The entities that can be accessed by this Sel node. */
int get_Sel_n_accessed_entities(ir_node *sel);
entity *get_Sel_accessed_entity(ir_node *sel, int pos);





/** compute the field temperature. */
void compute_field_temperature(void);

/** free occupied memory, reset */
void free_field_temperature(void);

#endif /* _FIELD_TEMPERATURE_H_ */
