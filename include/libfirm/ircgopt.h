/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Removal of unreachable methods.
 * @author  Hubert Schmid
 * @date    09.06.2002
 * @version $Id$
 * @summary
 *  (TODO: translate to english)
 *  Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 *  der nicht erreichbaren Methoden wird aus der Abschätzung der
 *  Aufrufrelation bestimmt.
 */
#ifndef FIRM_IR_ICGOPT_H
#define FIRM_IR_ICGOPT_H

#include "firm_types.h"

/* Entfernt alle Methoden, die von den Methoden aus "keep_arr"
 * (bezgl. der Abschaetzung get_Call_callee) nicht erreichbar sind. Die
 * Abschaetzung der Aufrufrelation muss entsprechend an den
 * Call-Operationen gespeichert sein. Die "entity->link"s werden dabei
 * ueberschrieben.
 *
 * Frees all interprocedural loop information. */
void gc_irgs(int n_keep, ir_entity *keep_arr[]);

#endif
