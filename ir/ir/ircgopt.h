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

/*
 * Project:     libFIRM
 * File name:   ir/ir/ircgopt.h
 * Purpose:     Removal of unreachable methods.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 */

/**
 * @file irgopt.h
 *
 * Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 * der nicht erreichbaren Methoden wird aus der Abschätzung der
 * Aufrufrelation bestimmt.
 */
#ifndef _FIRM_IR_ICGOPT_H_
#define _FIRM_IR_ICGOPT_H_

#include "firm_types.h"

/* Entfernt alle Methoden, die von den Methoden aus "keep_arr"
 * (bezgl. der Abschaetzung get_Call_callee) nicht erreichbar sind. Die
 * Abschaetzung der Aufrufrelation muss entsprechend an den
 * Call-Operationen gespeichert sein. Die "entity->link"s werden dabei
 * ueberschrieben.
 *
 * Frees all interprocedural loop information. */
void gc_irgs(int n_keep, ir_entity *keep_arr[]);

#endif /* _FIRM_IR_ICGOPT_H_ */
