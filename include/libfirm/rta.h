/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief    Interprocedural analysis to improve the call graph estimate.
 * @author   Florian
 * @date     09.06.2002
 * @version  $Id$
 * @brief
 * (TODO translate to english)
 * Intraprozedurale Analyse zur Absch�tzung der Aufrufrelation. Es wird
 * die Menge der instantiierten Klassen bestimmt, und daraus eine Absch�tzung
 * der aufgerufenen Methoden.
 *
 * Voraussetzung ist, dass das Programm keine Methodenzeiger handhaben kann.
 * In diesem Fall koennten Methoden verloren gehen.  Oder wir muessen nach
 * allen "freien" Methoden suchen (siehe cgana).
 *
 * @@@ Die Analyse sollte wissen, von welchen Klassen Instanzen ausserhalb
 * der Uebersetzungseinheit alloziert werden koennen.  Diese muessen in
 * die initiale Menge allozierter Klassen aufgenommern werden.
 *
 * Nach: David F. Bacon and Peter F. Sweeney,
 *       Fast static analysis of C++ virtual function calls
 *       OOPSLA 1996
 */
#ifndef FIRM_ANA_RTA_H
#define FIRM_ANA_RTA_H

#include "firm_types.h"

#include "begin.h"

/**
 * Initialise the RTA data structures and perform RTA.
 * Expects that all allocations are done by Alloc nodes.
 */
FIRM_API void rta_init(void);

/**
 * Delete all graphs that have been found to be dead.
 */
FIRM_API void rta_delete_dead_graphs (void);

/** Clean up our data structures.
 * Finishes the RTA.
 */
FIRM_API void rta_cleanup(void);

/** Returns non-zero if the given class is alive. */
FIRM_API int rta_is_alive_class(ir_type *clazz);

/** Returns non-zero if the given graph is alive. */
FIRM_API int rta_is_alive_graph(ir_graph *graph);

/** report for all graphs and types whether they are alive */
FIRM_API void rta_report(void);

#include "end.h"

#endif
