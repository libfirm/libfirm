/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @summary
 * (TODO translate to english)
 * Intraprozedurale Analyse zur Abschätzung der Aufrufrelation. Es wird
 * die Menge der instantiierten Klassen bestimmt, und daraus eine Abschätzung
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

/**
 * Initialise the RTA data structures and perform RTA.
 *
 * @param do_verbose If == 1, print statistics, if > 1, chatter about every detail
 */
void rta_init(int do_verbose);

/**
 * Delete all graphs that have been found to be dead.
 */
void rta_delete_dead_graphs (void);

/** Clean up our data structures.
 * Finishes the RTA.
 */
void rta_cleanup(void);

/** Returns non-zero if the given class is alive. */
int rta_is_alive_class(ir_type *clazz);

/** Returns non-zero if the given graph is alive. */
int rta_is_alive_graph(ir_graph *graph);

/** report for all graphs and types whether they are alive */
void rta_report(void);

#endif

/*
 * $Log$
 * Revision 1.15  2006/12/18 16:02:21  beck
 * removed useles include
 *
 * Revision 1.14  2006/01/13 21:52:00  beck
 * renamed all types 'type' to 'ir_type'
 *
 * Revision 1.13  2004/10/21 07:23:34  goetz
 * comments
 *
 * Revision 1.12  2004/10/20 14:59:27  liekweg
 * Removed ecg
 *
 * Revision 1.11  2004/10/18 12:47:46  liekweg
 * minor fix
 *
 * Revision 1.10  2004/09/24 13:59:04  beck
 * fixed doxygen comments, removed initialization for description entities
 *
 * Revision 1.9  2004/08/19 16:51:02  goetz
 * fixed some errors, pushed closer to inteded firm semantics
 *
 * Revision 1.8  2004/06/18 17:34:31  liekweg
 * Removed field checks --flo
 *
 * Revision 1.7  2004/06/18 13:12:43  liekweg
 * final bug fix (calls via consts)
 *
 * Revision 1.6  2004/06/17 14:21:14  liekweg
 * major bugfix
 *
 * Revision 1.5  2004/06/17 08:33:01  liekweg
 * Added comments; added remove_irg
 *
 * Revision 1.4  2004/06/15 11:44:54  beck
 * New inlining schema implemented:
 *
 * small functions that should be inlined in libFirm are implemented in _t.h files
 * with a __ prefix.
 * Preprocessor magic is used to automatically inline these functions whenever a _t.h
 * file is included instead of a .h file.
 * Note that this magic did not work outside libFirm without accessing _t.h files.
 *
 * Revision 1.3  2004/06/13 15:03:45  liekweg
 * RTA auf Iterative RTA aufgebohrt --flo
 *
 * Revision 1.2  2004/06/12 17:09:46  liekweg
 * RTA works, outedges breaks.  "Yay." --flo
 *
 * Revision 1.1  2004/06/11 18:24:18  liekweg
 * Added RTA --flo
 *
 */
