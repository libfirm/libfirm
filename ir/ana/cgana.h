/*
 * Project:     libFIRM
 * File name:   ir/ana/cgana.h
 * Purpose:     Intraprozedural analyses to estimate the call graph.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universit‰t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * Intraprozedurale Analyse zur Absch‰tzung der Aufrulrelation. Es
 * wird eine Menge von freien Methoden und anschlieﬂend die an den
 * Call-Operationen aufrufbaren Methoden bestimmt.
 *
 */

#ifndef _CGANA_H_
#define _CGANA_H_

#include "entity.h"

/* Methoden sind "frei", wenn ihr Funktionszeiger (potentiell)
 * "explizit" bekannt ist, d.h.:
 *
 * - die Methode ist von aussen sichtbar (external_visible).
 *
 * - ihr Funktionszeiger ist "frei", d.h. der Funktionszeiger wurde
 *   nicht ausschliesslich an den entsprechenden Eingang eines
 *   Call-Knotens weitergegeben, sondern z.B. in den Speicher
 *   geschrieben, als Parameter uebergeben, ...
 *
 * Die main-Methode ist immer in der Menge enthalten.
 *
 * Die Links an den "ir_node"s werden geloescht. */

/** Analyses a rough estimation of the possible call graph.
 *
 *  Bestimmt fuer jede Call-Operation die Menge der aufrufbaren Methode
 *  und speichert das Ergebnis in der Call-Operation. (siehe
 *  "set_Call_callee"). Die Methode gibt die Menge der
 *  "freien" Methoden zurueck, die vom Aufrufer wieder freigegeben
 *  werden muss (free).
 *  The algorithm implements roughly Static Class Hierarchy Analysis
 *  as described in "Optimization of Object-Oriented Programs Using
 *  Static Class Hierarchy Analysis" by Jeffrey Dean and David Grove
 *  and Craig Chambers.
 *
 *  Performs some optimizations possible by the analysed information:
 *    - Replace SymConst nodes by Const nodes if possible,
 *    - Replace (Sel-method(Alloc)) by Const method,
 *    - Replaces unreachable Sel nodes by Bad  (@@@ was genau meint unreachable?)
 *    - Replaces Sel-method by Const if the Method is never overwritten */
/*  @@@ I assume this can not be called via JNI :-( -- how to obtain the array pointer? */
void cgana(int *len, entity ***free_methods);

/* Optimize the address expressions passed to call nodes.
 * Performs only the optimizations done by cgana. */
/* @@@ move to irgopt ?! */
/* @@@ not fully implemented as buggy !!!  */
void opt_call_addrs(void);
#endif /* _CGANA_H_ */
