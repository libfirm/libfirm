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
 * @brief       Implementation of the RA-Interface for an external, (non-SSA) register allocator.
 * @author      Daniel Grund
 * @date        17.01.2006
 * @version     $Id$
 *
 * The external register allocator is a program:
 *    PROG -i INPUTFILE -o OUTPUTFILE
 *
 *   1) Input file defines the interference graph
 *   2) Output file contains the instructions to perform
 *


The input file format
----------------------

inputfile	::= regs nodes interf affinities .

regs		::= 'regs' regcount .						// Anzahl der register (0..regcount-1), die zur Verfuegung stehen

nodes		::= 'nodes' '{' node* '}' .					// All nodes in the graph

node		::= node-info
			  | node-info '<' reg-nr '>' .				// Reg-nr is present in case of constraints

node-info	::= node-nr spill-costs .

interf		::= 'interferences' '{' i-edge* '}' .		// Interference edges of the graph

i-edge		::= '(' node-nr ',' node-nr ')' .

affinities	::= 'affinities' '{' a-edge* '}' .			// Affinity edges of the graph

a-edge		::= '(' node-nr ',' node-nr ',' weight ')' .


weight, regcount, node-nr ::= int32 .
spill-costs ::= int32 .									// negative spill costs indicate unspillable

The output file format
-----------------------

outputfile	::= spills | allocs .

spills		::= 'spills' node-nr+ .

allocs		::= 'allocs' alloc* .

alloc		::= node-nr reg-nr .

*/

#ifndef FIRM_BE_BERAEXTERN_H
#define FIRM_BE_BERAEXTERN_H

#include "bera.h"

#endif /* FIRM_BE_BERAEXTERN_H */
