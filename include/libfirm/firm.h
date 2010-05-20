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
 * @brief     Central firm header.
 * @author    Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 * @version   $Id$
 * @brief
 *  Central FIRM header.
 *
 *  FIRM is a full graph based intermediate representation in SSA Form
 *  with a novel concept to model side effects.  It allows fast, aggressive
 *  optimizations.
 *
 *  This header is the central header of the library implementation of this
 *  IR.
 *
 *  The internal representation of a program in firm is separated into five
 *  different modules:
 *   - Firm Graphs representing the code of a program. (Subdirectory ir.)
 *     Firm Graphs are assembled out of several data structures:
 *     irprog: represents a program.  Allows access to all types and all
 *       FIRM graphs for procedures and other global things.
 *     irgraph: represents a procedure.  Allows access to the code of the
 *       procedure, the actual FIRM graph.
 *     irnode: A node of a FIRM graph.  Nodes are typed with an opcode and a mode
 *   and represent instructions in a program.
 *     irop: The opcode of FIRM nodes.
 *     irmode: The mode of FIRM nodes.  Most modes correspond to machine known
 *       data types (int, float, pointer).
 *   - Entities representing program known objects. (Subdirectory tr.)
 *     All variables and procedures are entities.
 *   - Types describing the type system for the program. (Subdirectory tr.)
 *   - Target Values representing program known constants. (Subdirectory tv.)
 *   - Identifiers representing any Strings used in the program. (Subdirectory ident.)
 *
 *   Further this library supplies functionality to build and optimize FIRM graphs
 *   and further functionality needed in a compiler.  Finally there is more
 *   generic functionality to support implementations using firm.  (Code generation,
 *   further optimizations).
 */
#ifndef FIRM_COMMON_FIRM_H
#define FIRM_COMMON_FIRM_H

#include "analyze_irg_args.h"
#include "be.h"
#include "callgraph.h"
#include "cdep.h"
#include "cgana.h"
#include "dbginfo.h"
#include "execfreq.h"
#include "execution_frequency.h"
#include "field_temperature.h"
#include "firm_common.h"
#include "firmstat.h"
#include "firm_types.h"
#include "height.h"
#include "ident.h"
#include "interval_analysis.h"
#include "irarch.h"
#include "ircgcons.h"
#include "ircgopt.h"
#include "irconsconfirm.h"
#include "ircons.h"
#include "irdom.h"
#include "vrp.h"
#include "irdump.h"
#include "iredgekinds.h"
#include "iredges.h"
#include "irextbb.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irio.h"
#include "irloop.h"
#include "irmemory.h"
#include "irmode.h"
#include "irnode.h"
#include "irop.h"
#include "iropt.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irpass.h"
#include "irprintf.h"
#include "irprog.h"
#include "irsimpletype.h"
#include "irtypeinfo.h"
#include "irvrfy.h"
#include "lowering.h"
#include "pseudo_irg.h"
#include "rta.h"
#include "seqnumbers.h"
#include "structure.h"
#include "timing.h"
#include "trouts.h"
#include "tv.h"
#include "typerep.h"

#endif
