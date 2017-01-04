/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Central firm header.
 * @author    Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 * This header includes all other firm headers and can be included conveniently
 * by users of the library.
 */

/** @mainpage
 *
 *  FIRM is a full graph based intermediate representation in SSA Form
 *  with a novel concept to model side effects.  It allows fast, aggressive
 *  optimizations.
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
 *   generic functionality to support implementations using firm.
 *   (Code generation, further optimizations).
 */

/** @defgroup irana Analyses */

/** @defgroup adt Abstract Data Structures
 * This module contains abstract datatypes like lists and hashmaps.
 * They're provided as a convenience, the firm API is fully functionaly without
 * them so you can just as well use a library like glib and libapr or write
 * your own.
 */

/** @defgroup algorithms Algorithms
 * This module contains generic algorithms like bipartite matching or solvers
 * for linear equation systems.
 * They're provided as a convenience, the firm API is fully functionaly without
 * them so you can just as well use a library like glib and libapr or write
 * your own.
 */

/** @defgroup printing Printing and Visualisation
 * This module contains functions for printing and visualizing libfirm
 * data structures like programs, graphs and nodes for humans.
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
#include "firm_common.h"
#include "firm_types.h"
#include "heights.h"
#include "ident.h"
#include "ircgopt.h"
#include "ircons.h"
#include "irconsconfirm.h"
#include "irdom.h"
#include "irdump.h"
#include "iredgekinds.h"
#include "iredges.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irio.h"
#include "irloop.h"
#include "irmemory.h"
#include "irmode.h"
#include "irnode.h"
#include "irop.h"
#include "iropt.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irprintf.h"
#include "irprog.h"
#include "irverify.h"
#include "lowering.h"
#include "target.h"
#include "timing.h"
#include "tv.h"
#include "typerep.h"
#include "vrp.h"

#endif
