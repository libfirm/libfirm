/*
 * Project:     libFIRM
 * File name:   ir/opt/scalar_replace.h
 * Purpose:     scalar replacement of arrays and compounds
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _SCALAR_REPLACE_H_
# define _SCALAR_REPLACE_H_

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "tv.h"
#include "irgraph_t.h"
#include "irouts.h"
#include "pset.h"
#include "hashptr.h"
#include "irdump.h"
#include "ircons.h"
#include "irgmod.h"
#include "obst.h"
#include "irloop.h"
#include "irdom.h"
#include "irflag_t.h"
#include "array.h"


/*
 * find possible scalar replacements
 */
void find_scalar_replacements(ir_graph *irg);

#endif /* _SCALAR_REPLACE_H_ */
