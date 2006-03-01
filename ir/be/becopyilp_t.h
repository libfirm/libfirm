/**
 * Author:      Daniel Grund
 * Date:		28.02.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Common stuff used by all ILP fomulations.
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <lpp/lpp.h>
#include <lpp/lpp_net.h>
#include <lpp/lpp_cplex.h>
#include <lpp/lpp_remote.h>
#include "xmalloc.h"
#include "pset.h"
#include "irprog.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "bechordal_t.h"
#include "becopyopt_t.h"
#include "becopystat.h"
#include "besched_t.h"
#include "phiclass.h"

#define LPP_HOST "i44pc52"
#define LPP_SOLVER "cplex"

#define MAX(a,b) ((a<b)?(b):(a))
#define MIN(a,b) ((a<b)?(a):(b))
#define EPSILON 0.00001
