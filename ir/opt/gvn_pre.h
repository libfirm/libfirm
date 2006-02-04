/*
 * Project:     libFIRM
 * File name:   ir/opt/gvn_pre.h
 * Purpose:     Global Value Numbering Partial Redundancy Elimination
 * Author:      Michael Beck, Rubino Geiss
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef __FIRM_OPT_GVN_PRE_H__
#define __FIRM_OPT_GVN_PRE_H__

#include "firm_types.h"

/**
 * Does Partial Redundancy Elimination combined with
 * Global Value Numbering.
 * Can be used to replace place_code() completely.
 *
 * Based on VanDrunen and Hosking 2004.
 */
void do_gvn_pre(ir_graph *irg);

#endif /* __FIRM_OPT_GVN_PRE_H__ */
