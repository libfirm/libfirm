/*
 * Project:     libFIRM
 * File name:   ir/ana/structure.h
 * Purpose:     structure analysis
 * Author:      Michael Beck
 * Modified by:
 * Created:     05.04.2007
 * CVS-ID:      $Id: $
 * Copyright:   (c) 2007 Universität Karlsruhe
 * License:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_STRUCTURE_H
#define _FIRM_STRUCTURE_H

#include "firm_types.h"

typedef enum ir_region_kind {
	ir_rk_Unknown,     /**< Unknown region kind. */
	ir_rk_Block,       /**< A Block simply wraps a basic block, needed for the construction. */
	ir_rk_Sequence,    /**< A sequence of regions. */
	ir_rk_IfThen,      /**< An if-then. */
	ir_rk_IfThenElse,  /**< An if-then-else. */
	ir_rk_Case,        /**< A Case like in Pascal. No fall through is allowed. */
	ir_rk_Switch,      /**< A Switch like in C. Fall through is allowed. */
	ir_rk_Proper,      /**< A proper region. */
	ir_rk_SelfLoop,    /**< A self loop. */
	ir_rk_RepeatLoop,  /**< A Repeat loop. */
	ir_rk_WhileLoop,   /**< A While loop. */
	ir_rk_NaturalLoop, /**< A natural loop. */
	ir_rk_Improper,    /**< An improper region. */
} ir_region_kind;

#endif /* _FIRM_STRUCTURE_H */
