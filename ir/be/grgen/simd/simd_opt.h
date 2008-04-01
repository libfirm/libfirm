/*******************************************************************************************
* Program:  simd_opt.h
* Function: Part of the simd optimization. Searches match of complex operation pattern and
*           inserts the complex operation if possible.
* Author:   Andreas Schoesser
* Date:     13.02.2007
*******************************************************************************************/


#ifndef __SIMD_OPT_H__
#define __SIMD_OPT_H__

/** Performs the searching and rewriting of patterns for the given ir graph */
void ext_grs_simd_opt(ir_graph *irg);

#endif
