/*************************************************************************
* Program:  presets.h
* Function: Preset values for simd optimizer
* Author:   Andreas Schoesser
* Date:     2006-12-20
*************************************************************************/

#ifndef _PRESETS_H_
#define _PRESETS_H_

#include "firm_config.h"

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif

//#ifndef INCLUDE_GENERATED_PATTERNS
//	#define INCLUDE_GENERATED_PATTERNS 1
//#endif

// ----------------------------------------------------------------------
// Possible VisualStudio command lines
// ----------------------------------------------------------------------
// --c99 -I C:\DevStudio\Include -f grs-create-pattern C:\diplomarbeit\firmext\grs\simd\testprograms\patterns\vadd_memory\vadd_memory.c
// --c99 -I C:\DevStudio\Include -f grs-simd-opt C:\diplomarbeit\firmext\grs\simd\testprograms\patterns\vadd_memory\vadd_memory.c


// ----------------------------------------------------------------------
// Presets for RULE generation
// ----------------------------------------------------------------------

#define MEMORY_ARRAY "memory"
#define REGISTER_STORE 1
#define REGISTER_LOAD  0
#define MEMORY_STORE 1
#define MEMORY_LOAD  0

#define RESULT_NAME    "Res"
static char *SIMD_ARGUMENTS[] = { "Arg_0", "Arg_1", "Arg_2", "Arg_3" };
#define MAX_SIMD_ARGUMENTS 4

#define VECTOR_OP_NAME "Vector_op"
#define VECTOR_OP_PROJM_NAME "Vector_op_ProjM"
#define VECTOR_OP_PROJDATA_NAME "Vector_op_ProjData"
#define COMPLEX_OPERATION_BLOCK_NAME "Vector_op_Block"
#define VECTOR_RESULT_NAME "Vector_Result"


#define EMIT "Emit"
#define DESTROYS "Destroys"
#define PRIORITY "Priority"
#define COST_SAVINGS "CostSavings"

#define VECTOR_BASE_TYPE "VectorBase"
#define VECTOR_BASE_NAME "NodeVectorBase"

#define MAX_ADDRESS_VARIANTS 3

#define PRIORITY_CLEANUP 10000



// ----------------------------------------------------------------------
// DIRECTRORY and FILE presets
// ----------------------------------------------------------------------

#define GRGEN_LOCATION "C:\\diplomarbeit\\grgen\\"
#define GENERATION_DEST "C:\\diplomarbeit\\firmext\\grs\\simd\\generated"
#define GRGEN_GRAPH_FILE "C:\\diplomarbeit\\firmext\\grs\\simd\\C_patterns\\complex_instructions.grg"
#define RULENAME_FILE "C:\\diplomarbeit\\firmext\\grs\\simd\\generated\\simd_rules.h"
#define BE_SPEC_FILE "C:\\diplomarbeit\\libfirm\\ir\\be\\ia32\\ia32_simd_spec.pl"

// ----------------------------------------------------------------------
// Optimization settings
// ----------------------------------------------------------------------

#define VERBOSE_REWRITE         1
#define PROMPT_REWRITE          1
#define PROMPT_NO_PATTERN_FOUND 0
#define DUMP_SEARCH_PLAN        0

//#define DUMP_EVERYTHING			1


#endif
