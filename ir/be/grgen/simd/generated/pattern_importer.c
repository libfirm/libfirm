/*************************************************************************
* Program:  pattern_importer.c
* Function: Includes all the generated patterns. This wrapper can turn off
*			the includation of generated patterns in case the gerenation went
*			wrong.
* Author:   Andreas Schoesser
* Date:     2007-01-09
*************************************************************************/

#include "../simd_presets.h"


// Only include the patterns of the INCLUDE_GENERATED_PATTERNS macro
// is set in simd_presets.h. Can be turned off in case gen_patterns.c
// contains errors

#ifdef INCLUDE_GENERATED_PATTERNS
  #include "firm.h"
  #include "../firm_node_ext.h"
  #include "gen_patterns.c"
#endif
