/*************************************************************************
* Program:  grgen_dumper.c
* Function: Dumps parts of a firm graph (those which have to be extracted
*			 as search and replace patterns) as a grgen rule.
* Author:   Andreas Schoesser
* Date:     2006-12-07
*************************************************************************/

#ifndef GRGEN_DUMPER_H
#define GRGEN_DUMPER_H

#include "create_pattern_t.h"


typedef struct			// Holds information needed througout the usage of a grgen dumper instance
{
	FILE *output_file;	// The file the grgen rules will be dumped to
} grgen_dumper_env_t;



grgen_dumper_env_t	*init_grgen_dumper(char *file, int append);
int					dump_irgraph_grgen(grgen_dumper_env_t *grgen_dumper_env, graph_ana_info_t *walker_info);
void				deinit_grgen_dumper(grgen_dumper_env_t *grgen_dumper_env);
void				dump_irgraph_complete_grgen(ir_graph *irg, char *filename, int append);

#endif
