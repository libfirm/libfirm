/*************************************************************************
* Program:  be_spec_dumper.c
* Function: Generates and dumps the specification for the ia32 backend
*           for each generated pattern.
* Author:   Andreas Schoesser
* Date:     2007-03-02
*************************************************************************/

#ifndef __BE_SPEC_DUMPER__
#define __BE_SPEC_DUMPER__


typedef struct				// Holds the environment used by the backend specification dumper
{
	FILE *output_file;		// The file the backend specification is written to
} be_spec_env_t;


be_spec_env_t	*init_be_spec();
void			dump_be_spec(be_spec_env_t *be_spec_env, graph_ana_info_t *graph_ana_info, int uses_memorys);
void			deinit_be_spec(be_spec_env_t *be_spec_env);

#endif
