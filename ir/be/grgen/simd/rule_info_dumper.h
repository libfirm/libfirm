/***********************************************************************
* Program:		rule_info_dumper.c
* Function:	Dumps C source code containig information about the
*				generated GrGen rules into a .h file to be included
*				when the compiler is recompiled to use the new GrGen
*				rules.
* Author:		Andreas Schoesser
* Date:		2007-06-28
************************************************************************/

#ifndef __RULE_INFO_DUMPER__
#define __RULE_INFO_DUMPER__

#include <stdio.h>
#include "create_pattern_t.h"



typedef struct			// Holds information throughout the lifetime of a rule_info_dumper instance
{
	FILE *output_file;	// The file the rule information is written to
	int num_rules;		// Holds the number of rules dumped so far
} rule_info_env_t;


rule_info_env_t *init_rule_info_dumper(int max_num_rules);
void deinit_rule_info_dumper(rule_info_env_t *rule_info_env);
void dump_rule_info(rule_info_env_t *rule_info_env, graph_ana_info_t *graph_ana_info, int arity);


#endif
