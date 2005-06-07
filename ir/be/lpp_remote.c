/**
 * Author:      Daniel Grund
 * Date:		02.06.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "lpp_remote.h"
#include "xmalloc.h"
#include "assert.h"
#include "mps.h"

/* CPLEX-account related stuff */
#define DELETE_FILES		/**< deletes all dumped files after use. Files on server are always deleted. */
#define SSH_USER_HOST   "kb61@sp-smp.rz.uni-karlsruhe.de"
#define SSH_PASSWD_FILE "/ben/daniel/.smppw"
#define EXPECT_FILENAME "runme" /* name of the expect-script */

static INLINE FILE *ffopen(const char *base, const char *ext, const char *mode) {
	FILE *out;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s.%s", base, ext);
	if (! (out = fopen(buf, mode))) {
		fprintf(stderr, "Cannot open file %s in mode %s\n", buf, mode);
		return NULL;
	}
	return out;
}

static void lpp_write_cmd(lpp_t *lpp) {
	FILE *out = ffopen(lpp->name, "cmd", "wt");
	fprintf(out, "set logfile %s.sol\n", lpp->name);
	fprintf(out, "set mip strategy mipstart 1\n");
	fprintf(out, "set mip emphasis 3\n"); /* moving best bound */
	fprintf(out, "set mip strategy variableselect 3\n"); /* strong branching */
	fprintf(out, "read %s.mps\n", lpp->name);
	fprintf(out, "read %s.mst\n", lpp->name);
	fprintf(out, "optimize\n");
	fprintf(out, "display solution variables -\n");
	fprintf(out, "quit\n");
	fclose(out);
}

static void lpp_write_exp(lpp_t *lpp) {
	FILE *pwfile, *out;
	char passwd[128];

	pwfile = fopen(SSH_PASSWD_FILE, "rt");
	fgets(passwd, sizeof(passwd), pwfile);
	fclose(pwfile);

	out = ffopen(EXPECT_FILENAME, "exp", "wt");
	fprintf(out, "#! /usr/bin/expect\n");
	fprintf(out, "spawn scp %s.mps %s.mst %s.cmd %s:\n", lpp->name, lpp->name, lpp->name, SSH_USER_HOST); /* copy problem files */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn ssh %s \"./cplex90 < %s.cmd\"\n", SSH_USER_HOST, lpp->name); /* solve */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn scp %s:%s.sol .\n", SSH_USER_HOST, lpp->name); /*copy back solution */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn ssh %s ./dell\n", SSH_USER_HOST); /* clean files on server */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);
	fclose(out);
}

static void lpp_read_solution(lpp_t *lpp) {
	FILE *in;
	double sol_time;
	unsigned iter;
	int vars_section = 0;
	char var_name[128];
	double var_value;

	if (!(in = ffopen(lpp->name, "sol", "rt"))) {
		lpp->sol_state = unknown;
		return;
	}
	while (!feof(in)) {
		char buf[1024];
		fgets(buf, sizeof(buf), in);

		/* error and solution state */
		if (!strncmp(buf, "CPLEX Error", 11))
			lpp->error = xstrdup(buf);
		else if (!strncmp(buf, "Warning:", 8))
			lpp->error = xstrdup(buf);
		else if (!strncmp(buf, "Integer optimal solution:", 25))
			lpp->sol_state = optimal;
		else if (!strcmp(buf, "No integer feasible solution exists."))
			lpp->sol_state = infeasible;
		else if (!strcmp(buf, "Error termination, integer feasible:"))
			lpp->sol_state = feasible;
		/* stats */
		else if (sscanf(buf, "Solution time = %lg sec. Iterations = %u", &sol_time, &iter) == 2) {
			lpp->sol_time = sol_time;
			lpp->iterations = iter;
		}
		/* variable values */
		else if(!strcmp(buf, "Variable Name           Solution Value")) {
			int i;
			vars_section = 1;
			for(i=0; i<lpp->var_next; ++i) {
				name_t *var = lpp->vars[i];
				var->value = 0;
				var->value_kind = value_solution;
			}
		}
		else if(!strncmp(buf, "All other var", 13))
			vars_section = 0;
		else if (vars_section) {
			if (sscanf(buf, "%s %lg", var_name, &var_value) == 2)
				lpp->vars[lpp_get_var_idx(lpp, var_name)]->value = var_value;
			else
				assert(0 && "There should be variables to read in!");
		}
	}
	fclose(in);
	if (lpp->error) {
		printf("\n%s\n", lpp->error);
		assert(0);
	}
}

#ifdef DELETE_FILES
static void lpp_delete_files(lpp_t *lpp) {
	char buf[1024];
	int end = snprintf(buf, sizeof(buf), "%s", lpp->name);

	snprintf(buf+end, sizeof(buf)-end, ".mps");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".mst");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".cmd");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".sol");
	remove(buf);
	remove(EXPECT_FILENAME ".exp");
}
#endif

void lpp_solve_remote(lpp_t *lpp) {
	FILE *out;
	out = ffopen(lpp->name, "mps", "wt");
	mps_write_mps(lpp, s_mps_free, out);
	fclose(out);

	out = ffopen(lpp->name, "mst", "wt");
	mps_write_mst(lpp, s_mps_free, out);
	fclose(out);

	lpp_write_cmd(lpp);
	lpp_write_exp(lpp);

	/* call the expect script */
	chmod(EXPECT_FILENAME ".exp", 0700);
	system(EXPECT_FILENAME ".exp");

	lpp_read_solution(lpp);
#ifdef DELETE_FILES
	lpp_delete_files(lpp);
#endif
}
