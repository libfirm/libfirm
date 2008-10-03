#include "heuristical.h"
#include "html_dumper.h"

void solve_pbqp_heuristical(pbqp *pbqp)
{
	if (pbqp->dump_file) {
		dump_input(pbqp);
	}
}
