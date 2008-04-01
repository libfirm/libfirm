#include "auxilary_t.h"



double _ext_grs_ln2;
double _ext_grs_log_table[_ext_grs_MAX_LOG];

void _ext_grs_log_table_init(void)
{
	int i;
	_ext_grs_ln2 = log(2.0);
	for (i=1; i < _ext_grs_MAX_LOG; i++) {
		_ext_grs_log_table[i] = log((double)i) / _ext_grs_ln2;
	}
}
