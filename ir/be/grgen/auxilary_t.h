#ifndef _EXT_GRS_AUXILARY_T_H_
#define _EXT_GRS_AUXILARY_T_H_


#include <math.h>
#include "common_t.h"



#ifndef ARR_SIZE
#define ARR_SIZE(a)	(sizeof(a)/sizeof(a[0]))
#endif


#define _ext_grs_MAX_LOG 10000
extern double _ext_grs_log_table[_ext_grs_MAX_LOG];
extern double _ext_grs_ln2;


void _ext_grs_log_table_init(void);

static double INLINE _log2(int x) {

	if (x >= ARR_SIZE(_ext_grs_log_table))
		return log(x) / _ext_grs_ln2;

	return _ext_grs_log_table[x];

}


#endif /* _EXT_GRS_AUXILARY_T_H_ */
