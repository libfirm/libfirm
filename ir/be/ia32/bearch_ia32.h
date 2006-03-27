#ifndef _BEARCH_IA32_H_
#define _BEARCH_IA32_H_

#include "pset.h"
#include "../bearch.h"

#define USE_SSE2(cg) ((cg)->fp_kind == fp_sse2)
#define USE_x87(cg)  ((cg)->fp_kind == fp_x87)

extern const arch_isa_if_t ia32_isa_if;

#endif /* _BEARCH_IA32_H_ */
