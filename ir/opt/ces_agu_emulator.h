#ifndef __CES_AGU_EMULATOR_H__
#define __CES_AGU_EMULATOR_H__


#include "ces_si_tools.h"
#include "irnodeset.h"

/* framework includes */
#include "irnodemap.h" 
#include "adt/plist.h"


struct agu_params {
	int c1;
	int c2;
	int c3;
	int counter;
	int span;
	int skip;
	int stride;
	int total;
	int total_skips;
	int total_strides;
};

typedef void(*next_address_fun)(struct agu_params* agu_params);
typedef int (*match_memop_fun)(struct load_base* current_base, struct load_base* old_base, struct agu_params* params);
typedef void(*advance_addr_fun)(struct agu_params* params);
typedef	int(*adjust_params_fun)(struct agu_params* params, struct load_base* current_base, struct load_base* old_base);
typedef	void(*initialize_fun)(struct agu_params* params, struct load_base* current_base);

struct agu_strategy{
	initialize_fun initialize;
	advance_addr_fun advance_addr;
	match_memop_fun match_memop;
	adjust_params_fun adjust_params;
	char name[50];
};
struct stream_description {
	plist_t* memops;
	ir_nodeset_t* convex_set;
	struct agu_strategy strategy;
	struct agu_params params;
};

const struct agu_strategy* ces_agu_strategies(plist_t* qloads, struct agu_params* params, ir_nodemap* load_bases);

#endif //__CES_AGU_EMULATOR_H__
