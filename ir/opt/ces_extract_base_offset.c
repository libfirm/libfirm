#include "ces_extract_base_offset.h"

#include "statev.h"
#include "debug.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irop.h"
#include "iroptimize.h"
#include "irverify.h"
#include "irprintf.h"
#include "adt/plist.h"
#include "tv.h"
#include "dbginfo.h"
#include "obstack.h"
#include "irouts.h"
#include "irdump_t.h"
#include "irnodemap.h"
#include "error.h"
#include "firm.h"
#include "bearch.h"
#include "bemodule.h"
#include "bemodule_t.h"

#include <assert.h>
#include <math.h>

extern int is_ProjP(ir_node* node);

/*
 * operates on duplicated tree
 * comparisons are done on original nodes
 * results are in duplicated irg
 */
int ces_extract_base_offset(ir_node* ptr, struct load_base* load_base) {
	int result = 0;

	//ATM we cannot analyse PHI
//    if( is_Phi(ptr) || is_Unknown(ptr) || !is_Add(ptr) || is_Unknown(get_Add_left(ptr)) )
	if( is_Phi(ptr) || is_Unknown(ptr) )
		return 0;


	//for bucketizing save original base node, not the copy
	if( is_ProjP(ptr) || is_Address(ptr) || is_Sel(ptr) ) {
		//Case without add
		load_base->base = ptr;
		load_base->c1 = 0;
		load_base->c2 = 0;
		load_base->c3 = 0;
		result = 1;

	} else if (is_Add(ptr)) {
		assert( is_Sel(get_Add_left(ptr)) || is_Phi(get_Add_left(ptr)) || is_ProjP(get_Add_left(ptr)) || is_Address(get_Add_left(ptr)) || is_Const(get_Add_left(ptr)) );
		load_base->base = get_Add_left(ptr); //set base
		assert(load_base->base);
		//cases with one add
		ir_node* right = get_Add_right(ptr);
		ir_node* left = get_Add_left(ptr);

		//case: (base + a*x)
		if (is_Mul(left) || is_Mul(right)) {
			ir_node* mul = is_Mul(left) ? left : right;
			load_base->c1 = get_binop_right(mul); //multiplicator
			load_base->c1_value = get_tarval_long(get_Const_tarval(load_base->c1));
			load_base->x = get_binop_left(mul);
			result=2;

		//case: (base + c3)
		} if (is_irn_constlike(right)) {
			load_base->c3 = right;
			load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
			result=1+2;

			//case: (base + 1*x + c3) --  cannot happen because of assert
		} else if( is_Phi(get_Add_left(ptr)) && is_irn_constlike(right) ) {
			load_base->c1_value = 1;
			load_base->c1 = (void *)1;
			load_base->x = get_binop_left(ptr);
			load_base->c3 = get_binop_right(ptr);
			load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
			DB((ces_dbg, LEVEL_DEFAULT, (PIRATE("PHI sighted. investigate!\nthis case is wrong, if add_left is phi it is still "
					"add_left==base cannot happen due to assert(is_projp(left)")) ));
			assert(0);
			result=1+3;

			//case: (base + 1*x)
		} else if (is_Proj(right)) {
			load_base->c1 = (void *)1;
			load_base->c1_value = 1;
			load_base->x  = right;
			result=1+4;

			//case: (base + 2*c1 * x)
		} else if (is_Shl(right)) {
			ir_node* shlLeft = right;
			assert( is_Proj(get_binop_left(shlLeft)) && is_irn_constlike(get_binop_right(shlLeft)) );

			load_base->c1 = get_binop_right(shlLeft);
			load_base->c1_value = pow(2, get_tarval_long(get_Const_tarval(load_base->c1)));
			load_base->x = get_binop_left(shlLeft);
			result=1+5;

			// cases with nested add: base + ( nodeX + nodeY)
		} else if (is_Add(right)) {
			ir_node* add2 = right;
			left = get_Add_left(add2);
			right = get_Add_right(add2);

			if (is_Mul(left) || is_Mul(right)) {
				//mul-left == x
				//mul-right== a
				//add-left ==c3
				ir_node* mul = is_Mul(left) ? left : right;
				load_base->c1 = get_binop_right(mul); //multiplicator
				load_base->c1_value = get_tarval_long(get_Const_tarval(load_base->c1));
				load_base->x  = get_binop_left(mul);
				load_base->c3 = is_Mul(left) ? right : left;
				if (!is_Const(load_base->c3))
					result=-1;
				else
					load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
				
				result=1+5;
			//case: base + (1*x + c3)
			} else if( is_Proj(get_Add_left(add2)) && is_Const(get_Add_right(add2)) ) {
				load_base->c1 = (void *)1;
				load_base->c1_value = 1;
				load_base->x  = get_Add_left(add2);
				load_base->c3 = get_Add_right(add2);
				load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
				result=1+1+6;

				//case: mirrored previous case -> base + (c3 + 1*x)
			} else if( is_Proj(get_Add_right(add2)) && is_Const(get_Add_left(add2)) ) {
				load_base->c1 = (void *)1;
				load_base->c1_value = 1;
				load_base->x  = get_Add_right(add2);
				load_base->c3 = get_Add_left(add2);
				load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
				result=1+1+7;
		
				//case: base + (x + (2^c1) * x) == base + (3*x)
			} else if( is_Proj(get_Add_left(add2)) && is_Shl(get_Add_right(add2)) ) {
				//TODO: what?? assert(ces_get_original(ces_copy_map, get_Add_left(add2)) == ces_get_original(ces_copy_map, get_Shl_left(get_Add_right(add2))) );
				assert(get_irn_link(get_Add_left(add2)) == get_irn_link(get_Shl_left(get_Add_right(add2))) );
				load_base->x  = get_Add_left(add2);
				load_base->c1 = get_binop_right(get_Add_right(add2));
				load_base->c1_value = 1 + pow(2, get_tarval_long(get_Const_tarval(load_base->c1)));
				result=1+1+8;

				//case: base + (2^c1*x + c3) == base + 2*x + c3
			} else if( is_Shl(get_Add_left(add2)) && is_Const(get_Add_right(add2)) ) {
				load_base->c1 = get_binop_right(get_Add_left(add2));
				load_base->c1_value = pow(2, get_tarval_long(get_Const_tarval(load_base->c1)));
				load_base->x  = get_binop_left(get_Add_left(add2));
				load_base->c3 = get_Add_right(add2);
				load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
				result=1+1+9;

				//case: see assert
			} else if( is_Add(get_Add_left(add2)) && is_Shl(get_Add_right(get_Add_right(add2))) ) {
				load_base->c2 = get_binop_right(get_Add_right(add2));
				load_base->c2_value = get_tarval_long(get_Const_tarval(load_base->c2)) * 2;
				load_base->y = (void *)5;
				load_base->x  = get_binop_left(get_Add_left(add2));
				load_base->c1 = get_binop_right(get_Add_right(get_Add_left(add2)));
				load_base->c1_value = get_tarval_long(get_Const_tarval(load_base->c1)) * 3;
				assert(0 && "is this case correct? 1xget_Add_right too much");
				result=1+1+10;

				//case: base + (c3 + 2^c1*x)
			} else if( is_Const(get_Add_left(add2)) && is_Shl(get_Add_right(add2)) ) {
				load_base->c3  = get_Add_left(add2);
				load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
				load_base->x = get_Shl_left(get_Add_right(add2));
				load_base->c1 = get_Shl_right(get_Add_right(add2));
				load_base->c1_value = pow(2, get_tarval_long(get_Const_tarval(load_base->c1)));
				result=1+1+11;

				//case: base + (
			} else if( is_Add(get_Add_right(add2)) ) {
				ir_node* add3 = get_Add_right(add2);
				if( is_Proj(get_Add_left(add3)) && is_Shl(get_Add_right(add3)) ) {
					load_base->x  = get_Add_left(add3);
					load_base->c1 = get_Shl_right(get_Add_right(add3));
					load_base->c1_value = get_tarval_long(get_Const_tarval( load_base->c1 )) * 3;

					load_base->c3 = get_Add_left(add2);
					load_base->c3_value = get_tarval_long(get_Const_tarval(load_base->c3));
					// 				load_base->c1 = get_Shl_right(get_Add_right(add3));//this line gives strange segfault
					result=1+1+12;
				}
			}
		} 
	} else {
		DBG((ces_dbg, LEVEL_3, YELLOW("could not match subgraph\n")));
		char title[30];
		ir_snprintf(title, 20, "subgraph_nomatch_%+F", ptr);
		DBG_DO(ces_dbg, LEVEL_5, dump_ir_graph(get_irn_irg(ptr), title));
	}

  if (result < 1) {
		DBG((ces_dbg, LEVEL_3, YELLOW("no base xtracted\n")));
		char title[30];
		ir_snprintf(title, 20, "no_base_%+F", get_irn_link(ptr));
		DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(get_irn_irg(ptr), title));
	}

	load_base->mode = get_irn_mode(load_base->base);
	load_base->size = get_mode_size_bits(load_base->mode);

	return result;
}

