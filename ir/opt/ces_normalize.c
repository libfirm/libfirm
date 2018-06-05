#include "ces_normalize.h"
#include "ces_si_tools.h"
#include "ces_time_measure.h"
#include "ces_extract_base_offset.h"

/* framework includes */
#include "firm.h"
#include "statev.h"
#include "debug.h"
#include "irnode.h"
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
#include "debug.h"
#include "error.h"
#include "irflag.h"
#include <setjmp.h>

/* local variables */

/* forward declarations */
void ces_normalize_walker(ir_node* node, void* env_p);

int handle_add(ir_node* node);
static int handle_shl(ir_node* node);


/* returns the on memop
 * this is an addr_irg with known structure:
 * end->return->proj->MEMOP
 */
static ir_node* get_memop(ir_graph* irg) {
	ir_node* end_block = get_irg_end_block(irg);
	return skip_Proj(get_Return_mem(get_Block_cfgpred(end_block,0)));
}


/*
 * merges a Mul(x, i) and a Proj(x) to Mul(x, i+1)
 */
int ces_reassociate_mul_proj(ir_node** add) {
	ir_node* left = get_Add_left(*add);
	ir_node* right = get_Add_right(*add);
	ir_node* mul = is_Mul(left) ? left : right;
	ir_node* addend = !is_Mul(left) ? left : right; //choose the opposite
	ir_node* multiplicant = get_Mul_left(mul);
	ir_node* multiplicator = get_Mul_right(mul);
	if ( get_irn_link(addend) != get_irn_link(multiplicant))
		return 0;

	ir_mode* mode = get_irn_mode(multiplicant);

	ir_tarval* tv = get_mode_one(mode);
	tv = get_Const_tarval(multiplicator);
	tv = tarval_add(tv, get_tarval_one(mode)); //tarval++

	if (tv == tarval_bad) {
		DBG((ces_dbg, LEVEL_3, YELLOW("reassociate failed: %+F\n"),*add));
		return 0;
	}

	ir_node* blk = get_nodes_block(mul);
	ir_graph* irg = get_irn_irg(blk);
	multiplicator = new_rd_Const(get_irn_dbg_info(left), irg, tv);
	set_Mul_right(mul, multiplicator);

	exchange(*add, mul);
	*add = mul;
	return 1;
}

int ces_reassociate_mul_add(ir_node** add) {
	if (!is_Add(*add))
		return 0;
	ir_node* left = get_Add_left(*add);
	ir_node* right = get_Add_right(*add);
	ir_node* mul1 = is_Mul(left) ? left : right;
	ir_node* next_level = !is_Mul(left) ? left : right; //choose the opposite

	if (!is_Add(next_level))
		return 0;

	left = get_Add_left(next_level);
	right = get_Add_right(next_level);

	if (!(is_Mul(left) || is_Mul(right)) )
		return 0;
	ir_node* mul2 = is_Mul(left) ? left: right;

	if ( get_irn_link(get_Mul_left(mul1)) != get_irn_link(get_Mul_left(mul2)) )
		return 0;

	//replace mul1->multiplicant by mul1->multiplicant + mul2->multiplicant
	ir_tarval* tv2 = get_Const_tarval(get_Mul_right(mul2));
	ir_tarval* tv1 = get_Const_tarval(get_Mul_right(mul1));

	if ( (tv1 == tarval_bad) || (tv2 == tarval_bad)) {
		DBG((ces_dbg, LEVEL_3, YELLOW("reassociate failed: %+F\n"),*add));
		return 0;
	}
	tv1 = tarval_add(tv1, tv2);
	ir_node* new_const = new_rd_Const(get_irn_dbg_info(mul2), get_irn_irg(mul2), tv1);
	set_Mul_right(mul2, new_const);
	//del_node(mul1); //no necessary, it will float - no other users (it has been cloned with only one user)
	exchange(*add , next_level); //replace & delete *add
	*add = next_level;
	return 1;
}


int ces_reassociate_mul_mul(ir_node** add) {
	ir_node* mul1 = get_Add_left(*add);
	ir_node* mul2 = get_Add_right(*add);

	if ( get_irn_link(get_Mul_left(mul1)) != get_irn_link(get_Mul_left(mul2)))
		return 0;

	ir_mode* mode = get_irn_mode(mul1);

	ir_tarval* tv;
	tv = get_Const_tarval(get_Mul_right(mul1));
	tv = tarval_add(tv, get_Const_tarval(get_Mul_right(mul2)));

	if (tv == tarval_bad) {
		DBG((ces_dbg, LEVEL_3, YELLOW("reassociate failed @ %+F\n"),*add));
		return 0;
	}

	ir_node* blk = get_nodes_block(mul1);
	ir_graph* irg = get_irn_irg(blk);
	ir_node* multiplicator = new_rd_Const(get_irn_dbg_info(mul1), irg, tv);
	ir_node* irn = new_rd_Mul(get_irn_dbg_info(mul1), blk, get_Mul_left(mul1), multiplicator, mode);

	exchange(*add, irn); //replace and delete add
	*add = irn;
	return 1;
}

/*
 * Reassociate Shl. We transform Shl(x, const) into Mul's if possible.
 * taken from libFirm after removal in 	6525c4651a345ff3de23afa6ab98f8e92fd88b39
 */
int ces_reassociate_shl(ir_node **node)
{
	ir_node *n = *node;
	ir_node *c = get_Shl_right(n);
	ir_node *x, *blk, *irn;
	ir_graph *irg;
	ir_mode *mode;
	ir_tarval *tv;

	if (! is_Const(c))
		return 0;

	x = get_Shl_left(n);
	mode = get_irn_mode(x);

	tv = get_mode_one(mode);
	tv = tarval_shl(tv, get_Const_tarval(c));

	if (tv == tarval_bad)
		return 0;

	blk = get_nodes_block(n);
	irg = get_irn_irg(blk);
	c = new_r_Const(irg, tv);
	irn = new_rd_Mul(get_irn_dbg_info(n), blk, x, c, mode);

	if (irn != n) {
		exchange(n, irn);
		*node = irn;
		return 1;
	}
	return 0;
}

/*
 * this is to the best of my knowledge
 */
void ces_fix_mode(ir_node* node) {
	ir_mode* left = get_irn_mode(get_binop_left(node));
	ir_mode* right = get_irn_mode(get_binop_right(node));
	ir_mode* new_mode = get_irn_mode(node);

	if (is_ProjP(get_binop_left(node)) || is_ProjP(get_binop_right(node)))
		new_mode = mode_P;
	else if (mode_is_signed(left) && !mode_is_signed(right)) {
		//do NOT insert conv node into right
		new_mode = left;
	} else if (!mode_is_signed(left) && mode_is_signed(right)) {
		//do NOT insert conv node into left
		new_mode = right;
	}
	set_irn_mode(node, new_mode);
}


 int handle_add(ir_node* node) {

	ir_node* left = get_Add_left(node);
	ir_node* right= get_Add_right(node);

	/*
		if (is_Shl(get_Add_right(node)) && is_Shl(get_Add_left(node)) ) {
		ir_node* left = get_Add_left(node);
		ir_node* right = get_Add_right(node);
		//check required structure
		if( get_Shl_left(left) == get_Shl_left(right) && is_Const(get_Shl_right(left)) is_Const(get_Shl_right(right))){
		int factor =
		//remultiply
		ir_node* mul = new_rd_Mul
		}
		}
	 */
	if( is_ProjP(right) ) {
		//			DBG((ces_dbg, LEVEL_5, "%+F add transform1\n",load));
		ces_exchange_edge(node, right, node, left);
		//			DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(load, "ces_after_trans1_"));
		return 1;
	}

	if( !(is_Sel(left) || is_Proj(left)) && !is_irn_constlike(left) && is_irn_constlike(right) ) {
		//left(node)==proj has precedence over left(node)==const
		//			DBG((ces_dbg, LEVEL_5, "%+F add transform2\n", load));
		ces_exchange_edge(node,left, node, right);
		//			DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(load, "ces_after_trans2_"));
		return 1;
	}
	if( is_Add(right) && is_ProjP(get_Add_left(right)) ) {
		//			DBG((ces_dbg, LEVEL_5, "%+F add transform3\n", load));
		ces_exchange_edge(right, get_Add_left(right), node, left );
		//potentially fix mode of add
		ces_fix_mode(right);
		ces_fix_mode(node);
		//			DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(load, "ces_after_trans3_"));
		//Verify transform
		/* AS: can't irg_verify here due to walk within walk. */
		/* irg_verify(get_irn_irg(left)); */

		//recheck lower layer - this should be fixed with a queue
		//ces_normalize_load_ptr(right, load);
		DBG((ces_dbg, LEVEL_1, RED("recheck lower layer. fix code")));
		ces_dump_irg(node, "recheck_lower_layer");
		return 1;
	}

	if (is_Mul(left) && is_Mul(right) ) {
		optimization_state_t state;
		save_optimization_state(&state);
		set_optimize(0);
		ir_node* orig = get_irn_link(node);
		if (ces_reassociate_mul_mul(&node)) {
			set_irn_link(orig,node);
			set_irn_link(node, orig);
			//				DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(node, "ces_after_add_mul_"));
			restore_optimization_state(&state);
			return 1;
		}
	}

	if ( (is_Mul(left) && !is_Mul(right)) || (is_Mul(right) && !is_Mul(left)) ) {
		optimization_state_t state;
		save_optimization_state(&state);
		set_optimize(0);
		ir_node* orig = get_irn_link(node);
		int result = 0;
		result |= ces_reassociate_mul_proj(&node);
		result |= ces_reassociate_mul_add(&node);
		if (result) {
			set_irn_link(orig,node);
			set_irn_link(node, orig);
			//				DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(node, "ces_after_reassociate_mul_"));
			restore_optimization_state(&state);
			return 1;
		}
	}

	if (is_Add(right) && is_Const(left) && is_Sel(get_Add_left(right)) && is_Const(get_Add_right(right))  ){
		//collapse const      sel const
		//                     |   |
		//      const(left) (right)add
		//          |___  _____|
		//              add
		ir_tarval* sum = get_Const_tarval(left);
		ir_tarval* tv = tarval_add(sum, get_Const_tarval(get_Add_right(right)));
		ir_node* newConst = new_rd_Const(get_irn_dbg_info(right), get_irn_irg(node), tv);
		ir_node* sel = get_Add_left(right);
		exchange(right, sel); //replace and delete add
		set_Add_right(node, newConst);
		set_Add_left(node, sel);
		DB((ces_dbg, LEVEL_4, YELLOW("sel rule normalization")"\n"));
		// end is:      sel---add(node)---const
		//do not goto, left is changed
	}
	return 0;
}

static int handle_shl(ir_node* node) {
	extern int ces_reassociate_shl(ir_node**);
	ir_node* orig = get_irn_link(node);
	optimization_state_t state;
	save_optimization_state(&state);
	set_optimize(0);

	int result = ces_reassociate_shl(&node);
	restore_optimization_state(&state);
	if (result){
		set_irn_link(orig,node);
		set_irn_link(node, orig);
		//			DBG((ces_dbg, LEVEL_5, "after reassoc_shl: %+F\n", node));
		//			DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(node, "ces_after_shl_"));
	}
	return 0;
}


 void ces_normalize_walker(ir_node* node, void* env_p) {
	struct env{
		jmp_buf lonjmp_env;
		ir_graph* irg;
	};
	struct env* env = (struct env*)env_p;
	if( is_Load(node) || is_Store(node) ) {
		DBG((ces_dbg, LEVEL_3, RED("Error:") "Memop that depends on loaded address! %+F\n", node));
		DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(get_irn_irg(node), "err_memop_depends_on_memop"));
		longjmp(env->lonjmp_env,1);
	}

	if( is_Phi(node))
		return;

	int restart = 0;
	do {
		switch( get_irn_opcode(node) ) {
		case iro_Add:
			restart = handle_add(node);
			break;
		case iro_Shl: {
			restart = handle_shl(node);
			break;
		}
		case iro_Const:
		case iro_Address:
		case iro_Offset:
		case iro_Proj:
		case iro_Load:
		case iro_Store:
		case iro_Start:
		case iro_Sel:
		case iro_Mul:
		case iro_Block:
		case iro_Unknown:
			break;
		case iro_Call:
		case iro_Phi: {
			char title[30];
			ir_snprintf(title, 20, "normalize_uns_%+F", node);
			DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(get_irn_irg(node), title));
			DBG((ces_dbg, LEVEL_3, YELLOW("Warning: unsupported node %+F\n"), node));
			break;
		}
		default:
			DBG((ces_dbg, LEVEL_3, RED("unhandled opcode %+F\n"),node));
		}
	}while (restart);
}


static void optimize_new_nodes(int optimize) {
	static optimization_state_t state;
	if(optimize == false) {
		save_optimization_state(&state);
		set_optimize(0);
	} else
		restore_optimization_state(&state);
}

/*
	 * uses ces_exchange wich changes node content instead of just relinking nodes
	 * thus changes original tree as well as the duplicate
	 */
ir_node* ces_normalize_memop(ir_graph* irg) {
	struct {
		jmp_buf lonjmp_env; //hint: do not declare local variables before setjmp. content is undefined!!
		ir_graph* irg;
	} env;

	//= try
	if (setjmp(env.lonjmp_env) == 0) {
		optimize_new_nodes(false);
		ir_node* memop = get_memop(irg);
		irg_walk(get_memop_ptr(memop), NULL, ces_normalize_walker, &env);
	}//=catch(continue) irg_walk was aborted, continue
	optimize_new_nodes(true);

	DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(get_memop(irg), "normalized"));
	return get_memop(irg);
}
