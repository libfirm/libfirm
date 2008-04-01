#ifndef _EXT_GRS_BASE_T_H_
#define _EXT_GRS_BASE_T_H_


#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif
#include "common_t.h"
#include "base.h"
#include "action_t.h"





#define OFFSET_OF(type, member) ((char*) &((((type)*)0)->(member)) - (char*)0)
#define CONTAINER_OF(ptr, type, member) \
	(((type) *) ((char *) (ptr) - OFFSET_OF((type), (member))))






/* maps opcodes to ops */
extern ir_op **_ext_grs_op_map;
/* maps modecodes to modes */
extern ir_mode **_ext_grs_mode_map;


/**
 * a 2-dim flexible array telling wether an opcode belongs
 * to a sub op of the op given by a second opcode. usage:
 * _ext_grs_op_is_a[o1][o2] != 0   IFF   o1 inherits o2 */
//extern int *_ext_grs_op_is_a;
extern unsigned char *_ext_grs_op_is_a;
/** the width of the  */
extern int _ext_grs_op_is_a_width;

/** comfortable acces to the is_a matrix (o1 and o2 are opcodes) */
#define _ext_grs_OP_IS_A(o1, o2) \
	_ext_grs_op_is_a[((int)o1)*(_ext_grs_op_is_a_width) + ((int)o2)]

/**
 * a flexible array of flexible arrays each containing all inheriting
 * firm ops of that firm op, the opcode of wich is the index entered
 * in the array of arrays
 * USAGE: _ext_grs_all_sub_ops_of_op[opcode]
 * this yields an flexible array of ptrs to firm ops inheriting
 * form the given opcodes op */
extern ir_op ***_ext_grs_all_sub_ops_of_op;
/* a flexible array of flexible arrays each containing all firm ops
 * a given firm op inherits from, the given opcode is the index entered
 * in the array of arrays
 * USAGE: _ext_grs_all_super_ops_of_op[opcode]
 * this yields an flexible array of ptrs to firm ops inheriting
 * form the given opcodes op */
extern ir_op ***_ext_grs_all_super_ops_of_op;

/** maps ir op names (e.g. "Add") to ir ops (i.e. *ir_op) */
extern lc_pset *_ext_grs_op_name_table;
/** maps mode names (e.g. "M") to modes (i.e. *ir_mode) */
extern lc_pset *_ext_grs_mode_name_table;
/** remembers the maximum opcode present */
extern int _ext_grs_max_opcode;
/** remembers the maximum modecode present */
extern int _ext_grs_max_modecode;


/** offset of private data area in ir graphs */
extern unsigned _ext_grs_private_ofs_g;
/** the private data area in ir graphs */
typedef struct _ext_grs_irg_private_t {
	/** a flag, tells wether matching has been disabled
	 *  for this it graph by calling ext_grs_disable_matching() */
	int matching_enabled;
	/** lists of firm nodes according to their opcode and modecode */
	lc_list_t *node_list;
	/** number of instances according to the opcode and modecode */
	int *n_instances;
} ext_grs_irg_private_t;

/* dimension of dynamic allocated 2-dim arrays in private data
 * area of firm graphs. These two arrays are the n_instances
 * and the node_list. Note that these two arrays have the same
 * dimension for all ir graphs. That means if the maximal
 * opcode/modecode exceeds the respsective dimension the
 * arrays of ALL ir graphs have to be reallcated (this is done
 * by the hooks of new ops and modes) */
extern int _ext_grs_irgpr_op_dim;
extern int _ext_grs_irgpr_mode_dim;

/* easy acces two these dynamic 2-dim arrays */
#define _ext_grs_N_INSTANCES(pr_g, opc, mc) \
	((pr_g)->n_instances[_ext_grs_irgpr_mode_dim * (opc) + (mc)])
#define _ext_grs_NODE_LIST(pr_g, opc, mc) \
	((pr_g)->node_list[_ext_grs_irgpr_mode_dim * (opc) + (mc)])

/** offset of private data in ir nodes */
extern unsigned _ext_grs_private_ofs_n;
/** the private data area in ir nodes */
typedef struct _ext_grs_irn_private_t {
	/** list of firm nodes according to this nodes opcode and modecode */
	lc_list_t node_list;
	/** some auxilary data, needed to determine wether a ir node has
	 * already been matched. If so, it points to a pattern node it is
	 * matched from, otherwise it is NULL */
	ext_grs_node_t *preimage;
	/** flag, tells wether this nodes has been deleted on a rewrite step */
	int deleted;
} ext_grs_irn_private_t;


/** offset of private data in ir edges */
extern unsigned _ext_grs_private_ofs_e;
/** the private data area in ir edges */
typedef struct _ext_grs_iredges_private_t {
	ext_grs_edge_t *preimage;
	unsigned long visited;
} ext_grs_iredges_private_t;


static INLINE ext_grs_irn_private_t *_ext_grs_get_irn_private(ir_node *irn) {
	return get_irn_data(irn, ext_grs_irn_private_t, _ext_grs_private_ofs_n);
}

static INLINE ext_grs_irg_private_t *_ext_grs_get_irg_private(ir_graph *irg) {
	return get_irg_data(irg, ext_grs_irg_private_t, _ext_grs_private_ofs_g);
}

static INLINE ir_op *_ext_grs_lookup_op(char *op_name) {

	ir_op *op = alloca(sizeof(*op));

	memset(op, 0, sizeof(*op));
	op->name = new_id_from_str(op_name);

	op = lc_pset_find(
		_ext_grs_op_name_table, op, HASH_STR(op_name, strlen(op_name)));
	return op;
}

static INLINE ir_mode *_ext_grs_lookup_mode(char *name) {

	ir_mode *mode = alloca(sizeof(*mode));

	memset(mode, 0, sizeof(*mode));
	mode->name = new_id_from_str(name);

	mode = lc_pset_find(
		_ext_grs_mode_name_table, mode, HASH_STR(name, strlen(name)));
	return mode;
}

/** get an op by its name */
#define ext_grs_lookup_op(name)					_ext_grs_lookup_op((name))
/** get a mode by its name */
#define ext_grs_lookup_mode(name)				_ext_grs_lookup_mode((name))






#endif /* _EXT_GRS_BASE_T_H_ */
