/**
 * @author Daniel Grund
 * @date 10.03.2005
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#define __USE_BSD
#include <string.h>

#include "obst.h"
#include "set.h"
#include "pset.h"
#include "list.h"
#include "debug.h"

#include "irdom.h"
#include "irouts.h"
#include "bephicoalilp_t.h"
#include "benumb_t.h"
#include "bera_t.h"
#include "belive_t.h"
#include "bechordal_t.h"


#define MAX_COLORS 32

/**
 * define get_weight to sth representing the _gain_ if node n and m
 * have the same color. Must return values MIN_WEIGHT <= . <= MAX_WEIGHT.
 */
#define get_weight(n,m) 1
#define MAX_WEIGHT 127
#define MIN_WEIGHT 0

/** define this to sth which returns 0 iff c is NOT a possible color for node n */
#define is_possible_color(n,c) 1

/** define this to sth which returns 1 iff not all colors can be assigned to node n */
#define is_constrained(n) 0

#undef DUMP_MATRICES
#define DUMP_MPS
#undef DUMP_LPO
#undef DUMP_LPP
#define DO_SOLVE
#define DELETE_FILES
#undef USE_SOS
#define SSH_USER_HOST "kb61@sp-smp.rz.uni-karlsruhe.de"

/* The overhead stuff */
#define DEBUG_LVL SET_LEVEL_1
#define SET_LIVING_INIT 32
#define MAX_Q_INIT 0
#define MIN_Q_INIT 0

static firm_dbg_module_t *dbgphi = NULL;

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

/** A type storing names of the x variables in the form x[NUMBER]_[COLOR] */
typedef struct _x_vec_t {
	int n, c;
} x_vec_t;

/**
 * A type storing the unmodified '0-1 quadratic program' of the form
 * min f = xQx
 * udN:  Ax  = e
 *       Bx <= e
 *        x \in {0, 1}
 *
 * This problem is called the original problem
 */
typedef struct _problem_instance_t {
	ir_graph* irg;
	const char *name;
	int x_dim, A_dim, B_dim;
	char *Q, *A, *B;
	x_vec_t *x;				/**< stores the names of the x variables. Sorted first by node-num then by color. */

	/* needed only for linearizations */
	int bigM, maxQij, minQij, correction;

	/* overhead needed to build this */
	set *num2pos;
	struct obstack ob;
	int curr_col;
	int curr_row;
} problem_instance_t;

/**
 * For each node taking part in the opt-problem its position in the
 * x-variable-vector is stored in a set. This set maps the node-nr (given by
 * benumb) to  the position in the vector.
 */
typedef struct _num2pos_t {
	int num, pos;
} num2pos_t;

/* Nodes have consecutive numbers so... */
#define HASH_NUM(num) num

static int pi_num2pos_cmp(const void *x, const void *y, size_t size) {
	return ((num2pos_t *)x)->num != ((num2pos_t *)y)->num;
}

/**
 * Sets the first position of node with number num to pos.
 * See x_vec_t *x in _problem_instance_t.
 */
static INLINE void pi_set_first_pos(problem_instance_t *pi, int num, int pos) {
	num2pos_t find;
	find.num = num;
	find.pos = pos;
	set_insert(pi->num2pos, &find, sizeof(find), HASH_NUM(num));
}

/**
 * Get position by number. (First possible color)
 * returns -1 if not found.
 */
static INLINE int pi_get_first_pos(problem_instance_t *pi, int num) {
	num2pos_t find, *found;
	find.num = num;
	found = set_find(pi->num2pos, &find, sizeof(find), HASH_NUM(num));
	if (found) {
		assert(pi->x[found->pos].n == num && (found->pos == 0 || pi->x[found->pos-1].n != num) && "pi->num2pos is broken!");
		return found->pos;
	} else
		return -1;
}

/**
 * Get position by number and color.
 * returns -1 if not found.
 */
static INLINE int pi_get_pos(problem_instance_t *pi, int num, int col) {
	num2pos_t find, *found;
	find.num = num;
	int pos;
	found = set_find(pi->num2pos, &find, sizeof(find), HASH_NUM(num));
	if (!found)
		return -1;
	pos = found->pos;
	while (pos < pi->x_dim && pi->x[pos].n == num && pi->x[pos].c < col)
		pos++;

	if (pi->x[pos].n == num && pi->x[pos].c == col)
		return pos;
	else
		return -1;
}

/**
 * Checks if all nodes in living are live_out in block block.
 */
static INLINE int all_live_out(ir_node *block, pset *living) {
	ir_node *n;
	for (n = pset_first(living); n; n = pset_next(living))
		if (!is_live_out(block, n)) {
			pset_break(living);
			return 0;
		}
	return 1;
}

/**
 * Finds all cliques in the interference graph, which are not conatained in
 * another one (or at least an approximation of that).
 * This is used for the matrix B.
 */
static void pi_clique_finder(ir_node *block, void *env) {
	enum phase_t {growing, shrinking} phase = growing;
	problem_instance_t *pi = env;
	struct list_head *head = &get_ra_block_info(block)->border_head;
	border_t *b;
	pset *living = pset_new_ptr(SET_LIVING_INIT);

	list_for_each_entry_reverse(border_t, b, head, list) {
		const ir_node *irn = b->irn;
		if (!is_possible_color(n, pi->curr_col))
			continue;

		if (b->is_def) {
			DBG((dbgphi, LEVEL_2, "Def %n\n", irn));
			pset_insert_ptr(living, irn);
			phase = growing;
		} else { /* is_use */
			DBG((dbgphi, LEVEL_2, "Use %n\n", irn));

			/* before shrinking the set store the current 'maximum' clique;
			 * do NOT if clique is a single node
			 * do NOT if all values are live_out */
			if (phase == growing && pset_count(living) >= 2 && !all_live_out(block, living)) {
				ir_node *n;
				for (n = pset_first(living); n; n = pset_next(living)) {
					int pos = pi_get_pos(pi, get_irn_graph_nr(n), pi->curr_col);
					pi->B[pi->curr_row*pi->x_dim + pos] = 1;
					DBG((dbgphi, LEVEL_2, "B[%d, %d] := %d\n", pi->curr_row, pos, 1));
				}
				pi->curr_row++;
			}
			pset_remove_ptr(living, irn);
			phase = shrinking;
		}
	}

	del_pset(living);
}

#ifdef DUMP_MATRICES
/**
 * Dump the raw matrices of the problem to a file for debugging.
 */
static void pi_dump_matrices(problem_instance_t *pi) {
	int i, o;
	FILE *out = ffopen(pi->name, "matrix", "wt");

	DBG((dbgphi, LEVEL_1, "Dumping raw...\n"));
	fprintf(out, "\n\nx-names =\n");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "%5d %2d\n", pi->x[i].n, pi->x[i].c);

	fprintf(out, "\n\n-Q =\n");
	for (i=0; i<pi->x_dim; ++i) {
		for (o=0; o<pi->x_dim; ++o)
			fprintf(out, "%d", -pi->Q[i*pi->x_dim + o]);
		fprintf(out, "\n");
	}

	fprintf(out, "\n\nA =\n");
	for (i=0; i<pi->A_dim; ++i) {
		for (o=0; o<pi->x_dim; ++o)
			fprintf(out, "%d", pi->A[i*pi->x_dim + o]);
		fprintf(out, "\n");
	}

	fprintf(out, "\n\nB =\n");
	for (i=0; i<pi->B_dim; ++i) {
		for (o=0; o<pi->x_dim; ++o)
			fprintf(out, "%d", pi->B[i*pi->x_dim + o]);
		fprintf(out, "\n");
	}
	fclose(out);
}
#endif

#ifdef DUMP_MPS
/**
 * Dumps an mps file representing the problem. This is NOT the old-style,
 * fixed-column format. Spaces are separators, MARKER-lines are used in
 * COLUMN section to define binaries.
 */
static void pi_dump_mps(problem_instance_t *pi) {
	int i, o, max_abs_Qij;
	FILE *out = ffopen(pi->name, "mps", "wt");

	DBG((dbgphi, LEVEL_1, "Dumping mps...\n"));
	max_abs_Qij = pi->maxQij;
	if (-pi->minQij > max_abs_Qij)
		max_abs_Qij = -pi->minQij;
	pi->bigM = pi->A_dim * max_abs_Qij;
	DBG((dbgphi, LEVEL_2, "BigM = %d\n", pi->bigM));

	fprintf(out, "NAME %s\n", pi->name);

	fprintf(out, "ROWS\n");
	fprintf(out, " N obj\n");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, " E cQ%d\n", i);
	for (i=0; i<pi->A_dim; ++i)
		fprintf(out, " E cA%d\n", i);
	for (i=0; i<pi->B_dim; ++i)
		fprintf(out, " L cB%d\n", i);
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, " L cy%d\n", i);

	fprintf(out, "COLUMNS\n");
	/* the x vars come first */
	/* mark them as binaries */
	fprintf(out, "    MARKI0\t'MARKER'\t'INTORG'\n");
#ifdef USE_SOS
	int sos_cnt = 0;
	fprintf(out, " S1 SOS_%d\t'MARKER'\t'SOSORG'\n", sos_cnt++);
#endif
	for (i=0; i<pi->x_dim; ++i) {
#ifdef USE_SOS
		if (i>0 && pi->x[i].n != pi->x[i-1].n) {
			fprintf(out, "    SOS_%d\t'MARKER'\t'SOSEND'\n", sos_cnt++);
			fprintf(out, " S1 SOS_%d\t'MARKER'\t'SOSORG'\n", sos_cnt++);
		}
#endif
		/* participation in objective */
		fprintf(out, "    x%d_%d\tobj\t%d\n", pi->x[i].n, pi->x[i].c, -pi->bigM);
		/* in Q */
		for (o=0; o<pi->x_dim; ++o) {
			int Qoi = pi->Q[o*pi->x_dim + i];
			if (Qoi)
				fprintf(out, "    x%d_%d\tcQ%d\t%d\n", pi->x[i].n, pi->x[i].c, o, Qoi);
		}
		/* in A */
		for (o=0; o<pi->A_dim; ++o) {
			int Aoi = pi->A[o*pi->x_dim + i];
			if (Aoi)
				fprintf(out, "    x%d_%d\tcA%d\t%d\n", pi->x[i].n, pi->x[i].c, o, Aoi);
		}
		/* in B */
		for (o=0; o<pi->B_dim; ++o) {
			int Boi = pi->B[o*pi->x_dim + i];
			if (Boi)
				fprintf(out, "    x%d_%d\tcB%d\t%d\n", pi->x[i].n, pi->x[i].c, o, Boi);
		}
		/* in y */
		fprintf(out, "    x%d_%d\tcy%d\t%d\n", pi->x[i].n, pi->x[i].c, i, 2*pi->bigM);
	}
#ifdef USE_SOS
	fprintf(out, "    SOS_%d\t'MARKER'\t'SOSEND'\n", sos_cnt++);
#endif
	fprintf(out, "    MARKI1\t'MARKER'\t'INTEND'\n"); /* end of marking */

	/* next the s vars */
	for (i=0; i<pi->x_dim; ++i) {
		/* participation in objective */
		fprintf(out, "    s%d_%d\tobj\t%d\n", pi->x[i].n, pi->x[i].c, 1);
		/* in Q */
		fprintf(out, "    s%d_%d\tcQ%d\t%d\n", pi->x[i].n, pi->x[i].c, i, -1);
	}

	/* next the y vars */
	for (i=0; i<pi->x_dim; ++i) {
		/* in Q */
		fprintf(out, "    y%d_%d\tcQ%d\t%d\n", pi->x[i].n, pi->x[i].c, i, -1);
		/* in y */
		fprintf(out, "    y%d_%d\tcy%d\t%d\n", pi->x[i].n, pi->x[i].c, i, 1);
	}

	fprintf(out, "RHS\n");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "    rhs\tcQ%d\t%d\n", i, -pi->bigM);
	for (i=0; i<pi->A_dim; ++i)
		fprintf(out, "    rhs\tcA%d\t%d\n", i, 1);
	for (i=0; i<pi->B_dim; ++i)
		fprintf(out, "    rhs\tcB%d\t%d\n", i, 1);
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "    rhs\tcy%d\t%d\n", i, 2*pi->bigM);

	fprintf(out, "ENDATA\n");
	fclose(out);

	out = ffopen(pi->name, "mst", "wt");
	fprintf(out, "NAME\n");
	for (i=0; i<pi->x_dim; ++i) {
		int val, n, c;
		n = pi->x[i].n;
		c = pi->x[i].c;
		if (get_irn_color(get_irn_for_graph_nr(pi->irg, n)) == c)
			val = 1;
		else
			val = 0;
		fprintf(out, "    x%d_%d\t%d\n", n, c, val);
	}
	fprintf(out, "ENDATA\n");
	fclose(out);
}
#endif

#if defined(DUMP_LPO) || defined(DUMP_LPP)
/**
 * Dumps constraints used by both linearizations
 */
static void pi_dump_lp_common_constraints(problem_instance_t *pi, FILE *out) {
	int i, o;
	/* knapsack constraints */
	for (i=0; i<pi->A_dim; ++i)	{
		for (o=0; o<pi->x_dim; ++o)
			if (pi->A[i*pi->x_dim + o])
				fprintf(out, "+x%d_%d ", pi->x[o].n, pi->x[o].c);
		fprintf(out, " = 1;\n");
	}
	fprintf(out, "\n\n");

	/* interference graph constraints */
	for (i=0; i<pi->B_dim; ++i)	{
		for (o=0; o<pi->x_dim; ++o)
			if (pi->B[i*pi->x_dim + o])
				fprintf(out, "+x%d_%d ", pi->x[o].n, pi->x[o].c);
		fprintf(out, " <= 1;\n");
	}
	fprintf(out, "\n\n");

	/* integer constraints */
	fprintf(out, "int x%d_%d", pi->x[0].n, pi->x[0].c);
	for (i=1; i<pi->x_dim; ++i)
		fprintf(out, ", x%d_%d", pi->x[i].n, pi->x[i].c);
	fprintf(out, ";\n");
}
#endif

#ifdef DUMP_LPO
/**
 * Dumps the problem instance as a MILP. The original problem is transformed into:
 * min f = es - Mex
 * udN:  Qx -y -s +Me = 0
 *       Ax  = e
 *       Bx <= e
 *        y <= 2M(e-x)
 *        x \in N   y, s >= 0
 *
 * with M >= max sum Q'ij * x_j
 *            i   j
 */
static void pi_dump_lp_org(problem_instance_t *pi) {
	int i, o, max_abs_Qij;
	FILE *out = ffopen(pi->name, "lpo", "wt");

	DBG((dbgphi, LEVEL_1, "Dumping lp_org...\n"));
	/* calc the big M for Q */
	max_abs_Qij = pi->maxQij;
	if (-pi->minQij > max_abs_Qij)
		max_abs_Qij = -pi->minQij;
	pi->bigM = pi->A_dim * max_abs_Qij;
	DBG((dbgphi, LEVEL_2, "BigM = %d\n", pi->bigM));

	/* generate objective function */
	fprintf(out, "min: ");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "+s%d_%d -%dx%d_%d ", pi->x[i].n, pi->x[i].c, pi->bigM, pi->x[i].n, pi->x[i].c);
	fprintf(out, ";\n\n");

	/* constraints for former objective function */
	for (i=0; i<pi->x_dim; ++i)	{
		for (o=0; o<pi->x_dim; ++o) {
			int Qio = pi->Q[i*pi->x_dim + o];
			if (Qio) {
				if (Qio == 1)
					fprintf(out, "+x%d_%d ", pi->x[o].n, pi->x[o].c);
				else if(Qio == -1)
					fprintf(out, "-x%d_%d ", pi->x[o].n, pi->x[o].c);
				else
					fprintf(out, "%+dx%d_%d ", Qio, pi->x[o].n, pi->x[o].c);
			}
		}
		fprintf(out, "-y%d_%d -s%d_%d +%d= 0;\n", pi->x[i].n, pi->x[i].c, pi->x[i].n, pi->x[i].c, pi->bigM);
	}
	fprintf(out, "\n\n");

	/* constraints for (special) complementary condition */
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "y%d_%d <= %d - %dx%d_%d;\n", pi->x[i].n, pi->x[i].c, 2*pi->bigM, 2*pi->bigM, pi->x[i].n, pi->x[i].c);
	fprintf(out, "\n\n");

	pi_dump_lp_common_constraints(pi, out);
	fclose(out);
}
#endif

#ifdef DUMP_LPP
/**
 * Dumps the problem instance as a MILP. The original problem is transformed into:
 * min f = es
 * udN:  Q'x -y -s = 0
 *       Ax  = e
 *       Bx <= e
 *        y <= M(e-x)
 *        x \in N   y, s >= 0
 *
 * with Q' = (q'_ij) := q_ij - minQij
 * and M >= max sum Q'ij * x_j
 *           i   j
 */
static void pi_dump_lp_pos(problem_instance_t *pi) {
	int i, o;
	FILE *out = ffopen(pi->name, "lpp", "wt");

	DBG((dbgphi, LEVEL_1, "Dumping lp_pos...\n"));
	/* Norm the Matrix: Qij >=0 and \exists i,j Qij=0 */
	for (i=0; i<pi->x_dim; ++i)
		for (o=0; o<pi->x_dim; ++o)
			pi->Q[i*pi->x_dim + o] -= pi->minQij;
	/* now Q' is stored in Q */

	/* calc the big M for Q'
	 * maxQ'ij = maxQij-minQij
	 * #nodes = A_dim
	 * So max sum Q'ij * x_j <= A_dim * maxQ'ij
	 *     i   j
	 */
	pi->bigM = pi->A_dim * (pi->maxQij - pi->minQij);
	DBG((dbgphi, LEVEL_2, "BigM = %d\n", pi->bigM));

	/* clac the correction term for the obj func
	 * xQx = xQ'x + minQij * k^2
	 * where k, in general, is the knapsack size of wx = k.
	 * Here w=1 and so 1x = #nodes = A_dim
	 */
	pi->correction = pi->minQij * pi->A_dim * pi->A_dim;
	DBG((dbgphi, LEVEL_2, "Correction = %d\n", pi->correction));

	/* generate objective function */
	fprintf(out, "min: ");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "+s%d_%d ", pi->x[i].n, pi->x[i].c);
	fprintf(out, "+ correction\n");
	fprintf(out, "correction = %d ", pi->correction);
	fprintf(out, ";\n\n");

	/* constraints for former objective function */
	for (i=0; i<pi->x_dim; ++i)	{
		for (o=0; o<pi->x_dim; ++o) {
			int Qio = pi->Q[i*pi->x_dim + o];
			if (Qio) {
				if (Qio != 1)
					fprintf(out, "+%d", Qio);
				fprintf(out, "+x%d_%d ", pi->x[o].n, pi->x[o].c);
			}
		}
		fprintf(out, "-y%d_%d -s%d_%d = 0;\n", pi->x[i].n, pi->x[i].c, pi->x[i].n, pi->x[i].c);
	}
	fprintf(out, "\n\n");

	/* constraints for (special) complementary condition */
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "y%d_%d<=%d-%dx%d_%d;\n", pi->x[i].n, pi->x[i].c, pi->bigM, pi->bigM, pi->x[i].n, pi->x[i].c);
	fprintf(out, "\n\n");

	pi_dump_lp_common_constraints(pi, out);
	fclose(out);
}
#endif

#ifdef DO_SOLVE
/**
 * Invoke an external solver
 */
static void pi_solve_ilp(problem_instance_t *pi) {
	FILE *out;
	char buf[1024];

	/* write command file for CPLEX */
	out = ffopen(pi->name, "cmd", "wt");
	fprintf(out, "read %s.mps\n", pi->name);
	fprintf(out, "read %s.mst\n", pi->name);
	fprintf(out, "set mip strategy mipstart 1\n");
	fprintf(out, "optimize\n");
	fprintf(out, "set logfile %s.sol\n", pi->name);
	fprintf(out, "display solution variables 1-%d\n", pi->x_dim);
	fprintf(out, "set logfile cplex.log\n");
	fprintf(out, "quit\n");
	fclose(out);

	snprintf(buf, sizeof(buf), "scp %s.mps %s.mst %s.cmd %s:", pi->name, pi->name, pi->name, SSH_USER_HOST);
	system(buf);
	snprintf(buf, sizeof(buf), "ssh %s \"./cplex90 < %s.cmd\"", SSH_USER_HOST, pi->name);
	system(buf);
	snprintf(buf, sizeof(buf), "scp %s:%s.sol .", SSH_USER_HOST, pi->name);
	system(buf);
}

/**
 * Sets the colors of irns according to the values of variables found in the
 * output file of the solver.
 */
static void pi_apply_solution(problem_instance_t *pi) {
	FILE *in = ffopen(pi->name, "sol", "rt");

	DBG((dbgphi, LEVEL_1, "Applying solution...\n"));
	while (!feof(in)) {
		char buf[1024];
		int num = -1, col = -1, val = -1;
		if (fscanf(in, "x%d_%d %d.%s\n", &num, &col, &val, buf) != 3) {
			while(fscanf(in, "%1020s\n", buf) != 1);
			continue;
		}
		if (val == 1) {
			DBG((dbgphi, LEVEL_1, "x%d_%d = %d\n", num, col, val));
			set_irn_color(get_irn_for_graph_nr(pi->irg, num), col);
		}
	}
	fclose(in);
}
#endif /* DO_SOLVE */

#ifdef DELETE_FILES
static void pi_delete_files(problem_instance_t *pi) {
	char buf[1024];
	int end = snprintf(buf, sizeof(buf), "%s", pi->name);
#ifdef DUMP_MATRICES
	snprintf(buf+end, sizeof(buf)-end, ".matrix");
	remove(buf);
#endif
#ifdef DUMP_MPS
	snprintf(buf+end, sizeof(buf)-end, ".mps");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".mst");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".cmd");
	remove(buf);
#endif
#ifdef DUMP_LPO
	snprintf(buf+end, sizeof(buf)-end, ".lpo");
	remove(buf);
#endif
#ifdef DUMP_LPP
	snprintf(buf+end, sizeof(buf)-end, ".lpp");
	remove(buf);
#endif
}
#endif

/**
 * Let N be minimal so that the copy-minimization-problem resticted to
 * N possible colors has the same result as the original copy-min-prob with
 * MAX_COLORS possible colors.
 * It is assumed (not proven) that this is true, if for each phi and phi-arg
 * an extra register would be available. Constrained registers count an additinal
 * register too.
 * TODO Prove the above.
 *
 * This function returns M in env, where N <= M <= MAX_COLROS
 */
static void pi_colors_upper_bound(ir_node *block, void *env) {
	int *res = env;
	int i, max, max_pressure, special_cnt;
	if (*res == MAX_COLORS)
		return;

	/* get maximal register pressure */
	{
		struct list_head *head = &get_ra_block_info(block)->border_head;
		border_t *b;
		max_pressure = 0;
		list_for_each_entry(border_t, b, head, list)
			if (b->pressure > max_pressure) {
				max_pressure = b->pressure;
			}
	}

	/* count special nodes */
	special_cnt = 0;
	for (i = 0, max = get_irn_n_outs(block); i < max; ++i) {
		ir_node *n = get_irn_out(block, i);
		if (get_nodes_block(n) != block)
			continue;
		if (is_Phi(n) || is_phi_operand(n) || is_constrained(n))
			special_cnt++;
	}

	/* new max? */
	if (*res < max_pressure + special_cnt)
		*res = max_pressure + special_cnt;
	if (*res > MAX_COLORS)
		*res = MAX_COLORS;
}

/**
 * Generate the initial problem matrices and vectors.
 */
static problem_instance_t *new_pi(ir_graph *irg, pset *all_phi_nodes) {
	int max_needed_cols;
	problem_instance_t *pi = calloc(1, sizeof(problem_instance_t));
	pi->irg = irg;
	pi->name = 	get_entity_name(get_irg_entity(irg));
	pi->bigM = 1;
	pi->minQij = MIN_Q_INIT;
	pi->maxQij = MAX_Q_INIT;
	obstack_init(&pi->ob);

	DBG((dbgphi, LEVEL_1, "Generating new instance...\n"));
	pi->num2pos = new_set(pi_num2pos_cmp, 128);

	//TODO move pi_colors_upper_bound in "super-class" to fix issue with
	//invalid mst-values
	/* get max_needed_cols */
	max_needed_cols = 0;
	dom_tree_walk_irg(irg, pi_colors_upper_bound, NULL, &max_needed_cols);
	//TODO remove
	max_needed_cols = MAX_COLORS;
	DBG((dbgphi, LEVEL_1, "max_needed_cols: %d\n", max_needed_cols));

	/* Vector x
	 * one entry per node and possible color */
	{
		x_vec_t xx;
		for (xx.n=0; xx.n<get_graph_node_count(irg); ++xx.n) {
			ir_node *irn = get_irn_for_graph_nr(irg, xx.n);

			if (!is_allocatable_irn(irn))
				continue;
			DBG((dbgphi, LEVEL_2, "pi->num2pos %4d --> %4d\n", xx.n, pi->x_dim));
			pi_set_first_pos(pi, xx.n, pi->x_dim);

			pi->A_dim++;			/* one knapsack constraint for each node */
			for (xx.c=0; xx.c<max_needed_cols; ++xx.c) {
				if (!is_possible_color(irn, xx.c))
					continue;
				DBG((dbgphi, LEVEL_2, "Adding %n %d\n", irn, xx.c));
				obstack_grow(&pi->ob, &xx, sizeof(xx));
				pi->x_dim++;		/* one x variable for each node and color */
			}
		}
		pi->x = obstack_finish(&pi->ob);
	}

	/* Matrix Q
	 * weights for the 'same-color-optimization' target */
	{
		ir_node *phi, *arg;
		pi->Q = calloc(pi->x_dim*pi->x_dim, sizeof(pi->Q[0]));
		for (phi = pset_first(all_phi_nodes); phi; phi = pset_next(all_phi_nodes)) {
			unsigned phipos, argpos;
			int phinr, argnr;
			int i, max;

			for (i = 0, max = get_irn_arity(phi); i < max; ++i) {
				int weight;
				arg = get_irn_n(phi, i);
				if (phi_ops_interfere(phi, arg))
					continue;
				phinr = get_irn_graph_nr(phi);
				argnr = get_irn_graph_nr(arg);
				phipos = pi_get_first_pos(pi, phinr);
				argpos = pi_get_first_pos(pi, argnr);
				weight = -get_weight(phi, arg);

				DBG((dbgphi, LEVEL_2, "Q[%n, %n] := %d\n", phi, arg, weight));
				/* for all colors phi and arg have in common, set the weight for
				 * this pair in the objective function matrix Q */
				while (phipos < pi->x_dim && argpos < pi->x_dim &&
						pi->x[phipos].n == phinr && pi->x[argpos].n == argnr) {
					if (pi->x[phipos].c < pi->x[argpos].c)
						++phipos;
					else if (pi->x[phipos].c > pi->x[argpos].c)
						++argpos;
					else {
						pi->Q[(phipos++)*pi->x_dim + argpos++] = weight;

						if (weight < pi->minQij) {
							DBG((dbgphi, LEVEL_2, "minQij = %d\n", weight));
							pi->minQij = weight;
						}
						if (weight > pi->maxQij) {
							DBG((dbgphi, LEVEL_2, "maxQij = %d\n", weight));
							pi->maxQij = weight;
						}
					}
				}
			}
		}
	}

	/* Matrix A
	 * knapsack constraint for each node */
	{
		int row = 0, col = 0;
		pi->A = calloc(pi->A_dim*pi->x_dim, sizeof(pi->A[0]));
		while (col < pi->x_dim) {
			int curr_n = pi->x[col].n;
			while (col < pi->x_dim && pi->x[col].n == curr_n) {
				DBG((dbgphi, LEVEL_2, "A[%d, %d] := %d\n", row, col, 1));
				pi->A[row*pi->x_dim + col++] = 1;
			}
			++row;
		}
		assert(row == pi->A_dim);
	}

	/* Matrix B
	 * interference constraints using exactly those cliques not contained in others. */
	{
		int col;
		pi->B_dim = set_count(be_ra_get_ifg(irg))*max_needed_cols;  /* this is an upper bound, see realloc below */
		pi->B = calloc(pi->B_dim*pi->x_dim, sizeof(pi->B[0]));

		for (col = 0; col < max_needed_cols; ++col) {
			pi->curr_col = col;
			dom_tree_walk_irg(irg, pi_clique_finder, NULL, pi);
		}

		pi->B_dim = pi->curr_row;
		pi->B = realloc(pi->B, pi->B_dim*pi->x_dim*sizeof(pi->B[0]));
	}

	return pi;
}

/**
 * clean the problem instance
 */
static void free_pi(problem_instance_t *pi) {
	free(pi->Q);
	free(pi->A);
	free(pi->B);
	del_set(pi->num2pos);
	obstack_free(&pi->ob, NULL);
	free(pi);
}

void be_phi_coalesce_ilp(ir_graph *irg, pset *all_phi_nodes) {
	problem_instance_t *pi = new_pi(irg, all_phi_nodes);

#ifdef DUMP_MATRICES
	pi_dump_matrices(pi);
#endif

#ifdef DUMP_MPS
	pi_dump_mps(pi);
#endif

#ifdef DUMP_LPO
	pi_dump_lp_org(pi);
#endif

#ifdef DUMP_LPP
	pi_dump_lp_pos(pi);
#endif

#ifdef DO_SOLVE
	pi_solve_ilp(pi);
	pi_apply_solution(pi);
#endif

#ifdef DELETE_FILES
	pi_delete_files(pi);
#endif

	free_pi(pi);
}

void be_phi_coal_ilp_init(void) {
	dbgphi = firm_dbg_register("ir.be.phicoalilp");
	firm_dbg_set_mask(dbgphi, DEBUG_LVL);
}
