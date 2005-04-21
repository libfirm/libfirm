/**
 * Minimizing copies with an exact algorithm using mixed integer programming (MIP).
 * Problem statement as a 'quadratic 0-1 program with linear constraints' with
 * n binary variables. Constraints are knapsack (enforce color for each node) and
 * cliques of ifg (interference constraints).
 * Transformation into a 'mixed integer program' with n binary variables and
 * additional 2n real variables. Constraints are the above the transformed
 * objective function and 'complementary conditions' for two var classes.
 * @author Daniel Grund
 *
 * NOTE: Unfortunately no good solver is available locally (or even for linking)
 *       We use CPLEX 9.0 which runs on a machine residing at the Rechenzentrum.
 * @date 12.04.2005
 */

#include "becopyopt.h"

#define DUMP_MPS			/**< dumps the problem in "CPLEX"-MPS format. NOT fixed-column-MPS. */
#undef USE_SOS				/**< uses Special Ordered Sets when using MPS */
#define DO_SOLVE 			/**< solve the MPS output with CPLEX */
#undef DUMP_MATRICES		/**< dumps all matrices completely. only recommended for small problems */
#undef DUMP_LP				/**< dumps the problem in LP format. 'human-readable' equations etc... */
#define DELETE_FILES		/**< deletes all dumped files after use */

/* CPLEX-account related stuff */
#define SSH_USER_HOST_PATH "kb61@sp-smp.rz.uni-karlsruhe.de"
#define SSH_PASSWD "!cplex90"
#define EXPECT_FILENAME "runme" /** name of the expect-script */

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define SLOTS_NUM2POS 256
#define SLOTS_LIVING 32

/**
 * A type storing names of the x variables in the form x[NUMBER]_[COLOR]
 */
typedef struct _x_name_t {
	int n, c;
} x_name_t;

/**
 * For each node taking part in the opt-problem its position in the
 * x-variable-vector is stored in a set. This set maps the node-nr (given by
 * benumb) to the position in the vector.
 */
typedef struct _num2pos_t {
	int num, pos;
} num2pos_t;

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
	int x_dim, A_dim, B_dim;	/**< number of: x variables, rows in A, rows in B */
	x_name_t *x;				/**< stores the names of the x variables. all possible colors for a node are ordered and occupy consecutive entries. lives in obstack ob. */
	set *num2pos;				/**< maps node numbers to positions in x. */
	sp_matrix_t *Q, *A, *B;		/**< the (sparse) matrices of this problem */

	/* needed only for linearizations */
	int bigM, maxQij, minQij;

	/* overhead needed to build this */
	struct obstack ob;
	int curr_color;
	int curr_row;
} problem_instance_t;

/* Nodes have consecutive numbers so this hash shoud be fine */
#define HASH_NUM(num) num

static int set_cmp_num2pos(const void *x, const void *y, size_t size) {
	return ((num2pos_t *)x)->num != ((num2pos_t *)y)->num;
}

/**
 * Sets the first position of node with number num to pos.
 * See x_name_t *x in _problem_instance_t.
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

#ifdef DUMP_MATRICES
/**
 * Dump the raw matrices of the problem to a file for debugging.
 */
static void pi_dump_matrices(problem_instance_t *pi) {
	int i;
	FILE *out = ffopen(pi->name, "matrix", "wt");

	DBG((dbg, LEVEL_1, "Dumping raw...\n"));
	fprintf(out, "\n\nx-names =\n");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "%5d %2d\n", pi->x[i].n, pi->x[i].c);

	fprintf(out, "\n\n-Q =\n");
	matrix_dump(pi->Q, out, -1);

	fprintf(out, "\n\nA =\n");
	matrix_dump(pi->A, out, 1);

	fprintf(out, "\n\nB =\n");
	matrix_dump(pi->B, out, 1);

	fclose(out);
}
#endif

#ifdef DUMP_LP
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
static void pi_dump_lp(problem_instance_t *pi) {
	int i, max_abs_Qij;
	matrix_elem_t *e;
	FILE *out = ffopen(pi->name, "lpo", "wt");

	DBG((dbg, LEVEL_1, "Dumping lp...\n"));
	/* calc the big M for Q */
	max_abs_Qij = pi->maxQij;
	if (-pi->minQij > max_abs_Qij)
		max_abs_Qij = -pi->minQij;
	pi->bigM = pi->A_dim * max_abs_Qij;
	DBG((dbg, LEVEL_2, "BigM = %d\n", pi->bigM));

	/* generate objective function */
	fprintf(out, "min: ");
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "+s%d_%d -%dx%d_%d ", pi->x[i].n, pi->x[i].c, pi->bigM, pi->x[i].n, pi->x[i].c);
	fprintf(out, ";\n\n");

	/* constraints for former objective function */
	for (i=0; i<pi->x_dim; ++i)	{
		matrix_foreach_in_row(pi->Q, i, e) {
			int Qio = e->val;
				if (Qio == 1)
					fprintf(out, "+x%d_%d ", pi->x[e->col].n, pi->x[e->col].c);
				else if(Qio == -1)
					fprintf(out, "-x%d_%d ", pi->x[e->col].n, pi->x[e->col].c);
				else
					fprintf(out, "%+dx%d_%d ", Qio, pi->x[e->col].n, pi->x[e->col].c);
		}
		fprintf(out, "-y%d_%d -s%d_%d +%d= 0;\n", pi->x[i].n, pi->x[i].c, pi->x[i].n, pi->x[i].c, pi->bigM);
	}
	fprintf(out, "\n\n");

	/* constraints for (special) complementary condition */
	for (i=0; i<pi->x_dim; ++i)
		fprintf(out, "y%d_%d <= %d - %dx%d_%d;\n", pi->x[i].n, pi->x[i].c, 2*pi->bigM, 2*pi->bigM, pi->x[i].n, pi->x[i].c);
	fprintf(out, "\n\n");

	/* knapsack constraints */
	for (i=0; i<pi->A_dim; ++i)	{
		matrix_foreach_in_row(pi->Q, i, e)
			fprintf(out, "+x%d_%d ", pi->x[e->col].n, pi->x[e->col].c);
		fprintf(out, " = 1;\n");
	}
	fprintf(out, "\n\n");

	/* interference graph constraints */
	for (i=0; i<pi->B_dim; ++i)	{
		matrix_foreach_in_row(pi->Q, i, e)
			fprintf(out, "+x%d_%d ", pi->x[e->col].n, pi->x[e->col].c);
		fprintf(out, " <= 1;\n");
	}
	fprintf(out, "\n\n");

	/* integer constraints */
	fprintf(out, "int x%d_%d", pi->x[0].n, pi->x[0].c);
	for (i=1; i<pi->x_dim; ++i)
		fprintf(out, ", x%d_%d", pi->x[i].n, pi->x[i].c);
	fprintf(out, ";\n");

	fclose(out);
}
#endif

#ifdef DUMP_MPS
/**
 * Dumps an mps file representing the problem. This is NOT the old-style,
 * fixed-column format. Some white spaces are important, in general spaces
 * are separators, MARKER-lines are used in COLUMN section to define binaries.
 */
//BETTER use last 2 fields in COLUMNS section
static void pi_dump_mps(problem_instance_t *pi) {
	int i, max_abs_Qij;
	matrix_elem_t *e;
	FILE *out = ffopen(pi->name, "mps", "wt");

	DBG((dbg, LEVEL_1, "Dumping mps...\n"));
	max_abs_Qij = pi->maxQij;
	if (-pi->minQij > max_abs_Qij)
		max_abs_Qij = -pi->minQij;
	pi->bigM = pi->A_dim * max_abs_Qij;
	DBG((dbg, LEVEL_2, "BigM = %d\n", pi->bigM));

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
		matrix_foreach_in_col(pi->Q, i, e)
			fprintf(out, "    x%d_%d\tcQ%d\t%d\n", pi->x[i].n, pi->x[i].c, e->row, e->val);
		/* in A */
		matrix_foreach_in_col(pi->A, i, e)
			fprintf(out, "    x%d_%d\tcA%d\t%d\n", pi->x[i].n, pi->x[i].c, e->row, e->val);
		/* in B */
		matrix_foreach_in_col(pi->B, i, e)
			fprintf(out, "    x%d_%d\tcB%d\t%d\n", pi->x[i].n, pi->x[i].c, e->row, e->val);
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

#ifdef DO_SOLVE
/**
 * Invoke an external solver
 */
static void pi_solve_ilp(problem_instance_t *pi) {
	FILE *out;

	DBG((dbg, LEVEL_1, "Solving with CPLEX@RZ...\n"));
	/* write command file for CPLEX */
	out = ffopen(pi->name, "cmd", "wt");
	fprintf(out, "read %s.mps\n", pi->name);
	fprintf(out, "read %s.mst\n", pi->name);
	fprintf(out, "set mip strategy mipstart 1\n");
	fprintf(out, "set logfile %s.sol\n", pi->name);
	fprintf(out, "optimize\n");
	fprintf(out, "display solution variables 1-%d\n", pi->x_dim);
	fprintf(out, "set logfile cplex.log\n");
	fprintf(out, "quit\n");
	fclose(out);

	/* write expect-file for copying problem to RZ */
	out = ffopen(EXPECT_FILENAME, "exp", "wt");
	fprintf(out, "#! /usr/bin/expect\n");
	fprintf(out, "spawn scp %s.mps %s.mst %s.cmd %s:\n", pi->name, pi->name, pi->name, SSH_USER_HOST_PATH);
	fprintf(out, "expect \":\"\n");
	fprintf(out, "send \"%s\\n\"\n", SSH_PASSWD);
	fprintf(out, "interact\n");

	fprintf(out, "spawn ssh %s \"./cplex90 < %s.cmd\"\n", SSH_USER_HOST_PATH, pi->name);
	fprintf(out, "expect \":\"\n");
	fprintf(out, "send \"%s\\n\"\n", SSH_PASSWD);
	fprintf(out, "interact\n");

	fprintf(out, "spawn scp %s:%s.sol .\n", SSH_USER_HOST_PATH, pi->name);
	fprintf(out, "expect \":\"\n");
	fprintf(out, "send \"%s\\n\"\n", SSH_PASSWD);
	fprintf(out, "interact\n");
	fclose(out);

	/* call the expect script */
	chmod(EXPECT_FILENAME ".exp", 0700);
	system(EXPECT_FILENAME ".exp");
}

/**
 * Sets the colors of irns according to the values of variables found in the
 * output file of the solver.
 */
static void pi_apply_solution(problem_instance_t *pi) {
	FILE *in = ffopen(pi->name, "sol", "rt");

	if (!in)
		return;
	DBG((dbg, LEVEL_1, "Applying solution...\n"));
	while (!feof(in)) {
		char buf[1024];
		int num = -1, col = -1, val = -1;

		//TODO No integer feasible solution exists.

		if (fscanf(in, "x%d_%d %d.%s\n", &num, &col, &val, buf) != 3) {
			while(fscanf(in, "%1020s\n", buf) != 1);
			continue;
		}
		if (val == 1) {
			DBG((dbg, LEVEL_1, "x%d_%d = %d\n", num, col, val));
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
	DBG((dbg, LEVEL_1, "Deleting files...\n"));
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
	remove(EXPECT_FILENAME ".exp");
#endif
#ifdef DUMP_LP
	snprintf(buf+end, sizeof(buf)-end, ".lp");
	remove(buf);
#endif
}
#endif

/**
 * Collects all irns in currently processed register class
 */
static void pi_collect_x_names(ir_node *block, void *env) {
	problem_instance_t *pi = env;
	struct list_head *head = &get_ra_block_info(block)->border_head;
	border_t *curr;

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real) {
			x_name_t xx;
			pi->A_dim++;			/* one knapsack constraint for each node */

			xx.n = get_irn_graph_nr(curr->irn);
			pi_set_first_pos(pi, xx.n, pi->x_dim);
			//TODO iterate over all possible colors !!MUST BE IN ORDER!!
			for (xx.c=0; xx.c<MAX_COLORS; ++xx.c) {
				if (!is_possible_color(irn, xx.c))
					continue;
				DBG((dbg, LEVEL_2, "Adding %n %d\n", curr->irn, xx.c));
				obstack_grow(&pi->ob, &xx, sizeof(xx));
				pi->x_dim++;		/* one x variable for each node and color */
			}
		}
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
 * Finds cliques in the interference graph, considering only nodes
 * for which the color pi->curr_color is possible. Finds only 'maximal-cliques',
 * viz cliques which are not conatained in another one.
 * This is used for the matrix B.
 */
static void pi_clique_finder(ir_node *block, void *env) {
	problem_instance_t *pi = env;
	enum phase_t {growing, shrinking} phase = growing;
	struct list_head *head = &get_ra_block_info(block)->border_head;
	border_t *b;
	pset *living = pset_new_ptr(SLOTS_LIVING);

	list_for_each_entry_reverse(border_t, b, head, list) {
		const ir_node *irn = b->irn;
		if (!is_possible_color(n, pi->curr_col))
			continue;

		if (b->is_def) {
			DBG((dbg, LEVEL_2, "Def %n\n", irn));
			pset_insert_ptr(living, irn);
			phase = growing;
		} else { /* is_use */
			DBG((dbg, LEVEL_2, "Use %n\n", irn));

			/* before shrinking the set, store the current 'maximum' clique;
			 * do NOT if clique is a single node
			 * do NOT if all values are live_out */
			if (phase == growing && pset_count(living) >= 2 && !all_live_out(block, living)) {
				ir_node *n;
				for (n = pset_first(living); n; n = pset_next(living)) {
					int pos = pi_get_pos(pi, get_irn_graph_nr(n), pi->curr_color);
					matrix_set(pi->B, pi->curr_row, pos, 1);
					DBG((dbg, LEVEL_2, "B[%d, %d] := %d\n", pi->curr_row, pos, 1));
				}
				pi->curr_row++;
			}
			pset_remove_ptr(living, irn);
			phase = shrinking;
		}
	}

	del_pset(living);
}

/**
 * Generate the initial problem matrices and vectors.
 */
static problem_instance_t *new_pi(const copy_opt_t *co) {
	DBG((dbg, LEVEL_1, "Generating new instance...\n"));
	problem_instance_t *pi = calloc(1, sizeof(*pi));
	pi->irg = co->irg;
	pi->name = 	get_entity_name(get_irg_entity(co->irg));
	pi->num2pos = new_set(set_cmp_num2pos, SLOTS_NUM2POS);
	pi->bigM = 1;

	/* Vector x
	 * one entry per node and possible color */
	obstack_init(&pi->ob);
	dom_tree_walk_irg(co->irg, pi_collect_x_names, NULL, pi);
	pi->x = obstack_finish(&pi->ob);

	/* Matrix Q
	 * weights for the 'same-color-optimization' target */
	{
		unit_t *curr;
		pi->Q = new_matrix(pi->x_dim, pi->x_dim);

		list_for_each_entry(unit_t, curr, &co->units, units) {
			const ir_node *root, *arg;
			int rootnr, argnr;
			unsigned rootpos, argpos;
			int i;

			root = curr->nodes[0];
			rootnr = get_irn_graph_nr(root);
			rootpos = pi_get_first_pos(pi, rootnr);
			for (i = 1; i < curr->node_count; ++i) {
				int weight = -get_weight(root, arg);
				arg = curr->nodes[i];
				argnr = get_irn_graph_nr(arg);
				argpos = pi_get_first_pos(pi, argnr);

				DBG((dbg, LEVEL_2, "Q[%n, %n] := %d\n", root, arg, weight));
				/* for all colors root and arg have in common, set the weight for
				 * this pair in the objective function matrix Q */
				while (rootpos < pi->x_dim && argpos < pi->x_dim &&
						pi->x[rootpos].n == rootnr && pi->x[argpos].n == argnr) {
					if (pi->x[rootpos].c < pi->x[argpos].c)
						++rootpos;
					else if (pi->x[rootpos].c > pi->x[argpos].c)
						++argpos;
					else {
						matrix_set(pi->Q, rootpos++, argpos++, weight);

						if (weight < pi->minQij) {
							DBG((dbg, LEVEL_2, "minQij = %d\n", weight));
							pi->minQij = weight;
						}
						if (weight > pi->maxQij) {
							DBG((dbg, LEVEL_2, "maxQij = %d\n", weight));
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
		pi->A = new_matrix(pi->A_dim, pi->x_dim);
		while (col < pi->x_dim) {
			int curr_n = pi->x[col].n;
			while (col < pi->x_dim && pi->x[col].n == curr_n) {
				DBG((dbg, LEVEL_2, "A[%d, %d] := %d\n", row, col, 1));
				matrix_set(pi->A, row, col++, 1);
			}
			++row;
		}
		assert(row == pi->A_dim);
	}

	/* Matrix B
	 * interference constraints using exactly those cliques not contained in others. */
	{
		int color, expected_clipques = pi->A_dim/3 * MAX_COLORS;
		pi->B = new_matrix(expected_clipques, pi->x_dim);
		for (color = 0; color < MAX_COLORS; ++color) {
			pi->curr_color = color;
			dom_tree_walk_irg(pi->irg, pi_clique_finder, NULL, pi);
		}
		pi->B_dim = matrix_get_rowcount(pi->B);
	}

	return pi;
}

/**
 * clean the problem instance
 */
static void free_pi(problem_instance_t *pi) {
	del_matrix(pi->Q);
	del_matrix(pi->A);
	del_matrix(pi->B);
	del_set(pi->num2pos);
	obstack_free(&pi->ob, NULL);
	free(pi);
}

void co_ilp_opt(copy_opt_t *co) {
	dbg = firm_dbg_register("ir.be.copyoptilp");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	problem_instance_t *pi = new_pi(co);

#ifdef DUMP_MATRICES
	pi_dump_matrices(pi);
#endif

#ifdef DUMP_LP
	pi_dump_lp(pi);
#endif

#ifdef DUMP_MPS
	pi_dump_mps(pi);
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
