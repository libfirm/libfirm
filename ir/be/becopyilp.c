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
#include "becopystat.h"

#undef DUMP_MATRICES		/**< dumps all matrices completely. only recommended for small problems */
#define DUMP_MILP			/**< dumps the problem as Mixed Integer Linear Programming in "CPLEX"-MPS format. NOT fixed-column-MPS. */
#undef DO_SOLVE 			/**< solve the MPS output with CPLEX */
#undef DELETE_FILES		/**< deletes all dumped files after use */

/* CPLEX-account related stuff */
#define SSH_USER_HOST "kb61@sp-smp.rz.uni-karlsruhe.de"
#define SSH_PASSWD_FILE "/ben/daniel/.smppw"
#define EXPECT_FILENAME "runme" /** name of the expect-script */

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define SLOTS_NUM2POS 256
#define SLOTS_LIVING 32

/* get_weight represents the _gain_ if node n and m have the same color. */
#define get_weight(n,m) 1

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
	const copy_opt_t *co;			/** the original copy_opt problem */
	int x_dim, A_dim, B_dim;		/**< number of: x variables (equals Q_dim), rows in A, rows in B */
	x_name_t *x;					/**< stores the names of the x variables. all possible colors for a node are ordered and occupy consecutive entries. lives in obstack ob. */
	set *num2pos;					/**< maps node numbers to positions in x. */
	sp_matrix_t *Q, *A, *B;			/**< the (sparse) matrices of this problem */

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
	FILE *out = ffopen(pi->co->name, "matrix", "wt");

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

#ifdef DUMP_MILP
/**
 * Dumps an mps file representing the problem. This is NOT the old-style,
 * fixed-column format. Some white spaces are important, in general spaces
 * are separators, MARKER-lines are used in COLUMN section to define binaries.
 */
//BETTER use last 2 fields in COLUMNS section. See MPS docu for details
static void pi_dump_milp(problem_instance_t *pi) {
	int i, max_abs_Qij;
	const matrix_elem_t *e;
	FILE *out = ffopen(pi->co->name, "milp", "wt");

	DBG((dbg, LEVEL_1, "Dumping milp...\n"));
	max_abs_Qij = pi->maxQij;
	if (-pi->minQij > max_abs_Qij)
		max_abs_Qij = -pi->minQij;
	pi->bigM = pi->A_dim * max_abs_Qij;
	DBG((dbg, LEVEL_2, "BigM = %d\n", pi->bigM));

	matrix_optimize(pi->Q);
	bitset_t *good_row = bitset_alloca(pi->x_dim);
	for (i=0; i<pi->x_dim; ++i)
		if (matrix_row_first(pi->Q, i))
			bitset_set(good_row, i);

	fprintf(out, "NAME %s\n", pi->co->name);

	fprintf(out, "ROWS\n");
	fprintf(out, " N obj\n");
	for (i=0; i<pi->x_dim; ++i)
		if (bitset_is_set(good_row, i))
			fprintf(out, " E cQ%d\n", i);
	for (i=0; i<pi->A_dim; ++i)
		fprintf(out, " E cA%d\n", i);
	for (i=0; i<pi->B_dim; ++i)
		fprintf(out, " L cB%d\n", i);
	for (i=0; i<pi->x_dim; ++i)
		if (bitset_is_set(good_row, i))
			fprintf(out, " L cy%d\n", i);

	fprintf(out, "COLUMNS\n");
	/* the x vars come first */
	/* mark them as binaries */
	fprintf(out, "    MARKI0\t'MARKER'\t'INTORG'\n");
	for (i=0; i<pi->x_dim; ++i) {
		/* participation in objective */
		if (bitset_is_set(good_row, i))
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
		if (bitset_is_set(good_row, i))
			fprintf(out, "    x%d_%d\tcy%d\t%d\n", pi->x[i].n, pi->x[i].c, i, 2*pi->bigM);
	}

	fprintf(out, "    MARKI1\t'MARKER'\t'INTEND'\n"); /* end of marking */

	/* next the s vars */
	for (i=0; i<pi->x_dim; ++i)
		if (bitset_is_set(good_row, i)) {
			/* participation in objective */
			fprintf(out, "    s%d_%d\tobj\t%d\n", pi->x[i].n, pi->x[i].c, 1);
			/* in Q */
			fprintf(out, "    s%d_%d\tcQ%d\t%d\n", pi->x[i].n, pi->x[i].c, i, -1);
		}

	/* next the y vars */
	for (i=0; i<pi->x_dim; ++i)
		if (bitset_is_set(good_row, i)) {
			/* in Q */
			fprintf(out, "    y%d_%d\tcQ%d\t%d\n", pi->x[i].n, pi->x[i].c, i, -1);
			/* in y */
			fprintf(out, "    y%d_%d\tcy%d\t%d\n", pi->x[i].n, pi->x[i].c, i, 1);
		}

	fprintf(out, "RHS\n");
	for (i=0; i<pi->x_dim; ++i)
		if (bitset_is_set(good_row, i))
			fprintf(out, "    rhs\tcQ%d\t%d\n", i, -pi->bigM);
	for (i=0; i<pi->A_dim; ++i)
		fprintf(out, "    rhs\tcA%d\t%d\n", i, 1);
	for (i=0; i<pi->B_dim; ++i)
		fprintf(out, "    rhs\tcB%d\t%d\n", i, 1);
	for (i=0; i<pi->x_dim; ++i)
		if (bitset_is_set(good_row, i))
			fprintf(out, "    rhs\tcy%d\t%d\n", i, 2*pi->bigM);

	fprintf(out, "ENDATA\n");
	fclose(out);
}
#endif

#ifdef DO_SOLVE
/**
 * Dumps the known solution to a file to make use of it
 * as a starting solution respectively as a bound
 */
static void pi_dump_start_sol(problem_instance_t *pi) {
	int i;
	FILE *out = ffopen(pi->co->name, "mst", "wt");
	fprintf(out, "NAME\n");
	for (i=0; i<pi->x_dim; ++i) {
		int val, n, c;
		n = pi->x[i].n;
		c = pi->x[i].c;
		if (get_irn_color(get_irn_for_graph_nr(pi->co->irg, n)) == c)
			val = 1;
		else
			val = 0;
		fprintf(out, "    x%d_%d\t%d\n", n, c, val);
	}
	fprintf(out, "ENDATA\n");
	fclose(out);
}

/**
 * Invoke an external solver
 */
static void pi_solve_ilp(problem_instance_t *pi) {
	FILE *out, *pwfile;
	char passwd[128];

	DBG((dbg, LEVEL_1, "Solving with CPLEX@RZ...\n"));
	/* write command file for CPLEX */
	out = ffopen(pi->co->name, "cmd", "wt");
	fprintf(out, "set logfile %s.sol\n", pi->co->name);
#ifdef DUMP_MILP
	fprintf(out, "read %s.milp mps\n", pi->co->name);
#endif
#ifdef DUMP_MIQP
	fprintf(out, "read %s.miqp mps\n", pi->co->name);
#endif
	fprintf(out, "read %s.mst\n", pi->co->name);
	fprintf(out, "set mip strategy mipstart 1\n");
	fprintf(out, "set mip emphasis 3\n");
	fprintf(out, "optimize\n");
	fprintf(out, "display solution variables 1-%d\n", pi->x_dim);
	fprintf(out, "set logfile cplex.log\n");
	fprintf(out, "quit\n");
	fclose(out);

	/* write expect-file for copying problem to RZ */
	pwfile = fopen(SSH_PASSWD_FILE, "rt");
	fgets(passwd, sizeof(passwd), pwfile);
	fclose(pwfile);

	out = ffopen(EXPECT_FILENAME, "exp", "wt");
	fprintf(out, "#! /usr/bin/expect\n");
	fprintf(out, "spawn scp %s.miqp %s.milp %s.mst %s.cmd %s:\n", pi->co->name, pi->co->name, pi->co->name, pi->co->name, SSH_USER_HOST); /* copy problem files */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn ssh %s \"./cplex90 < %s.cmd\"\n", SSH_USER_HOST, pi->co->name); /* solve */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn scp %s:%s.sol .\n", SSH_USER_HOST, pi->co->name); /*copy back solution */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn ssh %s ./dell\n", SSH_USER_HOST); /* clean files on server */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);
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
	FILE *in = ffopen(pi->co->name, "sol", "rt");

	if (!in)
		return;
	DBG((dbg, LEVEL_1, "Applying solution...\n"));
	while (!feof(in)) {
		char buf[1024];
		int num = -1, col = -1, val = -1;

		fgets(buf, sizeof(buf), in);
		DBG((dbg, LEVEL_3, "Line: %s", buf));

		if (strcmp(buf, "No integer feasible solution exists.") == 0)
			assert(0 && "CPLEX says: No integer feasible solution exists!");

		if (strcmp(buf, "TODO Out of memory") == 0) {}

#ifdef DO_STAT
		{
			/* solution time */
			float sol_time;
			int iter;
			if (sscanf(buf, "Solution time = %f sec. Iterations = %d", &sol_time, &iter) == 2) {
				DBG((dbg, LEVEL_2, " Time: %f Iter: %d\n", sol_time, iter));
				curr_vals[I_ILP_TIME] += 10 * sol_time;
				curr_vals[I_ILP_ITER] += iter;
			}
		}
#endif

		/* variable value */
		if (sscanf(buf, "x%d_%d %d", &num, &col, &val) == 3 && val == 1) {
			DBG((dbg, LEVEL_2, " x%d_%d = %d\n", num, col, val));
			set_irn_color(get_irn_for_graph_nr(pi->co->irg, num), col);
		}
	}
	fclose(in);
}
#endif /* DO_SOLVE */

#ifdef DELETE_FILES
static void pi_delete_files(problem_instance_t *pi) {
	char buf[1024];
	int end = snprintf(buf, sizeof(buf), "%s", pi->co->name);
	DBG((dbg, LEVEL_1, "Deleting files...\n"));
#ifdef DUMP_MATRICES
	snprintf(buf+end, sizeof(buf)-end, ".matrix");
	remove(buf);
#endif
#ifdef DUMP_MILP
	snprintf(buf+end, sizeof(buf)-end, ".mps");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".mst");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".cmd");
	remove(buf);
	remove(EXPECT_FILENAME ".exp");
#endif
#ifdef DO_SOLVE
	snprintf(buf+end, sizeof(buf)-end, ".sol");
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
	bitset_t *pos_regs = bitset_alloca(pi->co->cls->n_regs);

	list_for_each_entry_reverse(border_t, curr, head, list)
		if (curr->is_def && curr->is_real) {
			x_name_t xx;
			pi->A_dim++;			/* one knapsack constraint for each node */

			xx.n = get_irn_graph_nr(curr->irn);
			pi_set_first_pos(pi, xx.n, pi->x_dim);

			// iterate over all possible colors in order
			bitset_clear_all(pos_regs);
			pi->co->isa->get_allocatable_regs(curr->irn, pi->co->cls, pos_regs);
			bitset_foreach(pos_regs, xx.c) {
				DBG((dbg, LEVEL_2, "Adding %n %d\n", curr->irn, xx.c));
				obstack_grow(&pi->ob, &xx, sizeof(xx));
				pi->x_dim++;		/* one x variable for each node and color */
			}
		}
}

/**
 * Checks if all nodes in living are live_out in block block.
 */
static INLINE int all_live_in(ir_node *block, pset *living) {
	ir_node *n;
	for (n = pset_first(living); n; n = pset_next(living))
		if (!is_live_in(block, n)) {
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

		if (b->is_def) {
			DBG((dbg, LEVEL_2, "Def %n\n", irn));
			pset_insert_ptr(living, irn);
			phase = growing;
		} else { /* is_use */
			DBG((dbg, LEVEL_2, "Use %n\n", irn));

			/* before shrinking the set, store the current 'maximum' clique;
			 * do NOT if clique is a single node
			 * do NOT if all values are live_in (in this case they were contained in a live-out clique elsewhere) */
			if (phase == growing && pset_count(living) >= 2 && !all_live_in(block, living)) {
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
	pi->co = co;
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
		int color, expected_clipques = pi->A_dim/4 * pi->co->cls->n_regs;
		pi->B = new_matrix(expected_clipques, pi->x_dim);
		for (color = 0; color < pi->co->cls->n_regs; ++color) {
			pi->curr_color = color;
			dom_tree_walk_irg(pi->co->irg, pi_clique_finder, NULL, pi);
		}
		pi->B_dim = matrix_get_rowcount(pi->B);
	}

	return pi;
}

/**
 * clean the problem instance
 */
static void free_pi(problem_instance_t *pi) {
	DBG((dbg, LEVEL_1, "Generating new instance...\n"));
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
	if (!strcmp(co->name, DEBUG_IRG))
		firm_dbg_set_mask(dbg, -1);

	problem_instance_t *pi = new_pi(co);
	DBG((dbg, 0, "\t\t\t %5d %5d %5d\n", pi->x_dim, pi->A_dim, pi->B_dim));

	if (pi->x_dim > 0) {
#ifdef DUMP_MATRICES
	pi_dump_matrices(pi);
#endif

#ifdef DUMP_MILP
	pi_dump_milp(pi);
#endif

#ifdef DO_SOLVE
	pi_dump_start_sol(pi);
	pi_solve_ilp(pi);
	pi_apply_solution(pi);
#endif

#ifdef DELETE_FILES
	pi_delete_files(pi);
#endif
	}
	free_pi(pi);
}
