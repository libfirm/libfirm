/*$ -fcombo $*/

/*
 * This is a example for the 'Top reaches if first'
 * problem.
 *
 * If the Cond is evaluated BEFORE the Phi(Unknown, 1)
 * gets the Const 1, we decide for != and cannot revert
 * this decision...
 */
int main(int argc) {
	int x;

	if (argc) {
		x = 1;
	} else {
		++argc;
	}

	if (x == 1) {
		printf("x == 1\n");
	} else {
		printf("x != 1\n");
	}
	return 0;
}
