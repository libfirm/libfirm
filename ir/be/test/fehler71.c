/*$ -fno-if-conv -fno-inline $*/

#include <stdio.h>
#include <stdlib.h>

int a = 42;

int changea(void) {
	a = 13;
	return 1;
}

int f2(void) {
	int t = a;
	int t2 = changea();
	changea();
	changea();
	int t3 = t + t2;
	changea();
	return t3;
}

int f(int f) {
	int t = a;
	changea();

	/* must not use source address mode (loading from a) for t+1 */
	if(f > 10000) {
		return t + 1;
	}
	return f + 2;
}

int main(void) {
	srand(0);
	printf("Res: %d (should be 43)\n", f(1000000));
	a = 42;
	printf("Res2: %d (should be 43)\n", f2());
	return 0;
}
