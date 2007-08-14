/*$ -fno-if-conv $*/

#include <stdio.h>
#include <stdlib.h>

int a = 42;

void changea(void) {
	a = 13;
}

int f(int f) {
	int t = a;
	changea();

	/* must not use source address mode (loading from a) for t+1 and t+2 */
	if(f > 10000) {
		return t + 1;
	}
	return f + 2;
}

int main(void) {
	srand(0);
	printf("Res: %d\n", f(rand()));
	return 0;
}
