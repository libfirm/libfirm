/*$ -fno-inline $*/
#include <stdio.h>

int cse1(int a) {
		int x = a * 3;
		int y = 3 * a;
		return x + y;
}

int cse2(int a, int b) {
		int x = a * b;
		int y = b * a;
		return x - y;
}

int main() {
		printf("cse1(3) = %d (should be 18)\n", cse1(3));
		printf("cse2(3,4) = %d (should be 0)\n", cse2(3,4));
		return 0;
}
