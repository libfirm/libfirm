#include <stdlib.h>

int A, B, C;

int test(int a, int b, int c) {
	switch (a) {
	case 1:
		B = b;
		return A + c;
	case 2:
		B = b;
		return c + A;
	case 3:
		return c + A;
	case 4:
		abort();
	}
	abort();
}

int main(int argc, char *argv[]) {
	return 0;
}
