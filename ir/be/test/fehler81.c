/*$ -fno-inline */
#include <stdio.h>

void test1(int a) {
	switch (a) {
	case 1:
		goto label;
	default:
		printf("default\n");
		break;
	label:
		printf("case 1\n");
	}
}

void test2(int a) {
	switch (a) case 1: printf("case 1\n");
	printf("end\n");
}

int main() {
	test1(1);
	test1(2);
	test2(1);
	test2(2);
	return 0;
}
