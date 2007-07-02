#include <stdio.h>

int arr0[] = { 0, 1, 2, 3 };
int arr1[4] = { 3, 2, 1 };
int arrm[3][4] = { {1}, {2,3,4,5}, {6, 7, 8 } };

int main()
{
	int i, i2;

	for(i = 0; i < (sizeof(arr0)/sizeof(arr0[0])); ++i) {
		printf("arr0[%d] = %d\n", i, arr0[i]);
	}
	for(i = 0; i < 4; ++i) {
		printf("arr1[%d] = %d\n", i, arr1[i]);
	}
	for(i = 0; i < 4; ++i) {
		for(i2 = 0; i2 < 3; ++i2) {
			printf("arrm[%d][%d] = %d\n", i2, i, arrm[i2][i]);
		}
	}

	return 0;
}
