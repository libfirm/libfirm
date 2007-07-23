#include <stdio.h>

void te(int *bla) {
	int *blup = bla;

	while(blup) {
		switch(*blup) {
		case 20:
			printf("Joa: %d\n", *(blup+1));
			blup = 0;
			break;
		}
	}
}

int main()
{
	int arr[] = { 20, 2, 4, 5, 6, 8 };

	te(arr);

	return 0;
}
