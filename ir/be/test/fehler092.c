/* fehler92: localopt messing up const/tarval modes */
#include <stdio.h>

int main(int argc, char **argv)
{
	unsigned int x;
	int y = 3;

	if(argc > 1) {
		rand();
		x = 20;
	} else {
		x = 10;
	}
	y <<= x;

	printf("Res: %d\n", y);
	return 0;
}
