/**
 * Outputs a random set of compiler flags
 *  @author Matthias Braun (warning: 10 minutes hack and no nice code!)
 */
#include <stdio.h>
#include <time.h>

const char* flags[] = {
	"-fif-conv",
	"-finline",
	"-fcse",
	"-fcode-place",
	"-fgvn-pre",
	"-fopt-loop-unrolling",
	"-fconfirm",
	"-fopt-proc-clone",
	"-fno-strength-red",
	"-fno-reassociation"
};

int main()
{
	int i;
	int flagcount = sizeof(flags) / sizeof(flags[0]);
	srand(time(0));

	for(i = 0; i < flagcount; ++i) {
		if(rand() % 2) {
			printf("%s ", flags[i]);
		}
	}
	printf("\n");

	return 0;
}
