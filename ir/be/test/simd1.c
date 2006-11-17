#include <stdio.h>

#define ITERS 8

unsigned int sse8_16bit_c(	const short * b1,
				const short * b2,
				const unsigned int stride)
{
	int i;
	int sse = 0;

	for (i=0; i<ITERS; i++) {
		sse += (b1[0] - b2[0])*(b1[0] - b2[0]);
		sse += (b1[1] - b2[1])*(b1[1] - b2[1]);
		sse += (b1[2] - b2[2])*(b1[2] - b2[2]);
		sse += (b1[3] - b2[3])*(b1[3] - b2[3]);
		sse += (b1[4] - b2[4])*(b1[4] - b2[4]);
		sse += (b1[5] - b2[5])*(b1[5] - b2[5]);
		sse += (b1[6] - b2[6])*(b1[6] - b2[6]);
		sse += (b1[7] - b2[7])*(b1[7] - b2[7]);

		b1 = (const short*)((char*)b1+stride);
		b2 = (const short*)((char*)b2+stride);
	}

	return(sse);
}

#define STRIDE 16
//#define MAX 65536
#define MAX (ITERS * STRIDE)

int main(int argc, char** argv){
	short cur[MAX];
	short ref[MAX];
	int sum = 0;
	int numofruns = 10;
	int i,ii;

	if(argc > 1) {
		numofruns = atoi(argv[1]);
	}

	for (i=0;i < numofruns; i++){
		// Reset cache. Alles andere ist unrealistisch.
		for(ii = 0; ii < MAX; ++ii) {
			cur[ii]=(ii)&0xff;
			ref[ii]=(ii+i+3)&0xff;
		}
		sum = sse8_16bit_c(cur, ref, STRIDE);
		if(i < 10)
			printf("sum[%i] = %i\n",i, sum);
	}

	return 0 ;
}
