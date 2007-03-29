#include <stdio.h>

unsigned int sse8_16bit_c(	const int * b1,
				const int * b2,
				const unsigned int stride)
{
	int i;
	int sse = 0;

	for (i=0; i<8; i++) {
		sse += (b1[0] - b2[0])*(b1[0] - b2[0]);
		sse += (b1[1] - b2[1])*(b1[1] - b2[1]);
		sse += (b1[2] - b2[2])*(b1[2] - b2[2]);
		sse += (b1[3] - b2[3])*(b1[3] - b2[3]);
		sse += (b1[4] - b2[4])*(b1[4] - b2[4]);
		sse += (b1[5] - b2[5])*(b1[5] - b2[5]);
		sse += (b1[6] - b2[6])*(b1[6] - b2[6]);
		sse += (b1[7] - b2[7])*(b1[7] - b2[7]);

		b1 = (const int*)((char*)b1+stride);
		b2 = (const int*)((char*)b2+stride);
	}

	return(sse);
}

#define MAX 65536

int main(){
  int cur[MAX];
  int ref[MAX];
  int sum = 0;
  int numofruns = 10;
  int i,ii;
  for (i=0;i < numofruns; i++){
    // Reset cache. Alles andere ist unrealistisch.
    for(ii = 0; ii<MAX;ii++){
      cur[ii]=(ii)&0xff;
      ref[ii]=(ii+i+3)&0xff;
    }
    sum = sse8_16bit_c(cur, ref, 32);
    printf("sum[%i] = %i\n",i, sum);
  }
  return 0 ;
}
