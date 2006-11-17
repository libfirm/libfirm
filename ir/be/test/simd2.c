#include <stdio.h>

#define abs(X) (((X)>0)?(X):-(X))

unsigned int sad16_c(	const unsigned char * const cur,
			const unsigned char * const ref,
			const unsigned int stride,
			const unsigned int best_sad)
{

	unsigned int sad = 0;
	unsigned int j;
	unsigned char const *ptr_cur = cur;
	unsigned char const *ptr_ref = ref;

	for (j = 0; j < 16; j++) {
			sad += abs(ptr_cur[0] - ptr_ref[0]);
			sad += abs(ptr_cur[1] - ptr_ref[1]);
			sad += abs(ptr_cur[2] - ptr_ref[2]);
			sad += abs(ptr_cur[3] - ptr_ref[3]);
			sad += abs(ptr_cur[4] - ptr_ref[4]);
			sad += abs(ptr_cur[5] - ptr_ref[5]);
			sad += abs(ptr_cur[6] - ptr_ref[6]);
			sad += abs(ptr_cur[7] - ptr_ref[7]);
			sad += abs(ptr_cur[8] - ptr_ref[8]);
			sad += abs(ptr_cur[9] - ptr_ref[9]);
			sad += abs(ptr_cur[10] - ptr_ref[10]);
			sad += abs(ptr_cur[11] - ptr_ref[11]);
			sad += abs(ptr_cur[12] - ptr_ref[12]);
			sad += abs(ptr_cur[13] - ptr_ref[13]);
			sad += abs(ptr_cur[14] - ptr_ref[14]);
			sad += abs(ptr_cur[15] - ptr_ref[15]);


			if (sad >= best_sad)
				return sad;

			ptr_cur += stride;
			ptr_ref += stride;

	}

	return sad;

}

int main(int argc, char** argv){
  unsigned char cur[65536];
  unsigned char ref[65536];
  int sum = 0;
  int numofruns = 100;
  int i,ii;

  if(argc > 1) {
	  numofruns = atoi(argv[1]);
  }

  for (i=0;i < numofruns; i++){
   // Reset cache. Alles andere ist unrealistisch.
    for(ii = 0; ii<65536;ii++){
      cur[ii]=ii&0xff;
      ref[ii]=(ii+4)&0xff;
    }
    sum += sad16_c(cur, ref, 64, 100000);
  }
  printf("sum = %i\n", sum);
  return 0 ;
}
