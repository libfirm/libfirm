#include <stdio.h>

void dequant_h263_inter_c(	short * data,
				const short * coeff,
				const unsigned int quant)
{
	const unsigned short quant_m_2 = quant << 1;
	const unsigned short quant_add = (quant & 1 ? quant : quant - 1);
	int i;

	for (i = 0; i < 64; i++) {
		short acLevel = coeff[i];

		if (acLevel == 0) {
			data[i] = 0;
		} else if (acLevel < 0) {
			acLevel = acLevel * quant_m_2 - quant_add;
			data[i] = (acLevel > 2048 ? acLevel : 2048);
		} else {
			acLevel = acLevel * quant_m_2 + quant_add;
			data[i] = (acLevel <= 2047 ? acLevel : 2047);
		}
	}
}

#define MAX 65536

int main(){
  short cur[MAX];
  short ref[MAX];
  int numofruns = 10;
  int i,ii;
  for (i=0;i < numofruns; i++){
    /* Reset cache. Alles andere ist unrealistisch. */
    for(ii = 0; ii<MAX;ii++){
      cur[ii]=0;
      ref[ii]=(ii+i+3)&0xff;
    }
    dequant_h263_inter_c(cur, ref, 1024*(i&0x3));
  }
  for(ii = 0; ii<64;ii++){
       printf("data[%i] = %i\n",ii, cur[ii]);
  }
  return 0 ;
}
