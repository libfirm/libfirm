#include <stdio.h>
#include <limits.h>

#define MAX (1 << 10)
#define TRUNC 0xff
#define MAX_SHOW TRUNC

void dump_field(short *field, int size, const char *name) {
  int i;
  printf("======== %s : START ========\n", name);
  for(i = 0; i < size; i++){
       printf("data[%i] = %i\n", i, field[i]);
  }
  printf("======== %s :  END  ========\n", name);
}

void dequant_h263_inter_c(short *data, const short *coeff, const unsigned int quant) {
	const unsigned short quant_m_2 = quant << 1;
	const unsigned short quant_add = (quant & 1 ? quant : quant - 1);
	int i;

	for (i = 0; i < MAX; i++) {
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

int main(int argc, char **argv){
  short cur[MAX];
  short ref[MAX];
  int numofruns = 30;
  int i,ii;

  if(argc > 1)
	  numofruns = atoi(argv[1]);

  for (i = 0; i < numofruns; i++){
    /* Reset cache. Alles andere ist unrealistisch. */
    for(ii = 0; ii < MAX; ii++){
      cur[ii] = 0;
      ref[ii] = (ii + i + 3) & TRUNC;
    }

	if (i == 0 && argc == 1)
	  dump_field(ref, MAX_SHOW, "ref");

	dequant_h263_inter_c(cur, ref, 1024 * (i & 0x3));
  }

  if (argc == 1)
    dump_field(cur, MAX_SHOW, "cur");

  return 0;
}
