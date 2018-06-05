/**
 * @file
 * @brief    Generate std.textio compatible pattern files for a testcase.
 * @author   Andreas Seltenreich
 *
 */
#include <stdio.h>
#include "testcase.h"

int main (int argc, char *argv[])
{
  int i=0;
  FILE *in, *out;

  if (argc != 3) {
    printf("usage: %s input_patternfilename output_patternfilename\n", argv[0]);
    return 1;
  }

  if (! (in = fopen(argv[1], "w"))) {
    perror("Cannot open input pattern file");
    return 2;
  }

  if (! (out = fopen(argv[2], "w"))) {
    perror("Cannot open output pattern file");
    return 3;
  }

  for (i=0; i<npatterns; i++) {
    fprintf(in, "%d %d %d\n",
	    (signed)patterns[i].control,
	    (signed)patterns[i].input0,
	    (signed)patterns[i].input1);
    fprintf(out, "%d\n",
	    (signed)test_atom(patterns[i].control,
		      patterns[i].input0,
		      patterns[i].input1));
  }
  return 0;
}


