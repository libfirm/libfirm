/*
 * Project:     GCC-fim
 * File name:   test/Empty.c
 * Purpose:     float test
 * Author:      Boris Boesler
 * Modified by: Michael Beck (for GCC-firm)
 * Created:     XX.08.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>
#include <float.h>

static void compute (double d1) {
  double d2, d3;
  d2 = 123456789.0;
  d3 = d1 + d2;
  printf(" expecting 123456789012345678.0: %19.1f\n", d3);
}

#define _p(x)	#x
#define p(x)	_p(x)

int main (int argc, char *argv[]) {
    float fminplus  = +FLT_MIN;
    float fminminus = -FLT_MIN;
    float fmaxplus  = FLT_MAX;
    float fmaxminus = -FLT_MAX;
    double dminplus  = +DBL_MIN;
    double dminminus = -DBL_MIN;
    double dmaxplus  = +DBL_MAX;
    double dmaxminus = -DBL_MAX;

    printf("\nExtreme representable float values:\n");
    printf(" +" p(FLT_MIN) " = %+1.8E\n", fminplus);
    printf(" -" p(FLT_MIN) " = %+1.8E\n", fminminus);
    printf(" +" p(FLT_MAX) " = %+1.8E\n", fmaxplus);
    printf(" -" p(FLT_MAX) " = %+1.8E\n", fmaxminus);

    printf("\nExtreme representable double values:\n");
    printf(" +" p(DBL_MIN) " = %+1.17E\n", dminplus);
    printf(" -" p(DBL_MIN) " = %+1.17E\n", dminminus);
    printf(" +" p(DBL_MAX) " = %+1.17E\n", dmaxplus);
    printf(" -" p(DBL_MAX) " = %+1.17E\n", dmaxminus);

    compute(1234567890000000000.0);

    return 0;
}
