/*
 * Project:     GCC-firm
 * File name:   test/Long.c
 * Purpose:     Long integer test program
 * Author:      Goetz Lindenmaier
 * Modified by: Michael Beck
 * Created:     XX.04.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>

static int hide_int (int i) {
    return i;
}

int main (int argc, char *argv[]) {
    long long lmax, lmin, lmin1, limax, limin1, limin, l1, l;
    int i;

    printf("LongLong.c\n");

    lmax   =  9223372036854775807LL; /* java.lang.Long.MAX_VALUE */
    lmin1  = -9223372036854775807LL; /* java.lang.Long.MIN_VALUE */
    lmin   = -9223372036854775808LL; /* java.lang.Long.MIN_VALUE */
    limax  =  2147483647L;          /* java.lang.Integer.MAX_VALUE */
    limin1 = -2147483647L;          /* java.lang.Integer.MAX_VALUE-1 */
    limin  = -2147483648L;          /* java.lang.Integer.MIN_VALUE */

    printf(" Long long values allowed in C:\n");
    /* So far, these values are not representable by libfirm. */
    printf("  LongLong.MAX_VALUE   ( 9223372036854775807LL) = %lld\n", lmax);
    printf("  LongLong.MIN_VALUE-1 (-9223372036854775807LL) = %lld\n", lmin1);
    printf("  LongLong.MIN_VALUE   (-9223372036854775808LL) = %lld\n", lmin);

    printf("\n Extreme integer values represented as long long:\n");
    printf("  Long.MAX_VALUE   ( 2147483647L) = %lld\n", limax);
    printf("  Long.MIN_VALUE-1 (-2147483647L) = %lld\n", limin1);
    printf("  Long.MIN_VALUE   (-2147483648L) = %lld\n", limin);

    printf("\n Computations exceeding these barriers:\n");
    i =  2147483647;
    //i =  2;
    l =  hide_int(i) + 3LL;
    printf("  long long l = (int i = 2147483647) + 3LL (2147483650): %lld\n", l);
    l = 2 * l;
    printf("  long long l = 2 * l (4294967300): %lld\n", l);
    l = l * -1;
    printf("  long long l = l * -1 (-4294967300): %lld\n", l);
    l = l / 3;
    printf("  long long l = l / 3 (-1431655766): %lld\n", l);
    l = l % 1000000000;
    printf("  long long l = l %% 1000000000 (-431655766): %lld\n", l);

    return 0;
}
