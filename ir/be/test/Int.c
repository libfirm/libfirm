/*
 * Project:     GCC-firm
 * File name:   test/Int.c
 * Purpose:     smallest possible Java program
 * Author:      Boris Boesler
 * Modified by: Michael Beck
 * Created:     XX.08.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>

static int hide_int (int i) { /* to cancel out optimization */
    return i;
}

int main (int argc, char *argv[]) {
    int imax  =  2147483647;    /* java.lang.Integer.MAX_VALUE */
    int imin1 = -2147483647;	 /* java.lang.Integer.MIN_VALUE+1 */
    int imin  = -2147483648;	 /* java.lang.Integer.MIN_VALUE */
    int i;

    printf("Int.c\n");

    printf(" Extreme constant integer values allowed in C:\n");
    printf("  Integer.MAX_VALUE   ( 2147483647) = %d\n", imax);
    printf("  Integer.MIN_VALUE-1 (-2147483647) = %d\n", imin1);
    printf("  Integer.MIN_VALUE   (-2147483648) = %d\n", imin);

    printf("\n Computations with integers:\n");
    i = 2147483647 - hide_int(147483647);      /* to cancel out optimization */
    printf("  i = 2147483647 - 147483647 (2000000000): %d\n", i);
    i = i / 5;
    printf("  i = i / 5 (400000000): %d\n", i);
    i = i * -2;
    printf("  i = i * -2 (-800000000): %d\n", i);
    i = i + 12344321;
    printf("  i = i + 12344321 (-787655679): %d\n", i);
    i = i % 100000000;
    printf("  i = i %% 100000000 (-87655679): %d\n", i);

    printf("\n Computations exceeding the maximal values:\n");
    i = imax + 1;
    printf("  i = imax + 1 () : %d\n", i);
    i = imax * 2;
    printf("  i = imax * 2 () : %d\n", i);
    i = imax - 1;
    printf("  i = imin - 1 () : %d\n", i);
    i = imin * 2;
    printf("  i = imin * 2 () : %d\n", i);

    return 0;
}
