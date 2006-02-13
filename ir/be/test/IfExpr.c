//
// GCC-firm Project
//
// $Id$
//
// Testprogram to test GCC-firm : Declatations - part ][

#include <stdio.h>

static int test (int arg0) {
    int a, b, c;
    a = arg0;
    c = 2;
    b = 69;
    if((a) == c) {
	c = b - 1;
    }
    else {
	c = a + 77;
    }
    return(c);
}

static int const_if(int arg0) {
    int a;
    if(2 < 3) {
	a = 5;
    }
    else {
	a = 7;
    }
    return(a);
}

int main(int argc, char *argv[]) {
    printf("IfExpr.c\n");

    printf(" test(2) = %d (should be 68)\n", test(2));
    printf(" test(3) = %d (should be 80)\n", test(3));
    printf(" const_if(0) = %d (should be 5)\n", const_if(0));

    return 0;
}
