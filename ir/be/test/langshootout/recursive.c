// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// recursive test, by bearophile, Jan 24 2006
// Compile with: -O3 -s -fomit-frame-pointer -funroll-loops

#include <stdio.h>

int Ack(int x, int y) {
    if (x == 0)
        return y+1;
    if (y == 0)
        return Ack(x-1, 1);
    return Ack(x-1, Ack(x, y-1));
}

int Fib(int n) {
    if (n < 2)
        return 1;
    return Fib(n-2) + Fib(n-1);
}

double FibFP(double n) {
    if (n < 2.0)
        return 1.0;
    return FibFP(n-2.0) + FibFP(n-1.0);
}

int Tak(int x, int y, int z) {
    if (y < x)
        return Tak( Tak(x-1, y, z), Tak(y-1, z, x), Tak(z-1, x, y) );
    return z;
}

double TakFP(double x, double y, double z) {
    if (y < x)
        return TakFP( TakFP(x-1.0, y, z), TakFP(y-1.0, z, x), TakFP(z-1.0, x, y) );
    return z;
}

int main(int argc, char **argv) {
    int n = 7;

 	if(argc > 1)
		n = atoi(argv[1]) - 1;
    printf("Ack(3,%d): %d\n", n+1, Ack(3, n+1));
    printf("Fib(%.1f): %.1f\n", 28.0+n, FibFP(28.0+n));
    printf("Tak(%d,%d,%d): %d\n", 3*n, 2*n, n, Tak(3*n, 2*n, n));
    printf("Fib(3): %d\n", Fib(3));
    printf("Tak(3.0,2.0,1.0): %.1f\n", TakFP(3.0, 2.0, 1.0));
    return 0;
}
