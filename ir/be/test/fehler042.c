/* float problems */
#include <stdio.h>

double Radix = 2.0;
double One   = 1.0;
double Zero  = 0.0;
double U1;
double W;
double Y;
double Precision;

int main() {
	W = One;
	Precision = Zero;
	do {
		Precision = Precision + One;
		W = W * Radix;
		Y = W + One;
	} while((Y - W) == One);

	U1 = One / W;
	printf("BLa. %.30e\n", U1);


	return 0;
}
