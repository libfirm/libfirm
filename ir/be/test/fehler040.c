
#define FABS(x)  fabs(x)
#define FLOOR(x) floor(x)

double One = 1.0;
double C, Y, Z;

int main()
{
	Y = One;
	Z = 0.00000000000000011102;
	printf("Y: %20.20f Z: %20.20f\n", Y, Z);
	/* ... D is power of 1/Radix < 1. */
	do  {
		C = Y;
		Y = Z;
		Z = Y * Y;
		//printf("Y: %30.30f Z:%30.30f\n", Y, Z);
	} while ((Y > Z) && (Z + Z > Z));

	printf("Res. %20.20f - %20.20f\n", Z, Y);
	return 0;
}
