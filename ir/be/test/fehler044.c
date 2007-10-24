#define IM 2147483648u

unsigned int current_random1 = 1892341778;
int imax1 = 403;
unsigned int current_random2 = 4247132568;
int imax2 = 403;

int main()
{
	int ival1 = current_random1 & (IM - 1);
	ival1 = (int) ((float) ival1 * (float) (imax1 + 0.999) / (float) IM);
	int ival2 = current_random2 & (IM - 1);
	ival2 = (int) ((float) ival2 * (float) (imax2 + 0.999) / (float) IM);

	printf("Res1: %d\n", ival1);
	printf("Res2: %d\n", ival2);

	return 0;
}
