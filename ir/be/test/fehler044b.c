#define IM 2147483648u

unsigned int current_random2 = 4247132568;
int imax2 = 403;

int main()
{
	int ival2 = current_random2 & (IM - 1);
	double intermediate = ((float) ival2 * (float)(imax2 + 0.999));
	printf("%3.15e => %u\n", intermediate, (unsigned int) intermediate);

	return 0;
}
