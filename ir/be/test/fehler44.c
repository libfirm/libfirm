unsigned int current_random = 1892341778;
int imax = 403;
#define IM 2147483648u

int main()
{
	int ival = current_random & (IM - 1);
	ival = (int) ((float) ival * (float) (imax + 0.999) / (float) IM);

	printf("Res: %d\n", ival);

	return 0;
}
