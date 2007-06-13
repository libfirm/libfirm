
volatile int i;
volatile float f;

int main()
{
	/* access */
	(void) i;
	(void) f;

	/* store */
	i = 5;
	f = 2.4f;

	return 0;
}
