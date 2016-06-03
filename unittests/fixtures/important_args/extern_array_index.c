// FIRM_IMPORTANT_ARGS: 0

extern int *array;
int f(int x);

// x is not important since the address of array has to be loaded from memory
int f(int x)
{
	return array[x];
}
