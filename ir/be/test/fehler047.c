/* Incorrect values when using C99 style initialisation */

int tab[] = {
	[5] = 23
};


int main(void)
{
	int i = tab[5];
	printf("%d (23)\n", i);
	return i != 23;
}
