extern int x;
int x;

int main()
{
	int i = 0;

	x = 0;
	do {
		x = x + i;
		++i;
	} while(i < 3);

	return x != 3;
}
