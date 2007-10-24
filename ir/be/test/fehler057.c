int floor_log2_wide (unsigned int x)
{
	x >>= 1;
	return x;
}

unsigned test_div(int x) {
  x /= -1;
  return x;
}

int X;

int main()
{
	printf("Res: %d\n", floor_log2_wide(4294967251));
	printf("Res: %d\n", test_div(-5));
        printf("Res: %d\n", -5%-1);
        printf("Res: %d\n", +5%-1);
        printf("Res: %d\n", -5%+1);
        printf("Res: %d\n", +5%+1);
        printf("Res: %d\n", X % -1);
        return 0;
}
