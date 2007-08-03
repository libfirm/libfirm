int floor_log2_wide (unsigned int x)
{
	x >>= 1;
	return x;
}

int main()
{
	printf("Res: %d\n", floor_log2_wide(4294967251));
}
