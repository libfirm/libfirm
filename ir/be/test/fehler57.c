int floor_log2_wide (unsigned int x)
{
	int log = -1;
	while (x != 0)
		log++, x >>= 1;
	return log;
}

int main()
{
	printf("Res: %d\n", floor_log2_wide(4294967251));
}
