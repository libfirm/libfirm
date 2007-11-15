int f(void)
{
label:
	printf("bla\n");
	goto label;

	return 0;
}

int main(void)
{
	return 0;
}
