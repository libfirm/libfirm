int a_number()
{
	return 7;
}

void a_m_serial()
{
	int n, a_number();
	n= a_number();
}

int main()
{
	a_m_serial();
	printf("Result: %d\n", a_number());
	return 0;
}
