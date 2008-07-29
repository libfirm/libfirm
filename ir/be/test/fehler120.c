/*$ -fno-inline $*/

int f(float x)
{
	return 1 << (int)x;
}

int main(void)
{
	return f(3.0) != 8;
}
