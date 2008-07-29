int f(int x)
{
	return sizeof(char*[x]);
}

int main(void)
{
	return f(23) != sizeof(char*) * 23;
}
