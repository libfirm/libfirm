static void crashme(void)
{
	* ( int * ) 0 = 0x12345678;
}

int main(int argc)
{
	if(argc > 1)
		crashme();
	return 0;
}
