int main(int argc, char *argv[])
{
	switch(argc)
	{
		case 0x666d7420:	// 'fmt '
			return 1;
		case 0x4c495354:	// 'LIST'
			return 2;
		default:
			break;
	}
	return 0;
}
