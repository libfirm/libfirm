unsigned short tdenm, tquot;

void test(unsigned short num[]) {

	unsigned int tnum = (((unsigned int) num[0]) << 16) + num[1];

	/* Do not execute the divide instruction if it will overflow. */
	if ((tdenm * 0xffffL) < tnum)
		tquot = 0xffff;
}

int main(int argc, char *argv[]) {
	return 0;
}
