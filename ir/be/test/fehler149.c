unsigned short tdenm = 0xff;
unsigned int ui = 0xffffffff;

int test(void) {

	unsigned int tnum = ui;

	/* Do not execute the divide instruction if it will overflow. */
	if ((tdenm * 0xffffL) < tnum)
		return 0;
	return 1;
}

int main(int argc, char *argv[]) {
	return test();
}
