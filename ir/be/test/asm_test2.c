
int main(int argc, char **argv) {
	int i;

	for(i = 0; i < 10; ++i) {
		__asm__ __volatile__("/* dummy3 */");
	}

	if(argc) {
		if(argc * 2 > 14) {
			__asm__ __volatile__("/* dummy */");
		} else {
			__asm__ __volatile__("/* dummy2 */");
		}
	}

	return 0;
}
