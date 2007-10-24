struct blup {
	char str[16];
	int dummy;
};

struct blup dumm[] = { { "Hallo" }, { "Welt" } };

int main() {
	puts(dumm[0].str);
	puts(dumm[1].str);
	return 0;
}
