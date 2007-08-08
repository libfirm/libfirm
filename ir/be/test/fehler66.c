double       a[1000000];
unsigned int b[1000000];
double       a2[1000000];
unsigned int b2[1000000];

int main() {
	int i;

	for(i = 0; i < 123; ++i) {
		a[i] = b[i];
		a2[i] = b2[i];
	}

	return 0;
}
