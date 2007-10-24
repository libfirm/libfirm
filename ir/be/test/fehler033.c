/* tailrec optimisation failing on struct parameters */

typedef struct {
	int a, b, c;
} stru;

void f(int a, const stru x) {
	if(a == 100)
		return;
	f(a+1, x);
}

int main() {
	return 0;
}
