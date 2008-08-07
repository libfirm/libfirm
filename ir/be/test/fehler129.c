char x;
char *p = &x;
int y = 0;

int main(void) {
	return (&x) - (p - y);
}
