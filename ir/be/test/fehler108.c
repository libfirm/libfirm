int f(void) {
	return 0;
}

int (*f_ptr) (void) = &f;

int main(void) {
	return (******f_ptr)();
}
