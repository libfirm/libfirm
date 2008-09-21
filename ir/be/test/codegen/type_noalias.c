
struct bar {
	unsigned int size;
};

char *foo;

void f(void)
{
	foo += 5;
	((struct bar*) (foo - 10))->size = 10;
	((struct bar*) (foo - 10))->size += 1;
	((struct bar*) (foo - 10))->size -= 1;
}
