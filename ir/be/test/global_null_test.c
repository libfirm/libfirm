static void f1(void) {
	printf("f1\n");
}

static void f2(void) {
	printf("f2\n");
}

int test(char *x)
{
  char ret = *x;

  if (x)
    f1();
  else
    f2();
  return ret;
}

int main(int argc, char *argv[]) {
	char x = '\0';

	return test(&x);
}
