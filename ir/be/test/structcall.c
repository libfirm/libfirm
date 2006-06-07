struct A {
  int a;
  int b;
};

int test(struct A arg) {
  return arg.a + arg.b;
}

int main(int argc, char *argv[]) {
  struct A a;

  a.a = 3;
  a.b = 4;

  printf("Sum = %d\n", test(a));
  return 0;
}
