enum t {
  a, b, c, d = 5
};

int main(int argc, char *argv[])
{
  enum t x;

  x = a;
  printf("a = %d\n", a);
  printf("b = %d\n", b);
  printf("c = %d\n", c);
  printf("d = %d\n", d);

	return 0;
}
