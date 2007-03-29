/*
 * Proj's of the arguments
 * Simple DF node.
 */
int add(int a, int b) {
  return a + b;
}

/*
 * A function call
 */
int two_times(int a) {
  return add(a, a);
}

int main(int argc, char *argv[])
{
  printf("%d\n", two_times(3));
  return 0;
}
