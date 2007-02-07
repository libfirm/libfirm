static int A;

/* This function produces unnecessary Phi nodes due to the way
 * x is assigned. Note: This is not a bug, its by the Phi construction algorithm. */
void test(int l, int m) {
  int i, x = m;

  for (i = 0; i < l; ++i) {
    A = x;

    if (l > 5)
      x = m;
  }
}

int main()
{
  test(4,5);
  printf("A = %d\n", A);
  return 0;
}
