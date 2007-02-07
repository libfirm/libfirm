int T6, T10, T13;

/*
 * The VanDrunen example for testimng GVN-PRE.
 * Compile with -f no-const-fold to get the same result ...
 */
int vandrunen(int t1, int c1, int t11)
{
  int t12;
  int t9;
  int t8;
  int t7;
  int t5;
  int t4;
  int t3;
  int t2;

  t2 = t1;

  for (;;) {
    t3 = t2 + 1;
    if (t3 > 50)
      return 0;

    if (c1 != 0) {
      t4 = t2 + t3;
      t5 = t4;
      T6 = t1 + t5;
      t8 = t1;
    } else {
      t7 = t3 + 1;
      t8 = t7;
    }
    t9 = t2 + t3;
    T10 = t9 + t8;
    t12 = t9 + t11;
    T13 = t12 + t3;
    t2 = t3;
  }
}

int main(int argc, char *argv[]) {
  vandrunen(10, argc & 1, 1);
  printf("%d %d %d\n", T6, T10, T13);
  return 0;
}
