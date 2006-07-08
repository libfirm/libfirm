static int test2(int a)
{
  int b = 0xff;

  switch (a * b) {
    case 0:
      b *= a;
    case 1:
      b *= a;
      ++b;
      break;
    case 2:
      b /= a;
      break;
    case 3:
      b *= a*a;
      break;
    case 4:
      b = a % 3;
      break;
    case 10000:
      b = 2;
      break;
    default:
      return 0;
  }
  return b;
}

int main()
{
  printf("SwitchTest\n");
  printf(" on %d %d\n", -1, test2(-1));
  printf(" on %d %d\n", 0, test2(0));
  printf(" on %d %d\n", 1, test2(1));
  printf(" on %d %d\n", 2, test2(2));
  printf(" on %d %d\n", 3, test2(3));
  printf(" on %d %d\n", 4, test2(4));
  printf(" on %d %d\n", 5, test2(5));
  printf(" on %d %d\n", 6, test2(6));

  return 0;
}
