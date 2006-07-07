int a = 3, b = 4, c = 3;

int truth(int a, int b, int c)
{
  return (a == c & (a < b));
}

int main()
{
  printf("truth(%d, %d, %d) = %d\n", a, b, c, truth(a, b, c));
  return 0;
}
