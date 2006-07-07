static unsigned int rotater3(unsigned int a)
{
  return (a << 3) | (a >> 29);
}

static unsigned int rotatel3(unsigned int a)
{
  return (a >> 3) | (a << 29);
}

static unsigned int rotater(unsigned int a, unsigned int b)
{
  return (a >> b) | (a << (32-b));
}


int main()
{
  printf("5 >>r>> 3 = %d\n", rotater3(5));
  printf("5 <<r<< 3 = %d\n", rotatel3(5));
  printf("5 >>r>> 4 = %d\n", rotater(5,4));

  return 0;
}
