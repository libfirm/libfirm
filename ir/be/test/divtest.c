int divtest_1(int a)
{
  return a / 2;
}

int divtest_2(int a)
{
  return a / 8;
}

unsigned divtest_3(unsigned a)
{
  return a / 8;
}

int modtest_1(int a)
{
  return a % 2;
}

int modtest_2(int a)
{
  return a % 8;
}

unsigned modtest_3(unsigned a)
{
  return a % 8;
}

int main()
{
  printf("+7 DIV +3 = %+d   +7 MOD +3 = %+d\n", +7 / +3, +7 % +3);
  printf("-7 DIV +3 = %+d   -7 MOD +3 = %+d\n", -7 / +3, -7 % +3);
  printf("+7 DIV -3 = %+d   +7 MOD -3 = %+d\n", +7 / -3, +7 % -3);
  printf("-7 DIV -3 = %+d   -7 MOD -3 = %+d\n", -7 / -3, -7 % -3);

  printf("DivTest 1 = %d\n", divtest_1(17));
  printf("DivTest 2 = %d\n", divtest_2(-17));
  printf("DivTest 3 = %u\n", divtest_3(17));

  printf("ModTest 1 = %d\n", modtest_1(17));
  printf("ModTest 2 = %d\n", modtest_2(-17));
  printf("ModTest 3 = %u\n", modtest_3(17));

	return 0;
}
