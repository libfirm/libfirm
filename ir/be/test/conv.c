int convs16_s32( short t) __attribute__((noinline));
short convs32_s16( int t) __attribute__((noinline));

int convu16_s32( unsigned short t) __attribute__((noinline));
short convu32_s16( unsigned int t) __attribute__((noinline));

unsigned int convs16_u32( short t) __attribute__((noinline));
unsigned short convs32_u16( int t) __attribute__((noinline));

unsigned int convu16_u32( unsigned short t) __attribute__((noinline));
unsigned short convu32_u16( unsigned int t) __attribute__((noinline));

int convs16_s32( short t)
{
  return t;
}

short convs32_s16( int t)
{
  return t;
}

int convu16_s32( unsigned short t)
{
  return t;
}

short convu32_s16( unsigned int t)
{
  return t;
}

unsigned int convs16_u32( short t)
{
  return t;
}

unsigned short convs32_u16( int t)
{
  return t;
}

unsigned int convu16_u32( unsigned short t)
{
  return t;
}

unsigned short convu32_u16( unsigned int t)
{
  return t;
}

int main()
{
  short s = 0x8000;
  int i = 0xF0008000;
  int a;

  printf(" %d -> %d = %d\n", s, i, convs16_s32( s));
  printf(" %d -> %d = %d\n", i, s, convs32_s16( i));

  printf(" %d -> %d = %d\n", i, s, convu32_s16( i));
  printf(" %d -> %d = %d\n", i, s, convu32_s16( i));

  printf(" %d -> %d = %d\n", i, s, convs32_u16( i));
  printf(" %d -> %d = %d\n", i, s, convs32_u16( i));

  printf(" %d -> %d = %d\n", i, s, convu32_u16( i));
  printf(" %d -> %d = %d\n", i, s, convu32_u16( i));

  return 0;
}
