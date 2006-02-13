#include <alloca.h>
#include <stdio.h>

struct x {
  int a, b;
};

static void t(struct x *p)
{
  printf("%d\n", p->a + p->b);
}

int main(int argc, char *argv[])
{
  struct x *p = alloca(sizeof(*p));

  p->a = argc;
  p->b = 3;

  t(p);
}
