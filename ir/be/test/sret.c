typedef struct _foo_t {
  int a;
  char b;
} foo_t;

foo_t sret_func(int c) {
  foo_t x;
  x.a = c;
  return x;
}

int main(int argc) {
  foo_t z;

  z = sret_func(argc);

  return z.b;
}
