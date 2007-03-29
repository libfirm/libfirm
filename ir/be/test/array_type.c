int foo[255][4];

int bar[4] = { 0, 1, 2, 3 };

int main(void) {
  printf("bar: %d %d %d %d (should be 0 1 2 3)\n", bar[0], bar[1], bar[2], bar[3]);
  return 0;
}
