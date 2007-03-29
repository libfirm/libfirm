int main(void) {
  int i = 13957;
  unsigned char c = printf("");

  i |= (c ? 0xffffff00 : 0xd3);
  printf("i: %d\n", i);
  return 0;
}
