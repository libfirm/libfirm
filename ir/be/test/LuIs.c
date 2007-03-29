/* testing conversion */
int test(unsigned long long x) {
  return x;
}

int main() {
  printf("%d\n", test(0x1234567812345678LL));
  return 0;
}
