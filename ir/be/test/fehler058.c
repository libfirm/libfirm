short ii = 230;

int main() {
  short i = ii & 255;
  printf("res = %d (should be 230)\n", i);
  return 0;
}
