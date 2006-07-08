struct opcode {
  struct {
    unsigned imm16:16;
  } foo;
};


int main(void) {
  int i;
  struct opcode oc = { { 42 } };

  i = oc.foo.imm16 & 0x80f4;
  printf("Result: %d (should be 32)\n", i);
  return 0;
}
