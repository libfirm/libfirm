struct opcode {
  struct {
    unsigned imm16:16;
  } foo;
};


int main(void) {
  int i;
  struct opcode oc;

  i = oc.foo.imm16 & 0x8000;
  printf("i: %d\n", i);
  return 0;
}
