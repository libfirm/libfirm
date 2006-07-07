#define MAX_OPERANDS 3

typedef unsigned short uint16_t;
typedef unsigned char uint8_t;

typedef struct ASMInstr {
    uint16_t sym;
    uint8_t op_type[MAX_OPERANDS]; /* see OP_xxx */
} ASMInstr;

const ASMInstr asm_instrs[] = {
    { 1, { 2, 3 }},
    /* last operation */
    { 0, },
};

int main()
{
  int i;

  printf("sizeof(asm_instrs[]) = %d\n", sizeof(asm_instrs));


  for (i = 0; i < sizeof(asm_instrs)/sizeof(asm_instrs[0]); ++i) {
    printf("%d.: %d { %d %d %d }\n", i,
	asm_instrs[i].sym,
	asm_instrs[i].op_type[0],
	asm_instrs[i].op_type[1],
	asm_instrs[i].op_type[2]
    );
  }

  return 0;
}
