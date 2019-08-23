#include <stdint.h>
/* The prototype of test cases. In contrast to the iCore Atom entity,
   a test_atom has only a single out signal to allow formulation of
   test cases in C. */
uint32_t __attribute__((special_instruction)) test_atom(uint8_t control, uint32_t input0, uint32_t input1);

typedef struct {
  uint8_t control;
  uint32_t input0;
  uint32_t input1;
} pattern;

extern pattern patterns[];
extern int npatterns;
