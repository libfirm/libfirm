
#ifdef BITSET_USE_STD

#include "bitfiddle.h"

typedef unsigned bitset_unit_t;

#define _bitset_inside_set(unit,bit) *(unit) |= (1 << (bit))
#define _bitset_inside_clear(unit,bit) *(unit) &= ~(1 << (bit))
#define _bitset_inside_flip(unit,bit) *(unit) ^= ~(1 << (bit))

#define _bitset_inside_nlz(unit) (nlz(*unit))
#define _bitset_inside_ntz(unit) (32 - nlz(~(*unit) & ((*unit) - 1)))
#define _bitset_inside_nto(unit) nlz(~(*unit))
#define _bitset_inside_nlo(unit) (32 - nlz((*unit) & (~(*unit) - 1)))

#define _bitset_inside_pop(unit) (popcnt(*unit))

#define _bitset_inside_and(tgt,src) (*(tgt) &= *(src))
#define _bitset_inside_andnot(tgt,src) (*(tgt) &= ~(*(src)))
#define _bitset_inside_or(tgt,src) (*(tgt) |= *(src))
#define _bitset_inside_xor(tgt,src) (*(tgt) ^= *(src))

#endif
