#ifndef BITWIDTH_H
#define BITWIDTH_H 1

#include <firm_types.h>
#include <stdbool.h>

typedef struct bitwidth {
   unsigned int stable_digits; // the amount of bits that is garanteed to be unused (starting from the MSB to the LSB)
   bool is_positive; // only meaningfull for a signed mode

   bool valid;
} bitwidth;

//helper function for getting the used bits
unsigned int bitwidth_used_bits(const ir_node *const node);

bitwidth* bitwidth_fetch_bitwidth(const ir_node *const node);

unsigned long bitwidth_upper_bound(const ir_node *const node);

void compute_bitwidth_for_si(ir_graph *irg);

void compute_bitwidth_info(ir_graph *irg);
void assure_bitwidth_info(ir_graph *irg);
void free_bitwidth_info(ir_graph *irg);
#endif
