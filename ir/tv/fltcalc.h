#ifndef _FLTCALC_H_
#define _FLTCALC_H_

enum {
  FC_ADD,
  FC_SUB,
  FC_MUL,
  FC_DIV,
  FC_NEG,
};

#define fc_add(a, b) fc_calc((a), (b), FC_ADD)
#define fc_sub(a, b) fc_calc((a), (b), FC_SUB)
#define fc_mul(a, b) fc_calc((a), (b), FC_MUL)
#define fc_div(a, b) fc_calc((a), (b), FC_DIV)
#define fc_neg(a) fc_calc((a), NULL, FC_NEG)

const void *fc_get_buffer(void);
const int fc_get_buffer_length(void);

void fc_val_from_str(const char *str, unsigned int len);
void fc_val_from_float(long double l);
long double fc_val_to_float(const void *val);

void fc_get_min(unsigned int num_bits);
void fc_get_max(unsigned int num_bits);
void fc_get_nan(void);
void fc_get_inf(void);

void fc_calc(const void *a, const void *b, int opcode);
char *fc_print_dec(const void *a);
int fc_comp(const void *a, const void *b);

#endif /* _FLTCALC_H_ */
