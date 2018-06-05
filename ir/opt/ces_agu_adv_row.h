#include "ces_si_tools.h"
#include "ces_agu_emulator.h" //road to include hell
#include "irnode.h"

/* public declarations */
void adv_row_init(struct agu_params* params, struct load_base* current_base);
void adv_row_advance_addr(struct agu_params* params);
int adv_row_match_memop(struct load_base* current_base, struct load_base* old_base, struct agu_params* params);
int adv_row_adjust_params(struct agu_params* params, struct load_base* current_base, struct load_base* old_base);
int adv_row_adjust_skip(struct agu_params* params, struct load_base* current_base, struct load_base* old_base);
int adv_row_adjust_stride(struct agu_params* params, struct load_base* current_base, struct load_base* old_base);
