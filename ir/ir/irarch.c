	/**
	 * @file irarch.c
	 * @date 28.9.2004
	 * @author Sebastian Hack
	 * @brief Machine dependent firm optimizations.
	 *
	 * $Id$
	 */

#include <stdlib.h>
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "tv.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "firmstat.h"
#include "ircons.h"
#include "irarch.h"
#include "firmstat.h"

#undef DEB

#define MAX_BITSTR 64

/** The params got from the factory in arch_dep_init(...). */
static const arch_dep_params_t *params = NULL;

/** The bit mask, which optimizations to apply. */
static arch_dep_opts_t opts;

void arch_dep_init(arch_dep_params_factory_t factory)
{
	opts = arch_dep_none;

	if(factory != NULL)
		params = factory();
}

void arch_dep_set_opts(arch_dep_opts_t the_opts) {
	opts = the_opts;
}

ir_node *arch_dep_replace_mul_with_shifts(ir_node *irn)
{
	ir_node *block = get_nodes_block(irn);
	ir_node *res = irn;
	ir_node *operand = NULL;
	ir_node *left, *right;
	ir_mode *mode = get_irn_mode(irn);
	tarval *tv = NULL;

	/* If the architecture dependent optimizations were not initialized
		 or this optimization was not enabled. */
	if(params == NULL || (opts & arch_dep_mul_to_shift) == 0)
		return irn;

	if(is_ir_node(irn)
			&& get_irn_opcode(irn) == iro_Mul
			&& mode_is_int(mode)) {

		left = get_binop_left(irn);
		right = get_binop_right(irn);

		/* Look, if one operand is a constant. */
		if(get_irn_opcode(left) == iro_Const) {
			tv = get_Const_tarval(left);
			operand = right;
		} else if(get_irn_opcode(right) == iro_Const) {
			tv = get_Const_tarval(right);
			operand = left;
		}

		if(tv != NULL) {
			int maximum_shifts = params->maximum_shifts;
			int also_use_subs = params->also_use_subs;
			int highest_shift_amount = params->highest_shift_amount;

			char *bitstr = get_tarval_bitpattern(tv);
			char *p;
			int i, last = 0;
			int counter = 0;
			int curr_bit;
			int compr_len = 0;
			char compr[MAX_BITSTR];

			int singleton;
			int end_of_group;
			int shift_with_sub[MAX_BITSTR] = { 0 };
			int shift_without_sub[MAX_BITSTR] = { 0 };
			int shift_with_sub_pos = 0;
			int shift_without_sub_pos = 0;

#if DEB
			{
				int val = (int) get_tarval_long(tv);
				fprintf(stderr, "Found mul with %d(%x) = ", val, val);
				for(p = bitstr; *p != '\0'; p++)
					printf("%c", *p);
				printf("\n");
			}
#endif

			for(p = bitstr; *p != '\0'; p++) {
				int bit = *p != '0';

				switch(bit - last) {
					case -1:	  // The last was 1 we are now at 0
					case 1: 	  // The last was 0 and we are now at 1
						compr[compr_len++] = counter;
						counter = 1;
						break;
					default:
						counter++;
				}

				last = bit;
			}
			compr[compr_len++] = counter;


#ifdef DEF
			{
				const char *prefix = "";
				for(i = 0; i < compr_len; i++, prefix = ",")
					fprintf(stderr, "%s%d", prefix, compr[i]);
				fprintf("\n");
			}
#endif

			// Go over all recorded one groups.
			curr_bit = compr[0];

			for(i = 1; i < compr_len; i = end_of_group + 2) {
				int j, zeros_in_group, ones_in_group;

				ones_in_group = compr[i];
				zeros_in_group = 0;

				// Scan for singular 0s in a sequence
				for(j = i + 1; j < compr_len && compr[j] == 1; j += 2) {
					zeros_in_group += 1;
					ones_in_group += (j + 1 < compr_len ? compr[j + 1] : 0);
				}
				end_of_group = j - 1;

				if(zeros_in_group >= ones_in_group - 1)
					end_of_group = i;

#ifdef DEB
				fprintf(stderr, "  i:%d, eg:%d\n", i, end_of_group);
#endif

				singleton = compr[i] == 1 && i == end_of_group;
				for(j = i; j <= end_of_group; j += 2) {
					int curr_ones = compr[j];
					int biased_curr_bit = curr_bit + 1;
					int k;

#ifdef DEB
					fprintf(stderr, "    j:%d, ones:%d\n", j, curr_ones);
#endif

					// If this ones group is a singleton group (it has no
					// singleton zeros inside
					if(singleton)
						shift_with_sub[shift_with_sub_pos++] = biased_curr_bit;
					else if(j == i)
						shift_with_sub[shift_with_sub_pos++] = -biased_curr_bit;

					for(k = 0; k < curr_ones; k++)
						shift_without_sub[shift_without_sub_pos++] = biased_curr_bit + k;

					curr_bit += curr_ones;
					biased_curr_bit = curr_bit + 1;

					if(!singleton && j == end_of_group)
						shift_with_sub[shift_with_sub_pos++] = biased_curr_bit;
					else if(j != end_of_group)
						shift_with_sub[shift_with_sub_pos++] = -biased_curr_bit;

					curr_bit += compr[j + 1];
				}

			}

			{
				int *shifts = shift_with_sub;
				int n = shift_with_sub_pos;
				int highest_shift_wide = 0;
				int highest_shift_seq = 0;
				int last_shift = 0;

				/* If we may not use subs, or we can achive the same with adds,
					 prefer adds. */
				if(!also_use_subs || shift_with_sub_pos >= shift_without_sub_pos) {
					shifts = shift_without_sub;
					n = shift_without_sub_pos;
				}

				/* If the number of needed shifts exceeds the given maximum,
					 use the Mul and exit. */
				if(n > maximum_shifts) {
#ifdef DEB
					fprintf(stderr, "Only allowed %d shifts, but %d are needed\n",
							maximum_shifts, n);
#endif
					return irn;
				}

				/* Compute the highest shift needed for both, the
					 sequential and wide representations. */
				for(i = 0; i < n; i++) {
					int curr = abs(shifts[i]);
					int curr_seq = curr - last;

					highest_shift_wide = curr > highest_shift_wide ? curr
						: highest_shift_wide;
					highest_shift_seq = curr_seq > highest_shift_seq ? curr_seq
						: highest_shift_seq;

					last_shift = curr;
				}

				/* If the highest shift amount is greater than the given limit,
					 give back the Mul */
				if(highest_shift_seq > highest_shift_amount) {
#ifdef DEB
					fprintf(stderr, "Shift argument %d exceeds maximum %d\n",
							highest_shift_seq, highest_shift_amount);
#endif
					return irn;
				}

				/* If we have subs, we cannot do sequential. */
				if(1 /* also_use_subs */) {
					if(n > 0) {
						ir_node *curr = NULL;

						i = n - 1;

						do {
							int curr_shift = shifts[i];
							int sub = curr_shift < 0;
							int amount = abs(curr_shift) - 1;
							ir_node *aux = operand;


							assert(amount >= 0 && "What is a negative shift??");

							if(amount != 0) {
								tarval *shift_amount = new_tarval_from_long(amount, mode_Iu);
								ir_node *cnst = new_r_Const(current_ir_graph, block, mode_Iu, shift_amount);
								aux = new_r_Shl(current_ir_graph, block, operand, cnst, mode);
							}

							if(curr) {
								if(sub)
									curr = new_r_Sub(current_ir_graph, block, curr, aux, mode);
								else
									curr = new_r_Add(current_ir_graph, block, curr, aux, mode);
							} else
								curr = aux;

						} while(--i >= 0);

						res = curr;
					}
				}

#ifdef DEB
				{
					const char *prefix = "";
					for(i = 0; i < n; i++) {
						fprintf(stderr, "%s%d", prefix, shifts[i]);
						prefix = ", ";
					}
					fprintf(stderr, "\n");
				}
#endif

			}

			if(bitstr)
				free(bitstr);
		}

	}

	if (res != irn)
	  stat_arch_dep_replace_mul_with_shifts(irn);

	return res;
}


static const arch_dep_params_t default_params = {
	1, /* also use subs */
	4, /* maximum shifts */
	31 /* maximum shift amount */
};

const arch_dep_params_t *arch_dep_default_factory(void) {
	return &default_params;
}
