/**
 * @file irarch.h
 * @date 1.10.2004
 * @author Sebastian Hack
 * @brief Some machine dependent optimizations.
 *
 * $Id$
 */

#ifndef _FIRM_IRARCH_H
#define _FIRM_IRARCH_H

#include "irnode.h"

/**
 * A parameter structure that drives the machine dependent Firm
 * optimizations.
 */
typedef struct {
  /* Mul optimization */
  int also_use_subs : 1;                /**< Use also Subs when resolving Muls to shifts */
  int maximum_shifts;                   /**< The maximum number of shifts that shall be inserted for a mul. */
  unsigned highest_shift_amount;        /**< The highest shift amount you want to
			                     tolerate. Muls which would require a higher
			                     shift constant are left. */

  /* Div/Mod optimization */
  int allow_mulhs   : 1;        /**< Use the Mulhs operation for division by constant */
  int allow_mulhu   : 1;        /**< Use the Mulhu operation for division by constant */
  int max_bits_for_mulh;        /**< Maximum number of bits the Mulh operation can take.
                                     Modes with higher amount of bits will use Mulh */
} arch_dep_params_t;

/**
 * A factory function, that provides architecture parameters for
 * machine dependent optimizations.
 */
typedef const arch_dep_params_t *(*arch_dep_params_factory_t)(void);

/**
 * A default parameter factory for testing purposes.
 */
const arch_dep_params_t *arch_dep_default_factory(void);

/**
 * Optimization flags.
 */
typedef enum {
  arch_dep_none         = 0,
  arch_dep_mul_to_shift = 1,	/**< optimize Mul into Shift/Add/Sub */
  arch_dep_div_by_const = 2,	/**< optimize Div into Shift/Add/Mulh */
  arch_dep_mod_by_const = 4	    /**< optimize Mod into Shift/Add/Mulh */
} arch_dep_opts_t;

/**
 * Initialize the machine dependent optimizations.
 * @param factory   A factory that delivers parameters for these
 *                  optimizations. If NULL is passed, or this method
 *                  is not called, the machine dependent optimizations
 *                  are not enabled at all.
 */
void arch_dep_init(arch_dep_params_factory_t factory);

/**
 * Set the optimizations that shall be applied.
 * @param opts An optimization bit mask.
 */
void arch_dep_set_opts(arch_dep_opts_t opts);

/**
 * Replace Muls with Shifts and Add/Subs.
 * This function is driven by the 3 parameters:
 * - also_use_subs
 * - maximum_shifts
 * - highest_shift_amount
 *
 * If irn is a Mul with a Const, the constant is inspected if it meets the
 * requirements of the three variables stated above. If a Shl/Add/Sub
 * sequence can be generated that meets these requirements, this expression
 * is returned. In each other case irn is returned unmodified.
 *
 * @param irn       The Firm node to inspect.
 * @return          A replacement expression for irn.
 */
ir_node *arch_dep_replace_mul_with_shifts(ir_node *irn);

/**
 * Replace Divs with Shifts and Add/Subs and Mulh.
 * This function is driven by the 3 parameters:
 * - allow_mulhu
 * - allow_mulhs
 * - max_bits_for_mulh
 *
 * If irn is a Div with a Const, the constant is inspected if it meets the
 * requirements of the variables stated above. If a Shl/Add/Sub/Mulh
 * sequence can be generated that meets these requirements, this expression
 * is returned. In each other case irn is returned unmodified.
 *
 * @param irn       The Firm node to inspect.
 * @return          A replacement expression for irn.
 */
ir_node *arch_dep_replace_div_by_const(ir_node *irn);

/**
 * Replace Mods with Shifts and Add/Subs and Mulh.
 * This function is driven by the 3 parameters:
 * - allow_mulhu
 * - allow_mulhs
 * - max_bits_for_mulh
 *
 * If irn is a Mod with a Const, the constant is inspected if it meets the
 * requirements of the variables stated above. If a Shl/Add/Sub/Mulh
 * sequence can be generated that meets these requirements, this expression
 * is returned. In each other case irn is returned unmodified.
 *
 * @param irn       The Firm node to inspect.
 * @return          A replacement expression for irn.
 */
ir_node *arch_dep_replace_mod_by_const(ir_node *irn);

/**
 * Replace DivMods with Shifts and Add/Subs and Mulh.
 * This function is driven by the 3 parameters:
 * - allow_mulhu
 * - allow_mulhs
 * - max_bits_for_mulh
 *
 * If irn is a DivMod with a Const, the constant is inspected if it meets the
 * requirements of the variables stated above. If a Shl/Add/Sub/Mulh
 * sequence can be generated that meets these requirements, this expression
 * is returned. In each other case irn is returned unmodified.
 *
 * @param div       After call contains the Firm node div result or NULL.
 * @param mod       After call contains the Firm node mod result or NULL.
 * @param irn       The Firm node to inspect.
 */
void arch_dep_replace_divmod_by_const(ir_node **div, ir_node **mod, ir_node *irn);

#endif /* _FIRM_IRARCH_H */
