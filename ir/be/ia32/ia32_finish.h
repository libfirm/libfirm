/**
 * Function prototypes for irg finishing.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_FINISH_H_
#define _IA32_FINISH_H_

/**
 * Check 2-Addesscode constraints and call peephole optimizations.
 * @param irg  The irg to finish
 * @param cg   The codegenerator object for the irg
 */
void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg);

#endif /* _IA32_FINISH_H_ */
