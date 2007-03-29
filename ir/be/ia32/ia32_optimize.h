/**
 * Function prototypes for ia32 optimizations
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_OPTIMIZE_H_
#define _IA32_OPTIMIZE_H_

/**
 * Prepares irg for codegeneration. Places consts and transform reference mode
 * nodes into mode_Iu nodes.
 * @param cg  The ia32 codegenerator object
 */
void ia32_pre_transform_phase(ia32_code_gen_t *cg);

/**
 * Performs address mode optimization.
 * @param cg  The ia32 codegenerator object
 */
void ia32_optimize_addressmode(ia32_code_gen_t *cg);

/**
 * Performs Peephole Optimizations
 */
void ia32_peephole_optimization(ir_graph *irg, ia32_code_gen_t *cg);

#endif /* _IA32_OPTIMIZE_H_ */
