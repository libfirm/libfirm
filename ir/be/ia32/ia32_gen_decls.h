/**
 * Header for ia32 assembler declarations dumper.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_GEN_DECLS_H_
#define _IA32_GEN_DECLS_H_

#include "bearch_ia32_t.h"

/**
 * Generate all entities.
 */
void ia32_gen_decls(FILE *out, ia32_code_gen_t *cg);

#endif /* _IA32_GEN_DECLS_H_ */
