/**
 * Header for ia32 assembler declarations dumper.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_GEN_DECLS_H_
#define _IA32_GEN_DECLS_H_

#include "../be.h"

/**
 * Generate all entities.
 */
void ia32_gen_decls(FILE *out, const be_main_env_t *main_env);

#endif /* _IA32_GEN_DECLS_H_ */
