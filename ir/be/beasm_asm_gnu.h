#ifndef BEASM_ASM_GNU_H
#define BEASM_ASM_GNU H

#include "beasm_dump_globals.h"

//#include <libfirm/adt/obst.h>

#include <obstack.h>
#include <xmalloc.h>

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

typedef struct _gnuasm_privdata_t {
	struct obstack common_obst;
	struct obstack data_obst;
	struct obstack rdata_obst;
	struct obstack code_obst;
} gnuasm_privdata_t;


assembler_t *gnuasm_create_assembler ( void );
void gnuasm_dump ( assembler_t *assembler, FILE* out );
void gnuasm_delete_assembler ( assembler_t *assembler );

#endif
