/**
 * the 'generic' assemblercode emitter
 * @author Hannes Jakschitsch
 * @date 19.01.2005
 */

#ifndef _BEASM_TYPES_H
#define _BEASM_TYPES_H

#include <stdlib.h>
#include <string.h>
#include "libfirm/firm.h"


/**
 *	Definition of symbolic segments
 **/
typedef enum _asm_segment_t {
  ASM_SEGMENT_CONST  = 1,	 /* < segment to store constant symbols */
  ASM_SEGMENT_DATA_INIT,	 /* < segment to store initialized symbols */
  ASM_SEGMENT_CODE,  		 /* < segment to store code */
  ASM_SEGMENT_DATA_UNINIT,  	 /* < segment to store uninitialized symbols */
} asm_segment_t;

typedef enum _asm_arith_operation_t {

  ASM_ARITH_OPERATION_ADD = 1,
  ASM_ARITH_OPERATION_SUB,
  ASM_ARITH_OPERATION_MUL

} asm_arith_operation_t;




typedef struct _assembler_t assembler_t;

typedef	void (*dump_align_proc)(void* priv_data, asm_segment_t segment, int align);
typedef	void (*dump_arith_tarval_proc)(void* priv_data, asm_segment_t segment, tarval *tv, int bytes);
typedef	void (*dump_atomic_decl_proc)(void* priv_data, asm_segment_t segment, int bytes);
typedef	void (*dump_string_proc)(void* priv_data, asm_segment_t segment, entity *ent);
typedef	void (*dump_external_declaration_proc)(void* priv_data, asm_segment_t segment, const char* ld_name);
typedef	void (*dump_local_declaration_proc)(void* priv_data, asm_segment_t segment, const char* ld_name );
// declares an uninitialized symbol.
typedef	void (*dump_declare_uninitialized_symbol_proc)(void* priv_data, asm_segment_t segment, const char* ldname, int bytes, int align, ent_visibility visibility);
typedef void (*dump_declare_initialized_symbol_proc)(void* priv_data, asm_segment_t segment, const char* ldname, int bytes, int align, ent_visibility visibility);
//typedef	void (*dump_declare_object_symbol_proc)(void* priv_data, asm_segment_t segment, const char* ld_name, int bytes);
//typedef	void (*dump_declare_function_symbol_proc)(void* priv_data, asm_segment_t segment, const char* ld_name);
//typedef	void (*dump_object_symbol_init_decl_proc)(void* priv_data, asm_segment_t segment, const char* ld_name);
// emits a zero padding directive.
typedef	void (*dump_zero_padding_proc)(void* priv_data, asm_segment_t segment, int bytes);
typedef	void (*dump_arith_op_proc)(void* priv_data, asm_segment_t segment, asm_arith_operation_t op);
typedef void (*dump_symconst_proc)(void* priv_data, asm_segment_t segment, ir_node *init);
/*typedef	void (*dump_size_proc)(void* priv_data, asm_segment_t segment, int size);
typedef	void (*dump_addr_name_proc)(void* priv_data, asm_segment_t segment, const char* addr_name);
typedef	void (*dump_addr_ent_proc)(void* priv_data, asm_segment_t segment, const char* addr_ent); */
typedef	void (*dump_newline_proc)(void* priv_data, asm_segment_t segment);

///////////////////////////////////////////////////////////////////////////

typedef void (*dump_header_proc)(void* priv_data);
typedef void (*dump_footer_proc)(void* priv_data);
typedef void (*dump_segment_header_proc)(void* priv_data);

struct _assembler_t {

	///////////////////////////////////////////////////////////////////////////
  // declarators

  dump_declare_uninitialized_symbol_proc dump_declare_uninitialized_symbol;
  dump_declare_initialized_symbol_proc dump_declare_initialized_symbol;

	///////////////////////////////////////////////////////////////////////////
  // initializers


  // declare an atomic value of BYTES size.
  // e.g. .long or .byte
  // needed when emitting an initializer
	dump_atomic_decl_proc dump_atomic_decl;
  // dump a target value. needed when emitting an initializer
	dump_arith_tarval_proc dump_arith_tarval;
  // dump a string. needed when emitting an initializer. improves readability.
	dump_string_proc dump_string;
  // dump a zero padding.
	dump_zero_padding_proc dump_zero_padding;
  // dump an arithmetic operator. used when initializers are expressions
  // that cannot be evaluated and must be computed by the assembler.
	dump_arith_op_proc dump_arith_op;
  // dump a symconst
  dump_symconst_proc dump_symconst;

	///////////////////////////////////////////////////////////////////////////
  // formatters.
	dump_newline_proc dump_newline;
	dump_header_proc  dump_header;
	dump_footer_proc  dump_footer;
	dump_segment_header_proc dump_segment_header;

	void *private_data;
};



#endif
