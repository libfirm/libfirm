
#include <stdlib.h>
#include <string.h>

#include "obst.h"
#include "beasm_asm_gnu.h"

static struct obstack *get_obstack_for_segment ( gnuasm_privdata_t *privdata, asm_segment_t segment ) {

	switch(segment) {
		case ASM_SEGMENT_CONST:
			return &privdata->rdata_obst;
			break;
		case ASM_SEGMENT_DATA_INIT:
			return &privdata->data_obst;
			break;
		case ASM_SEGMENT_CODE:
			return &privdata->code_obst;
			break;
		case ASM_SEGMENT_DATA_UNINIT:
			return &privdata->common_obst;
			break;
		default:
			assert(0 && "unknown segment type");
			break;
	}
}


/*
 * the dumper callbacks
 **/

void gnuasm_dump_align( gnuasm_privdata_t *privdata, asm_segment_t segment, int align ) {
	struct obstack* obst = get_obstack_for_segment ( privdata, segment );
 	obstack_printf(obst, "\t.align %d\n", align);
}

void gnuasm_dump_arith_tarval ( gnuasm_privdata_t *privdata, asm_segment_t segment,
			tarval *tv, int bytes ) {

	struct obstack* obst = get_obstack_for_segment ( privdata, segment );

	switch (bytes) {

	case 1:
		obstack_printf(obst, "0x%02x", get_tarval_sub_bits(tv, 0));
		break;
	case 2:
		obstack_printf(obst, "0x%02x%02x", get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 4:
		obstack_printf(obst, "0x%02x%02x%02x%02x",
		get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 8:
		obstack_printf(obst, "0x%02x%02x%02x%02x%02x%02x%02x%02x",
get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6), get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),	get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
    		break;

	default:
		fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
		assert(0);
	}
}


void gnuasm_dump_atomic_decl ( gnuasm_privdata_t *privdata, asm_segment_t segment, int bytes ) {

	struct obstack* obst = get_obstack_for_segment ( privdata, segment );

	switch (bytes) {

	case 1:
		obstack_printf(obst, "\t.byte\t");
		break;

	case 2:
		obstack_printf(obst, "\t.value\t");
		break;

	case 4:
		obstack_printf(obst, "\t.long\t");
		break;

	case 8:
		obstack_printf(obst, "\t.quad\t");
		break;

	default:
		fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
		assert(0);
  	}

//	obstack_printf(obst, "\n");
}

void gnuasm_dump_string ( gnuasm_privdata_t *privdata, asm_segment_t segment, entity* ent ) {

  int i, n;

  struct obstack* obst = get_obstack_for_segment ( privdata, segment );

  obstack_printf(obst, "\t.string \"");
  n = get_compound_ent_n_values(ent);

  for (i = 0; i < n-1; ++i) {
    ir_node *irn;
    int c;

    irn = get_compound_ent_value(ent, i);
    c = (int) get_tarval_long(get_Const_tarval(irn));

    switch (c) {
    case '"' : obstack_printf(obst, "\\\""); break;
    case '\n': obstack_printf(obst, "\\n");  break;
    case '\r': obstack_printf(obst, "\\r");  break;
    case '\t': obstack_printf(obst, "\\t");  break;
    default:
      if (isprint(c))
	obstack_printf(obst, "%c", c);
      else
	obstack_printf(obst, "%O", c);
      break;
    }
  }
  obstack_printf(obst, "\"\n");

}


void gnuasm_dump_declare_initialized_symbol(gnuasm_privdata_t* priv_data, asm_segment_t segment, const char* ld_name, int bytes, int align, ent_visibility visibility) {

  // get the obstack for the given segment (const, data)
  struct obstack* obst = get_obstack_for_segment ( priv_data, segment );

  // if the symbol is externally visibile, declare it so.
  if (visibility == visibility_external_visible)
    obstack_printf(obst, ".globl\t%s\n", ld_name);

  obstack_printf(obst, "\t.type\t%s,@object\n", ld_name);
  obstack_printf(obst, "\t.size\t%s,%d\n", ld_name, bytes);
  obstack_printf(obst, "\t.align\t%d\n", align);
  obstack_printf(obst, "\t%s:\n", ld_name);
}

void gnuasm_dump_declare_uninitialized_symbol(gnuasm_privdata_t* priv_data, asm_segment_t segment, const char* ld_name, int bytes, int align, ent_visibility visibility) {

  // external symbols are not required to be declared in gnuasm.
  if(visibility == visibility_external_allocated) return;

  // declare local, uninitialized symbol to the uninit-obst
  obstack_printf(&priv_data->common_obst, "\t.comm\t%s,%d,%d\n", ld_name, bytes, align);
}


void gnuasm_dump_zero_padding ( gnuasm_privdata_t *privdata, asm_segment_t segment, int size ) {

	struct obstack* obst = get_obstack_for_segment ( privdata, segment );
	obstack_printf(obst, "\t.zero\t%d\n", size);
}

////////////////////////////////////////////////////////////////////////////

void gnuasm_dump_arith_op ( gnuasm_privdata_t *privdata, asm_segment_t segment, asm_arith_operation_t op ) {
	struct obstack* obst = get_obstack_for_segment ( privdata, segment );
	switch (op) {
		case ASM_ARITH_OPERATION_ADD:
			obstack_printf(obst, "+");
			break;
		case ASM_ARITH_OPERATION_SUB:
			obstack_printf(obst, "-");
			break;
		case ASM_ARITH_OPERATION_MUL:
			obstack_printf(obst, "*");
			break;
	}
	//obstack_printf(obst, "+");
}

void gnuasm_dump_symconst ( gnuasm_privdata_t *privdata, asm_segment_t segment, ir_node *init ) {

  struct obstack* obst = get_obstack_for_segment ( privdata, segment );
  switch (get_SymConst_kind(init)) {
    case symconst_addr_name:
        obstack_printf(obst, "%s", get_id_str(get_SymConst_name(init)));
      break;

    case symconst_addr_ent:
      	obstack_printf(obst, "%s", get_entity_ld_name(get_SymConst_entity(init)));
      break;

    case symconst_size:
        obstack_printf(obst, "%d", get_type_size_bytes(get_SymConst_type(init)));
      break;

    default:
      assert(0 && "dump_atomic_init(): don't know how to init from this SymConst");
    }
}

void gnuasm_dump_newline ( gnuasm_privdata_t *privdata, asm_segment_t segment ) {
	struct obstack* obst = get_obstack_for_segment ( privdata, segment );
	obstack_printf(obst, "\n");
}

//////////////////////////////////////////////////////////////////////////////

void gnuasm_dump_header ( gnuasm_privdata_t *privdata ) {

}

void gnuasm_dump_footer ( gnuasm_privdata_t *privdata ) {

}

void gnuasm_dump_segment_header ( gnuasm_privdata_t *privdata ) {

}

//////////////////////////////////////////////////////////////////////////////

assembler_t *gnuasm_create_assembler ( void ) {

	gnuasm_privdata_t *priv_data = malloc ( sizeof(gnuasm_privdata_t ));
	assembler_t *assembler = malloc ( sizeof( assembler_t ));
	memset(assembler, 0, sizeof( assembler_t ));
	assembler->private_data = priv_data;

	obstack_init (&priv_data->common_obst);
	obstack_init (&priv_data->data_obst);
	obstack_init (&priv_data->rdata_obst);
	obstack_init (&priv_data->code_obst);


  assembler->dump_declare_uninitialized_symbol = (dump_declare_uninitialized_symbol_proc) &gnuasm_dump_declare_uninitialized_symbol;
  assembler->dump_declare_initialized_symbol = (dump_declare_initialized_symbol_proc) &gnuasm_dump_declare_initialized_symbol;

//  assembler->dump_align = (dump_align_proc) &gnuasm_dump_align;
	assembler->dump_arith_tarval = (dump_arith_tarval_proc) &gnuasm_dump_arith_tarval;
	assembler->dump_atomic_decl = (dump_atomic_decl_proc) gnuasm_dump_atomic_decl;
	assembler->dump_string = (dump_string_proc) &gnuasm_dump_string;
 	assembler->dump_zero_padding = (dump_zero_padding_proc) &gnuasm_dump_zero_padding;
	assembler->dump_arith_op = (dump_arith_op_proc) &gnuasm_dump_arith_op;
	assembler->dump_symconst = (dump_symconst_proc) &gnuasm_dump_symconst;
	assembler->dump_newline = (dump_newline_proc) &gnuasm_dump_newline;

	assembler->dump_header = (dump_header_proc) &gnuasm_dump_header;
	assembler->dump_footer = (dump_footer_proc) &gnuasm_dump_footer;
	assembler->dump_segment_header = (dump_segment_header_proc) &gnuasm_dump_segment_header;


	return assembler;

}

static void gnuasm_dump_obst ( struct obstack* obst, FILE* out ) {

	obstack_grow0 (obst, NULL, 0);
	void *data = obstack_finish (obst);
	fprintf(out, "%s", data);
}

void gnuasm_dump ( assembler_t *assembler, FILE* out ) {

	gnuasm_privdata_t *privdata = assembler->private_data;

//	fprintf(out, "<COMMON>\n");
	gnuasm_dump_obst ( &privdata->common_obst, out);
	fprintf(out, ".data\n");
	gnuasm_dump_obst ( &privdata->data_obst, out);
	fprintf(out, ".section .rodata\n");
	gnuasm_dump_obst ( &privdata->rdata_obst, out);
	fprintf(out, ".text\n");
	gnuasm_dump_obst ( &privdata->code_obst, out);
	//////

}

void gnuasm_delete_assembler ( assembler_t *assembler ) {

	free ( assembler->private_data );
	free ( assembler );
}
