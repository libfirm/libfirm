/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Dumps global variables and constants as ppc assembler.
 * @author  Moritz Kroll, Jens Mueller
 * @date    14.02.2006
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "xmalloc.h"
#include "obst.h"

#include "tv.h"
#include "irnode.h"
#include "entity.h"
#include "irprog.h"
#include "pset.h"

#include "ppc32_gen_decls.h"

/* Matze: TODO what the heck is this? changing it to non-extern for now so that firm compiles again... */
pset *symbol_pset;

/************************************************************************/

/*
 * returns the highest bit value
 */
static unsigned highest_bit(unsigned v)
{
  int res = -1;

  if (v >= (1U << 16U)) {
    res += 16;
    v >>= 16;
  }
  if (v >= (1U << 8U)) {
    res += 8;
    v >>= 8;
  }
  if (v >= (1U << 4U)) {
    res += 4;
    v >>= 4;
  }
  if (v >= (1U << 2U)) {
    res += 2;
    v >>= 2;
  }
  if (v >= (1U << 1U)) {
    res += 1;
    v >>= 1;
  }
  if (v >= 1)
    res += 1;

  return res;
}

/*
 * output the alignment
 */
static void ppc32_dump_align(struct obstack *obst, int align)
{
  int h = highest_bit(align);

  if ((1 << h) < align)
    ++h;
  align = (1 << h);

  if (align > 1)
    obstack_printf(obst, "\t.align %d\n", align);
}

static void dump_arith_tarval(struct obstack *obst, tarval *tv, int bytes)
{
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
    get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6), get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),
    get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
    break;

  case 10:
  case 12:
    break;

  default:
    fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
    assert(0);
  }
}

#if 0
/*
 * dump an arithmetic tarval
 */
static void ppc32_dump_arith_tarval(struct obstack *obst, tarval *tv, int bytes)
{
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

  case 10:
  case 12:
    break;

  default:
    fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
    assert(0);
  }
  dump_arith_tarval(obst, tv, bytes);
}
#endif

/*
 * dump an atomic value
 */
static void do_dump_atomic_init(struct obstack *obst, ir_node *init)
{
  ir_mode *mode = get_irn_mode(init);
  int bytes     = get_mode_size_bytes(mode);
  tarval *tv;

  switch (get_irn_opcode(init)) {

  case iro_Cast:
    do_dump_atomic_init(obst, get_Cast_op(init));
    return;

  case iro_Conv:
    do_dump_atomic_init(obst, get_Conv_op(init));
    return;

  case iro_Const:
    tv = get_Const_tarval(init);

    /* beware of old stuff */
    assert(! mode_is_reference(mode));

    /* it's a arithmetic value */
    dump_arith_tarval(obst, tv, bytes);
    return;

  case iro_SymConst:
    switch (get_SymConst_kind(init)) {
    case symconst_addr_name:
      obstack_printf(obst, "%s", get_id_str(get_SymConst_name(init)));
      break;

    case symconst_addr_ent:
      obstack_printf(obst, "%s", get_entity_ld_name(get_SymConst_entity(init)));
      break;

    case symconst_ofs_ent:
      obstack_printf(obst, "%d", get_entity_offset(get_SymConst_entity(init)));
      break;

    case symconst_type_size:
      obstack_printf(obst, "%d", get_type_size_bytes(get_SymConst_type(init)));
      break;

    case symconst_type_align:
      obstack_printf(obst, "%d", get_type_alignment_bytes(get_SymConst_type(init)));
      break;

    case symconst_enum_const:
      tv = get_enumeration_value(get_SymConst_enum(init));
      dump_arith_tarval(obst, tv, bytes);
      break;

    default:
      assert(!"dump_atomic_init(): don't know how to init from this SymConst");
    }
    return;

  case iro_Add:
    do_dump_atomic_init(obst, get_Add_left(init));
    obstack_printf(obst, " + ");
    do_dump_atomic_init(obst, get_Add_right(init));
    return;

  case iro_Sub:
    do_dump_atomic_init(obst, get_Sub_left(init));
    obstack_printf(obst, " - ");
    do_dump_atomic_init(obst, get_Sub_right(init));
    return;

  case iro_Mul:
    do_dump_atomic_init(obst, get_Mul_left(init));
    obstack_printf(obst, " * ");
    do_dump_atomic_init(obst, get_Mul_right(init));
    return;

  default:
    assert(0 && "dump_atomic_init(): unknown IR-node");
  }
}

/*
 * dump an atomic value
 */
static void dump_atomic_init(struct obstack *obst, ir_node *init)
{
  ir_mode *mode = get_irn_mode(init);
  int bytes     = get_mode_size_bytes(mode);

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

  case 10:
  case 12:
    /* handled in arith */
    break;

  default:
    fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
    assert(0);
  }

  do_dump_atomic_init(obst, init);
  obstack_printf(obst, "\n");
}

/************************************************************************/
/* Routines to dump global variables                                    */
/************************************************************************/

/**
 * Determine if an entity is a string constant
 * @param ent The entity
 * @return 1 if it is a string constant, 0 otherwise
 */
static int ent_is_string_const(ir_entity *ent)
{
  int res = 0;
  ir_type *ty;

  ty = get_entity_type(ent);

  /* if it's an array */
  if (is_Array_type(ty)) {
    ir_type *elm_ty = get_array_element_type(ty);

    /* and the array's element type is primitive */
    if (is_Primitive_type(elm_ty)) {
      ir_mode *mode = get_type_mode(elm_ty);

      /*
       * and the mode of the element type is an int of
       * the same size as the byte mode
       */
      if (mode_is_int(mode)
	 && get_mode_size_bits(mode) == get_mode_size_bits(mode_Bs))
      {
	int i, c, n;

	n = get_compound_ent_n_values(ent);
	for (i = 0; i < n; ++i) {
	  ir_node *irn = get_compound_ent_value(ent, i);
	  if(get_irn_opcode(irn) != iro_Const)
	    return 0;

	  c = (int) get_tarval_long(get_Const_tarval(irn));

	  if((i < n - 1 && !(isgraph(c) || isspace(c)))
	     || (i == n - 1 && c != '\0'))
	    return 0;
	}

	res = 1;
      }
    }
  }

  return res;
}

/**
 * Dump a atring constant.
 * No checks are made!!
 * @param obst The obst to dump on.
 * @param ent The entity to dump.
 */
static void dump_string_cst(struct obstack *obst, ir_entity *ent)
{
  int i, n;

  obstack_printf(obst, "\t.asciz \"");
  n = get_compound_ent_n_values(ent);

  for (i = 0; i < n-1; ++i) {
    ir_node *irn;
    int c;

    irn = get_compound_ent_value(ent, i);
    c = (int) get_tarval_long(get_Const_tarval(irn));

    switch (c) {
    case '"' : obstack_printf(obst, "\\\""); break;
    case '\n': obstack_printf(obst, "\\n"); break;
    case '\r': obstack_printf(obst, "\\r"); break;
    case '\t': obstack_printf(obst, "\\t"); break;
    default  :
      if (isprint(c))
        obstack_printf(obst, "%c", c);
      else
        obstack_printf(obst, "\\%o", c);
      break;
    }
  }
  obstack_printf(obst, "\"\n");
}

struct arr_info {
  int n_elems;
  int visit_cnt;
  int size;
};

/*
 * Dumps the initialization of global variables that are not
 * "uninitialized".
 */
static void dump_global(struct obstack *rdata_obstack, struct obstack *data_obstack, struct obstack *comm_obstack, ir_entity *ent)
{
  ir_type *ty         = get_entity_type(ent);
  const char *ld_name = get_entity_ld_name(ent);
  int align; //, h;
  struct obstack *obst = data_obstack;

  /*
   * FIXME: did NOT work for partly constant values
   */
  if (! is_Method_type(ty)) {
    ir_variability variability = get_entity_variability(ent);
    ir_visibility visibility = get_entity_visibility(ent);

    if (variability == variability_constant) {
      /* a constant entity, put it on the rdata */
      obst = rdata_obstack;
    }

    /* check, whether it is initialized, if yes create data */
    if (variability != variability_uninitialized) {
      if (visibility == visibility_external_visible) {
        obstack_printf(obst, ".globl\t%s\n", ld_name);
      }
//      obstack_printf(obst, "\t.type\t%s,@object\n", ld_name);
//      obstack_printf(obst, "\t.size\t%s,%d\n", ld_name, (get_type_size_bits(ty) + 7) >> 3);

      align = get_type_alignment_bytes(ty);
      ppc32_dump_align(obst, align);

      obstack_printf(obst, "%s:\n", ld_name);

      if (is_atomic_type(ty)) {
	if (get_entity_visibility(ent) != visibility_external_allocated)
          dump_atomic_init(obst, get_atomic_ent_value(ent));
      }
      else {
      	int i, size = 0;

      	if (ent_is_string_const(ent)) {
      	  dump_string_cst(obst, ent);
      	}
      	else if (is_Array_type(ty)) {
          int filler;

          /* potential spare values should be already included! */
       	  for (i = 0; i < get_compound_ent_n_values(ent); ++i) {
            ir_entity *step = get_compound_ent_value_member(ent, i);
            ir_type *stype = get_entity_type(step);

            if (get_type_mode(stype)) {
              int align = (get_type_alignment_bits(stype) + 7) >> 3;
              int n     = size % align;

              if (n > 0) {
                obstack_printf(obst, "\t.zero\t%d\n", align - n);
                size += align - n;
              }
            }
            dump_atomic_init(obst, get_compound_ent_value(ent, i));
            size += get_type_size_bytes(stype);
    	  }
          filler = get_type_size_bytes(ty) - size;

          if (filler > 0)
            obstack_printf(obst, "\t.zero\t%d\n", filler);
        }
        else if (is_compound_type(ty)) {
          ir_node **vals;
          int type_size, j;

          /* Compound entities are NOT sorted.
           * The sorting strategy used doesn't work for `value' compound fields nor
           * for partially_constant entities.
           */

          /*
           * in the worst case, every entity allocates one byte, so the type
           * size should be equal or bigger the number of fields
           */
          type_size = get_type_size_bytes(ty);
          vals      = xcalloc(type_size, sizeof(*vals));

          /* collect the values and store them at the offsets */
          for(i = 0; i < get_compound_ent_n_values(ent); ++i) {
            int                 graph_length, aipos, offset;
            struct arr_info     *ai;
            int                 all_n = 1;
            compound_graph_path *path = get_compound_ent_value_path(ent, i);

            /* get the access path to the costant value */
            graph_length = get_compound_graph_path_length(path);
            ai = xcalloc(graph_length, sizeof(struct arr_info));

            /* We wanna know how many arrays are on the path to the entity. We also have to know how
             * many elements each array holds to calculate the offset for the entity. */
            for (j = 0; j < graph_length; j++) {
              ir_entity *step      = get_compound_graph_path_node(path, j);
              ir_type   *step_type = get_entity_type(step);
              int       ty_size    = (get_type_size_bits(step_type) + 7) >> 3;
              int       k, n       = 0;

              if (is_Array_type(step_type))
                for (k = 0; k < get_array_n_dimensions(step_type); k++)
                  n += get_tarval_long(get_Const_tarval(get_array_upper_bound(step_type, k)));
              if (n) all_n *= n;
              ai[j].n_elems = n ? all_n + 1 : 0;
              ai[j].visit_cnt = 0;
              ai[j].size = ty_size;
            }

            aipos = graph_length - 1;
            if (aipos) aipos--;

            for (offset = j = 0; j < graph_length; j++) {
              ir_entity *step    = get_compound_graph_path_node(path, j);
              ir_type *step_type = get_entity_type(step);
              int ent_ofs        = get_entity_offset(step);
              int stepsize       = 0;

              /* add all positive offsets (= offsets in structs) */
              if (ent_ofs >= 0) offset += ent_ofs;

              if (j == graph_length - 1) {
                stepsize = (get_type_size_bits(step_type) + 7) >> 3;

                /* Search the next free position in vals depending on the information from above (ai). */
                while (vals[offset]) {
                  if (ai[aipos].visit_cnt < ai[aipos].n_elems) {
                    offset += stepsize;
                    ai[aipos].visit_cnt++;
                  }
                  else
                    while (aipos >= 0 && ai[aipos].visit_cnt == ai[aipos].n_elems) {
                      stepsize = ai[aipos--].size;
                      offset  += stepsize;
                  }
                }

                assert(aipos >= 0 && "couldn't store entity");
                vals[offset] = get_compound_ent_value(ent, i);
              }
            }

            free(ai);
          }

          /* now write them sorted */
          for(i = 0; i < type_size; ) {
            if (vals[i]) {
              dump_atomic_init(obst, vals[i]);
              i += (get_mode_size_bytes(get_irn_mode(vals[i])));
            }
            else {
              /* a gap */
              obstack_printf(obst, "\t.byte\t0\n");
              ++i;
            }
          }
          free(vals);
        }
        else {
          assert(0 && "unsupported type");
        }
      }
      obstack_printf(obst, "\n");
    }
    else if (visibility != visibility_external_allocated) {
      if (visibility == visibility_local) {
        obstack_printf(comm_obstack, "\t.local\t%s\n", ld_name);
      }

      /* calculate the alignment */
/*      align = get_type_alignment_bytes(ty);
      h = highest_bit(align);

      if ((1 << h) < align)
      	++h;
      align = (1 << h);

      if (align < 1)
      	align = 1;

      obstack_printf(comm_obstack, "\t.comm\t%s,%d,%d\n", ld_name, (get_type_size_bits(ty) + 7) >> 3, align);*/
      obstack_printf(comm_obstack, "\t.comm\t%s,%d\n", ld_name, (get_type_size_bits(ty) + 7) >> 3);
    }
  }
}

/*
 * Dumps declarations of global variables and the initialization code.
 */
void ppc32_dump_globals(struct obstack *rdata_obstack, struct obstack *data_obstack, struct obstack *comm_obstack)
{
  ir_type *gt = get_glob_type();
  int i, n = get_class_n_members(gt);

  for (i = 0; i < n; i++)
    dump_global(rdata_obstack, data_obstack, comm_obstack, get_class_member(gt, i));
}

void ppc32_dump_indirect_symbols(struct obstack *isyms)
{
  ir_entity *ent;

  foreach_pset(symbol_pset, ent) {
    const char *ld_name = get_entity_ld_name(ent);
    obstack_printf(isyms, ".non_lazy_symbol_pointer\n%s:\n\t.indirect_symbol _%s\n\t.long 0\n\n",ld_name,ld_name);
  }
}

/************************************************************************/

void ppc32_gen_decls(FILE *out) {
  struct obstack rodata, data, comm, isyms;
  int    size;
  char   *cp;

  obstack_init(&rodata);
  obstack_init(&data);
  obstack_init(&comm);

  ppc32_dump_globals(&rodata, &data, &comm);

  size = obstack_object_size(&data);
  cp   = obstack_finish(&data);
  if (size > 0) {
    fprintf(out, "\t.data\n");
    fwrite(cp, 1, size, out);
  }

  size = obstack_object_size(&rodata);
  cp   = obstack_finish(&rodata);
  if (size > 0) {
    fprintf(out, "\t.const_data\n");
    fwrite(cp, 1, size, out);
  }

  size = obstack_object_size(&comm);
  cp   = obstack_finish(&comm);
  if (size > 0) {
//    fprintf(out, "\t.common\n");
    fwrite(cp, 1, size, out);
    fprintf(out, "\n");
  }

  obstack_free(&rodata, NULL);
  obstack_free(&data, NULL);
  obstack_free(&comm, NULL);

  obstack_init(&isyms);

  ppc32_dump_indirect_symbols(&isyms);

  size = obstack_object_size(&isyms);
  cp   = obstack_finish(&isyms);
  if (size > 0) {
	  fprintf(out, "\t.data\n");
	  fwrite(cp, 1, size, out);
  }

  obstack_free(&isyms,NULL);
}
