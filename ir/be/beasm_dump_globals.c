/**
 * dumper for initialized and uninitialized global data. based on cggg's implementation.
 * @author Hannes Jakschitsch
 * @date 19.01.2005
 */

#include "beasm_dump_globals.h"



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


/**
 * Determine if an entity is a string constant
 * @param ent The entity
 * @return 1 if it is a string constant, 0 otherwise
 */
static int ent_is_string_const(entity *ent)
{
  int res = 0;
  type *ty;

  ty = get_entity_type(ent);

  /* if it's an array */
  if (is_Array_type(ty)) {
    type *elm_ty = get_array_element_type(ty);

    /* and the array's alement type is primitive */
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


/*
 * dump an atomic value
 */
static void dump_atomic_value( assembler_t *assembler,
		asm_segment_t target_segment, ir_node *init)
{
  ir_mode *mode = get_irn_mode(init);
  int bytes     = get_mode_size_bytes(mode);
  tarval *tv;

  switch (get_irn_opcode(init)) {

  case iro_Cast:
    dump_atomic_value(assembler, target_segment, get_Cast_op(init));
    return;

  case iro_Conv:
    dump_atomic_value(assembler, target_segment, get_Conv_op(init));
    return;

  case iro_Const:
    tv = get_Const_tarval(init);

    /* beware of old stuff */
    assert(! mode_is_reference(mode));

    /* it's a arithmetic value */
    assembler->dump_arith_tarval(assembler->private_data, target_segment, tv, bytes);
    return;

  case iro_SymConst:
    assembler->dump_symconst(assembler->private_data, target_segment, init);
  /*
    switch (get_SymConst_kind(init)) {
    case symconst_addr_name:
	assembler->dump_addr_name(assembler->private_data, target_segment, get_id_str(get_SymConst_name(init)));
      break;

    case symconst_addr_ent:
        assembler->dump_addr_ent(assembler->private_data, target_segment, get_entity_ld_name(get_SymConst_entity(init)));
      break;

    case symconst_size:
      	assembler->dump_size(assembler->private_data, target_segment, get_type_size_bytes(get_SymConst_type(init)));
      break;

    default:
      assert(0 && "dump_atomic_init(): don't know how to init from this SymConst");
    } */
    return;

  case iro_Add:
    dump_atomic_value(assembler, target_segment, get_Add_left(init));
    assembler->dump_arith_op(assembler->private_data, target_segment, ASM_ARITH_OPERATION_ADD);
    dump_atomic_value(assembler, target_segment, get_Add_right(init));
    return;

  case iro_Sub:
    dump_atomic_value(assembler, target_segment, get_Sub_left(init));
    assembler->dump_arith_op(assembler, target_segment, ASM_ARITH_OPERATION_SUB);
    dump_atomic_value(assembler, target_segment, get_Sub_right(init));
    return;

  case iro_Mul:
    dump_atomic_value(assembler, target_segment, get_Mul_left(init));
    assembler->dump_arith_op(assembler, target_segment, ASM_ARITH_OPERATION_MUL);
    dump_atomic_value(assembler, target_segment, get_Mul_right(init));
    return;

  default:
    assert(0 && "dump_atomic_init(): unknown IR-node");
  }
}


/*
 * dump an atomic value
 */
static void dump_atomic_init(assembler_t* assembler,
	       asm_segment_t target_segment, ir_node *init)
{

  ir_mode *mode = get_irn_mode(init);
  int bytes     = get_mode_size_bytes(mode);

  assembler->dump_atomic_decl(assembler->private_data, target_segment, bytes);
  dump_atomic_value(assembler, target_segment, init);
  assembler->dump_newline(assembler->private_data, target_segment);
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
static void asm_dump_global ( assembler_t *assembler, entity *ent)
{
  type *ty            = get_entity_type(ent);
  const char *ld_name = get_entity_ld_name(ent);
  int align, is_constant, h;
  int i,j,size = 0;

  asm_segment_t target_segment = ASM_SEGMENT_DATA_INIT;

  /*
   * FIXME: did NOT work for partly constant values
   */

  /* ignore methods, they are emitted later */
  if(is_Method_type(ty))
	  return;

  /* get the properties of the entity */
  ent_variability variability = get_entity_variability(ent);
  ent_visibility  visibility  = get_entity_visibility(ent);

  if (variability == variability_constant) {
     /* a constant entity, put it into the const segment */
     target_segment = ASM_SEGMENT_CONST;
  }

  /* check, wether it is initialized, if yes create data */
  if (variability != variability_uninitialized ) {

/*  	if (visibility == visibility_external_visible) {
	      assembler->dump_external_declaration(assembler->private_data, target_segment, ld_name);
	//   obstack_printf(obst, ".globl\t%s\n", ld_name);
	}
  	align = get_type_alignment_bytes(ty);

    //  obstack_printf(obst, "\t.type\t%s,@object\n", ld_name);
    //  obstack_printf(obst, "\t.size\t%s,%d\n", ld_name, (get_type_size_bits(ty) + 7) >> 3);
  	assembler->dump_declare_object_symbol(assembler->private_data, target_segment, ld_name, (get_type_size_bits(ty) + 7) >> 3);

  	align = get_type_alignment_bytes(ty);
  	assembler->dump_align(assembler->private_data, target_segment, align );
	assembler->dump_object_symbol_init_decl(assembler->private_data, target_segment, ld_name);
*/
	align = get_type_alignment_bytes(ty);
	assembler->dump_declare_initialized_symbol ( assembler->private_data, target_segment, ld_name, (get_type_size_bits(ty)+7)>>3, align, visibility );




	/* dumps of the different entity type initializiations */

	/* atomic types */
	if (is_atomic_type(ty)) {
      		if (get_entity_visibility(ent) != visibility_external_allocated)
      			dump_atomic_init(assembler, target_segment, get_atomic_ent_value(ent));
  	}
  	else if(ent_is_string_const(ent)) {
       		assembler->dump_string(assembler->private_data, target_segment, ent);
  	}
	else if(is_Array_type(ty)) {

        	int filler;

        /* potential spare values should be already included! */
     		for (i = 0; i < get_compound_ent_n_values(ent); ++i) {
        		entity *step = get_compound_ent_value_member(ent, i);
            		type *stype  = get_entity_type(step);

            		if (get_type_mode(stype)) {

              			int align = (get_type_alignment_bits(stype) + 7) >> 3;
              			int n     = size % align;

              			if (n > 0) {
					assembler->dump_zero_padding(assembler->private_data, target_segment, align-n);
					size += align - n;
              			}
            		}
            		dump_atomic_init(assembler, target_segment, get_compound_ent_value(ent, i));
            		size += get_type_size_bytes(stype);
       		}

		filler = get_type_size_bytes(ty) - size;

		if (filler > 0)
			assembler->dump_zero_padding(assembler->private_data, target_segment, filler);
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
        	vals      = calloc(type_size, sizeof(*vals));

        	/* collect the values and store them at the offsets */
		for(i = 0; i < get_compound_ent_n_values(ent); ++i) {
		    int graph_length, aipos, offset, stepsize;
		    struct arr_info *ai;
		    int found                 = 0;
		    int all_n                 = 1;
		    entity *member            = get_compound_ent_value_member(ent, i);
		    compound_graph_path *path = get_compound_ent_value_path(ent, i);
		    entity *node              = get_compound_graph_path_node(path, 0);
		    type *node_type           = get_entity_type(node);

		    /* get the access path to the costant value */
		    graph_length = get_compound_graph_path_length(path);
		    ai = calloc(graph_length, sizeof(struct arr_info));

		    /* We wanna know how many arrays are on the path to the entity. We also have to know how
		     * many elements each array holds to calculate the offset for the entity. */
		    for (j = 0; j < graph_length; j++) {
		      entity *step    = get_compound_graph_path_node(path, j);
		      type *step_type = get_entity_type(step);
		      int ty_size     = (get_type_size_bits(step_type) + 7) >> 3;
		      int n           = 0;
		      int k;

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
		      entity *step    = get_compound_graph_path_node(path, j);
		      type *step_type = get_entity_type(step);
		      int ent_ofs     = get_entity_offset_bytes(step);
		      int stepsize    = 0;

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
		      dump_atomic_init(assembler, target_segment, vals[i]);
		      i += (get_mode_size_bytes(get_irn_mode(vals[i])));
		    }
		    else {
		      /* a gap */
		      //obstack_printf(obst, "\t.byte\t0\n");
		      assembler->dump_zero_padding(assembler->private_data, target_segment, 1);
		      ++i;
		    }
		  }
		  free(vals);
		}
		else
		  assert(0 && "unsupported type");

		assembler->dump_newline(assembler->private_data, target_segment);
	    }

	    /* uninitialized, but allocated here */
	    else {

	/*	if (visibility != visibility_external_allocated) {

		if (visibility == visibility_local) {
			assembler->dump_local_declaration(assembler->private_data, ASM_SEGMENT_COMMON , ld_name);
		} */

	      	/* calculate the alignment */

           target_segment = ASM_SEGMENT_DATA_UNINIT;

	      	align = get_type_alignment_bytes(ty);
	      	h = highest_bit(align);

		if ((1 << h) < align)
			++h;
		align = (1 << h);
		if (align < 1)
			align = 1;

		// declare the symbol.
		assembler->dump_declare_uninitialized_symbol(assembler->private_data,  target_segment , ld_name, (get_type_size_bits(ty)+7)>>3, align, visibility);
	    }
}




/*
 * dump the global symbols
 **/

void asm_dump_globals ( assembler_t *assembler ) {

  type *gt = get_glob_type();
  int i, n = get_class_n_members(gt);

  for (i = 0; i < n; i++)
    asm_dump_global( assembler, get_class_member(gt, i) );

}
