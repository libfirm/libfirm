 /* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Goetz Lindenmaier
*
* testprogram.
*/

# include <string.h>
# include <stdio.h>

# include "irvrfy.h"
# include "irdump.h"
# include "firm.h"

/**
*  imperative programs.
*  It constructs the IR for the following program:
*
*
*  main(): int
*    int a[10];
*
*    return (a[3]);
*  end;
*
*  The array is placed on the stack, i.e., a pointer to the array
*  is obtained by selecting the entity "a" from the stack.  The variables
*  on the stack are considered to be entities of the method, as locals
*  of a method are only visible within the method.  (An alternative to
*  make the method owner of the stack variables is to give the ownership
*  to the class representing the C-file.  This would extend the visibility
*  of the locals, though.)
**/


#define OPTIMIZE_NODE 0

int
main(void)
{
  /* describes the general structure of a C-file */
  type           *owner;        /* the class standing for everything in this file */
  type           *proc_main;    /* Typeinformation for method main. */
  entity         *proc_main_e;  /* The entity describing that method main is an
                                   entity of the fake class representing the file. */

  /* describes types defined by the language */
  type           *prim_t_int;

  /* describes the array and its fields. */
  entity         *array_ent;    /* the entity representing the array as member
                                   of the stack/method */
  type           *array_type;   /* the type information for the array */
  entity         *field_ent;    /* the entity representing a field of the
				   array */

  /* holds the graph and nodes. */
  ir_graph       *main_irg;
  ir_node        *array_ptr, *c3, *elt, *val, *x;

  init_firm (NULL);

  printf("\nCreating an IR graph: ARRAY-STACK_EXAMPLE...\n");

  /* make basic type information for primitive type int.
     In Sather primitive types are represented by a class.
     This is the modeling appropriate for other languages.
     Mode_i says that all language-integers shall be implemented
     as a 32 bit processor-integer value.  */
  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_Is);

  /* build typeinformation of procedure main */
  owner = new_type_class (id_from_str ("ARRAY-STACK_EXAMPLE", 19));
  proc_main = new_type_method(id_from_str("main_tp", 4), 0, 1);
  set_method_res_type(proc_main, 0, prim_t_int);
  proc_main_e = new_entity (owner, id_from_str ("main", 4), proc_main);

  /* make type information for the array and set the bounds */
# define N_DIMS 1
# define L_BOUND 0
# define U_BOUND 9
  array_type = new_type_array(id_from_str("a_tp", 4), N_DIMS, prim_t_int);
  current_ir_graph = get_const_code_irg();
  set_array_bounds(array_type, 0,
		   new_Const(mode_Iu, new_tarval_from_long (L_BOUND, mode_Iu)),
		   new_Const(mode_Iu, new_tarval_from_long (U_BOUND, mode_Iu)));

  main_irg = new_ir_graph (proc_main_e, 4);

  /* The array is an entity of the method, placed on the mehtod's own memory,
     the stack frame. */
  array_ent = new_entity(get_cur_frame_type(), id_from_str("a", 1), array_type);
  /* As the array is accessed by Sel nodes, we need information about
     the entity the node selects.  Entities of an array are it's elements
     which are, in this case, integers. */
  /* change entity owner types.   */
  field_ent = get_array_element_entity(array_type);



  /* Now the "real" program: */
  /* Select the array from the stack frame.  */
  array_ptr = new_simpleSel(get_store(), get_irg_frame(main_irg), array_ent);
  /* Load element 3 of the array. For this first generate the pointer
     to this the element by a select node.  (Alternative: increase
     array pointer by (three * elt_size), but this complicates some
     optimizations.) The type information accessible via the entity
     allows to generate the pointer increment later. */
  c3 = new_Const (mode_Iu, new_tarval_from_long (3, mode_Iu));
  {
     ir_node *in[1];
     in[0] = c3;
     elt = new_Sel(get_store(), array_ptr, 1, in, field_ent);
  }
  val = new_Load(get_store(), elt);
  set_store(new_Proj(val, mode_M, 0));
  val = new_Proj(val, mode_Is, 2);

  /* return the result of procedure main */
  {
     ir_node *in[1];
     in[0] = val;

     x = new_Return (get_store (), 1, in);
  }
  mature_block (get_irg_current_block(main_irg));

  /* complete the end_block */
  add_in_edge (get_irg_end_block(main_irg), x);
  mature_block (get_irg_end_block(main_irg));

  finalize_cons (main_irg);

  printf("Optimizing ...\n");
  dead_node_elimination(main_irg);

  /* verify the graph */
  irg_vrfy(main_irg);

  printf("Dumping the graph and a type graph.\n");
  dump_ir_block_graph (main_irg);
  dump_type_graph(main_irg);
  dump_ir_block_graph_w_types(main_irg);
  dump_all_types();
  printf("Use xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
