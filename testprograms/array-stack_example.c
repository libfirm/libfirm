 /* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** testprogram.
*/

# include "irdump.h"
# include "firm.h"

/**  This example describes representation of stack allocated variables of
***  imperative programs.
***  It constructs the IR for the following program:
***
***
***  main(): int
***    int a[10];
***
***    return (a[3]);
***  end;
***
***  The array is placed on the stack, i.e., a pointer to the array
***  is obtained by selecting the entity "a" from the stack.  The variables
***  on the stack are considered to be entities of the method, as locals
***  of a method are only visible within the method.  (An alternative to
***  make the method owner of the stack variables is to give the ownership
***  to the class representing the C-file.  This would extend the visibility
***  of the locals, though.)
**/


#define OPTIMIZE_NODE 0

int
main(void)
{
  /* describes the general structure of a C-file */
  type_class     *owner;        /* the class standing for everything in this file */
  type_method    *proc_main;    /* Typeinformation for method main. */
  entity         *proc_main_e;  /* The entity describing that method main is an
                                   entity of the fake class representing the file. */

  /* describes types defined by the language */
  type_primitive *prim_t_int;

  /* describes the array and its fields. */
  entity         *array_ent;    /* the entity representing the array as member
                                   of the stack/method */
  type_array     *array_type;   /* the type information for the array */
  entity         *field_ent;    /* the entity representing a field of the array */

  /* Needed while finding the element size.  */
  type_primitive *elt_type;
  ir_mode        *elt_type_mode;
  int            size;
  ir_node        *arr_size;

  /* holds the graph and nodes. */
  ir_graph       *main_irg;
  ir_node        *array, *array_ptr, *c3, *elt, *val, *x;


  init_firm ();

  printf("creating an IR graph: ARRAY-STACK_EXAMPLE...\n");

  /* make basic type information for primitive type int.
     In Sather primitive types are represented by a class.
     This is the modeling appropriate for other languages.
     Mode_i says that all language-integers shall be implemented
     as a 32 bit processor-integer value.  */
  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_i);

  /* build typeinformation of procedure main */
  owner = new_type_class (id_from_str ("ARRAY-STACK_EXAMPLE", 19));
  proc_main = new_type_method(id_from_str("main", 4), 0, 1);
  set_method_res_type(proc_main, 0, (type *)prim_t_int);
  proc_main_e = new_entity ((type*)owner, id_from_str ("main", 4), (type *)proc_main);

  main_irg = new_ir_graph (proc_main_e, 4);

  /* make type information for the array and set the bounds */
# define N_DIMS 1
# define L_BOUND 0
# define U_BOUND 9
  array_type = new_type_array(id_from_str("a", 1), N_DIMS);
  set_array_bounds(array_type, 1, L_BOUND, U_BOUND);
  set_array_element_type(array_type, (type*)prim_t_int);
  /* The array is an entity of the method, placed on the mehtod's own memory,
     the stack frame. */
  array_ent = new_entity((type *)proc_main, id_from_str("a", 1), (type *)array_type);
  /* As the array is accessed by Sel nodes, we need information about
     the entity the node select.  Entities of an array are it's elements
     which are, in this case, integers. */
  /* change entity owner types.   */
  field_ent = new_entity((type*)array_type, id_from_str("array_field", 11), (type*)prim_t_int);

  /* Now the "real" program: */
  /* Select the array from the stack frame.  */
  array_ptr = new_simpleSel(get_store(), main_irg->frame, array_ent);
  /* Load element 3 of the array. For this first generate the pointer
     to this the element by a select node.  (Alternative: increase
     array pointer by (three * elt_size), but this complicates some
     optimizations.) The type information accessible via the entity
     allows to generate the pointer increment later. */
  c3 = new_Const (mode_I, tarval_from_long (mode_I, 3));
  {
     ir_node *in[1];
     in[0] = c3;
     elt = new_Sel(get_store(), array_ptr, 1, in, field_ent);
  }
  val = new_Load(get_store(), elt);
  set_store(new_Proj(val, mode_M, 0));
  val = new_Proj(val, mode_i, 1);

  /* return the result of procedure main */
  {
     ir_node *in[1];
     in[0] = val;

     x = new_Return (get_store (), 1, in);
  }
  mature_block (main_irg->current_block);

  /* complete the end_block */
  add_in_edge (main_irg->end_block, x);
  mature_block (main_irg->end_block);

  /* verify the graph */
  irg_vrfy(main_irg);

  printf("\nDone building the graph.\n");
  printf("Dumping the graph and a type graph.\n");
  dump_ir_block_graph (main_irg);
  dump_type_graph(main_irg);

  printf("\nuse xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (1);
}
