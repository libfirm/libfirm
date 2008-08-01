/*
 * Project:     libFIRM
 * File name:   testprograms/array-heap_example.c
 * Purpose:     Show representation of dynamically allocated array.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <string.h>
#include <stdio.h>

#include <libfirm/firm.h>

/**
*  variables of imperative programs.
*  It constructs the IR for the following program:
*
*
*  main(): int
*    int *a[10];
*
*    a = malloc(sizeof(a[10]));
*    return (a[3]);
*  end;
*
*  The array is placed on the heap.  The pointer to the array that
*  is a local variable is represented as a dataflow edge.
*  There are two ways to model allocation to the heap in programs with
*  explicit memory allocation:
*  1. Model the calls to malloc and free as simple procedure (of compiler
*     known procedures returning a pointer.  This is the simpler way of
*     generating FIRM, but restricts the information that can be deduced
*     for the call.
*  2. Insert an Alloc node.  A later pass can lower this to the compiler
*     known function.  This makes the allocation explicit in FIRM, supporting
*     optimization.
*     A problem is modeling free.  There is no free node in FIRM.  Is this
*     a necessary extension?
*  This example shows the second alternative, where the size of the array
*  is explicitly computed.
**/

#define OPTIMIZE_NODE 0

int
main(void)
{
  char *dump_file_suffix = "";

  /* describes the method main */
  ir_type     *owner;
  ir_type     *proc_main;
  ir_entity   *proc_main_e;

  /* describes types defined by the language */
  ir_type     *prim_t_int;

  /* describes the array and its fields. */
  ir_type     *array_type;   /* the ir_type information for the array */
  ir_entity   *array_ent;    /* the ir_entity representing a field of the array */

  /* Needed while finding the element size.  */
  ir_type     *elt_type;
  ir_mode  *elt_type_mode;
  int       size;
  ir_node  *arr_size;

  /* holds the graph and nodes. */
  ir_graph *main_irg;
  ir_node  *array, *array_ptr, *c3, *elt, *val, *x;

  init_firm (NULL);

  /* make basic ir_type information for primitive ir_type int.
     In Sather primitive types are represented by a class.
     This is the modeling appropriate for other languages.
     Mode_i says that all integers shall be implemented as a
     32 bit integer value.  */
  prim_t_int = new_type_primitive(new_id_from_chars ("int", 3), mode_Is);

  printf("\nCreating an IR graph: ARRAY-HEAP_EXAMPLE...\n");

  /* first build procedure main */
  owner = get_glob_type();
  proc_main = new_type_method(new_id_from_chars("ARRAY-HEAP_EXAMPLE_main", 23), 0, 1);
  set_method_res_type(proc_main, 0, (ir_type *)prim_t_int);
  proc_main_e = new_entity ((ir_type*)owner, new_id_from_chars("ARRAY-HEAP_EXAMPLE_main", 23), (ir_type *)proc_main);

  /* make ir_type information for the array and set the bounds */
# define N_DIMS 1
# define L_BOUND 0
# define U_BOUND 9
  current_ir_graph = get_const_code_irg();
  array_type = new_type_array(new_id_from_chars("a", 1), N_DIMS, prim_t_int);
  set_array_bounds(array_type, 0,
		   new_Const(mode_Iu, new_tarval_from_long (L_BOUND, mode_Iu)),
		   new_Const(mode_Iu, new_tarval_from_long (U_BOUND, mode_Iu)));
  /* As the array is accessed by Sel nodes, we need information about
     the ir_entity the node selects.  Entities of an array are it's elements
     which are, in this case, integers. */
  main_irg = new_ir_graph (proc_main_e, 4);
  array_ent = get_array_element_entity(array_type);

  /* Allocate the array. All program known variables that
     are not modeled by dataflow edges need an explicit allocate node.
     If the variable shall be placed on the stack, set stack_alloc.  */
  /*   first compute size in bytes. */
  elt_type = get_array_element_type(array_type);
  elt_type_mode = get_type_mode(elt_type);
  /*   better: read bounds out of array ir_type information */
  size = (U_BOUND - L_BOUND + 1) * get_mode_size_bytes(elt_type_mode);
  /*   make constant representing the size */
  arr_size  = new_Const(mode_Iu, new_tarval_from_long (size, mode_Iu));
  /*   allocate and generate the Proj nodes. */
  array     = new_Alloc(get_store(), arr_size, (ir_type*)array_type, stack_alloc);
  set_store(new_Proj(array, mode_M, pn_Alloc_M));   /* make the changed memory visible */
  array_ptr = new_Proj(array, mode_P, pn_Alloc_res);  /* remember the pointer to the array */

  /* Now the "real" program: */
  /* Load element 3 of the array. For this first generate the pointer to this
     array element by a select node.  (Alternative: increase array pointer
     by (three * elt_size), but this complicates some optimizations. The
     ir_type information accessible via the ir_entity allows to generate the
     pointer increment later. */
  c3 = new_Const (mode_Iu, new_tarval_from_long (3, mode_Iu));
  {
     ir_node *in[1];
     in[0] = c3;
     elt = new_Sel(get_store(), array_ptr, 1, in, array_ent);
  }
  val = new_Load(get_store(), elt, mode_Is);
  set_store(new_Proj(val, mode_M, pn_Load_M));
  val = new_Proj(val, mode_Is, pn_Load_res);

  /* return the result of procedure main */
  {
     ir_node *in[1];
     in[0] = val;

     x = new_Return (get_store (), 1, in);
  }
  mature_immBlock (get_irg_current_block(main_irg));

  /* complete the end_block */
  add_immBlock_pred (get_irg_end_block(main_irg), x);
  mature_immBlock (get_irg_end_block(main_irg));

  irg_finalize_cons (main_irg);

  printf("Optimizing ...\n");
  dead_node_elimination(main_irg);

  /* verify the graph */
  irg_vrfy(main_irg);

  printf("Dumping the graph and a ir_type graph.\n");
  dump_ir_block_graph (main_irg, dump_file_suffix);
  dump_type_graph(main_irg, dump_file_suffix);
  dump_all_types(dump_file_suffix);
  printf("Use ycomp to view these graphs:\n");
  printf("ycomp GRAPHNAME\n\n");

  return 0;
}
