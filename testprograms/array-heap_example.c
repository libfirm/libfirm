 /* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** testprogram.
*/

# include "irdump.h"
# include "firm.h"

/**  This example describes a possible representation of heap allocated
***  variables of imperative programs.
***  It constructs the IR for the following program:
***
***
***  main(): int
***    int *a[10];
***
***    a = malloc(sizeof(a[10]));
***    return (a[3]);
***  end;
***
***  The array is placed on the heap.  The pointer to the array that
***  is a local variable is represented as a dataflow edge.
***  There are two ways to model allocation to the heap in programs with
***  explicit memory allocation:
***  1. Model the calls to malloc and free as simple procedure (of compiler
***     known procedures returning a pointer.  This is the simpler way of
***     generating FIRM, but restricts the information that can be deduced
***     for the call.
***  2. Insert an Alloc node.  A later pass can lower this to the compiler
***     known function.  This makes the allocation explicit in FIRM, supporting
***     optimization.
***     A problem is modeling free.  There is no free node in FIRM.  Is this
***     a necessary extension?
***  This example shows the second alternative, where the size of the array
***  is explicitly computed.
**/

#define OPTIMIZE_NODE 0

int
main(void)
{
  /* describes the method main */
  type     *owner;
  type     *proc_main;
  entity   *proc_main_e;

  /* describes types defined by the language */
  type     *prim_t_int;

  /* describes the array and its fields. */
  type     *array_type;   /* the type information for the array */
  entity   *array_ent;    /* the entity representing a field of the array */

  /* Needed while finding the element size.  */
  type     *elt_type;
  ir_mode  *elt_type_mode;
  int       size;
  ir_node  *arr_size;

  /* holds the graph and nodes. */
  ir_graph *main_irg;
  ir_node  *array, *array_ptr, *c3, *elt, *val, *x;

  init_firm ();

  /* make basic type information for primitive type int.
     In Sather primitive types are represented by a class.
     This is the modeling appropriate for other languages.
     Mode_i says that all integers shall be implemented as a
     32 bit integer value.  */
  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_i);

  printf("\nCreating an IR graph: ARRAY-HEAP_EXAMPLE...\n");

  /* first build procedure main */
  owner = get_glob_type();
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
  set_array_element_type(array_type, prim_t_int);
  /* As the array is accessed by Sel nodes, we need information about
     the entity the node selects.  Entities of an array are it's elements
     which are, in this case, integers. */
  array_ent = new_entity((type*)array_type, id_from_str("array_field", 11),
			 (type*)prim_t_int);

  /* Allocate the array. All program known variables that
     are not modeled by dataflow edges need an explicit allocate node.
     If the variable shall be placed on the stack, set stack_alloc.  */
  /*   first compute size in bytes. */
  elt_type = get_array_element_type(array_type);
  elt_type_mode = get_type_mode(elt_type);
  /*   better: read bounds out of array type information */
  size = (U_BOUND - L_BOUND + 1) * get_mode_size(elt_type_mode);
  /*   make constant representing the size */
  arr_size  = new_Const(mode_I, tarval_from_long (mode_I, size));
  /*   allocate and generate the Proj nodes. */
  array     = new_Alloc(get_store(), arr_size, (type*)array_type, stack_alloc);
  set_store(new_Proj(array, mode_M, 0));   /* make the changed memory visible */
  array_ptr = new_Proj(array, mode_p, 1);  /* remember the pointer to the array */

  /* Now the "real" program: */
  /* Load element 3 of the array. For this first generate the pointer to this
     array element by a select node.  (Alternative: increase array pointer
     by (three * elt_size), but this complicates some optimizations. The
     type information accessible via the entity allows to generate the
     pointer increment later. */
  c3 = new_Const (mode_I, tarval_from_long (mode_I, 3));
  {
     ir_node *in[1];
     in[0] = c3;
     elt = new_Sel(get_store(), array_ptr, 1, in, array_ent);
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
  mature_block (get_irg_current_block(main_irg));

  /* complete the end_block */
  add_in_edge (get_irg_end_block(main_irg), x);
  mature_block (get_irg_end_block(main_irg));

  printf("Optimizing ...\n");
  dead_node_elimination(main_irg);

  /* verify the graph */
  irg_vrfy(main_irg);

  printf("Dumping the graph and a type graph.\n");
  dump_ir_block_graph (main_irg);
  dump_type_graph(main_irg);
  printf("use xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (1);
}
