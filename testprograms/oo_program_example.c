/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** testprogram.
*/

# include "irdump.h"
# include "firm.h"

/**  This file constructs the IR for the following program:
***
***  class PRIMA
***    a: int;
***    b: real;
***    c(int): int;
***    set_a(int): void;
***  end
***
***  c(d: int): int
***    return (d + a);
***  end
***
***  set_a(e:int): void
***    self.a = e;
***  end
***
***  main(): int
***    o: PRIMA;
***    o.set_a(2);
***    return o.c(5);
***  end
***
**/

#define OPTIMIZE_NODE 0

int
main(void)
{
  type_primitive *prim_t_int;
  type_class   *owner, *class_prima, *class_langint;
  type_method  *proc_main, *proc_set, *proc_c;
  type_pointer *class_p_ptr;
  entity       *proc_main_e, *proc_set_e, *proc_c_e, *a_e;

  ir_graph     *main_irg, *set_a_irg, *c_irg;
  ir_node      *c2, *c5, *obj_o, *obj_size, *proc_ptr, *call, *x;
  ir_node      *self, *par1, *a_ptr;
  ir_node      *a_val;

  int i;

  init_firm ();

  set_opt_constant_folding(0);
  set_opt_cse(0);

  /* make basic type information for primitive type int.*/
  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_i);

  /* first build procedure main */
  printf("creating an IR graph: OO_PROGRAM_EXAMPLE...\n");
  owner = get_glob_type();
  proc_main = new_type_method(id_from_str("main", 4), 0, 1);
  set_method_res_type(proc_main, 0, (type *)prim_t_int);

  proc_main_e = new_entity ((type *)owner, id_from_str ("main", 4),
                            (type *)proc_main);
  main_irg = new_ir_graph (proc_main_e, 4);
  /* Remark that this irg is the main routine of the program. */
  set_irp_main_irg(main_irg);

  /* There is only one block in main, it contains the constants and the calls. */
  c2 = new_Const (mode_i, tarval_from_long (mode_i, 2));
  c5 = new_Const (mode_i, tarval_from_long (mode_i, 5));

  /* allocate the defined object and generate the type information */
  class_prima = new_type_class(id_from_str ("PRIMA", 5));
  obj_size = new_SymConst((type_or_id *)class_prima, size);
  obj_o    = new_Alloc(get_store(), obj_size, (type *)class_prima, heap_alloc);
  set_store(new_Proj(obj_o, mode_M, 0));  /* make the changed memory visible */
  obj_o    = new_Proj(obj_o, mode_p, 1);  /* remember the pointer to the object */
  /* we need type information for pointers to the class: */
  class_p_ptr = new_type_pointer (id_from_str ("class_prima_ptr", 15),
				  (type *)class_prima);

  /* get the pointer to the procedure from the class type */
  proc_set = new_type_method(id_from_str("set_a", 5), 2, 0);
  set_method_param_type(proc_set, 0, (type *)class_p_ptr);
  set_method_param_type(proc_set, 1, (type *)prim_t_int);
  proc_set_e = new_entity((type *)class_prima, id_from_str ("set_a", 5),
			  (type*)proc_set);
  proc_ptr = new_simpleSel(get_store(),  /* The memory the object is allocated in */
   		      obj_o,             /* The pointer to the object */
		      proc_set_e );      /* The feature to select */

  /* call procedure set_a, first built array with parameters */
  {
    ir_node *in[2];
    in[0] = obj_o;
    in[1] = c2;
    call = new_Call(get_store(), proc_ptr, 2, in, proc_set);
  }
  /* make the change to memory visible */
  set_store(new_Proj(call, mode_M, 0));

  /* get the pointer to the procedure from the class type */
  proc_c   = new_type_method(id_from_str("c", 1 ), 2, 1);
  set_method_param_type(proc_c, 0, (type *)class_p_ptr);
  set_method_param_type(proc_c, 1, (type *)prim_t_int);
  set_method_res_type(proc_c, 0, (type *)prim_t_int);
  proc_c_e = new_entity((type *)class_prima, id_from_str ("c", 1),
			(type*)proc_c);
  proc_ptr = new_simpleSel(get_store(), obj_o, proc_c_e);

  /* call procedure c, first built array with parameters */
  {
    ir_node *in[2];
    in[0] = obj_o;
    in[1] = c5;
    call = new_Call(get_store(), proc_ptr, 2, in, proc_c);
  }
  /* make the change to memory visible */
  set_store(new_Proj(call, mode_M, 0));

  /* return the results of procedure main */
  {
     ir_node *in[1];
     /* Select the result tuple from the call, then the proper
        result from the tuple. */
     in[0] = new_Proj(new_Proj(call, mode_T, 1), mode_I, 0);

     x = new_Return (get_store (), 1, in);
  }
  mature_block (main_irg->current_block);

  /* complete the end_block */
  add_in_edge (main_irg->end_block, x);
  mature_block (main_irg->end_block);

  printf("\nDone building the graph.\n");
  irg_vrfy(main_irg);

  /****************************************************************************/

  printf("\ncreating IR graph for set_a: \n");

  set_a_irg = new_ir_graph (proc_set_e, 4);

  /* get the procedure parameter */
  self = new_Proj(set_a_irg->args, mode_p, 0);
  par1 = new_Proj(set_a_irg->args, mode_I, 1);
  /* Create and select the entity to set */
  class_langint = new_type_class(id_from_str ("Int", 3));
  a_e = new_entity((type *)class_prima, id_from_str ("a", 1),
		   (type*)class_langint);
  a_ptr = new_simpleSel(get_store(), self, a_e);
  /* perform the assignment */
  set_store(new_Proj(new_Store(get_store(), a_ptr, par1), mode_M, 0));

  /* return nothing */
  x = new_Return (get_store (), 0, NULL);
  mature_block (set_a_irg->current_block);

  /* complete the end_block */
  add_in_edge (set_a_irg->end_block, x);
  mature_block (set_a_irg->end_block);

  printf("\nDone building the graph.\n");
  irg_vrfy(set_a_irg);

  /****************************************************************************/

  printf("\ncreating IR graph for c: \n");

  c_irg = new_ir_graph (proc_c_e, 4);

  /* get the procedure parameter */
  self = new_Proj(c_irg->args, mode_p, 0);
  par1 = new_Proj(c_irg->args, mode_I, 1);

  /* Select the entity and load the value */
  a_ptr = new_simpleSel(get_store(), self, a_e);
  a_val = new_Load(get_store(), a_ptr);
  set_store(new_Proj(a_val, mode_M, 0));
  a_val = new_Proj(a_val, mode_I, 1);

  /* return the result */
  {
    ir_node *in[1];
    in[0] = new_Add(par1, a_val, mode_I);

    x = new_Return (get_store (), 1, in);
  }
  mature_block (c_irg->current_block);

  /* complete the end_block */
  add_in_edge (c_irg->end_block, x);
  mature_block (c_irg->end_block);

  /* verify the graph */
  irg_vrfy(main_irg);

  printf("\nDone building the graph.\n");

  printf("Dumping graphs of all procedures.\n");

  for (i = 0; i < get_irp_n_irgs(); i++) {
    dump_ir_block_graph (get_irp_irg(i));
    dump_type_graph(get_irp_irg(i));
  }

  /****************************************************************************/

  printf("\nuse xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (1);
}
