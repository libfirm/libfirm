import firmjni.*;

/* Copyright (C) 2002 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Goetz Lindenmaier
**
** This is an example of how to use the JNI interface of Firm.
**
*/

/**
***  An empty Firm program.
***
***  This file constructs the ir for the following pseudo-program:
***
***  main() {
***    return;
***  }
***
***
**/
class Empty {
    public static void main (String[] args) {
	Firm firm = new Firm();

	System.out.println("\nCreating an IR graph: EMPTY...");

	/* init library */
	Firm.initFirm();

	/** Build type information for the procedure. **/

	/* FIRM was designed for oo languages where all methods beint to a class.
	 * For imperative languages like C we view a file as a large class containing
	 * all functions in this file as methods.
	 * This clas is generated automatically.
	 */
	int owner = Irprog.getGlobType();

	/* The type of the method */
	int name = Ident.idFromStr("main", 4);
	int proc_main = Type.newTypeMethod(name, 0, 0);
	/* An entity representing the method.  Owner of the entity is the global class
	   type mentioned above. */
	int ent = Entity.newEntity (owner, name, proc_main);

	/** Build code for the procedure. **/

	/* Generates the basic graph for the method represented by entity ent, that
	 * is, generates start and end blocks and nodes and a first, initial block.
	 * The constructor needs to know the number of local variables (including
	 * the arguments) in the method.
	 */
	int irg = Irgraph.newIrGraph (ent, 0);

	/* The constructor new_ir_graph() generated a region to place nodes in.
	 * This region is accessible via the attribut current_block of irg and
	 * it is not matured.
	 * Generate the return node into this region. The Return node is needed to
	 * return at least the memory. */
	//ir_node *in[0]; /* this is the array containing the return parameters */
	int []in  = new int[0];
	int x = Ircons.newReturn (Ircons.getStore(), in.length, in);

	/* Test enumerators */
	if (Irnode.getIrnModecode(x) != Irmode.irm_X)
	    System.out.println(" wrong modecode");
	else
	    System.out.println(" proper modecode Execution.");

	/* Now we generated all instructions for this block and all its predecessor
	 * blocks so we can mature it.  (There are not too much.) */
	Ircons.matureBlock (Irgraph.getIrgCurrentBlock(irg));

	/* This adds the in edge of the end block which originates at the return statement.
	 * The return node passes controlflow to the end block.  */
	Ircons.addInEdge (Irgraph.getIrgEndBlock(irg), x);
	/* Now we can mature the end block as all it's predecessors are known. */
	Ircons.matureBlock (Irgraph.getIrgEndBlock(irg));

	/* Verify the graph.  Finds some very bad errors in the graph. */
	//irg_vrfy(irg);
	Ircons.finalizeCons (irg);

	System.out.println("Done building the graph.  Dumping it.");
	Irdump.dumpIrBlockGraph (irg);
	Irdump.dumpAllTypes();


	System.out.println("use xvcg to view this graph:");
	System.out.println("/ben/goetz/bin/xvcg GRAPHNAME\n");
    }
}
