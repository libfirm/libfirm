/*
 * Project:     libFIRM
 * File name:   firmjni/testprograms/Empty.java
 * Purpose:     This is an example of how to use the JNI interface of Firm.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

import firmjni.*;

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

	System.out.println("\nCreating an IR graph: EMPTY...");

	/* init library: Java did not support the callback, so ALWAYS use 0 here */
	Firm.initFirm(0);

	/** Build type information for the procedure. **/

	/* FIRM was designed for oo languages where all methods belong to a class.
	 * For imperative languages like C we view a file as a large class containing
	 * all functions in this file as methods.
	 * This class is generated automatically.	 */
	int owner = Irprog.getGlobType();

	/* The type of the method */
	int name = Ident.idFromStr("EMPTY_main", 10);
	int proc_main = Type.newTypeMethod(name, 0, 0);
	/* An entity representing the method.  Owner of the entity is the global
	   class type mentioned above. */
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
	int []in  = new int[0];
	int x = Ircons.newReturn (Ircons.getStore(), in.length, in);

	/* Now we generated all instructions for this block and all its predecessor
	 * blocks so we can mature it.  (There are not too much.) */
	Ircons.matureBlock (Irgraph.getIrgCurrentBlock(irg));

	/* This adds the in edge of the end block which originates at the return statement.
	 * The return node passes controlflow to the end block.  */
	Ircons.addInEdge (Irgraph.getIrgEndBlock(irg), x);
	/* Now we can mature the end block as all it's predecessors are known. */
	Ircons.matureBlock (Irgraph.getIrgEndBlock(irg));

	/* Verify the graph.  Finds some very bad errors in the graph. */
	Irvrfy.irgVrfy(irg);
	Ircons.finalizeCons (irg);

	System.out.println("Done building the graph.  Dumping it.");
	Irdump.dumpIrBlockGraph (irg);
	Irdump.dumpAllTypes();


	System.out.println("use xvcg to view this graph:");
	System.out.println("/ben/goetz/bin/xvcg GRAPHNAME\n");
    }
}
