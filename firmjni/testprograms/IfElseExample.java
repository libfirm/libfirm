/*
 * Project:     libFIRM
 * File name:   firmjni/testprograms/IfElseExample.java
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
*  This file constructs the ir for the following pseudo-program:
*
*  main() {
*    int a = 0;
*    int b = 1;
*
*    if (a > 2)
*      { a = b; }
*    else
*      { b = 2; }
*
*    return a, b;
**/

class IfElseExample {

    public static void main (String[] args) {

	System.out.println("\nCreating an IR graph: IfElseExample...");

	/* init library */
	Firm.initFirm(0);
	Dbginfo.dbgInit();

	/** Build type information for the compilation unit. **/

	/* FIRM was designed for oo languages where all methods belong to a class.
	 * For imperative languages like C we view a file as a large class
	 * containing all functions in this file as methods.
	 * This class is generated automatically.	 */
	int owner = Irprog.getGlobType();

	/* Basic type information for primitive type int. */
	int primIntTypeName = Ident.newIdFromStr("int");
	int primIntType = Type.newTypePrimitive(primIntTypeName, Irmode.getModeIs());

	/* The type of the method */
	int tpName = Ident.newIdFromStr("IF_ELSE_EXAMPLE_main_p");
	int procMain = Type.newTypeMethod(tpName, 0, 2);
	Type.setMethodResType(procMain, 0, primIntType);
	Type.setMethodResType(procMain, 1, primIntType);

	/* An entity representing the method.  Owner of the entity is the global
	    class type mentioned above. */
	int name = Ident.newIdFromStr("IF_ELSE_EXAMPLE_main");
	int ent = Entity.newEntity (owner, name, procMain);

	/** Build code for the procedure. **/

	int irg = Irgraph.newIrGraph (ent, 2);

	/* Generate two constants */
	int c0 = Ircons.newConst (Irmode.getModeIs(), Tv.newTarvalFromLong (0, Irmode.getModeIs()));
	int c1 = Ircons.newConst (Irmode.getModeIs(), Tv.newTarvalFromLong (1, Irmode.getModeIs()));

	/* Set a and b to constants */
	Ircons.setValue (0, c0);  /* this (0) is variable a */
	Ircons.setValue (1, c1);  /* this (1) is variable b */

	/* The expression that evaluates the condition */
	int c2 = Ircons.newConst(Irmode.getModeIs(),
				 Tv.newTarvalFromLong (2, Irmode.getModeIs()));
	int cmpGt = Ircons.newProj(Ircons.newCmp(Ircons.getValue(0, Irmode.getModeIs()), c2),
				   Irmode.getModeb(), Irnode.Gt);
	int x = Ircons.newCond (cmpGt);
	int f = Ircons.newProj (x, Irmode.getModeX(), 0); /* if condition is false */
	int t = Ircons.newProj (x, Irmode.getModeX(), 1); /* if condition is true */

	Ircons.matureImmBlock (Irgraph.getIrgCurrentBlock(irg));

	/* generate and fill the then block */
	int b = Ircons.newImmBlock ();
	Ircons.addImmBlockPred (b, t);
	Ircons.setValue (0, Ircons.getValue(1, Irmode.getModeIs()));
	Ircons.matureImmBlock (b);
	int x_then = Ircons.newJmp ();

	/* generate and fill the else block */
	b = Ircons.newImmBlock ();
	Ircons.addImmBlockPred (b, f);
	Ircons.setValue (1, Ircons.newConst (Irmode.getModeIs(),
					     Tv.newTarvalFromLong (2, Irmode.getModeIs())));
	Ircons.matureImmBlock (b);
	int x_else = Ircons.newJmp ();

	/* generate the join block and add all cfg edges */
	b = Ircons.newImmBlock ();
	Ircons.addImmBlockPred (b, x_then);
	Ircons.addImmBlockPred (b, x_else);

	int[] in = new int[2]; /* this is the array containing the return parameters */
	in[0] = Ircons.getValue(0, Irmode.getModeIs());
	in[1] = Ircons.getValue(1, Irmode.getModeIs());
	x = Ircons.newReturn (Ircons.getStore(), 2, in);

	/* Now generate all instructions for this block and all its predecessor
	   blocks so we can mature it. */
	Ircons.matureImmBlock (Irgraph.getIrgCurrentBlock(irg));

	/* This adds the in edge of the end block which originates at the
	   return statement.  The return node passes control flow to the
	   end block.  */
	Ircons.addImmBlockPred (Irgraph.getIrgEndBlock(irg), x);
	/* Now we can mature the end block as all it's predecessors are known. */
	Ircons.matureImmBlock (Irgraph.getIrgEndBlock(irg));

	Irvrfy.irgVrfy(irg);
	Ircons.finalizeCons (irg);

	System.out.println("Done building the graph.  Optimizing it.");
	Irgopt.localOptimizeGraph(irg);
	Irgopt.deadNodeElimination(irg);

	Irdump.dumpIrBlockGraph (irg, "");
	Irdump.dumpAllTypes("");

	System.out.println("use xvcg to view this graph:");
	System.out.println("/ben/goetz/bin/xvcg GRAPHNAME\n");

    }
}
