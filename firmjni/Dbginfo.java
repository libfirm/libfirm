package firmjni;

/*
 * Project:     libFIRM / Recoder frontend for libFIRM.
 * File name:   firmjni/Dbginfo.java
 * Purpose:
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     26.2.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

import firmjni.*;

public class Dbginfo {

    // Set env var LD_LIBRARY_PATH accordingly.
    static {
        System.loadLibrary("firmjni");
    }

    public static native void dbgInit ();

    public static void myJavaDbgInfoMergePair(int new_node, int old_node, int info) {
	System.out.println("Optimization: "+ info);
        System.out.println("new Node " + Irnode.getIrnNodeNr(new_node));
        System.out.println("old Node " + Irnode.getIrnNodeNr(old_node));
    }

    public static void myJavaDbgInfoMergeSets(int new_nodes[], int old_nodes[], int info) {
	System.out.println("Optimization: "+ info);
        System.out.print("new Nodes: ");
	for (int i = 0; i < new_nodes.length; i++)
	  System.out.print(Irnode.getIrnNodeNr(new_nodes[i]) + ", ");
	System.out.print("\nold Nodes: ");
	for (int i = 0; i < old_nodes.length; i++)
	  System.out.print(Irnode.getIrnNodeNr(old_nodes[i]) + ", ");
	System.out.println("");
    }
}
