package firmjni;

/*
 * Project:     libFIRM / Recoder frontend for libFIRM.
 * File name:   firmjni/Dbginfo.java
 * Purpose:
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     26.2.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

import firmjni.*;

public class Dbginfo {

  public Dbginfo (java.lang.Object file, int line)
  {
    _file = file;
    _line = line;
  }

  // Set env var LD_LIBRARY_PATH accordingly.
  static {
    System.loadLibrary("firmjni");
    dbgInit ();
  }

  public static native void dbgInit ();

  /** Return the dbg object index of the given node. A new dbg object is created, if needed. */
  public static native int getDbgInfoIdx (int node);

  /** Return the dbg object index of the given node, or <TT>-1</TT> if none has been set. */
  public static native int doGetDbgInfoIdx (int node);

  /** Return the dbg object of the given node, or <TT>null</TT> if none has been set. */
  public static Dbginfo getDbgInfo (int node)
  {
    int idx = doGetDbgInfoIdx (node);

    if (-1 == idx) {
      return (null);
    }

    return (_infos [idx]);
  }

  public static void setDbgInfo (int node, java.lang.Object file, int line)
  {
    int idx = doGetDbgInfoIdx (node);

    if (_infos.length < idx) {
      Dbginfo [] infos = new Dbginfo [idx+1];

      for (int i = 0; i < _infos.length; i ++) {
        infos [i] = _infos [i];
      }

      _infos = infos;
    }

    _infos [idx] = new Dbginfo (file, line);
  }

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

  /**
     <P>Return the file this debug info object is from:</P>
  */
  public java.lang.Object getFile ()
  {
    return (_file);
  }

  /**
     <P>Return which line this debug info object is from:</P>
  */
  public int getLine ()
  {
    return (_line);
  }

  private java.lang.Object _file;
  private int _line;

  private static Dbginfo [] _infos = new Dbginfo [1000];
}
