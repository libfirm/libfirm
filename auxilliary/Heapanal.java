//
// Time-stamp: <20.01.2004 14:37:44h liekweg>
//
// $Id$
//

package firmjni;

/**
   <P>Start C-Firm Heap Analysis from Java.</P>

   @version     $Id$
   @since   Mon Jan 19 16:15:41 2004
   @author  F. Liekweg, Universität Karlsruhe (TH), Germany
 */

public class Heapanal
{
    static {
        System.loadLibrary("firmjni");
        System.loadLibrary("heapanal");
    }

  private Heapanal () {}

  public static native void initAnal ();
  public static native void deInitAnal ();

  /** Analyse one method */
  public static native void analHeap (int firmMethod);
  /** Analyse all methods */
  public static native void analHeap ();


  // ToDo:  Zugriff auf die ir_node->ana.... - Felder.

  /* interprete.c:
     typedef struct obset abstval;


    set_irn_abst_value(ir_node *n, abstval *os);
    abstval *get_irn_abst_value(ir_node *n);
  */

}


/*
  Local Variables:
  c-basic-offset: 2
  End:
*/
