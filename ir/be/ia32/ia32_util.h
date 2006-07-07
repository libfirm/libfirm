/**
 * Contains some useful functions for ia32 backend.
 * @author Christian Wuerdig
 * $Id$
 */

#ifndef _IA32_UTIL_H_
#define _IA32_UTIL_H_

/**
 * Returns the first Proj with given mode connected to irn.
 * @param irn  The irn
 * @param First proj with mode == mode or NULL if none found
 */
ir_node *ia32_get_proj_for_mode(const ir_node *irn, ir_mode *mode);

/**
 * Returns the first Proj with mode != mode_M connected to irn.
 * @param irn  The irn
 * @param First proj with mode != mode_M or NULL if none found
 */
ir_node *ia32_get_res_proj(const ir_node *irn);

#endif /* _IA32_UTIL_H_ */
