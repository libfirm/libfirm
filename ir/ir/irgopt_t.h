/**
 * Internal irgopt functions.
 */

#ifndef _IRGOPT_T_H
#define _IRGOPT_T_H

//void copy_node (ir_node *n, void *env);
void copy_preds(ir_node *n, void *env);
void firm_copy_node (ir_node *n, void *env);

#endif
