#ifndef FIRM_ANA_CALL_SITES_H
#define FIRM_ANA_CALL_SITES_H

#include "firm_types.h"
#include "pmap.h"
#include "begin.h"

/**
 * A data structure to access all call sites in the program.
 * Either unstructured, by caller or by callee.
 */

typedef struct call_sites_t {
	pmap *by_caller;
	pmap *by_callee;
	ir_node **all;
} call_sites_t;

/**
 * Allocates a call_sites_t on the heap, initializes it by calling
 * call_sites_init and returns a pointer to it.
 *
 * Users must free the returned memory with call_sites_destroy and free.
 *
 * @return  A pointer to a freshly allocated and initialized call_sites_t*
 */
FIRM_API call_sites_t *call_sites_get(void);

/**
* Initializes the structure that is referenced by given pointer by calling
* call_sites_init and returns its address.
*
* Users must free the returned memory with call_sites_destroy.
*
* @param self The call_sites_t* to initialize
* @return  The given call_sites_t*
*/
FIRM_API call_sites_t *call_sites_init(call_sites_t *self);

/**
 * Free all memory referenced by given call_sites_t*. Users still have to free
 * the memory occupied by the structure itself.
 *
 * @param self The call_sites_t* to destroy
 */
FIRM_API void call_sites_destroy(call_sites_t *self);

/**
 * Returns the number of outgoing calls registered for caller.
 *
 * @param  self   The instance of call_sites_t to operate on
 * @param  caller The irg to get the number of outgoing calls for
 * @return        The number of outgoing calls registered for caller
 */
FIRM_API size_t call_sites_get_n_calls_from(const call_sites_t *self,
                                            const ir_graph *caller);

/**
 * Return the pos-th outgoing call registered for caller.
 *
 * @param  self   The instance of call_sites_t to operate on
 * @param  caller The irg that issues the requested call
 * @param  pos    The index of the registered call
 * @return        The requested call
 */
FIRM_API ir_node *call_sites_get_call_from(const call_sites_t *self,
                                           const ir_graph *caller, size_t pos);

/**
 * Returns the array of outgoing calls registered for caller.
 *
 * @param  self   The instance of call_sites_t to operate on
 * @param  caller Tne irg to get the list of outgoing calls for
 * @return        The array of outgoing calls registered for caller
 */
FIRM_API ir_node **call_sites_get_calls_from(const call_sites_t *self,
                                             const ir_graph *caller);

/**
 * Returns the number of incoming calls for callee.
 *
 * @param  self   The instance of call_sites_t to operate on
 * @param  callee The irg to get the number of incoming calls for
 * @return        The number of calls issued in caller .
 */
FIRM_API size_t call_sites_get_n_calls_to(const call_sites_t *self,
                                          const ir_graph *callee);

/**
 * Return the pos-th incoming call registered for callee.
 *
 * @param  self   The instance of call_sites_t to operate on
 * @param  callee The irg that is target of the requested call
 * @param  pos    The index of the registered call
 * @return        The requested call
 */
FIRM_API ir_node *call_sites_get_call_to(const call_sites_t *self,
                                         const ir_graph *callee, size_t pos);

/**
 * Returns the array of incoming calls registered for callee.
 *
 * @param  self   The instance of call_sites_t to operate on
 * @param  callee Tne irg to get the list of incoming calls for
 * @return        The array of incoming calls registered for callee
 */
FIRM_API ir_node **call_sites_get_calls_to(const call_sites_t *self,
                                           const ir_graph *callee);

// TODO relocate; this is more utility than that it belongs here
FIRM_API ir_graph *call_site_get_callee_irg(const ir_node *call);

/**
 * Register the given call with this instance of call_sites_t.
 *
 * Useful for keeping an instance of call_sites_t up to date, e.g. when
 * creating new calls.
 *
 * @param self The instance of call_sites_t to operate on
 * @param call The call to add to this call_sites_t
 */
FIRM_API void call_sites_register_call(call_sites_t *self, ir_node *call);

/**
 * Register all calls in given irg with this instance of call_sites_t.
 *
 * Useful for keeping an instance of call_sites_t up to date, e.g. when
 * creating new irgs.
 *
 * @param self The instance of call_sites_t to operate on
 * @param irg  The irg that should be searched for calls
 */
FIRM_API void call_sites_register_irg_calls(call_sites_t *self, ir_graph *irg);

#define foreach_call_from(self, caller, call)                                  \
	ARR_FOREACH_ITEM (call_sites_get_calls_from((self), (caller)), ir_node *,  \
	                  call)

#define foreach_call_to(self, callee, call)                                    \
	ARR_FOREACH_ITEM (call_sites_get_calls_to((self), (callee)), ir_node *,    \
	                  call)

#include "end.h"

#endif
