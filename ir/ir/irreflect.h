/**
 * @file reflect.h
 * @date 9.9.2004
 * @author Sebastian Hack
 * @brief Reflection for Firm operations.
 *
 * $Id$
 */

#ifndef __REFLECT_H
#define __REFLECT_H

#include <limits.h>
#include <stdbool.h>

#include "irop.h"
#include "irnode.h"

#define RFLCT_MC(m) rflct_ms_ ## m
typedef enum {
  RFLCT_MC(None)  = 0,
  RFLCT_MC(Mem)   = 2,
  RFLCT_MC(Bool)  = 4,
  RFLCT_MC(IntS)  = 8,
  RFLCT_MC(IntU)  = 16,
  RFLCT_MC(Float) = 32,
  RFLCT_MC(Ref)   = 64,
  RFLCT_MC(Char)  = 128,
  RFLCT_MC(X)     = 256,
  RFLCT_MC(BB)    = 512,
  RFLCT_MC(Cf)    = RFLCT_MC(X) | RFLCT_MC(BB),

  RFLCT_MC(Int) = RFLCT_MC(IntS) | RFLCT_MC(IntU),
  RFLCT_MC(Intb) = RFLCT_MC(Int) | RFLCT_MC(Bool),
  RFLCT_MC(Num) = RFLCT_MC(Int) | RFLCT_MC(Float),
  RFLCT_MC(NumP) = RFLCT_MC(Num) | RFLCT_MC(Ref),
  RFLCT_MC(Data) = RFLCT_MC(NumP) | RFLCT_MC(Char),
  RFLCT_MC(Datab) = RFLCT_MC(Data) | RFLCT_MC(Bool),
  RFLCT_MC(DataM) = RFLCT_MC(Data) | RFLCT_MC(Mem),
  RFLCT_MC(DataMX) = RFLCT_MC(Data) | RFLCT_MC(Mem) | RFLCT_MC(X),
  RFLCT_MC(Lh) = RFLCT_MC(Mem) | RFLCT_MC(BB),

  RFLCT_MC(Any) = -1

} rflct_mode_class_t;

typedef struct _rflct_arg_t {
  const char *name;  /**< The name of the argument (just a description). */

  bool is_variadic; /**< True, if this argument can have multiple parameters. */
  rflct_mode_class_t accepted_modes; /**< The set of accepted modes. */

  int mode_equals; /**< If not variadic: You can specify the index of
			another argument meaning, that the mode of the
			operand binding at this argument must be the same
			as the mode of the operand binding to the argument
			at index. If you don't want to express such a
			dependency, just give -1 here.

			If variadic: If true, the modes of all
			variadic operands binding to this argument
			must be the same. If false, they can differ. */
} rflct_arg_t;

typedef struct _rflct_sig_t {
	int defs; /**< The number of defined arguments in the signature. */
	int uses; /**< The number of used arguments in the signature. */

	rflct_arg_t *args; /**< The arguments array. */
} rflct_sig_t;

#define RFLCT_ARG_IS_A(arg,modes) (((arg)->accepted_modes & modes) != 0)
#define RFLCT_ARG_VALID(arg) ((arg)->name != NULL)
#define RFLCT_SIG_VALID(sig) ((sig) != INT_MAX)

/**
 * Get the mode class for an IR mode.
 * @param mode An IR mode.
 * @return The corresponding smallest reflection mode class.
 */
rflct_mode_class_t rflct_get_mode_class(const ir_mode *mode);

/**
 * Get the number of signatures for a Firm opcode.
 * @param opc The opcode.
 * @return The number of signatures for this opcode.
 */
int rflct_get_signature_count(opcode opc);

/**
 * Try to get the signature, that matches to a given instance
 * of a Firm node.
 * @param irn The node.
 * @return The first matching signature or -1, if no signature matches.
 */
int rflct_get_signature(ir_node *irn);

/**
 * Get the number of in arguments.
 * An in argument is a use of a value.
 * @param opc The opcode.
 * @param sig The signature you are refering to.
 * @return The number of arguments.
 */
int rflct_get_in_args_count(opcode opc, int sig);

/**
 * Get the number of out arguments.
 * An out argument is a def of a value.
 * @param opc The opcode.
 * @param sig The signature you are refering to.
 * @return The number of arguments.
 */
int rflct_get_out_args_count(opcode opc, int sig);

/**
 * Get the array of use args.
 * The array is terminated with an entry for which
 * <code>RFLCT_ARG_VALID</code> is 0.
 * @param opc The opcode.
 * @param sig The signature you are referring to (Must be between
 * 0 and the signature count).
 * @return The array.
 */
const rflct_arg_t *rflct_get_in_args(opcode opc, int sig);

/**
 * Get the array of def args.
 * The array is terminated with an entry for which
 * <code>RFLCT_ARG_VALID</code> is 0.
 * @param opc The opcode.
 * @param sig The signature you are referring to (Must be between
 * 0 and the signature count).
 * @return The array.
 */
const rflct_arg_t *rflct_get_out_args(opcode opc, int sig);

/**
 * Make a string representation of a signature of an opcode.
 * @param buf The buffer to put the string to.
 * @param n The size of buf.
 * @param opc The opcode.
 * @param sig The signature.
 * @return buf.
 */
char *rflct_to_string(char *buf, int n, opcode opc, int sig);

/**
 * Get a string representation of a mode class.
 * @param str The buffer to put the string to.
 * @param n The size of the buffer.
 * @param mc The mode class.
 * @return str.
 */
char *rflct_mode_class_name(char *str, int n, rflct_mode_class_t mc);

/**
 * Create a new opcode.
 * @param opc The Firm opcode.
 * @param name A name.
 * @param commutative True, if the opcode is commuatative.
 */
void rflct_new_opcode(opcode opc, const char *name, bool commutative);

/**
 * Add a signature to the opcode.
 * @param opc The opcode.
 * @param args The signature.
 * @return true, if the signature was added sucessfully, false if no
 * more signatures can be added to the opcode.
 */
bool rflct_opcode_add_signature(opcode opc, rflct_sig_t *sig);

/**
 * Allocate a new signature.
 * @param defs Number of def arguments.
 * @param uses Number of use arguments.
 * @return An allocated signature.
 */
rflct_sig_t *rflct_signature_allocate(int defs, int uses);

/**
 * Set an argument in a signature.
 * @param sig The signature.
 * @param is_use true, if the argument is a use, else it is considered a
 * def.
 * @param num The index of the argument.
 * @param name The name of the argument.
 * @param mc The mode class of the argument.
 * @param is_variadic true, if the argument is variadic.
 * @param mode_equals This variable has following meaning: If the
 * argument is variadic, a 1 indicates that all operands binding to this
 * argument must have the same mode. A 0 indicates, that their mode must
 * be of the specified mode class but can differ. If the argument is non
 * variadic, a positive number indicates, that the mode of the operand
 * binding to this argument must be the same as the one binding to
 * argument indexed by mode_equals. If the mode should not be dependent
 * to another mode, set -1 here.
 * @return The index of the argument. Only use this index in mode_equals
 * parameters of other arguments.
 */
int rflct_signature_set_arg(rflct_sig_t *sig, bool is_use, int num,
		const char *name, rflct_mode_class_t mc, bool is_variadic, int mode_equals);

/**
 * Get the arguments array index for an argument.
 * @param sig The signature.
 * @param is_use True, if the argument indicates a use or def argument.
 * @param num The number of the argument.
 * @return The index into the arguments array.
 */
int rflct_signature_get_index(const rflct_sig_t *sig, bool is_use, int num);


#endif
