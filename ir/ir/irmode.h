/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
 * All rights reserved.
 */
/* $Id$ */

/**
 * @file irmode.h
 *    irmode -- Modes for ir operators
 *
 * @author Christian Schaefer, Matthias Heil
 *
 * This module specifies the modes that type the firm nodes.  It defines
 * a datasturcture that describes a mode and implements constructors and
 * access routines to this datastructure. Further it defines a set of
 * predefined modes.
 *
 * SEE ALSO:
 *    UKA tech report 1999-44 for more information about modes.
 *
 */
#ifndef _IRMODE_H_
#define _IRMODE_H_

#include "ident.h"

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
  typedef struct tarval tarval;
#endif

/**
 * Contains relevant information about a mode.
 *
 * Neccessary information about a mode is stored in this struct
 * which is used by the tarval module to perform calculations
 * and comparisons of values of a such described mode.
 *
 * ATTRIBUTES:
 *  -  modecode code:           An unambigous int (enum) for the mode
 *  -  ident *name:             Name of this mode. Two modes are different if the name is different.
 *  -  mode_sort sort:          sort of mode specifying possible usage kategories
 *  -  int    size:             size of the mode in Bits.
 *  -  int    align:            byte alignment
 *  -  unsigned sign:1:         signedness of this mode
 *  -  ... more to come
 *
 * SEE ALSO:
 *    The tech report 1999-44 describing FIRM and predefined modes
 *    tarval.h
 */
typedef struct ir_mode ir_mode;

/* ********** Predefined modes ********** */

/**
 * Predefined mode according to tech report 1999-14.
 */
typedef enum { /* irm is short for `ir mode' */
  irm_BB,                       /**< basic block */
  irm_X,                        /**< execution */
  irm_F,                        /**< float(32) */
  irm_D,                        /**< double(64) */
  irm_E,                        /**< extended(80) */
  irm_Bs,                       /**< signed byte(8) */
  irm_Bu,                       /**< unsigned byte(8) */
  irm_Hs,                       /**< signed short(16) */
  irm_Hu,                       /**< unsigned short(16) */
  irm_Is,                       /**< signed int(32) */
  irm_Iu,                       /**< unsigned int(32) */
  irm_Ls,                       /**< signed long(64) */
  irm_Lu,                       /**< unsigned long(64) */
  irm_C,                        /**< character */
  irm_P,                        /**< pointer */
  irm_b,                        /**< internal boolean */
  irm_M,                        /**< memory */
  irm_T,                        /**< tuple */
  irm_U,                        /**< unicode character */
  irm_ANY,                      /**< undefined mode */
  irm_BAD,                      /**< bad mode */
  irm_max                       /**< maximum value for modecode */
} modecode;

/** These values represent the different mode classes of value representations.
 */
typedef enum {
  /* Predefined sorts of modes */
  irms_auxiliary,         /**< Only for Firm use. Not extensible. (irm_T) */
  irms_control_flow,       /**< Marks all control flow modes. Not extensible. (irm_BB, irm_X) */
  irms_memory,             /**< Marks the memory mode.  Not extensible. (irm_M) */
  irms_internal_boolean,  /**< Internal boolean representation.
		               Storing to memory impossible, convert first. (irm_b) */
                            /** user-extensible sorts of modes **/
  irms_int_number,        /**< A mode to represent int numbers.
		               Integer computations can be performed. */
  irms_float_number,      /**< A mode to represent float numbers.
		               Floating point computations can be performed. */
  irms_reference,         /**< A mode to represent entities.
		               Restricted int computations can be performed */
  irms_character          /**< A mode to represent characters/symbols
		               ?? Are computations allowed? as int?? */
} mode_sort;

/** These values represent the different arithmetic operations possible with a mode.
    Further arithmetics can be defined, e.g., for @@@ modes.
 */
typedef enum {
  irma_uninitialized = 0,
  irma_none = 1,              /**< For modes for which no representation is specified.
				   These are modes of sort auxiliary, internal_boolean and
				   character. */
  irma_twos_complement = 2,   /**< Values of the mode are represented as two's complement.
				   Only legal for modes of sort int_number and reference. */
  irma_ones_complement,       /**< Values of the mode are represented  as one's complement.
			           Only legal for modes of sort int_number and reference. */
  irma_int_BCD,               /**< Values of the mode are represented as binary coded decimals.
			           Only legal for modes of sort int_number and reference. */
  irma_ieee754 = 256,         /**< Values of the mode are represented according to ieee754
				   floatingpoint standard.  Only legal for modes of sort float_number. */
  irma_float_BCD,             /**< Values of the mode are represented  as binary coded decimals
				   according to @@@ which standars??? Only legal for modes of
				   sort float_number. */
  irma_max
} mode_arithmetic;


/* ********** Constructor for user defined modes **************** */
/**
 * Creates a new mode.
 *
 * @param name		the name of the mode to be created
 * @param sort		the mode_sort of the mode to be created
 * @param bit_size	number of bits this mode allocate
 * @param align		the byte alignment for an entity of this mode (in bits)
 * @param sign		non-zero if this is a signed mode
 * @param arithmetic    arithmetic operations possible with a mode
 *
 * This function constructs a new mode given by the parameters.
 * If the parameters match an already defined mode, this mode is returned
 * (including the default modes).
 * If the mode is newly allocated, a new unique mode_code is choosen.
 * Also, special value tarvals will be calculated such as null,
 * min, max and can be retrieved using the get_mode_* fuctions
 *
 * @return
 * 	The new mode or NULL on error.
 *
 * @note
 * 	It is allowed to construct the default modes. So, a call
 * 	new_ir_mode("Is", irms_int_number, 32, 4, 1) will return mode_Is.
 */
ir_mode *new_ir_mode(const char *name, mode_sort sort, int bit_size, int align, int sign, mode_arithmetic arithmetic);

/**
 *   Checks whether a pointer points to a mode.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is a mode, else false
 */
int is_mode (void *thing);

/* ********** Access methods to read mode information *********** */

/** Returns the classification of the mode */
modecode get_mode_modecode(const ir_mode *mode);

/** Returns the ident* of the mode */
ident *get_mode_ident(const ir_mode *mode);

/** Returns the null-terminated name of this mode. */
const char *get_mode_name(const ir_mode *mode);

/** Returns a coarse classification of the mode. */
mode_sort get_mode_sort(const ir_mode *mode);

/** Returns the size of values of the mode in bits. */
int get_mode_size_bits(const ir_mode *mode);

/** Returns the size of values of the mode in bytes.  If the size is not
    dividable by 8 returns -1. */
int get_mode_size_bytes(const ir_mode *mode);

/** Returns the alignment of values of the mode in bytes. */
int get_mode_align(const ir_mode *mode);

/** Returns the signess of a mode */
int get_mode_sign (const ir_mode *mode);

/** Returns the arithmetic of a mode */
int get_mode_arithmetic (const ir_mode *mode);

/** Returns the stored intermediate information. */
void* get_mode_link(const ir_mode *mode);

/** Stores new intermediate information. */
void set_mode_link(ir_mode *mode, void *l);

/**
 * Returns the smallest representable value of a given mode.
 *
 * For modes of the sort float_number this is the most negative value
 * bigger than -infinit.
 */
tarval *get_mode_min(ir_mode *mode);

/**
 * Returns the biggest representable value o f a given mode.
 *
 * For modes of the sort float_number this is the largest value lower
 * than infinit.
 */
tarval *get_mode_max(ir_mode *mode);

/**
 * Returns the value Zero represented in this mode.
 *
 * Zero is the additive neutral element and as such
 * is defined only for modes allowing addition, i.e.
 * floats and ints, and references (NULL-Pointer)
 * else returns tarval_bad.
 */
tarval *get_mode_null(ir_mode *mode);

/**
 * Returns the value One, represented in this mode.
 *
 * One, being the multiplicative neutral element,
 * is defined only for modes allowing multiplication,
 * i.e. ints and floats.
 */
tarval *get_mode_one(ir_mode *mode);

/**
 * Returns the positive infinite value of a mode.
 *
 * This is only valid for float_numbers, other modes
 * will result in tarval_bad.
 */
tarval *get_mode_infinite(ir_mode *mode);

/**
 * Returns the NAN value of a given mode.
 *
 * This is only valid for float_numbers, other modes
 * will result in tarval_bad.
 */
tarval *get_mode_NAN(ir_mode *mode);

/* -- Auxiliary modes necessary for the Firm representation -- */
extern ir_mode *mode_T;  /**< tuple (none) */
extern ir_mode *mode_X;  /**< execution */
extern ir_mode *mode_M;	 /**< memory */
extern ir_mode *mode_BB; /**< block */

/* -- A set of predifined, numerical modes according to Techreport 1999-44 -- */
extern ir_mode *mode_F;	 /**< signed float(32) */
extern ir_mode *mode_D;  /**< signed double(64) */
extern ir_mode *mode_E;  /**< signed extended(80) */
extern ir_mode *mode_Bs; /**< signed byte (former char) */
extern ir_mode *mode_Bu; /**< unsigned byte (former char) */
extern ir_mode *mode_Hs; /**< signed short integer */
extern ir_mode *mode_Hu; /**< unsigened short integer */
extern ir_mode *mode_Is; /**< signed integer */
extern ir_mode *mode_Iu; /**< unsigned integer */
extern ir_mode *mode_Ls; /**< signed long integer */
extern ir_mode *mode_Lu; /**< unsigned long integer */

extern ir_mode *mode_b;  /**< internal boolean */
extern ir_mode *mode_C;  /**< 8 bit char */
extern ir_mode *mode_U;  /**< 16 bit unicode char */
extern ir_mode *mode_P;  /**< pointer */

extern ir_mode *mode_ANY;/**< undefined mode */
extern ir_mode *mode_BAD;/**< bad mode */

/*@{*/
/** Access routines for JNI Interface */
ir_mode *get_modeT(void);
ir_mode *get_modeF(void);
ir_mode *get_modeD(void);
ir_mode *get_modeE(void);
ir_mode *get_modeBs(void);
ir_mode *get_modeBu(void);
ir_mode *get_modeHs(void);
ir_mode *get_modeHu(void);
ir_mode *get_modeIs(void);
ir_mode *get_modeIu(void);
ir_mode *get_modeLs(void);
ir_mode *get_modeLu(void);
ir_mode *get_modeC(void);
ir_mode *get_modeU(void);
ir_mode *get_modeP(void);
ir_mode *get_modeb(void);
ir_mode *get_modeX(void);
ir_mode *get_modeM(void);
ir_mode *get_modeBB(void);
ir_mode *get_modeANY(void);
ir_mode *get_modeBAD(void);

/**
   Functions to check, whether a modecode is signed, float, int, num, data,
   datab or dataM.

   For more exact definitions read the corresponding pages
   in the firm documentation or the following enumeration

   The set of "float" is defined as:
   float = {irm_F, irm_D, irm_E}

   The set of "int" is defined as:
   int   = {irm_Bs, irm_Bu, irm_Hs, irm_Hu, irm_Is, irm_Iu, irm_Ls, irm_Lu}

   The set of "num" is defined as:
   num   = {irm_F, irm_D, irm_E, irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu}
            = {float || int}

   The set of "data" is defined as:
   data  = {irm_F, irm_D, irm_E irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_P}
            = {num || irm_C || irm_P}

   The set of "datab" is defined as:
   datab = {irm_F, irm_D, irm_E, irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_P, irm_b}
            = {data || irm_b }

   The set of "dataM" is defined as:
   dataM = {irm_F, irm_D, irm_E, irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_P, irm_M}
            = {data || irm_M}
*/
/*@}*/
/* Test for a certain class of modes. */
int mode_is_signed (const ir_mode *mode);
int mode_is_float (const ir_mode *mode);
int mode_is_int (const ir_mode *mode);
int mode_is_character (const ir_mode *mode);
int mode_is_reference (const ir_mode *mode);
int mode_is_num (const ir_mode *mode);
int mode_is_data (const ir_mode *mode);
int mode_is_datab (const ir_mode *mode);
int mode_is_dataM (const ir_mode *mode);

/** Returns true if sm can be converted to lm without loss
   according to firm definiton */
int smaller_mode(const ir_mode *sm, const ir_mode *lm);

/** mode module initialization, call once before use of any other function **/
void init_mode (void);

#endif /* _IRMODE_H_ */
