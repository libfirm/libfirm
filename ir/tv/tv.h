/**
 * @file tv.h
 *
 * Declarations for Target Values.
 */

/* $Id$ */

/*
Discussion of new interface, proposals by Prof. Waite:
(email of 13.6.2001)
> 1. You say that you plan to replace the tv module.  That replacement is
>    absolutely essential for an ANSI C translator:  Section 6.1.3.2 of the
>    standard says that the representation of an integer_constant depends
>    upon its value as well as any suffixes that are attached to it.  The
>    possible Firm modes for such a constant are i, I, l, and L.  The
>    current tv module provides only one integer conversion routine, and
>    that requires conversion by the client.  Since the type of the value
>    argument is long, this may preclude the representation of an unsigned
>    long constant.
>
>    There is a similar problem with floating constants.  Floating
>    constants can be suffixed in C, and the mode depends upon the suffix.
>    It can indicate that the constant is of type long double, which your
>    current tv module is incapable of representing.
>
>    Your tv module interface accepts two kinds of information: modes and
>    values.  Values obtained from the program text might be uninterpreted
>    strings, strings interpreted as integers, and strings interpreted as
>    reals.  Values provided by the compiler are usually integers.  Modes are
>    always Firm modes.  It seems to me that the tv module should provide
>    tarval* constructors for three of the four kinds of values.  Each of these
>    constructors should have an ir_mode parameter and one or more parameters
>    appropriate for the kind of value.  As is currently the case, one
>    constructor should be provided for both compiler-generated integers and
>    source strings interpreted as integers.  (This avoids problems of
>    different conversion radices -- the client does the conversion.)  For
>    symmetry, the constructor for source strings interpreted as reals should
>    accept a long double parameter and require the client to do the
>    conversion.

*/

#ifndef _TV_H_
#define _TV_H_

# include "irmode.h"
# include "entity.h"
# include "irnode.h"    /* for pnc_number enum */

/****h* libfirm/tv
 *
 * NAME
 *    tv -- TargetValue, short tarval.
 *   Internal representation for machine values.
 *
 * AUTHORS
 *    Christian von Roques
 *    Matthias Heil
 *
 * DESCRIPTION
 *    Tarvals represent target machine values.  They are typed by modes.
 *   Tarvals only represent values of mode_sort:
 *     int_number,
 *     float_number,
 *     boolean,
 *     reference,
 *     character
 *
 *   In case of references the module accepts an entity to represent the
 *   value.
 *    Furthermore, computations and conversions of these values can
 *   be performed.
 *
 * USES
 *    This module is closely related to the irmode module, as the modes
 *   defined there are thoroughly used throughout the whole module.
 *    Also, the comparison functions rely on the definition of comparison
 *   values in the irnode module.
 *
 * HISTORY
 *    The original tv module originated in the fiasco compiler written ...
 *    This is the new version, described in the tech report 1999-14 by ...
 *
 * SEE ALSO
 *    Techreport 1999-14
 *    irmode.h for the modes definitions
 *    irnode.h for the pnc_numbers table
 *
 *    tarval_init1 and tarval_init2 for initialization of the
 *   module
 *
 ******/

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
  typedef struct tarval tarval;
#endif

/* ************************ Constructors for tarvals ************************ */
/****f* tv/new_tarval_from_str
 *
 * NAME
 *    new_tarval_from_str
 *   Constructor function for new tarvals.
 *
 * SYNOPSIS
 *    tarval *new_tarval_from_str(const char *s, size_t len, ir_mode *mode)
 *
 * DESCRIPTION
 *    This function creates a new tarval representing the value represented
 *   by a CString, aka char array. If a tarval representing this value already
 *   exists, this tarval is returned instead of a new one. So tarvals are
 *   directly comparable since their representation is unique.
 *
 * PARAMETERS
 *   str  - The String representing the target value
 *   len  - The length of the string
 *   mode - The mode requested for the result tarval
 *
 *    This function accepts the following strings:
 *   if mode is int_number:
 *      0(x|X)[0-9a-fA-F]+ (hexadecimal representation)
 *      0[0-7]*            (octal representation)
 *      (+|-)?[1-9][0-9]*  (decimal representation)
 *   if mode if float_number:
 *      (+|-)?(decimal int) (. (decimal int))? ((e|E)(+|-)?(decimal int))?
 *   if mode is boolean: true, True, TRUE ... False... 0, 1,
 *   if mode is reference: hexadecimal of decimal number as int
 *   if mode is character: hex or dec
 *    Leading and/or trailing spaces are ignored
 *
 * RESULT
 *    A tarval of proper type representing the requested value is returned.
 *   Tarvals are unique, so for any value/mode pair at most one tarval will
 *   exist, which will be returned upon further requests with an identical
 *   value/mode pair.
 *
 * NOTES
 *    If the string is not representable in the given mode an assertion is
 *   thrown.
 *
 * SEE ALSO
 *   irmode.h for predefined modes
 *   new_tarval_from_long
 *   new_tarval_from_double
 *
 ******/
tarval *new_tarval_from_str(const char *str, size_t len, ir_mode *mode);

/****f* tv/new_tarval_from_long
 *
 * NAME
 *    new_tarval_from_long
 *   Constructor function for new tarvals
 *
 * SYNOPSIS
 *    tarval *new_tarval_from_long(const long l. ir_mode *mode)
 *
 * DESCRIPTION
 *    This function creates a new tarval representing the value represented
 *   by a long integer. If a tarval representing this value already exists,
 *   this tarval is returned instead of a new one. So tarvals are directly
 *   comparable since their representation is unique.
 *
 * PARAMETERS
 *   l    - The long representing the value
 *   mode - The mode requested for the result tarval
 *
 * RESULT
 *    A tarval of proper type representing the requested value is returned.
 *   Tarvals are unique, so for any value/mode pair at most one tarval will
 *   exist, which will be returned upon further requests with an identical
 *   value/mode pair.
 *
 * NOTES
 *    If the long is not representable in the given mode an assertion is
 *   thrown.
 *
 * SEE ALSO
 *   irmode.h for predefined modes
 *   new_tarval_from_str
 *   new_tarval_from_double
 *
 ******/

tarval *new_tarval_from_long(long l, ir_mode *mode);
/**
 * This returns a long int with the value represented value, or
 * gibberish, depending on the size of long int and the size of the
 * stored value. It works for e.g. 1 as mode_Ls, but not for
 * get_mode_max(mode_Ls).
 * This will overflow silently, so use only if you know what
 * you are doing! (better check with tarval_is_long...)
 */
long tarval_to_long(tarval *tv);
/**
 * This validates if tarval_to_long will return a satisfying
 * result. I.e. if tv is an int_number and between min, max
 * of long int (signed!)
 */
int tarval_is_long(tarval *tv);

/****f* tv/new_tarval_from_double
 *
 * NAME
 *    new_tarval_from_double
 *   Constructor function for new tarvals
 *
 * SYNOPSIS
 *    tarval *new_tarval_from_double(const long double d, ir_mode *mode)
 *
 * DESCRIPTION
 *    This function creates a new tarval representing the value represented
 *   by a long double. If a tarval representing this value already exists,
 *   this tarval is returned instead of a new one. So tarvals are directly
 *   comparable since their representation is unique.
 *
 * PARAMETERS
 *   d    - The long double representing the value
 *   mode - The mode requested for the result tarval
 *
 *    Only modes of sort float_number can be constructed this way.
 *
 * RESULT
 *    A tarval of proper type representing the requested value is returned.
 *   Tarvals are unique, so for any value/mode pair at most one tarval will
 *   exist, which will be returned upon further requests with an identical
 *   value/mode pair.
 *
 * NOTES
 *    If the long double is not representable in the given mode an assertion
 *   is thrown. This will happen for any mode not of sort float_number
 *
 * SEE ALSO
 *   irmode.h for predefined values
 *   new_tarval_from_str
 *   new_tarval_from_long
 *
 ******/
tarval *new_tarval_from_double(long double d, ir_mode *mode);
long double tarval_to_double(tarval *tv);
int tarval_is_double(tarval *tv);
/* The tarval represents the address of the entity.  As the address must
   be constant the entity must have as owner the global type. */
tarval *new_tarval_from_entity (entity *ent, ir_mode *mode);
entity *tarval_to_entity(tarval *tv);
int tarval_is_entity(tarval *tv);

/** ********** Access routines for tarval fields ********** **/

/****f* tv/get_tarval_*
 *
 * NAME
 *   get_tarval_mode
 *   get_tarval_ ...
 *
 * SYNOPSIS
 *   ir_mode *get_tarval_mode(tarval *tv)
 *   ...
 *
 * DESCRIPTION
 *    These are access function for tarval struct members. It is encouraged
 *   to use them instead of direct access to the struct fields.
 *
 * PARAMETERS
 *   tv - The tarval to access fields of
 *
 * RESULT
 *   get_tv_mode: The mode of the tarval
 *
 * SEE ALSO
 *   the struct tarval
 *
 ******/
/* get the mode of the tarval */
#ifdef TARVAL_ACCESS_DEFINES
#  include "tv_t.h"
#  define get_tarval_mode(tv) (tv)->mode
#else
ir_mode *get_tarval_mode (tarval *tv);
#endif
/** Testing properties of the represented values **/
/* Returns 0 if tv is positive, else > 0. @@@ not tested! */
int tarval_is_negative(tarval *a);

/** Some special values **/
extern tarval *tarval_bad;                  tarval *get_tarval_bad(void);
extern tarval *tarval_undefined;            tarval *get_tarval_undefined(void);
/* These two are the only valid mode_b tarvals! */
extern tarval *tarval_b_false;              tarval *get_tarval_b_false(void);
extern tarval *tarval_b_true;               tarval *get_tarval_b_true(void);

extern tarval *tarval_P_void;               tarval *get_tarval_P_void(void);

/* These functions calculate and return a tarval representing the requested
 * value.
 * The functions get_mode_{Max,Min,...} return tarvals retrieved from these
 * functions, but these are stored on initialization of the irmode module and
 * therefore the irmode functions should be prefered to the functions below. */
tarval *get_tarval_max(ir_mode *mode);
tarval *get_tarval_min(ir_mode *mode);
tarval *get_tarval_null(ir_mode *mode);
tarval *get_tarval_one(ir_mode *mode);
tarval *get_tarval_nan(ir_mode *mode);
tarval *get_tarval_inf(ir_mode *mode);

/* ******************** Arithmethic operations on tarvals ******************** */

/****f* tv/tarval_cmp
 *
 * NAME
 *    tarval_cmp
 *   Compares two tarvals
 *
 * SYNOPSIS
 *    pnc_number tarval_comp(tarval *a, tarval *b)
 *
 * DESCRIPTION
 *    Compare a with b and return a pnc_number describing the relation
 *   between a and b.  This is either Uo, Lt, Eq, Gt, or False if a or b
 *   are symbolic pointers which can not be compared at all.
 *
 * PARAMETERS
 *    a - A tarval
 *    b - A tarval
 *   a and b are tarvals to be compared
 *
 * RESULT
 *    The pnc_number best describing the relation between a and b is returned.
 *   This means the mode with the least bits set is returned, e.g. if the
 *   tarvals are equal the pnc_number 'Eq' is returned, not 'Ge' which
 *   indicates 'greater or equal'
 *
 * SEE ALSO
 *    irnode.h for the definition of pnc_numbers
 *
 ******/
pnc_number tarval_cmp(tarval *a, tarval *b);

/****f* tv/tarval_convert_to
 *
 * NAME
 *    tarval_convert_to
 *   Converts a tarval to another mode
 *
 * SYNOPSIS
 *    tarval *tarval_convert_to(tarval *src, ir_mode *mode)
 *
 * DESCRIPTION
 *    Convert tarval 'src' to mode 'mode', this will suceed if and only if mode
 *   'mode' is wider than the mode of src, as defined in the firm documentation
 *   and as returned by the function mode_is_smaller defined in irmode.h.
 *
 * PARAMETERS
 *    src  - The tarval to convert
 *    mode - Tho mode to convert to
 *
 * RESULT
 *    If a tarval of mode 'mode' with the result of the conversion of the 'src'
 *   tarvals value already exists, it will be returned, else a new tarval is
 *   constructed and returned
 *
 * NOTES
 *    Illegal conversations will trigger an assertion
 *
 * SEE ALSO
 *    FIRM documentation for conversion rules
 *    mode_is_smaller defined in irmode.h
 *
 ******/
tarval *tarval_convert_to(tarval *src, ir_mode *m);

/****f* tv/tarval_calculations
 *
 * NAME
 *    tarval_neg  - Negation of a tarval
 *    tarval_add  - Addition of two tarvals
 *    tarval_sub  - Subtraction from a tarval
 *    tarval_mul  - Multiplication of tarvals
 *    tarval_quo  - 'Exact' division
 *    tarval_div  - Integer division
 *    tarval_mod  - Remainder of integer division
 *    tarval_abs  - Absolute value
 *    tarval_and  - Bitwise and
 *    tarval_or   - Bitwise or
 *    tarval_eor  - Bitwise exclusive or
 *    tarval_shl  - Left shift
 *    tarval_shr  - Unsigned right shift
 *    tarval_shrs - Signed right shift
 *    tarval_rot  - Rotation
 *
 * SYNOPSIS
 *    tarval *tarval_neg (tarval *a)
 *    tarval *tarval_add (tarval *a, tarval *b)
 *    tarval *tarval_sub (tarval *a, tarval *b)
 *    tarval *tarval_mul (tarval *a, tarval *b)
 *    tarval *tarval_quo (tarval *a, tarval *b)
 *    tarval *tarval_div (tarval *a, tarval *b)
 *    tarval *tarval_mod (tarval *a, tarval *b)
 *    tarval *tarval_abs (tarval *a)
 *    tarval *tarval_and (tarval *a, tarval *b)
 *    tarval *tarval_or  (tarval *a, tarval *b)
 *    tarval *tarval_eor (tarval *a, tarval *b)
 *    tarval *tarval_shl (tarval *a, tarval *b)
 *    tarval *tarval_shr (tarval *a, tarval *b)
 *    tarval *tarval_shrs(tarval *a, tarval *b)
 *    tarval *tarval_rot (tarval *a, tarval *b)
 *
 * DESCRIPTION
 *    These function implement basic computations representable as opcodes
 *   in FIRM nodes.
 *
 * PARAMETERS
 *    tarval_neg:
 *    traval_abs:
 *      a - the tarval to operate on
 *
 *    all oters:
 *      a - the first operand tarval
 *      b - the second operand tarval
 *
 * RESULT
 *    If neccessary a new tarval is constructed for the resulting value,
 *   or the one already carrying the computation result is retrieved and
 *   returned as result.
 *
 * NOTES
 *    The order the arguments are given in is important, imagine postfix
 *   notation.
 *    Illegal operations will trigger an assertion.
 *    The sort member of the struct mode defines which operations are valid
 *
 ******/
tarval *tarval_neg(tarval *a);             /* negation */
tarval *tarval_add(tarval *a, tarval *b);  /* addition */
tarval *tarval_sub(tarval *a, tarval *b);  /* subtraction */
tarval *tarval_mul(tarval *a, tarval *b);  /* multiplication */
tarval *tarval_quo(tarval *a, tarval *b);  /* floating point division */
tarval *tarval_div(tarval *a, tarval *b);  /* integer division */
tarval *tarval_mod(tarval *a, tarval *b);  /* remainder */
tarval *tarval_abs(tarval *a);             /* absolute value */
tarval *tarval_and(tarval *a, tarval *b);  /* bitwise and */
tarval *tarval_or (tarval *a, tarval *b);  /* bitwise or */
tarval *tarval_eor(tarval *a, tarval *b);  /* bitwise exclusive or (xor) */
tarval *tarval_shl(tarval *a, tarval *b);  /* bitwise left shift */
tarval *tarval_shr(tarval *a, tarval *b);  /* bitwise unsigned right shift */
tarval *tarval_shrs(tarval *a, tarval *b); /* bitwise signed right shift */
tarval *tarval_rot(tarval *a, tarval *b);  /* bitwise rotation */

/** *********** Output of tarvals *********** **/
/****f* tv/tarval_bitpattern
 *
 * NAME
 *    tarval_bitpattern
 *   Bit representation of a tarval value, as string of '0' and '1'
 *
 * SYNOPSIS
 *    char *tarval_bitpattern(tarval *tv)
 *
 * DESCRIPTION
 *    This function returns a printable bit representation of any value
 *   stored as tarval. This representation is a null terminated C string.
 *
 * PARAMETERS
 *    tv - The tarval
 *
 * RESULT
 *    As usual in C a pointer to a char is returned. The length of the
 *   returned string if fixed, just read as many chars as the mode defines
 *   as size.
 *
 * NOTE
 *    The string is allocated using malloc() and is free()ed on the next call
 *    of this function.
 *    The string consists of the ascii characters '0' and '1' and is
 *    null terminated
 *
 * SEE ALSO
 *    irmode.h for the definition of the ir_mode struct
 *    the size member of aforementioned struct
 *
 ******/
char *tarval_bitpattern(tarval *tv);

/**
 * returns bitpattern [from, to[
 */
char *tarval_sub_bitpattern(tarval *tv, int from, int to);

/* Identifying some tarvals ??? */
/* This function is deprecated and its use strongly discouraged */
long tarval_classify(tarval *tv);

/** Initialization of the tarval module **/
void init_tarval_1(void); /* call before init_mode */
void init_tarval_2(void); /* call after init_mode */

#endif  /* _TV_H_ */
